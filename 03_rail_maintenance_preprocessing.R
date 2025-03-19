rm(list = ls()) #clear memory

library(readxl) #Package for reading data stored in excel files
path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK"
foldernam = "Asset Data" #Folder name
files = list.files(file.path(path,foldernam), full.names = T) #List files in working directory


###################
### Reprofiling ###
###################


### reprofiling

substr = "maintenance_" #Substring to search for in file names
ffiles = files[grepl(substr, files)] #Find files with "Defects" in the name
#fffiles = paste0(foldernam, ffiles) #Print the files found
dm = as.data.frame(readxl::read_excel(ffiles[1]))


substr = "maintenance ma" #Substring to search for in file names
ffiles = files[grepl(substr, files)] #Find files with "Defects" in the name
machin = as.data.frame(readxl::read_excel(ffiles[1]))

machin$Navn = c("Milling", "Milling (HS)", "Grinding", "Grinding (HS)", "Planing")

dm[dm$Slibetog == "Speno_RPS32-1 (old)" | dm$Slibetog == "Speno_RPS32-1 (old2)", "Slibetog"] = "Speno_RPS32-1"
dm[dm$Slibetog == "AlphaRailTeam_SF 03 FFS (old)", "Slibetog"] = "AlphaRailTeam_SF 03 FFS"
dm[dm$Slibetog == "Unknown", "Slibetog"] = "Speno_RPS32-1" #All unknowns have comment with "slibning" mentioned, so just choosing the typical slibtog
dm[dm$Slibetog == "Speno_RR1455/1205", "Slibetog"] = "Loram_SPML16"
#View(dm)
trains = unique(dm$Slibetog)
train_mat = matrix(NA, nrow = length(trains), ncol = NROW(machin))

for(i in 1:length(trains)){
  train_mat[i,] = grepl(trains[i], machin$Betingelse)
}

train_mat2 = which(train_mat, arr.ind = T)[order(which(train_mat, arr.ind = T)[,1] ),]

for(i in 1:length(trains)){
  idx = as.numeric(train_mat2[i,2])
  train_mat2[i,1] = machin$Navn[idx]
}
mat = cbind(trains, train_mat2[,1])


#function to extract grinded or milled milimeters from comments
extract_mm <- function(input_string) {
  # Regular expression to match the number before "mm"
  regex <- "\\b([0-9]+(?:[.,][0-9]+)?)\\s*mm\\b"
  
  # Extract the matched number
  match <- regmatches(input_string, regexpr(regex, input_string))
  
  # If a match is found, return the first match
  if (length(match) > 0) {
    # Remove spaces and replace comma with dot for proper numeric conversion
    number <- gsub(",", ".", gsub(" ", "", match))
    return(as.numeric(sub("mm", "", number)))
  } else {
    return(NA)
  }
}

#function to extract number of grinding stones given slibetog
find_numeric_after_underscore <- function(input_string) {
  # Find the position of underscore
  underscore_index <- regexpr("_", input_string)
  
  # If underscore not found or found at the end of the string, return NA
  if (underscore_index == -1 || underscore_index == nchar(input_string)) {
    return(NA)
  }
  
  # Extract substring after the underscore
  substring_after_underscore <- substr(input_string, underscore_index + 1, nchar(input_string))
  
  # Find numeric numbers occurring uninterrupted after the occurrence of "_"
  numbers <- regmatches(substring_after_underscore, regexpr("\\d+", substring_after_underscore))
  
  # Convert to numeric vector
  numbers <- as.numeric(numbers)
  
  # If no numeric values are found, return NA
  if (length(numbers) == 0 || all(is.na(numbers))) {
    return(NA)
  }
  
  # Return the first numeric value found
  return(numbers[1])
}

##Determine what type of reprofiling
dm$stone = NA
dm$action = NA
dm$amount = NA
for(i in 1:NROW(dm)){
  idx = which(dm$Slibetog[i] == mat[,1])
  dm$action[i] = mat[idx,2]
  dm$amount[i] = extract_mm(dm$`Bemærkninger til slib`[i])
  dm$stone[i] = find_numeric_after_underscore(dm$Slibetog[i])
}

dm[dm$action != "Grinding", "stone"] = NA

dm = dm[, c("BTR", "Spor", "Event (Date)", "From", "To", "Formål", "Passages","stone", "action", "amount")]
colnames(dm) = c("BTR", "Spor", "Time", "From", "To", "Reason", "Passages", "stone","action", "amount")


###If observations occur more than once per day, accumulate passages
dm2 = matrix(NA, nrow = 0, ncol = NCOL(dm))
colnames(dm2) = colnames(dm)
dm2 = data.frame(dm2)

checked = rep(F, NROW(dm))
for(i in 1:NROW(dm)){
  idx = which(!checked &
                rowSums(as.vector(dm[i, c("BTR", "Spor", "Time", "From", "To", "action")]) == dm[ , c("BTR", "Spor", "Time", "From", "To", "action")]) == 6
  ) 
  if(length(idx) > 1){
    foo = dm[idx[1], ]
    foo$Passages = sum(dm[idx, "Passages"], na.rm = T)
    dm2 = rbind(dm2, foo)
  } else if(!checked[i]) {
    dm2 = rbind(dm2, dm[i,])
  }
  checked[idx] = T
}

dm2$removed_mm[dm2$action == "Grinding"] = 0.13*dm2$stone[dm2$action == "Grinding"]/32*dm2$Passages[dm2$action == "Grinding"]
dm2$amount[is.na(dm2$amount)] = 0
dm2[dm2$action == "Milling" & dm2$amount > 10, "amount"] = 1.5
dm2$removed_mm[dm2$action == "Milling"] = 1.5*dm2$Passages[dm2$action == "Milling"]
dm2$removed_mm[dm2$action == "Grinding (HS)"] = 0.03*dm2$Passages[dm2$action == "Grinding (HS)"]

dm2$BTR <- as.numeric(ifelse(substr(dm2$BTR, 1, 1) == "0", substring(dm2$BTR, 2), dm2$BTR)) #omit the first zero in BTR
colnames(dm2)[2] = "Track"

write.csv(dm2, file="rail_maintenance.csv", row.names = F)
#saveRDS(dm2, file="rail_maintenance.Rda")
