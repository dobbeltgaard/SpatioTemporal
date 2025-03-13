
rm(list = ls())
library(readxl)
library(stringr)
library(dplyr)

path = "C:/Users/atbd/COWI/A235142 - Predictive maintanance ph.d project (1502) - Documents/Data/BDK/"
#path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
#path = "C:/Users/askbi/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
#path = "D:/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"


asset.files = list.files(paste0(path, "Asset Data"), full.names = T)

#Read superstructure
super.file = asset.files[which(grepl("Superstructure",asset.files))]
R <- read_excel(super.file)
R$BTRn = as.numeric(R$BTR)

#Read curvature
curve.file = asset.files[which(grepl("urvature",asset.files))]
C <- read_excel(curve.file)

#Read loading
load.file = asset.files[which(grepl("load",asset.files))]
L <- read_excel(load.file)

#Read Speeds
speed.file = asset.files[which(grepl("speed",asset.files))]
S <- read_excel(speed.file)

#Read turnover segments
turnover.file = asset.files[which(grepl("segments_turnouts",asset.files))]
SC <- read_excel(turnover.file)
split_vec <- strsplit(gsub(" ", "", SC$`BTR-Spor`), "-")
left_part <- sapply(split_vec, `[`, 1)
right_part <- sapply(split_vec, `[`, 2)
SC$BTR = left_part
SC$Spor = right_part
SC$BTRn = as.numeric(SC$BTR)



####################################
### CREATE NESTED DATA STRUCTURE ###
####################################
RosRing = NULL
BTRs = as.vector(unique(R[R$BTRn >= 14000 & R$BTRn <= 14058,"BTR"]))$BTR #loop through these. Will happen later.
#btr = "014034" #"014043" # # #We start by looking at one BTR. Later, we should loop through BTRs.
#BTRs = as.numeric(BTRs)
for(btr in BTRs){
  R1 = R[R$BTR == btr,] #BTR level 
  tracks = unique(R1$Spor)
  for(trk in tracks){
    tonnage = as.data.frame((L[L$BTR == btr & L$Spor == trk, c("MGT_min", "MGT_max", "EMGT_min", "EMGT_max")]))
    tonnage$date = (L[L$BTR == btr & L$Spor == trk, c("Event (Date)")])$`Event (Date)`
    tonnage = tonnage[, order(names(tonnage))]
    tonnage = tonnage[order(tonnage$date, decreasing = F),]
    
    R2 = R1[R1$Spor == trk, ]
    C1 = C[C$BTR == btr,] #find curved segments
    C2 = C1[C1$Spor == trk, ]
    S2 = S[S$BTR == btr & S$Spor == trk,]
    SC2 = SC[SC$BTR == btr & SC$Spor == trk,]
    
    
    #calculate curvature and overheight
    curvature <- gsub(".*\\] ", "", C2$Curvature)
    curvature <- gsub("[{}]", "", curvature) #as.data.frame(do.call(rbind, str_match_all(curvature, "(-?\\d+\\.?\\d*)")))
    curvature <- matrix(as.numeric(unlist(strsplit(curvature, ";"))), ncol=2,byrow = 1)
    curvature <- apply(curvature, 1, mean)
    C2$curve = curvature
    overheight <- gsub(".*\\] ", "", C2$`Overhøjde [mm]`)
    overheight <- gsub("[{}]", "", overheight) #as.data.frame(do.call(rbind, str_match_all(curvature, "(-?\\d+\\.?\\d*)")))
    overheight <- matrix(as.numeric(unlist(strsplit(overheight, ";"))), ncol=2,byrow = 1)
    overheight <- apply(overheight, 1, mean)
    C2$overheight = overheight
    
    #create mesh
    mesh = sort(unique(c(C2$Fra, R2$Fra, C2$Til,R2$Til)), decreasing = F)
    X = as.data.frame(unique(cbind(fra = head(mesh,-1), til = head(mesh,-1)+diff(mesh))))
    
    X$seg_type = NA; X$curve = NA; X$overheight = NA; X$track_type = NA
    X$rail_left_type = NA; X$rail_right_type = NA; X$steel_left = NA; X$steel_right = NA
    X$rail_left_age = as.Date(NA); X$rail_right_age = as.Date(NA); X$sleepers_fastening = NA
    X$sleepers_age = as.Date(NA); X$ballast_type = NA; X$ballast_age = as.Date(NA)
    X$speed = NA; X$SC = NA
    
    R2 = as.data.frame(R2); C2 = as.data.frame(C2); S2 = as.data.frame(S2); SC2 = as.data.frame(SC2)
    for(i in 1:NROW(X)){
      ### Get superstructure data ###
      idx = sapply(R2$Fra, FUN = max, X$fra[i]) < sapply(R2$Til, FUN = min, X$til[i]) #necessary and sufficient condition for overlap
      X[i, c("track_type","sleepers_fastening","sleepers_age","ballast_type","ballast_age")] =
        (R2[which(idx), ][1,c( "Sportype","Sveller: Overbygningstype",
                               "Sveller: Ibrugtagningsdato","Ballast: Type","Ballast: Ibrugtagningsdato")])
      X[i, c("rail_left_type","steel_left","rail_left_age")] = 
        R2[idx & R2$`Skinne-streng` == "Venstre", c("Skinne: Type","Skinne: Stålkvalitet", "Skinne: Ibrugtagningsdato")]
      X[i, c("rail_right_type","steel_right","rail_right_age")] =
        R2[idx & R2$`Skinne-streng` == "Højre", c("Skinne: Type","Skinne: Stålkvalitet", "Skinne: Ibrugtagningsdato")]
      
      ### Get curvature data ###
      idx = sapply(C2$Fra, FUN = max, X$fra[i]) < sapply(C2$Til, FUN = min, X$til[i]) #necessary and sufficient condition for overlap
      if(!any(idx)){
        X[i, c("seg_type", "curve", "overheight")] = c("Straight", 0, 0)
      } else { X[i, c("seg_type", "curve", "overheight")] = C2[idx, c("DK_type_element", "curve", "overheight")] }
      
      ### Get Speed data ###
      idx = sapply(S2$Fra, FUN = max, X$fra[i]) < sapply(S2$Til, FUN = min, X$til[i]) #necessary and sufficient condition for overlap
      X[i, c("speed")] = S2[idx, c("Line speed [km/h]")][1]
      
      ###  Get S&C data ###
      idx = sapply(SC2$From, FUN = max, X$fra[i]) < sapply(SC2$To, FUN = min, X$til[i]) #necessary and sufficient condition for overlap
      if(any(idx)){ X[i, c("SC")] = T } else {X[i, c("SC")] = F}
    }
    
    RosRing[[btr]][[trk]] = list(
      tonnage = tonnage,
      statics = X, 
      meas = list()
    )
  }
}
names(RosRing) = as.numeric(names(RosRing))


###################################################
### ALL VARIABLES AND THEIR SPATIAL RESOLUTIONS ###
###################################################
destination = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/database"

PATTERNS = c("corr", "vrid", "hojd_L","hojd_R","overhojd", "wear", "rail", "side", "spor")
var.idx = apply(sapply(PATTERNS, FUN = grepl, x = list.files(path), ignore.case = T),MARGIN = 1, FUN = any)
variables = data.frame( name = c(list.files(path)[var.idx], unique(sub("_Campaign.*", "", list.files(paste0(path,"BDK_Noise_all"))))), 
                        resolution = NA, n_files = NA)

all.all.files = list.files(destination, recursive = T, full.names = T)

for(i in 1:NROW(variables)){
  files.of.variable = all.all.files[grepl(variables$name[i], all.all.files, ignore.case = T)]
  variables$n_files[i] = length(files.of.variable)
  x = read.csv(tail(files.of.variable,1), sep = ",") 
  #names(x) = c("BTR", "Spor", "fra", "til", "date", "x")
  variables$resolution[i] = as.numeric(quantile(abs(diff(x$fra)), 0.5))
  print(paste("Loop", i, "has resolution", as.numeric(quantile(abs(diff(x$fra)), 0.5))))
}
sum(variables$n_files)

allowed_resolutions <- c(0.00025, 0.001)
variables$resolution <- sapply(variables$resolution, function(x) {
  allowed_resolutions[which.min(abs(allowed_resolutions - x))]
})
variables


#####################################################
### SPLIT MEASUREMENT DATA TO BTR SEPARATED FILES ###
#####################################################


destination = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/database"
#destination = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/database"
#destination = "C:/Users/ATBD/Downloads/database"

list.files(destination)

btr_range = c(14000,14058)

#04.03.25: RAN THE LOOP FROM i = 3:10; took roughly 35 min in average per iteration
#05.03.25: RAN THE LOOP FROM i = 11:15; took roughly 1 hour and 20 minutes in average per iteration
#06.03.25: RAN THE LOOP FROM i = 16:27; 
#06.03.25: RAN THE LOOP FROM i = 28:34; 
for(i in 35:35){
  
  files.of.variable = all.all.files[grepl(variables$name[i], all.all.files, ignore.case = T)]
  
  dir_path = file.path(destination, variables$name[i])
  if (!dir.exists(dir_path)) { dir.create(dir_path, showWarnings = FALSE) }
  
  start_time <- Sys.time() #start timer
  for(j in 1:length(files.of.variable)){ #loops through all campaigns for variable i
    date_nam = sub(".*Campaign_", "",files.of.variable[j])
    
    x = read.csv(files.of.variable[j], sep = ";")
    names(x) = c("BTR", "Spor", "fra", "til", "date", "x")
    BTRs = unique(x$BTR)
    chosenBTRs = BTRs[BTRs>=btr_range[1] & BTRs<=btr_range[2]]
    if(length((chosenBTRs)) == 0) next
    x = x[x$BTR %in% chosenBTRs,]
    
    for(btr in chosenBTRs){
      idx = x$BTR == as.numeric(btr) #extract rows with corresponding btr
      nam = paste0("BTR_",btr,"_",variables$name[i], "_Campaign_", date_nam) #construct name
      if(! nam %in% list.files(file.path(destination, variables$name[i]))){ #if not already saved, then create new file
        write.csv(x[idx, ],file = file.path(destination, variables$name[i],nam), row.names=FALSE)
      }
      x = x[!idx, ] #remove extracted rows for better speeds
    }
  }
  end_time <- Sys.time() #stop timer
  elapsed_time <- end_time - start_time
  print(paste("Variable =", i, "::: Runtime =", elapsed_time))
}


#remember to check if the correct data are pasted in the correct files


############################################
##### COLLECT IN NESTED DATA STRUCTURE #####
############################################

### NOTE: After all measurement files for a pattern are run through, 
### a version of the data structure should be saved to a folder.

convert_to_numeric <- function(vec) {
  if (is.numeric(vec)) {
    return(vec)  # Return as is if already numeric
  } else {
    return(as.numeric(gsub(",", ".", vec)))  # Convert string to numeric
  }
}


destination = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/database"
folders = list.files(destination)

for(i in 1:length(folders)){
  start_time <- Sys.time() #stop timer
  varnam = folders[i] #variable name
  files = list.files(file.path(destination, folders[i]), full.names = T) #files of that variables  
  res = variables[variables$name == varnam,"resolution"] #get resolution for file
  
  for(j in 1:length(files)){
    btr=sub(".*BTR_(\\d+).*", "\\1", files[j]) #btr number
    x = read.csv(files[j], sep = ","); x_entire = x; #read x
    
    for(trk in names(RosRing[[btr]])){#get different tracks
      x = x_entire; #reinstanziate x
      idx = x$Spor == trk #get track indices
      if(sum(idx) == 0) next #print(paste("Check 1. Skipping", varnam, "BTR:", btr, "Track:", trk)); next
      x = x[idx, ]
      pos1 = head(RosRing[[btr]][[trk]]$statics$fra,1) #range of btr and track
      pos2 = tail(RosRing[[btr]][[trk]]$statics$til,1) - res
      mesh = seq(pos1, pos2, by = res) #make mesh
      
      #does x cover the the interval (pos1, pos2)
      cond1 = any(abs(x$fra-pos1) <= res)
      cond2 = any(abs(x$fra-pos2) <= res)
      if(cond1 & cond2){ # x covers the interval (pos1, pos2)
        idx1 = which.min(abs(x$fra-pos1))
        idx2 = which.min(abs(x$fra-pos2))
      } else if(cond1 | cond2){ # x covers some of the interval (pos1, pos2)
        val1 = max(min(x$fra),pos1)
        val2 = min(max(x$fra),pos2)
        idx1 = which.min(abs(x$fra-val1))
        idx2 = which.min(abs(x$fra-val2))
      }
      if(!(cond1 | cond2)) next #print(paste("Check 2. Skipping", varnam, "BTR:", btr, "Track:", trk)); next
      if(idx2 < idx1) next #print(paste("Check 3. Skipping", varnam, "BTR:", btr, "Track:", trk)); next
        
      x = x[idx1:idx2, ]
      x <- x[order(x$fra), ]
      x <- x[!duplicated(x$fra), ]  # Remove duplicate rows based on 'fra'
      x$x = convert_to_numeric(x$x)
      
      if(nrow(x) < 2 || all(is.na(x$x))) next #print(paste("Check 4. Skipping", varnam, "BTR:", btr, "Track:", trk)); next
      
      #Compute gaps and find large gaps
      gaps = diff(x$fra)
      gap_idx = which(gaps >= (2*res))
      
      #Interpolation with NA for large gaps
      interp_values = approx(x = x$fra, y = x$x, xout = mesh, method = "linear", rule = 2)
      meas = data.frame(pos = mesh, x = interp_values$y)
      
      #Set NA for large gaps
      for (idx in gap_idx) {
        # Find positions in 'mesh' that fall within the large gap
        missing_mask = (meas$pos > x$fra[idx]) & (meas$pos < x$fra[idx + 1])
        meas$x[missing_mask] = NA  # Assign NA to those positions
      }
      colnames(meas) = c("pos", x$date[1])
      
      #stack measurements nested list structure
      if(is.null(RosRing[[btr]][[trk]]$meas[[varnam]])){ #if there is no previous measurements
        RosRing[[btr]][[trk]]$meas[[varnam]] = meas
      } else{
        RosRing[[btr]][[trk]]$meas[[varnam]] = cbind(RosRing[[btr]][[trk]]$meas[[varnam]],meas[,2])
        colnames(RosRing[[btr]][[trk]]$meas[[varnam]])[NCOL(RosRing[[btr]][[trk]]$meas[[varnam]])] = x$date[1]
      }
    } 
  }
  end_time <- Sys.time() #stop timer
  elapsed_time <- end_time - start_time
  print(paste("Variable", varnam, "took",elapsed_time))
}


#save(RosRing, file = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/RosRing.RData")



#load(file="C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/RosRing.RData")
#load(file = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/RosRing.RData")


btr = names(RosRing)[1]
trk = names(RosRing[[btr]])[2]
names(RosRing[[btr]][[trk]])

x = RosRing[[btr]][[trk]]$meas




