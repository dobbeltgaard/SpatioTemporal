
rm(list = ls())
library(readxl)
library(stringr)
library(plyr); library(dplyr)
library(tibble)
source("99_translations_and_naming_convetions.R")

#path = "C:/Users/atbd/COWI/A235142 - Predictive maintanance ph.d project (1502) - Documents/Data/BDK/"
path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
#path = "C:/Users/askbi/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
#path = "D:/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"


##################
### UT DEFECTS ###
##################

UT_files = list.files(paste0(path, "BDK_UT"), full.names = T)
UT_files = UT_files[grepl("Defects", UT_files)]
mat = matrix(NA, nrow = length(UT_files), ncol = 60)
for(i in 1:length(UT_files)){
  f = read_excel(UT_files[i])
  mat[i, 1:length(names(f))] = names(f) 
  if(i == 1){ ff = f} else { ff = rbind(ff,f) }
}

cols.remove = 
  c("Event (Time)",  "UT-operatør", "GPS", "Svejsernr.","Svejsning lbnr.","Journal nr.",
    "B-scan nummers","Flere over 3 sveller","Hovedbredde forøget","Fejlbeskrivelse",
    "Fejlens status","Målt håndholdt","Lasket fejl","Valseværk","Måned","Højdeslid",
    "Sideslid","Indpasser","Svejsningens nummer","Fejlen kan fjernes ved","Sikret med lasker",
    "Sikret ved LA","Defect class","Isolerklæbestød","Fejl udbedret","FejlenKanSikresMed",
    "clDate","closedBy", "Gammel fejlIDs")
ff = ff[,!names(ff) %in% cols.remove] #remove irrelevant columns
ff = ff[!is.na(ff$FejlID), ] #remove NaNs
ff = ff[!grepl("^5000", as.character(ff$FejlID)), ] #remove defects starting with 5000
ff = ff[!ff$`Under grænsen` == "Yes", ]
ff = ff[!ff$`Fejl fundet i` == "Ingen fejl", ]

#TRANSLATION
names(ff) = translate_strings(names(ff))
fff = ff
fff[, c("Rail_string","Defect_found_in","Track_type","Visible","Combined_defect","Under_limit","Curvature","State")] = 
  apply(ff[,c("Rail_string","Defect_found_in","Track_type","Visible","Combined_defect","Under_limit","Curvature","State")], 2, translate_strings)
fff = fff[!apply(X = fff[ ,c("BTR", "Track", "From", "To", "Rail_string")],1, function(x) any(is.na(x))), ] #remove NaNs in important columns

d <- as.data.frame(fff)
d$Defect_ID2 = NA
for(i in 1:NROW(d)){
  check = (d$BTR == d$BTR[i] & d$Track == d$Track[i] & d$From == d$From[i] &d$To == d$To[i] & d$Rail_string == d$Rail_string[i] & d$Defect_ID != d$Defect_ID[i])
  if(sum(check) > 0){
    d$Defect_ID2[i] = paste0(d[c(i,which(check)), "Defect_ID"],collapse=" ")
  }
}
d$Defect_ID3 = NA
for(i in 1:NROW(d)){
  if(!is.na(d$Defect_ID2[i])) d$Defect_ID3[i] = min(as.numeric(unlist(strsplit(d$Defect_ID2[i], " ")))) #use minimum defect number for match cycles
}
d$Defect_ID3[is.na(d$Defect_ID3)] = d$Defect_ID[is.na(d$Defect_ID3)]

IDs = unique(d$Defect_ID3) #collect defect IDs
IDs = IDs[!is.na(IDs)] #remove nans

defect_set = list()
for(i in 1:length(IDs)){
  idx = d$Defect_ID3 == IDs[i]
  foo = d[idx,]
  x = split(foo,list(foo$Rail_string,foo$Defect_found_in), drop=TRUE) #split to unique defects, so rail string and defect_found_in surely matches
  
  for(j in 1:NROW(x)){
    if(NROW(x) < 2){ nam = toString(IDs[i])} else {nam = paste0(sprintf("%.0f", 100000000),toString(IDs[i]),j) }# defect_set[[toString(IDs[i])]] = x[[1]][order(x[[1]]$Date),]; next} #x[[1]]; next} #if data correct, then no splitting is needed
    foo2 = x[[j]][order(x[[j]]$Date),]
    
    if(NROW(foo2) > 1){
      idxfoo2 = rep(T, NROW(foo2));  
      if(any(head(foo2$State == "Removed",-1)) & !all(foo2$State == "Removed")){ idxfoo2[foo2$State == "Removed"] = F; idxfoo2[length(idxfoo2)]=T;} #filter out observations with "Removed" under state, excect from endpoint
      foo2 = foo2[idxfoo2, ]
      if(sum(idxfoo2) > 1){ #if more than 1 obs is left, then check for time differences
        idx2foo2 = rep(T, NROW(foo2));
        idx2foo2 = outer(foo2$Date, foo2$Date, FUN = difftime, units = "days")[,1] > 30 #only obs with bigger distance than 30 days
        idx2foo2[!idx2foo2][which.max(foo2$Date[!idx2foo2])] = T
        foo2 = foo2[idx2foo2, ]
      }
    }
    foo2$Maintenance_date = NA
    foo2$Maintenance_type = NA
    foo2$Amount = NA
    defect_set[[nam]] = foo2
  }
}
names(defect_set) = as.numeric(names(defect_set))

#save(defect_set, file = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_without_maintenance.RData")
load("C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_without_maintenance.RData")

#################################################
### COMBINE DEFECT DATA WITH MAINTENANCE DATA ###
#################################################
main = read.csv(file = "rail_maintenance.csv")
for(i in 1:length(defect_set)){
  if(NROW(defect_set[[i]])>1){ #if there are more than one observation
    idx = defect_set[[i]]$BTR[1] == main$BTR & defect_set[[i]]$Track[1] == main$Track
    cond1 = defect_set[[i]]$From[1] >= main$From[idx] & defect_set[[i]]$From[1] <= main$To[idx]
    cond2 = defect_set[[i]]$To[1] >= main$From[idx] & defect_set[[i]]$To[1] <= main$To[idx]
    foo = main[idx,][cond1 | cond2,]
    if(NROW(foo)>0){
      for(j in 1:NROW(foo)){
        cond3 = foo$Time[j] > defect_set[[i]]$Date  
        cond4 = foo$Time[j] < defect_set[[i]]$Date
        if(any(cond3) & any(cond4)){ #if maintenance is between 
          defect_set[[i]][which(cond4)[1],c("Maintenance_date","Maintenance_type","Amount")] =
            foo[j,c("Time", "action","removed_mm") ]
        }
      }
    }
  }
}
#save(defect_set, file = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_with_maintenance.RData")

# COMBINE with static data. Tonnage. Speed. Curveture.


M = matrix(NA, nrow = length(defect_set), ncol = 5)
for(i in 1:length(defect_set)){
  foo = defect_set[[i]]
  M[i,1] = NROW(foo)
  if(NROW(foo) < 2){M[i,2] = F;  M[i,3] = F; M[i,4] = F; } 
  M[i,2] = foo$UIC[length(foo$UIC)]
  M[i,3] = any(!is.na(foo$Maintenance_date))
  }



table(M[,1])
table(M[,2])
table(M[,3])


which(M[,1] ==3)



which(M[,3] == 1)

defect_set[[3749 ]]


#incorporate maintenance


#Before sending to Xiaokun: Only send defects with more than one obs. Remember to remove unneeded columns for better readability.



length(defect_set)
## This defect_set could function as a database over defects.
## Then functions could be written for instance to: 
# - taburalize defect counts based on UIC, or spatial distribution, etc.
# - create defect sets for specific BTR or spatial/temporal periods
# - create spatial-temporal mesh to combine with network measurement data
# - create dataset for paired defects so that crack propagation model can be made 
# consider to write one function, that calls the defect set, and then 
# manipulates data so that the preferred format comes out. 










