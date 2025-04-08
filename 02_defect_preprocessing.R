
rm(list = ls())
library(readxl)
library(stringr)
library(plyr); library(dplyr)
library(tibble)
source("99_translations_and_naming_convetions.R")

#path = "C:/Users/atbd/COWI/A235142 - Predictive maintanance ph.d project (1502) - Documents/Data/BDK/"
#path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
path = "C:/Users/askbi/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
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
    foo2$Passages = NA
    defect_set[[nam]] = foo2
  }
}
names(defect_set) = as.numeric(names(defect_set))

#save(defect_set, file = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set.RData")

### this data set should be loaded into the spatio-temporal network! ###


#################################################
### COMBINE DEFECT DATA WITH MAINTENANCE DATA ###
#################################################
rm(list = ls())
load("C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_without_maintenance.RData")
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
          defect_set[[i]][which(cond4)[1],c("Maintenance_date","Maintenance_type","Amount", "Passages")] =
            foo[j,c("Time", "action","removed_mm", "Passages") ]
        }
      }
    }
  }
}
## save(defect_set, file = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_with_maintenance.RData")

#############################################################
#### COMBINE with static data. Tonnage. Speed. Curveture. ###
#############################################################
## rm(list = ls())
## library(readxl)
## source("99_translations_and_naming_convetions.R")
## load("C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_with_maintenance.RData")

path = "C:/Users/askbi/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
asset.files = list.files(paste0(path, "Asset Data"), full.names = T)

#Read superstructure
super.file = asset.files[which(grepl("Superstructure",asset.files))]
R <- read_excel(super.file)
R$BTRn = as.numeric(R$BTR)
R$Sportype = translate_strings(R$Sportype) 
R$`Skinne-streng` = translate_strings(R$`Skinne-streng`) 
R$`Skinne: Type` = translate_strings(R$`Skinne: Type`)

#Read curvature
curve.file = asset.files[which(grepl("urvature",asset.files))]
C <- read_excel(curve.file)
C$DK_type_element = translate_strings(C$DK_type_element) 
curvature <- gsub(".*\\] ", "", C$Curvature)
curvature <- gsub("[{}]", "", curvature) #as.data.frame(do.call(rbind, str_match_all(curvature, "(-?\\d+\\.?\\d*)")))
curvature <- matrix(as.numeric(unlist(strsplit(curvature, ";"))), ncol=2,byrow = 1)
curvature <- apply(curvature, 1, mean)
C$curve = curvature
overheight <- gsub(".*\\] ", "", C$`Overhøjde [mm]`)
overheight <- gsub("[{}]", "", overheight) #as.data.frame(do.call(rbind, str_match_all(curvature, "(-?\\d+\\.?\\d*)")))
overheight <- matrix(as.numeric(unlist(strsplit(overheight, ";"))), ncol=2,byrow = 1)
overheight <- apply(overheight, 1, mean)
C$overheight = overheight


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

### GET ASSET INFORMATION ###
for(i in 1:length(defect_set)){
  idx = defect_set[[i]]$BTR[1] == R$BTR & 
    defect_set[[i]]$Track[1] == R$Spor &
    defect_set[[i]]$Rail_string[1] == R$`Skinne-streng`
  
  cond1 = defect_set[[i]]$From[1] >= R$Fra[idx] & defect_set[[i]]$From[1] <= R$Til[idx]
  cond2 = defect_set[[i]]$To[1] >= R$Fra[idx] & defect_set[[i]]$To[1] <= R$Til[idx]
  
  if(NROW(R[idx, ][cond1 | cond2,] ) > 1){ #if there are more than one match, then find the minimum distance
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(R[idx, ][cond1 | cond2, ][,c("Fra", "Til")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = R[idx, ][cond1 | cond2, ][idx2, ]
  } else if(NROW(R[idx, ][cond1 | cond2,] )  == 0){ #if there are no matches, then find the best match
    if(NROW(R[idx, ]) == 0){ #in case, it is not possible to get any matches, then assign NA
      defect_set[[i]]$Rail_type2 = NA
      defect_set[[i]]$Track_type2 = NA
      defect_set[[i]]$Steel_type2 = NA
      next; #jump to next i in loop
    }
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(R[idx, ][,c("Fra", "Til")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = R[idx, ][idx2, ]
  } else { #if there is just one match
    foo = R[idx, ][cond1 | cond2, ]
  }
  defect_set[[i]]$Rail_type2 = foo$`Skinne: Type`
  defect_set[[i]]$Track_type2 = foo$Sportype
  defect_set[[i]]$Steel_type2 = foo$`Skinne: Stålkvalitet`
}


### GET CURVATURE INFORMAION ###
for(i in 1:length(defect_set)){
  idx = defect_set[[i]]$BTR[1] == C$BTR & defect_set[[i]]$Track[1] == C$Spor
  cond1 = defect_set[[i]]$From[1] >= C$Fra[idx] & defect_set[[i]]$From[1] <= C$Til[idx]
  cond2 = defect_set[[i]]$To[1] >= C$Fra[idx] & defect_set[[i]]$To[1] <= C$Til[idx]
  
  if(NROW(C[idx, ][cond1 | cond2,] ) > 1){ #if there are more than one match, then find the minimum distance
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(C[idx, ][cond1 | cond2, ][,c("Fra", "Til")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = C[idx, ][cond1 | cond2, ][idx2, ]
  } else if(NROW(C[idx, ][cond1 | cond2,] )  == 0){ #if there are no matches, then it is straight track
    defect_set[[i]]$Track_type3 = "Straigt_track"
    defect_set[[i]]$Curve = 0
    defect_set[[i]]$Overheight = 0
    next; #jump to next i in loop
  } else { #if there is just one match
    foo = C[idx, ][cond1 | cond2, ]
  }
  defect_set[[i]]$Track_type3 = foo$DK_type_element
  defect_set[[i]]$Curve = foo$curve
  defect_set[[i]]$Overheight = foo$overheight
}


### GET TONNAGE INFORMATION ###
for(i in 1:length(defect_set)){
  idx = defect_set[[i]]$BTR[1] == L$BTR & defect_set[[i]]$Track[1] == L$Spor
  cond1 = defect_set[[i]]$From[1] >= L$From[idx] & defect_set[[i]]$From[1] <= L$To[idx]
  cond2 = defect_set[[i]]$To[1] >= L$From[idx] & defect_set[[i]]$To[1] <= L$To[idx]
  
  if(NROW(L[idx, ][cond1 | cond2,] ) > 1){ #if there are more than one match, then find the minimum distance
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(L[idx, ][cond1 | cond2, ][,c("From", "To")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = L[idx, ][cond1 | cond2, ][idx2, ]
  } else if(NROW(L[idx, ][cond1 | cond2,] )  == 0){ #if there are no matches, then assign NAs
    defect_set[[i]]$MGT_min = NA
    defect_set[[i]]$MGT_max = NA
    defect_set[[i]]$EMGT_min = NA
    defect_set[[i]]$EMGT_max = NA
    next; #jump to next i in loop
  } else { #if there is just one match
    foo = L[idx, ][cond1 | cond2, ]
  }
  defect_set[[i]]$MGT_min = foo$MGT_min
  defect_set[[i]]$MGT_max = foo$MGT_max
  defect_set[[i]]$EMGT_min = foo$EMGT_min
  defect_set[[i]]$EMGT_max = foo$EMGT_max
}

### GET LINE SPEED ###
for(i in 1:length(defect_set)){
  idx = defect_set[[i]]$BTR[1] == S$BTR & defect_set[[i]]$Track[1] == S$Spor
  cond1 = defect_set[[i]]$From[1] >= S$Fra[idx] & defect_set[[i]]$From[1] <= S$Til[idx]
  cond2 = defect_set[[i]]$To[1] >= S$Fra[idx] & defect_set[[i]]$To[1] <= S$Til[idx]
  
  if(NROW(S[idx, ][cond1 | cond2,] ) > 1){ #if there are more than one match, then find the minimum distance
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(S[idx, ][cond1 | cond2, ][,c("Fra", "Til")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = S[idx, ][cond1 | cond2, ][idx2, ]
  } else if(NROW(S[idx, ][cond1 | cond2,] )  == 0){ #if there are no matches, then assign NAs
    defect_set[[i]]$Line_speed = NA
    next; #jump to next i in loop
  } else { #if there is just one match
    foo = S[idx, ][cond1 | cond2, ]
  }
  defect_set[[i]]$Line_speed = foo$`Line speed [km/h]`
}

### GET TURNOUT INDICATOR ###
for(i in 1:length(defect_set)){
  idx = defect_set[[i]]$BTR[1] == SC$BTR & defect_set[[i]]$Track[1] == SC$Spor
  cond1 = defect_set[[i]]$From[1] >= SC$From[idx] & defect_set[[i]]$From[1] <= SC$To[idx]
  cond2 = defect_set[[i]]$To[1] >= SC$From[idx] & defect_set[[i]]$To[1] <= SC$To[idx]
  
  if(NROW(SC[idx, ][cond1 | cond2,] ) > 1){ #if there are more than one match, then find the minimum distance
    p1 = c(defect_set[[i]]$From[1],defect_set[[i]]$To[1])
    p2s = as.matrix(SC[idx, ][cond1 | cond2, ][,c("From", "To")])
    idx2 = which.min(apply(p2s, MARGIN = 1, FUN = function(x) sum((x - p1)^2) )) #find minimum distance
    foo = SC[idx, ][cond1 | cond2, ][idx2, ]
    defect_set[[i]]$Turnout_indicator = 1
  } else if(NROW(SC[idx, ][cond1 | cond2,] )  == 0){ #if there are no matches, then assign 0
    defect_set[[i]]$Turnout_indicator = 0
    next; #jump to next i in loop
  } else { #if there is just one match
    defect_set[[i]]$Turnout_indicator = 1
  }
}

### DETERMINE DEFECT SIZE ###
for(i in 1:length(defect_set)){
  defect_set[[i]]$defect_size = -defect_set[[i]]$Depth_from + defect_set[[i]]$Depth_to 
  }


#save(defect_set, file = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/defect_trajectories_reference_dataset.RData")


#############################################################
### CONSTRUCT DEFECT DATAFRAME WITH TRAJECTORIES IN PAIRS ###
#############################################################
rm(list = ls())
load("C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/defect_trajectories_reference_dataset.RData")

D = NULL
for(i in 1:length(defect_set)){
  n_trajectories = NROW(defect_set[[i]]) - 1
  
  if(n_trajectories < 1){ next } #if no trajectories can be made, skip to next defect
  
  d = data.frame(ID = rep(names(defect_set)[i], n_trajectories), Date1 = as.Date(NA), Date2 = as.Date(NA), 
                 Size1 = NA, Size2 = NA, Maintenance_date = as.Date(NA), Maintenance_type = NA, 
                 Maintenance_amount = NA, Passages = NA, Defect_in = NA, UIC = NA, Rail_string = NA, 
                 Track_type = NA, Track_type2 = NA, Track_type3 = NA, Year = NA, 
                 Defect_group1 = NA, Defect_group2 = NA, Combined_defect = NA, 
                 Profile = NA, Rail_type = NA, Steel = NA, Steel2 = NA, Overheight = NA, 
                 Curve = NA, MGT_min = NA, MGT_max = NA,EMGT_min = NA,
                 EMGT_max = NA, Line_speed = NA, Turnout_indicator = NA, State = NA)
  
  for(j in 1:n_trajectories){
    d$Date1[j] = defect_set[[i]]$Date[j]
    d$Date2[j] = defect_set[[i]]$Date[j+1]
    d$Size1[j] = defect_set[[i]]$defect_size[j]
    d$Size2[j] = defect_set[[i]]$defect_size[j+1]
    d$Defect_group1[j] = defect_set[[i]]$Defect_group[j]
    d$Defect_group2[j] = defect_set[[i]]$Defect_group[j+1]
    
    #Maintenance type
    d$Maintenance_date[j] = defect_set[[i]]$Maintenance_date[j+1]
    d$Maintenance_type[j] = defect_set[[i]]$Maintenance_type[j+1]
    d$Maintenance_amount[j] = defect_set[[i]]$Amount[j+1]
    d$Passages[j] = defect_set[[i]]$Passages[j+1]
    
    d$Defect_in[j] = defect_set[[i]]$Defect_found_in[j+1]
    d$UIC[j] = defect_set[[i]]$UIC[j+1]
    d$Rail_string[j] = defect_set[[i]]$Rail_string[j+1]
    d$Track_type[j] = defect_set[[i]]$Track_type[j+1]
    d$Track_type2[j] = defect_set[[i]]$Track_type2[j+1]
    d$Track_type3[j] = defect_set[[i]]$Track_type3[j+1]
    d$Year[j] = defect_set[[i]]$Year[j+1]
    d$Combined_defect[j] = defect_set[[i]]$Combined_defect[j+1]
    d$Profile[j] = defect_set[[i]]$Profile[j+1]
    d$Rail_type[j] = defect_set[[i]]$Rail_type2[j+1]
    d$Steel[j] = defect_set[[i]]$Steel[j+1]
    d$Steel2[j] = defect_set[[i]]$Steel_type2[j+1]
    d$Overheight[j] = defect_set[[i]]$Overheight[j+1]
    d$Curve[j] = defect_set[[i]]$Curve[j+1]
    
    d[j, c("MGT_min", "MGT_max", "EMGT_min" ,"EMGT_max" ,"Line_speed", "Turnout_indicator", "State")] = 
      defect_set[[i]][j+1, c("MGT_min", "MGT_max", "EMGT_min" ,"EMGT_max" ,"Line_speed", "Turnout_indicator", "State")]
  }
  if(is.null(D)){
    D = d
  } else {
    D = rbind(D,d)
  }
}


#MAINTENANCE ENCODING
D$Removed_status = D$State == "Removed" #Encoding of binary Remove control action
D$Grinding = D$Maintenance_type %in% c("Grinding", "Grinding (HS)") 
D$Milling = D$Maintenance_type %in% c("Milling", "Milling (HS)")
D$Planing = D$Maintenance_type %in% c("Planing")
D$Maintenance_indicator = 0
D$Maintenance_indicator[!is.na(D$Maintenance_date)] = 1
D$Passages[is.na(D$Maintenance_date)] = 0

sum(is.na(D$Maintenance_date) == (D$Grinding | D$Milling | D$Planing)) #CHECK IF MAINTENANCE DATES EXISTS WHEN MAINTENANCE IS CONDUCTED:

#TURNOUT ENCODING
idx = D$Turnout_indicator %in% c(1) & D$Track_type %in% c("Standard_track"); D$Track_type[idx] = "Switch" #Find turnout_indications not repored in track_type
idx = !D$Turnout_indicator %in% c(1) & !D$Track_type %in% c("Standard_track", "Unknown"); D$Turnout_indicator[idx] = 1 #Overwrite turnout indications, where track type reports turnout
idx = D$Rail_type %in% c("Switch") & D$Turnout_indicator %in% c(0); D$Turnout_indicator[idx] = 1 #Overwrite turnout indications where rail_type is reported as switch

#WELD ENCODINGS
D$Aluminothermic_weld = D$Defect_in %in% c("Aluminothermic_welding")
D$Flash_butt_weld = D$Defect_in %in% c("Flash_butt_welding")
D$Other_weld = !D$Defect_in %in% c("Aluminothermic_welding","Flash_butt_welding","Clean_rail")
D$Weld = !D$Defect_in %in% c("Clean_rail")

#CURVE ENCODINGS
D$In_curve = D$Track_type3 %in% c("Curve")
D$In_trans_curve = D$Track_type3 %in% c("Transition_curve")
D$In_straight_track = D$Track_type3 %in% c("Straigt_track")

#PROFILE ENCODINGS
idx = (is.na(D$Profile) & is.na(D$Rail_type)) | 
  (is.na(D$Profile) & D$Rail_type %in% c("Switch")); D = D[!idx,]  #remove rows with no useful information on profile (*= 8 rows)
D$Rail_type[is.na(D$Rail_type)] = D$Profile[is.na(D$Rail_type)] #overwrite NANs
D$Profile[is.na(D$Profile)] = D$Rail_type[is.na(D$Profile)] #Overwrite nans
library(stringr)
extract_two_digits <- function(strings) {str_extract(strings, "\\d{2}")} #convert strings to digits
D$Rail_weight_1m = extract_two_digits(D$Profile)

#STEEL ENCODINGS
extract_three_digits <- function(strings) {str_extract(strings, "\\d{3}")}
D$Steel_hardness = extract_three_digits(D$Steel)

#REMOVE OBS MISSING SUBSTANTIAL INFORMATION
idx = is.na(D$UIC) | is.na(D$Size1) | is.na(D$Size2) | is.na(D$Date1) | is.na(D$Date2); D = D[!idx, ]

#CONSTRUCT VARIABLES FOR MODELING
D$t = as.numeric(D$Date2 - D$Date1)/365
D$dSize = D$Size2 - D$Size1
D$Age = (as.numeric( D$Date1 - as.Date(D$Year)) / 365)

#DEFINE VARIABLE FOR EXTRACTION
essential_cols = c("ID","dSize", "t", "UIC")
reference_cols = c("Size1", "Size2","Date1", "Date2", "Rail_string")
maintenan_cols = c("Maintenance_date","Maintenance_indicator", "Grinding", "Milling", "Planing", "Passages","Removed_status")
structure_cols = c("In_straight_track", "In_curve", "In_trans_curve","Turnout_indicator","Weld","Aluminothermic_weld","Flash_butt_weld","Other_weld")
covariate_cols = c("Age", "Curve", "Rail_weight_1m", "Steel_hardness", "Line_speed", "MGT_max")

#Checking number of NANS
#sum(apply(X = D[, c(essential_cols,reference_cols, structure_cols, covariate_cols)], MARGIN = 1, FUN = function(x) any(is.na(x)) ))

# write.csv(x = D[, c(essential_cols, reference_cols, maintenan_cols, structure_cols, covariate_cols)],row.names = F, 
#           file = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/defect_trajectories.csv")

#DATASET WITH: 
# CONTROL ACTIONS "REMOVED STATUS"
# NAN-free COVARIATES: (Maintenance_indicator, Defect_in, "UIC", Track_type, Track_type3, Combined defect,
# Profile, Steel, Curve, MGT_max, Line_speed, Turnout indicator, Age)
# IDENTIFIABLE UIC TO A REASONABLE DEGREE
# ID's THAT CAN BE LOOKED UP IN A REFERENCE DATASET



# fit  = lm(formula = Size2 ~ Size1 +Age+ I(t*MGT_max) + I(Maintenance_indicator) + Line_speed, data = D)
# summary(fit)
# AIC(fit)


## This defect_set could function as a database over defects.
## Then functions could be written for instance to: 
# - taburalize defect counts based on UIC, or spatial distribution, etc.
# - create defect sets for specific BTR or spatial/temporal periods
# - create spatial-temporal mesh to combine with network measurement data
# - create dataset for paired defects so that crack propagation model can be made 
# consider to write one function, that calls the defect set, and then 
# manipulates data so that the preferred format comes out. 



#################################################################
### CONSTRUCT DEFECT DATAFRAME WITH TRAJECTORIES NOT IN PAIRS ###
#################################################################
rm(list = ls())
load("defect_trajectories_reference_dataset.RData")

D <- dplyr::bind_rows(defect_set, .id = "ID")



#MAINTENANCE ENCODING
D$Removed_status = D$State == "Removed" #Encoding of binary Remove control action
D$Grinding = D$Maintenance_type %in% c("Grinding", "Grinding (HS)") 
D$Milling = D$Maintenance_type %in% c("Milling", "Milling (HS)")
D$Planing = D$Maintenance_type %in% c("Planing")
D$Maintenance_indicator = 0
D$Maintenance_indicator[!is.na(D$Maintenance_date)] = 1
D$Passages[is.na(D$Maintenance_date)] = 0

sum(is.na(D$Maintenance_date) == (D$Grinding | D$Milling | D$Planing)) #CHECK IF MAINTENANCE DATES EXISTS WHEN MAINTENANCE IS CONDUCTED:

#TURNOUT ENCODING
idx = D$Turnout_indicator %in% c(1) & D$Track_type %in% c("Standard_track"); D$Track_type[idx] = "Switch" #Find turnout_indications not repored in track_type
idx = !D$Turnout_indicator %in% c(1) & !D$Track_type %in% c("Standard_track", "Unknown"); D$Turnout_indicator[idx] = 1 #Overwrite turnout indications, where track type reports turnout
idx = D$Rail_type %in% c("Switch") & D$Turnout_indicator %in% c(0); D$Turnout_indicator[idx] = 1 #Overwrite turnout indications where rail_type is reported as switch

#WELD ENCODINGS
D$Aluminothermic_weld = D$Defect_found_in %in% c("Aluminothermic_welding")
D$Flash_butt_weld = D$Defect_found_in %in% c("Flash_butt_welding")
D$Other_weld = !D$Defect_found_in %in% c("Aluminothermic_welding","Flash_butt_welding","Clean_rail")
D$Weld = !D$Defect_found_in %in% c("Clean_rail")

#CURVE ENCODINGS
D$In_curve = D$Track_type3 %in% c("Curve")
D$In_trans_curve = D$Track_type3 %in% c("Transition_curve")
D$In_straight_track = D$Track_type3 %in% c("Straigt_track")

#PROFILE ENCODINGS
idx = (is.na(D$Profile) & is.na(D$Rail_type)) | 
  (is.na(D$Profile) & D$Rail_type %in% c("Switch")); D = D[!idx,]  #remove rows with no useful information on profile (*= 8 rows)
D$Rail_type[is.na(D$Rail_type)] = D$Profile[is.na(D$Rail_type)] #overwrite NANs
D$Profile[is.na(D$Profile)] = D$Rail_type[is.na(D$Profile)] #Overwrite nans
library(stringr)
extract_two_digits <- function(strings) {str_extract(strings, "\\d{2}")} #convert strings to digits
D$Rail_weight_1m = extract_two_digits(D$Profile)

#STEEL ENCODINGS
extract_three_digits <- function(strings) {str_extract(strings, "\\d{3}")}
D$Steel_hardness = extract_three_digits(D$Steel)

#REMOVE OBS MISSING SUBSTANTIAL INFORMATION
idx = is.na(D$UIC) | is.na(D$defect_size) | is.na(D$Date); D = D[!idx, ]


D$Age = (as.numeric( as.Date(D$Date) - as.Date(D$Year)) / 365)


cols1 = c("ID", "BTR", "Track", "From", "To", "Rail_string")
cols2 = c("Date", "defect_size")
cols3 = c("Defect_group", "Visible", "UIC","Combined_defect")
cols4 = c("Maintenance_date","Maintenance_indicator", "Grinding", "Milling", "Planing", "Passages","Removed_status")
cols5 = c("In_straight_track", "In_curve", "In_trans_curve","Turnout_indicator","Weld","Aluminothermic_weld","Flash_butt_weld","Other_weld")
cols6 = c("Age", "Curve", "Rail_weight_1m", "Steel_hardness", "Line_speed", "MGT_max")

idx = rowSums(apply(D[, cols6], 2, FUN = function(x) is.na(x))) == 0; D = D[idx, ] #only keep complete covariates


#Count how many times ID appears
library(dplyr)
id_counts <- D %>%
  count(ID)

freq_of_freqs <- id_counts %>%
  count(n, name = "num_ids") %>%
  rename(times = n)

D2 <- D %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  ungroup()

D2 = as.data.frame(D2)

#write.csv(x =D[, c(cols1, cols2, cols3, cols4, cols5, cols6)],row.names = F, file = "C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/defect_trajectories_long_format.csv")






