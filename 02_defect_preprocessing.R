
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
#load("C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_without_maintenance.RData")

### this data set should be loaded into the spatio-temporal network! ###


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

#############################################################
#### COMBINE with static data. Tonnage. Speed. Curveture. ###
#############################################################

library(readxl)
source("99_translations_and_naming_convetions.R")
load("C:/Users/askbi/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/Defect_set_with_maintenance.RData")


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

####################################################
### CONSTRUCT DEFECT DATAFRAME WITH TRAJECTORIES ###
####################################################
D = NULL
for(i in 1:length(defect_set)){
  n_trajectories = NROW(defect_set[[i]]) - 1
  
  if(n_trajectories < 1){ next } #if no trajectories can be made, skip to next defect
  
  d = data.frame(ID = rep(names(defect_set)[i], n_trajectories), Date1 = as.Date(NA), Date2 = as.Date(NA), 
                 Size1 = NA, Size2 = NA, Maintenance_date = as.Date(NA), Maintenance_type = NA, 
                 Maintenance_amount = NA, Defect_in = NA, UIC = NA, Rail_string = NA, 
                 Track_type = NA, Track_type2 = NA, Track_type3 = NA, Year = NA, 
                 Defect_group1 = NA, Defect_group2 = NA, Combined_defect = NA, 
                 Profile = NA, Rail_type = NA, Steel = NA, Steel2 = NA, Overheight = NA, 
                 Curve = NA, MGT_min = NA, MGT_max = NA,EMGT_min = NA,
                 EMGT_max = NA, Line_speed = NA, Turnout_indicator = NA)
  
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
    
    d[j, c("MGT_min", "MGT_max", "EMGT_min" ,"EMGT_max" ,"Line_speed", "Turnout_indicator")] = 
      defect_set[[i]][j+1, c("MGT_min", "MGT_max", "EMGT_min" ,"EMGT_max" ,"Line_speed", "Turnout_indicator")]
  }
  if(is.null(D)){
    D = d
  } else {
    D = rbind(D,d)
  }
}

idx = is.na(D$Size1) | is.na(D$Size2) 
D = D[!idx, ]


#INITAL INVESTIGATION OF THE DATA IN D
D$t = as.numeric(D$Date2 - D$Date1)
D$dSize = D$Size2 - D$Size1
D$Maintenance_indicator = 0
D$Maintenance_indicator[!is.na(D$Maintenance_type)] = 1

fit  = lm(formula = dSize ~ t + Maintenance_indicator + 0, data = D)
summary(fit)

plot(D$dSize)
head(D)


M = matrix(NA, nrow = length(defect_set), ncol = 7)
for(i in 1:length(defect_set)){
  foo = defect_set[[i]]
  M[i,1] = NROW(foo)
  if(NROW(foo) < 2){M[i,2] = F;  M[i,3] = F; M[i,4] = F; } 
  M[i,2] = foo$UIC[length(foo$UIC)]
  M[i,3] = any(!is.na(foo$Maintenance_date))
  M[i,4] = !is.na(foo$MGT_min[1])
  M[i,5] = !is.na(foo$Curve[1])
  M[i,6] = !is.na(foo$Line_speed[1])
  M[i,7] = !is.na(foo$Turnout_indicator[1])
  }






table(M[,1])
table(M[,2])
table(M[,3])
table(M[,4])
table(M[,5])
table(M[,6])
table(M[,7])

which(M[,1] ==6)



which(M[,3] == 1 & M[,1] ==8)

defect_set[[16285  ]]


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










