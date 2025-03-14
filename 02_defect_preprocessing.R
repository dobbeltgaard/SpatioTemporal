
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
  defect_set[[toString(IDs[i])]] = d[idx,]
}
names(defect_set) = as.numeric(names(defect_set))



## This defect_set could function as a database over defects.
## Then functions could be written for instance to: 
# - taburalize defect counts based on UIC, or spatial distribution, etc.
# - create defect sets for specific BTR or spatial/temporal periods
# - create spatial-temporal mesh to combine with network measurement data
# - create dataset for paired defects so that crack propagation model can be made 
# consider to write one function, that calls the defect set, and then 
# manipulates data so that the preferred format comes out. 










