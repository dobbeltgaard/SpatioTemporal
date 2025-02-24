
rm(list = ls())
library(readxl)
library(stringr)

#path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
path = "C:/Users/atbd/COWI/A235142 - Predictive maintanance ph.d project (1502) - Documents/Data/BDK/"

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

#################
### Variables ###
#################
var_nam = c()
#either left or right rail: 
long = c("hojd", "side"); D = c("D1","D2","D3","D4")
for(i in long){for(j in D){var_nam = c(var_nam, paste(i,j, sep="_")) }}
corrugation = c("corr30_100", "corr100_300", "corr300_900")
twist = c("twist_2m", "twist_3m","twist_6m", "twist_9m", "twist_12m", "twist_15m")
more = c("wear_height", "inclination", "wear_width")
noise_midvalue = c(4,5,"6_3",8,10,"12_5",16,20,25,"31.5",40, 50, 63, 80, 100, 125, 160, 200)
noise = paste("noise",noise_midvalue, sep= "_")
other = c("UT", "eddy")
#c(paste(var_nam, "left", sep = "_"), paste(var_nam, "right", sep = "_"))
#for both:
no_LR = c("cant", "gauge")
maintenance = c("tamping", "grinding", "milling")
#static information: 
statics = c("curvature", "line_speed", "load", "turnout")

var_nam = c(var_nam, corrugation, twist, more, noise, other, no_LR, maintenance,statics)



#################################  
### From Roskilde to Ringsted ###
#################################
RosRing = NULL
idx = R$BTRn >= 14000 & R$BTRn <= 14058

R1 = R[idx,] #Stræknings level
unique(R1$BTR) #loop through these. Will happen later.

btr = "014034"#"014043" # # #We start by looking at one BTR. Later, we should loop through BTRs.
R1 = R[R$BTR == btr,] #BTR level 
RosRing[[btr]] = list() #insert btr in strækningslist


#Filter on track
tracks = unique(R1$Spor)

#track = unique(R2$Spor)
for(trk in tracks){
  
  tonnage = as.data.frame((L[L$BTR == btr & L$Spor == trk, c("MGT_min", "MGT_max", "EMGT_min", "EMGT_max")]))
  tonnage$date = (L[L$BTR == btr & L$Spor == trk, c("Event (Date)")])$`Event (Date)`
  tonnage = tonnage[, order(names(tonnage))]
  tonnage = tonnage[order(tonnage$date, decreasing = F),]
  
  RosRing[[btr]][[trk]] = list(
    #speed = as.numeric(S[S$BTR == btr & S$Spor == trk, c("Line speed [km/h]")]),
    tonnage = tonnage
  )

  #R3 = R2[R2$Spor == t,] #track level
  #rail = unique(R3$`Skinne-streng`) 
  #R4 = R3[R3$`Skinne-streng` == rail[1],] #rail level
  #pos = paste(R4$Fra, R4$Til,sep = ";")
  #for(p in pos){
   # RosRing[[btr]][[t]][[p]] = list()
  #}
}
RosRing



trk = tracks[2] #loop through these 
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




##################################
### TRY LOOPING THROUGH TRACKS ###
##################################
RosRing = NULL
btr = "014034" #"014043" # # #We start by looking at one BTR. Later, we should loop through BTRs.
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
  
  #R3 = R2[R2$Spor == t,] #track level
  #rail = unique(R3$`Skinne-streng`) 
  #R4 = R3[R3$`Skinne-streng` == rail[1],] #rail level
  # pos = paste(X$fra, X$til,sep = ";")
  # for(p in pos){
  #  RosRing[[btr]][[trk]][[p]] = list(
  #    statics = X[p]
  #  )
  # }
}


object.size(RosRing)
RosRing



#add meta information on track segment level
#add meta information on rail side of track segment level

RosRing



duplicated(R[foo.idx, c("Fra", "Til", "Skinne-streng")])
