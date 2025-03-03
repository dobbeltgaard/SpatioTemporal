
rm(list = ls())
library(readxl)
library(stringr)
library(dplyr)

path = "C:/Users/ATBD/OneDrive - COWI/Documents - A235142 - Predictive maintanance ph.d project (1502)/Data/BDK/"
#path = "C:/Users/atbd/COWI/A235142 - Predictive maintanance ph.d project (1502) - Documents/Data/BDK/"

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

###################################################
### ALL VARIABLES AND THEIR SPATIAL RESOLUTIONS ###
###################################################

PATTERNS = c("corr", "vrid", "hojd_L","hojd_R","overhojd", "wear", "rail", "side", "spor")
var.idx = apply(sapply(PATTERNS, FUN = grepl, x = list.files(path), ignore.case = T),MARGIN = 1, FUN = any)
variables = data.frame( name = c(list.files(path)[var.idx], unique(sub("_Campaign.*", "", list.files(paste0(path,"BDK_Noise_all"))))), 
                        resolution = NA, n_files = NA)

all.all.files = list.files(path, recursive = T, full.names = T)

for(i in 1:NROW(variables)){
  files.of.variable = all.all.files[grepl(variables$name[i], all.all.files, ignore.case = T)]
  variables$n_files[i] = length(files.of.variable)
  #x = read.csv(tail(files.of.variable,1), sep = ";") 
  #names(x) = c("BTR", "Spor", "fra", "til", "date", "x")
  #variables$resolution[i] = as.numeric(quantile(abs(diff(x$fra)), 0.5))
  #print(paste("Loop", i, "has resolution", as.numeric(quantile(abs(diff(x$fra)), 0.5))))
}
sum(variables$n_files)


#####################################################
### SPLIT MEASUREMENT DATA TO BTR SEPARATED FILES ###
#####################################################
destination = "C:/Users/ATBD/OneDrive - Danmarks Tekniske Universitet/SpatioTemporal/database"

btr_range = c(14000,14058)


i = 2 #loops through all variables
files.of.variable = all.all.files[grepl(variables$name[i], all.all.files, ignore.case = T)]
dir.create(file.path(destination, variables$name[i]), showWarnings = FALSE)


for(j in 1:length(files.of.variable)){ #loops through all campaigns for variable i
  start_time <- Sys.time() #stop timer
  date_nam = sub(".*Campaign_", "",files.of.variable[j])
  x = read.csv(files.of.variable[j], sep = ";")
  names(x) = c("BTR", "Spor", "fra", "til", "date", "x")
  BTRs = unique(x$BTR)
  chosenBTRs = BTRs[BTRs>=btr_range[1] & BTRs<=btr_range[2]]
  x = x[x$BTR %in% chosenBTRs,]
  
  for(btr in chosenBTRs){
    idx = x$BTR == as.numeric(btr) #extract rows with corresponding btr
    nam = paste0("BTR_",btr,"_",variables$name[i], "_Campaign_", date_nam) #construct name
    if(! nam %in% list.files(destination)){ #if not already saved, then create new file
      write.csv(x[idx, ],file = file.path(destination, variables$name[i],nam), row.names=FALSE)
    }
    x = x[!idx, ] #remove extracted rows for better speeds
  }
  
  end_time <- Sys.time() #stop timer
  elapsed_time <- end_time - start_time
  print(paste("File =", j, "::: Runtime =", elapsed_time))
}

#remember to check if the correct data are pasted in the correct files


############################################
##### COLLECT IN NESTED DATA STRUCTURE #####
############################################

### NOTE: After all measurement files for a pattern are run through, 
### a version of the data structure should be saved to a folder.

all.files = list.files(paste0(path), full.names = T)
pattern = "Hojd_L_D1"
files = list.files(all.files[grepl(pattern, all.files, ignore.case = T)], full.names = T)

res = 0.00025 #as.numeric(quantile(abs(x$fra - x$til), probs = 0.5)) #get spatial resolution

for(i in 1:2){ #run through files with pattern
  x = read.csv(files[i], sep = ";"); 
  names(x) = c("BTR", "Spor", "fra", "til", "date", "x")
  x_entire = x; #store initial data
  
  for(btr in names(RosRing)){
    for(trk in names(RosRing[[btr]])){
      x = x_entire #reinstantiate measurement file
      idx1 = x$BTR == as.numeric(btr) 
      idx2 = x$Spor[idx1] == trk
      x = x[idx1, ][idx2, ] #filtering so we only consider correct track and btr
      
      pos1 = head(RosRing[[btr]][[trk]]$statics$fra,1) #range of btr and track
      pos2 = tail(RosRing[[btr]][[trk]]$statics$til,1) - res
      mesh = seq(pos1, pos2, by = res) #make mesh
      
      #does x cover the the interval (pos1, pos2)
      cond1 = any(abs(x$fra-pos1) <= res)
      cond2 = any(abs(x$fra-pos2) <= res)
      #cond1 = any((x$fra >= pos1 - res) & (x$fra <= pos1 + res))
      #cond2 = any((x$fra >= pos2 - res) & (x$fra <= pos2 + res))
      if(cond1 & cond2){ # x covers the interval (pos1, pos2)
        idx1 = which.min(abs(x$fra-pos1))
        idx2 = which.min(abs(x$fra-pos2))
      } else if(cond1 | cond2){ # x covers some of the interval (pos1, pos2)
        val1 = max(min(x$fra),pos1)
        val2 = min(max(x$fra),pos2)
        idx1 = which.min(abs(x$fra-val1))
        idx2 = which.min(abs(x$fra-val2))
      }
      
      if(cond1 | cond2){ #storage of values:
        # x = x[idx1:idx2, ]
        # 
        # #how to put the relevant x values into the mesh
        # meas = data.frame(pos = mesh, x = NA)
        # start = which.min(abs(x$fra[1] - mesh))
        # meas$x = NA
        # 
        # len_x = length(x$x)
        # len_target = length(meas$x[start:(start + len_x - 1)])
        # #meas$x[start:NROW(x)] = x$x
        # if (len_x == len_target) {
        #   meas$x[start:(start + len_x - 1)] = x$x
        # } else {
        #   warning("Mismatch in lengths: x$x has ", len_x, " elements, but target range has ", 
        #           len_target, " elements in BTR ", btr, " track ", trk)
        # }
        
        x = x[idx1:idx2, ]
        
        #Compute gaps and find large gaps > 0.0005
        gaps = diff(x$fra)
        gap_idx = which(gaps > (2*res))
        
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
        if(length(RosRing[[btr]][[trk]]$meas) == 0){ #if there is no previous measurements
          RosRing[[btr]][[trk]]$meas[[pattern]] = meas
        } else{
          RosRing[[btr]][[trk]]$meas[[pattern]] = cbind(RosRing[[btr]][[trk]]$meas[[pattern]], 
                                                        meas[,2])
          
          colnames(RosRing[[btr]][[trk]]$meas[[pattern]])[NCOL(RosRing[[btr]][[trk]]$meas[[pattern]])] = x$date[1]
        }
      }
    }
  }
}


btr = names(RosRing)[1]
trk = names(RosRing[[btr]])[2]

tail(RosRing[[btr]][[trk]]$meas[[pattern]],20)

RosRing[[btr]][[trk]]$statics

object.size(RosRing)

#needs check for
# - what if there is no data added from a file
# - what if partial data is added? Is it still correct? Probably, but you should check again
# - what makes the code slow?



# for(f in files){
#   dates <- regmatches(f, gregexpr("\\d{4}-\\d{2}-\\d{2}", f))[[1]]
#   print(diff(as.Date(dates)))
#   x = read.csv(f, sep = ";")
#   names(x) = c("BTR", "Spor", "fra", "til", "date", pattern)
#   print(sum(x$til-x$fra))
#   print(object.size(x)/1E6)
# }
# 
# 
# quantile(x$til-x$fra)




























