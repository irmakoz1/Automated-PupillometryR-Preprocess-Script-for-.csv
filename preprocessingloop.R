library(PupillometryR)
library(foreach)

## DEFINE PROCESSING FUNCTION
preprocess <- function(filepath, baseline=4) {
  
  file <- read.csv(filepath)
  print(dim(file)) ; flush.console()
  
  file$Trial2 <- paste(file$Trial,file$Repetition,sep="_")
  file$pupilsDiameterL[file$pupilsDiameterL < 1 | file$pupilsDiameterL > 8] <- NA
  file$pupilsDiameterR[file$pupilsDiameterR < 1 | file$pupilsDiameterL > 8] <- NA
  file$User <- as.character(file$User)
  file$SoundType <- as.factor(file$SoundType)
  file$LightType <- as.factor(file$LightType)
  
  Sdata <- make_pupillometryr_data(data = file,
                                   subject = User,
                                   trial = Trial,                               
                                   time = Time,
                                   condition = SoundType,
                                   other= LightType)
  regressed_data <- regress_data(data = Sdata,
                                 pupil1 = pupilsDiameterR,
                                 pupil2 = pupilsDiameterL)
  mean_data <- calculate_mean_pupil_size(data = regressed_data,
                                         pupil1 = pupilsDiameterR,
                                         pupil2 = pupilsDiameterL)
  missing <- calculate_missing_data(mean_data,
                                    mean_pupil)
  mean_data2 <- clean_missing_data(mean_data,
                                   pupil = mean_pupil,
                                   trial_threshold = .50,
                                   subject_trial_threshold = .50)
  filtered_data <- filter_data(data = mean_data2,
                               pupil = mean_pupil,
                               filter = 'median',
                               degree = 11)
  int_data <- interpolate_data(data = filtered_data,
                               pupil = mean_pupil,
                               type = 'linear')
  base_data <- baseline_data(data = int_data,
                             pupil = mean_pupil,
                             start = 0,
                             stop = baseline*10)
  
  plot(base_data, pupil = mean_pupil, group = 'subject')
  base_data$light_condition <- ifelse(var(base_data$light)==0,"Static","Dynamic")
  base_data$sound_phase <- cut(1:((baseline+6+7)*10),breaks=c(0,baseline*10,(baseline+6)*10,(baseline+6+7)*10),
                               include.lowest=TRUE,labels=c("0_BASE","1_ON","2_OFF"))
  base_data
}

## PROCESSING LOOP
######################################
#
# 1: SET FOLDER PATH OF SINGLE PARTICIPANT
# 2: RUN LOOP AND SET CORRECT BASELINE FOR THIS PARTICIPANT
# 3: INSPECT GENERATED DATA
# 4: WRITE DATA TO CSV
#
######################################

setwd("C:/Users/irmak/Desktop/TEZ/data/newrealdata/pvdr51_14_02_17_21")
folder <- dir()
filepaths <- folder[grep("Trial",folder)]

user <- foreach(i=filepaths, .combine="rbind",.errorhandling="remove") %do% {
  preprocess(i, baseline=4)
}


head(user) ; dim(user)

plot(user$mean_pupil[1:150],type="p",lwd=4,col=as.factor(user$sound_phase))

write.csv(user,"C:/Users/irmak/Desktop/TEZ/data/preprocess/pvdr51_14_02_17_21_preprocessed.csv",row.names=FALSE)
