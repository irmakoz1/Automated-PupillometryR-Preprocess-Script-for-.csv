library(PupillometryR)

setwd("C:/Users/irmak/Desktop/TEZ/data/pvdr48_03_02_11_41")

file <- read.csv("C:/Users/irmak/Desktop/TEZ/data/newrealdata/pvdr24_14_12_15_32/Trial_1_0.csv")
head(file) ; str(file)


#preprocess

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
                               pupil2 = pupilsDiameterL
)

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
                           stop = 40)

plot(base_data, pupil = mean_pupil, group = 'subject')



setwd("C:/Users/irmak/Desktop/TEZ/data/preprocessed pvdr48")
write.csv(base_data,"filePP.csv")


