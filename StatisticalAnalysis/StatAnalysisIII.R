###############################################
# Visual Disturbances Film Project
# Stat Analysis III: Difference between viewings
# Author: Alexander Murph September 2019
# Bucknell University, UNC Chapel Hill
###############################################

# Notes: Script takes in data to clean and aggregate information for every
# (reduction_factor) timepoints.  
# Preps the data for the DiProPerm calculation.

##########################################################################################################
##########################################################################################################
## Update 9/28: Uploading this to GitHub.  If one wishes to recreate these results, they'll need to add ##
## in their own file paths -- these are specific to my directories.  Data will NOT be released yet.     ##
## When it is I'll update the file paths so these files will work directly from GitHub                  ##
##########################################################################################################
##########################################################################################################

# The size of the grouping of frames we wish to consider
reduction_factor = 10

# The files needed, and the file names to which we will save
film_files = c("../Data/Tati Project_Tati test_Hulot_Holiday_mpeg_48 (convert-video-online.com).wmv.tsv",
               "../Data/Tati Project_Tati test_Hulot_Holiday_mpeg_48 (convert-video-online.com).wmv.tsv",
               "../Data/Tati Project_Tati test_Lumiere.wmv.tsv",
               "../Data/Tati Project_Tati test_Lumiere.wmv.tsv",
               "../Data/Tati Project_Tati test_Musketeer_720p.wmv.tsv",
               "../Data/Tati Project_Tati test_Musketeer_720p.wmv.tsv",
               "../Data/Tati Project_Tati test_N_by_NW.wmv.tsv",
               "../Data/Tati Project_Tati test_N_by_NW.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime1.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime1.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime2.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime2.wmv.tsv")
Tati_NotTati = c(1,1,0,0,0,0,0,0,1,1,1,1)
nick_names = c("../Data/DataImputation/Hulot_Holiday_DataImputation_1.csv",
               "../Data/DataImputation/Hulot_Holiday_DataImputation_2.csv",
               "../Data/DataImputation/Lumiere_DataImputation_1.csv",
               "../Data/DataImputation/Lumiere_DataImputation_2.csv",
               "../Data/DataImputation/Musketeer_DataImputation_1.csv",
               "../Data/DataImputation/Musketeer_DataImputation_2.csv",
               "../Data/DataImputation/N_by_NW_DataImputation_1.csv",
               "../Data/DataImputation/N_by_NW_DataImputation_2.csv",
               "../Data/DataImputation/playtime1_DataImputation_1.csv",
               "../Data/DataImputation/playtime1_DataImputation_2.csv",
               "../Data/DataImputation/playtime2_DataImputation_1.csv",
               "../Data/DataImputation/playtime2_DataImputation_2.csv")
film_names_orig = c("holiday1", "holiday2", "lumiere1", "lumiere2", 
                    "musketeer1", "musketeer2", "NbyNW1", "NbyNW2", 
                    "playtime11", "playtime12", "playtime21", "playtime22")

#############################################################################
############ Data removing all NA values associated with saccade ############
#############################################################################

impute_vector = function(z){
  ## Function that smooths out the NAs with the fixation point that most recently 
  ## happened before present time point.
  ## Author: Alexander Murph
  NonNAindex = which(!is.na(z))
  firstNonNA = min(NonNAindex)
  current_impute_value = z[firstNonNA]
  for(index in 1:length(z)){
    if(is.na(z[index])){
      z[index] = current_impute_value
    }else{
      current_impute_value = z[index]
    }
  }
  if(anyNA(z)){
    print(z)
    print("didn't work")
    next
  }
  
  return(z)
}

for(film_number in 1:12) {
  # Determine if this is first, or second, viewing
  clip_number = as.character(2 - (film_number %% 2))
  
  # Read in the file, and do preliminary cleaning
  my_data = read.table(file = film_files[film_number], 
                       sep = '\t', header = TRUE) 
  current_name = nick_names[film_number]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$subject = substr(my_data$RecordingName, 1, 3)
  # my_data = my_data[which(!(my_data$subject %in% c("107", "113", "114", "124"))),]
  my_data = my_data[which(!(my_data$subject %in% c("107"))),]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$RecordingNumber = substring(my_data$RecordingName, 5)
  my_data$RecordingNumber = as.factor(my_data$RecordingNumber)
  my_data$GazePointIndex = as.numeric(my_data$GazePointIndex)
  my_data = my_data[which(my_data$RecordingNumber == clip_number),]
  
  # We cycle through the different subject-viewing to perform data imputation
  instance = as.factor(unique(my_data$RecordingName))
  data_matrix = matrix(NA, nrow = 1, ncol = 2*max(my_data$GazePointIndex))
  max_gaze_numbers = c()
  for(subject in 1:length(instance)) {
    if(all(is.na(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointX..MCSpx."]))){
      print("dropped value")
      # print(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointX..MCSpx."])
      next
    }
    if(all(is.na(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointY..MCSpx."]))){
      # print(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointY..MCSpx."])
      print("dropped value")
      next
    }
    imputted_data_x = impute_vector(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointX..MCSpx."])
    imputted_data_y = impute_vector(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointY..MCSpx."])
    new_data = c(rbind(imputted_data_x, imputted_data_y))
    temp_matrix = matrix(NA, nrow = 1, ncol = 2*max(my_data$GazePointIndex))
    temp_matrix[,1:length(new_data)] = new_data
    data_matrix = rbind(data_matrix, temp_matrix)
    max_gaze_numbers = c(max_gaze_numbers, max(my_data[which(my_data$RecordingName == instance[subject]),"GazePointIndex"]))
  }
  my_string_x = paste(film_names_orig[film_number],"X", sep = '')
  my_string_y = paste(film_names_orig[film_number],"Y", sep = '')
  
  names_1 = paste(my_string_x, 1:(max(my_data$GazePointIndex)), sep = '')
  names_2 = paste(my_string_y, 1:(max(my_data$GazePointIndex)), sep = '')
  # all_names = c(rbind(names_1, names_2))
  # all_data = data.frame(t(data_matrix[-1,]))
  # rownames(all_data) = all_names
  # colnames(all_data) = paste(film_names_orig[film_number], 1:ncol(all_data), sep = '')
  all_names = c(rbind(names_1, names_2))
  all_data = data.frame((data_matrix[-1,]))
  colnames(all_data) = all_names
  rownames(all_data) = paste(film_names_orig[film_number], 1:nrow(all_data), sep = '')
  if(anyNA(all_data[,1:min(max_gaze_numbers)])){
    print("Things went wrong -- try again")
  }
  write.csv(all_data[,1:min(max_gaze_numbers)], current_name)
  print("starting next one")
}
library(beepr)
beep(3)

film_names_orig = c("holiday","holiday", "lumiere", "lumiere", 
                    "musketeer", "musketeer", "NbyNW", "NbyNW", 
                    "playtime1", "playtime1", "playtime2", "playtime2")
index_list = 2*c(1:6) -1
for(i in index_list) {
  data1 = read.csv(nick_names[i])
  data2 = read.csv(nick_names[i+1])
  response = c(rep(0, times = nrow(data1)), rep(1, times = nrow(data2)))
  min_columns = min(ncol(data1), ncol(data2))
  full_data = rbind(as.matrix(data1[,1:min_columns]), as.matrix(data2[,1:min_columns]))
  full_data = as.data.frame(full_data)
  write.csv(full_data, paste("/Users/murph/Documents/OODA/", film_names_orig[i], ".csv", sep = ''))
  write.csv(response, paste("/Users/murph/Documents/OODA/", film_names_orig[i+1], "_response.csv", sep = ''))
}


holiday = as.matrix(read.csv(nick_names[1]))
lumiere = as.matrix(read.csv(nick_names[3]))
musketeer = as.matrix(read.csv(nick_names[5]))
NbyNW = as.matrix(read.csv(nick_names[7]))
playtime1 = as.matrix(read.csv(nick_names[9]))
playtime2 = as.matrix(read.csv(nick_names[11]))

min_cols = min(ncol(holiday), ncol(lumiere), ncol(musketeer), ncol(NbyNW), ncol(playtime1), ncol(playtime2))
 
all_data = rbind(lumiere[,1:min_cols],musketeer[,1:min_cols],NbyNW[,1:min_cols], 
                 holiday[,1:min_cols], playtime1[,1:min_cols], playtime2[,1:min_cols])
num_non_tati = nrow(lumiere) + nrow(musketeer) + nrow(NbyNW)
num_tati = nrow(all_data) - num_non_tati
response = c(rep(0, times = num_non_tati), rep(1, times = num_tati))

write.csv(all_data, paste("/Users/murph/Documents/OODA/", "all_films", ".csv", sep = ''))
write.csv(response, paste("/Users/murph/Documents/OODA/", "all_films", "_response.csv", sep = ''))

