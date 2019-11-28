###############################################
# Visual Disturbances Film Project
# Stat Analysis DiProPerm: Difference between viewings
# Author: Alexander Murph September/November 2019
# Bucknell University, UNC Chapel Hill
###############################################

# Notes: Script takes in data to clean and aggregate information for every
# (reduction_factor) timepoints.  
# Preps the data for the DiProPerm calculation (which is done in python).

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

#################################################################################
############ Function removing all NA values associated with saccade ############
#################################################################################

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


###############################################################################################################
############ Gathering all data, imputing it, then writing it in the format required for DiProPerm ############
###############################################################################################################
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
      next
    }
    if(all(is.na(my_data[which(my_data$RecordingName == instance[subject]),"FixationPointY..MCSpx."]))){
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

# reformat data then write to csv
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
  write.csv(full_data, paste("../Data/", film_names_orig[i], ".csv", sep = ''))
  write.csv(response, paste("../Data/", film_names_orig[i+1], "_response.csv", sep = ''))
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

write.csv(all_data, paste("../Data/", "all_films", ".csv", sep = ''))
write.csv(response, paste("../Data/", "all_films", "_response.csv", sep = ''))


################################################################################
############ Repeating process, for distance values (for study III) ############
################################################################################

nick_names = c("../Data/AverageChange/Hulot_Holiday_Change.csv",
               "../Data/AverageChange/Lumiere_by_Change.csv",
               "../Data/AverageChange/Musketeer_by_Change.csv",
               "../Data/AverageChange/N_by_NW_by_Change.csv",
               "../Data/AverageChange/playtime1_by_Change.csv",
               "../Data/AverageChange/playtime2_by_Change.csv")

data_names = c("holiday", "lumiere", "musketeer", 
               "NbyNW", "playtime1", "playtime2")

holiday = read.csv(nick_names[1])
names(holiday) = c("TimePoint", "AverageDistances")

lumiere = read.csv(nick_names[2])
names(lumiere) = c("TimePoint", "AverageDistances")

musketeer = read.csv(nick_names[3])
names(musketeer) = c("TimePoint", "AverageDistances")

NbyNW = read.csv(nick_names[4])
names(NbyNW) = c("TimePoint", "AverageDistances")

playtime1 = read.csv(nick_names[5])
names(playtime1) = c("TimePoint", "AverageDistances")

playtime2 = read.csv(nick_names[6])
names(playtime2) = c("TimePoint", "AverageDistances")

data_list = list(holiday, lumiere, musketeer, NbyNW, playtime1, playtime2)
max_index = max(nrow(holiday), nrow(lumiere), nrow(musketeer), nrow(NbyNW), 
                nrow(playtime1), nrow(playtime2))

full_data = data.frame(TimePoint = rep(1:max_index, times = 6),
                       AverageDistances = rep(NA, times = (max_index*6)),
                       Film = rep(data_names, each = max_index))

# gather all data into one data matrix
for(index in 1:6) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = (nrow(data_list[[index]])) + lower_index - 1
  full_data$AverageDistances[lower_index:upper_index] = data_list[[index]]$AverageDistances
}

# impute data and find min cols
col_vals = c()
for(value in levels(full_data$Film) ){
  col_vals = c(col_vals, length(full_data$AverageDistances[which(full_data$Film == value)]))
  full_data$AverageDistances[which(full_data$Film == value)] = 
    impute_vector(full_data$AverageDistances[which(full_data$Film == value)])
}
min_cols = min(col_vals)

# reformat data, then write it to csv.
average_distances_data = matrix(rep(NA, times = min_cols), nrow = 1, ncol = min_cols)
for(value in levels(full_data$Film) ){
  new_row = matrix(full_data$AverageDistances[which(full_data$Film == value)][1:min_cols], nrow = 1, ncol = min_cols)
  average_distances_data = rbind(average_distances_data, new_row)
}
average_distances_data = as.data.frame(average_distances_data[-1,])
response = c(1,0,0,0,1,1)

write.csv(average_distances_data, paste("../Data/", "all_distances", ".csv", sep = ''))
write.csv(response, paste("../Data/", "all_distances", "_response.csv", sep = ''))
