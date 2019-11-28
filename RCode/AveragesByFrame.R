###############################################
# Visual Disturbances Film Project
# Visualization Prep: Averages & Standard deviations
# Author: Alexander Murph August-November 2019
# Bucknell University, UNC Chapel Hill
###############################################

###############################################
## Notes: Script takes in data prepare for the box-drawing visualization
###############################################

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
nick_names = c("../Data/Hulot_Holiday_by_Frames_1.csv",
               "../Data/Hulot_Holiday_by_Frames_2.csv",
               "../Data/Lumiere_by_Frames_1.csv",
               "../Data/Lumiere_by_Frames_2.csv",
               "../Data/Musketeer_by_Frames_1.csv",
               "../Data/Musketeer_by_Frames_2.csv",
               "../Data/N_by_NW_by_Frames_1.csv",
               "../Data/N_by_NW_by_Frames_2.csv",
               "../Data/playtime1_by_Frames_1.csv",
               "../Data/playtime1_by_Frames_2.csv",
               "../Data/playtime2_by_Frames_1.csv",
               "../Data/playtime2_by_Frames_2.csv")

# Iterate through the films and gather the averages and standard deviations.
for(number in 1:12){

  film_number = number
  clip_number = as.character((film_number %% 2) + 1)
  
  my_data = read.table(file = film_files[film_number], 
                       sep = '\t', header = TRUE) 
  current_name = nick_names[film_number]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$subject = substr(my_data$RecordingName, 1, 3)
  my_data = my_data[which(!(my_data$subject %in% c("107", "113", "114", "124"))),]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$RecordingName = substring(my_data$RecordingName, 5)
  my_data$RecordingName = as.factor(my_data$RecordingName)
  my_data$GazePointIndex = as.numeric(my_data$GazePointIndex)
  
  max_index = max(my_data$GazePointIndex)
  
  new_data = data.frame(Average_X = NA, Average_Y = NA, 
                        SD_X = NA, SD_Y = NA)
  
  for(frame in 1:max_index) {
    temp_data = my_data[which(my_data$RecordingName == clip_number),]
    temp_data = temp_data[which(my_data$GazePointIndex == frame),]
    print(nrow(temp_data))
    temp_row = data.frame(Average_X = NA, Average_Y = NA, 
                          SD_X = NA, SD_Y = NA)
    temp_row[1,1] = mean(temp_data$FixationPointX..MCSpx., na.rm = T)
    temp_row[1,2] = mean(temp_data$FixationPointY..MCSpx., na.rm = T)
    temp_row[1,3] = sd(temp_data$FixationPointX..MCSpx., na.rm = T)
    temp_row[1,4] = sd(temp_data$FixationPointY..MCSpx., na.rm = T)
    
    if(anyNA(temp_row)) {
      # I realize this will work very poorly if the first frame doesn't have any
      # data, but I'm willing to gamble that this won't happen.
      temp_row = new_data[frame,]
      print("this happened")
      print(frame)
      print("\n")
    }
    
    new_data = rbind(new_data, temp_row)
  }
  
  new_data = new_data[-1,]
  
  write.csv(new_data, current_name)

}
beep(3)
