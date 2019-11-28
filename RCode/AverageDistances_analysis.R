###############################################
# Visual Disturbances Film Project
# Stat Analysis: Average Distances Between Films
# Author: Alexander Murph August-November 2019
# Bucknell University, UNC Chapel Hill
###############################################

###############################################
## Notes: Script takes in data to clean and aggregate information for every
# (reduction_factor) timepoints.  Then finds the average distances between viewings
# This analysis compares the average distance values between films.
# We use the created datasets to produce the graphs discussed in the paper (see bottom of script for graphs).
###############################################

film_files = c("../Data/Tati Project_Tati test_Hulot_Holiday_mpeg_48 (convert-video-online.com).wmv.tsv",
               "../Data/Tati Project_Tati test_Lumiere.wmv.tsv",
               "../Data/Tati Project_Tati test_Musketeer_720p.wmv.tsv",
               "../Data/Tati Project_Tati test_N_by_NW.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime1.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime2.wmv.tsv")
nick_names = c("../Data/AverageChange/Hulot_Holiday_Change.csv",
               "../Data/AverageChange/Lumiere_by_Change.csv",
               "../Data/AverageChange/Musketeer_by_Change.csv",
               "../Data/AverageChange/N_by_NW_by_Change.csv",
               "../Data/AverageChange/playtime1_by_Change.csv",
               "../Data/AverageChange/playtime2_by_Change.csv")
reduction_factor = 1

euc.dist <- function(x1, y1, x2, y2) {
  # General euclidean distance function.
  # Author: Alexander Murph
  if(anyNA(c(x1, y1, x2, y2))) {
    return(NA)
  }
  return(sqrt((x1 - x2) ^ 2 + (y1 - y2)^2))
}

# Iterate through the films and gather all average distances for the chosen reduction factor.
for(film_number in 1:6) {
  my_data = read.table(file = film_files[film_number], 
                       sep = '\t', header = TRUE) 
  current_name = nick_names[film_number]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$subject = substr(my_data$RecordingName, 1, 3)
  my_data = my_data[which(!(my_data$subject %in% c("107"))),]
  
  test_number = levels(droplevels(my_data$RecordingName))
  all_distances = NULL
  number = NULL
  all_maxes = NULL
  
  for(index in 1:25 * 2){
    temp_data_1 = my_data[which(my_data$RecordingName == test_number[index - 1]),]
    temp_data_2 = my_data[which(my_data$RecordingName == test_number[index]),]
    min_index = min(c(max(temp_data_1$GazePointIndex), 
                      max(temp_data_2$GazePointIndex)))
    all_maxes = c(all_maxes, 
                  max(temp_data_1$GazePointIndex), 
                  max(temp_data_2$GazePointIndex))
    
    for(gaze_index in 1:min_index ){
      row_1 = temp_data_1[which(temp_data_1$GazePointIndex == gaze_index),]
      row_2 = temp_data_2[which(temp_data_2$GazePointIndex == gaze_index),]
      distance = euc.dist(row_1$FixationPointX..MCSpx.[1], 
                           row_1$FixationPointY..MCSpx.[1],
                           row_2$FixationPointX..MCSpx.[1], 
                           row_2$FixationPointY..MCSpx.[1])
      all_distances = c(all_distances, distance)
      number = c(number, index)
    }
  }

  average_distances = NULL
  values = NULL
  
  # Grab the average of everyone's distance at a given GazePointIndex in time.
  # Applying a given reduction factor
  upper = floor(min(all_maxes) / reduction_factor)
  for(index_value in 1:upper){
    for(index in 1:25 * 2) {
      lower_index = ((index_value - 1)*reduction_factor + 1)
      upper_index = index_value * reduction_factor
      if(index_value == upper){
        upper_index = min(all_maxes)
      }
      value_index = which(number == index)[lower_index:upper_index]
      values = c(values, all_distances[value_index])
    }
    average_distances = c(average_distances, mean(values, na.rm = T))
    values = NULL
  }

  new_data = data.frame(average_distances)
  
  # Write the average distances data to data sub directory.
  write.csv(new_data, current_name)
  print("next film...")
}

library(beepr)
beep(3)

#######################################################################
############## Comparing Average Distances between films ##############
#######################################################################
library(ggplot2)
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
for(index in 1:6) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = (nrow(data_list[[index]])) + lower_index - 1
  full_data$AverageDistances[lower_index:upper_index] = data_list[[index]]$AverageDistances
}

ggplot(full_data, aes(x=TimePoint, y=AverageDistances, group=Film)) +
  geom_line(aes(color=as.factor(Film))) + labs(color = "Film") + 
  ggtitle("WithinSS for each film, First Viewings") + 
  xlab("TimePoint (in groups of 100)") 

# Pulling apart the time series
new_full_data_1 = data.frame(TimePoint = rep(1:max_index, times = 3),
                             AverageDistances = rep(NA, times = 3*max_index),
                             Film = rep(data_names[1:3], each = max_index))
new_full_data_2 = data.frame(TimePoint = rep(1:max_index, times = 3),
                             AverageDistances = rep(NA, times = 3*max_index),
                             Film = rep(data_names[4:6], each = max_index))
for(index in 1:3) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = lower_index + max_index - 1
  new_full_data_1$AverageDistances[lower_index:upper_index] = data_list[[index]]$AverageDistances[1:max_index]
}
for(index in 1:3) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = lower_index + max_index - 1
  new_full_data_2$AverageDistances[lower_index:upper_index] = data_list[[(index+3)]]$AverageDistances[1:max_index]
}


# Time series analysis
ggplot(new_full_data_1, aes(x = TimePoint, y = AverageDistances)) + 
  geom_line(aes(color = as.factor(Film))) + 
  facet_grid(Film ~ .) + theme(legend.position = "none", panel.background = element_blank()) + 
     scale_color_manual(values=c("red", "green", "blue")) + ylim(0,700) + xlab("Time (seconds)")+
  scale_x_continuous(breaks = round(seq(0, max(new_full_data_1$TimePoint), by = 50),0))
ggplot(new_full_data_2, aes(x = TimePoint, y = AverageDistances)) +
  geom_line(aes(color = (as.factor(Film)))) +
  facet_grid(Film ~ .) + theme(legend.position = "none", panel.background = element_blank()) +
  scale_color_manual(values=c("turquoise3", "steelblue3", "magenta2")) + ylim(0,700) + xlab("Time (seconds)")+
  scale_x_continuous(breaks = round(seq(0, max(new_full_data_1$TimePoint), by = 50),0))


# Box plot analysis
my.bp <- ggplot(data=rbind(new_full_data_1, new_full_data_2), aes(y= AverageDistances, x=as.factor(Film), fill = as.factor(Film)) ) # Creates boxplots
my.bp <- my.bp + geom_boxplot() # Adds color
my.bp <- my.bp +  ylab("Average Distances") + xlab("Films") # Adds kaveks
my.bp <- my.bp + scale_fill_manual(values=c("red", "green", "blue", 
                                             "turquoise3", "steelblue3", "magenta2"))
my.bp <- my.bp + theme(legend.position="none", panel.background = element_blank()) + coord_flip()
my.bp
