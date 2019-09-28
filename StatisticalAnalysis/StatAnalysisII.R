###############################################
# Visual Disturbances Film Project
# Stat Analysis II: TWC Between Films
# Author: Alexander Murph August 2019
# Bucknell University, UNC Chapel Hill
###############################################

# Notes: Script takes in data to clean and aggregate information for every
# (reduction_factor) timepoints.  Then finds the intra-cluster sum of squares
# variation for each aggregated time point.
# This analysis compares the raw TWC values between films.
# We use the created datasets to produce the graphs discussed in the paper.

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
               "../Data/Tati Project_Tati test_Lumiere.wmv.tsv",
               "../Data/Tati Project_Tati test_Musketeer_720p.wmv.tsv",
               "../Data/Tati Project_Tati test_N_by_NW.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime1.wmv.tsv",
               "../Data/Tati Project_Tati test_playtime2.wmv.tsv")

nick_names = c("../Data/WithinSS/Hulot_Holiday_WithinSS_full.csv",
               "../Data/WithinSS/Lumiere_WithinSS_full.csv",
               "../Data/WithinSS/Musketeer_WithinSS_full.csv",
               "../Data/WithinSS/N_by_NW_WithinSS_full.csv",
               "../Data/WithinSS/playtime1_WithinSS_full.csv",
               "../Data/WithinSS/playtime2_WithinSS_full.csv")

################################################################
############ Data allowing for only single clusters ############
################################################################

for(film_number in 1:6) {
  # Determine if this is first, or second, viewing
  clip_number = 1
  
  # Read in the file, and do preliminary cleaning
  my_data = read.table(file = film_files[film_number], 
                       sep = '\t', header = TRUE) 
  current_name = nick_names[film_number]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$subject = substr(my_data$RecordingName, 1, 3)
  # my_data = my_data[which(!(my_data$subject %in% c("107", "113", "114", "124"))),]
  # my_data = my_data[which(!(my_data$subject %in% c("107"))),]
  
  my_data = my_data[,c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "RecordingName",
                       "GazePointIndex")]
  
  my_data$RecordingNumber = substring(my_data$RecordingName, 5)
  my_data$RecordingNumber = as.factor(my_data$RecordingNumber)
  my_data$GazePointIndex = as.numeric(my_data$GazePointIndex)
  
  
  max_index = max(my_data$GazePointIndex)
  new_data = data.frame(TWC = NA)
  reduced = floor(max_index / reduction_factor)
  gazes_numbers = 1
  
  # We cycle through the groups of frames and perform the total within sum of squares
  # from the centroid using the kmeans function.
  for(frame in 1:reduced) {
    temp_data = my_data[which(my_data$RecordingNumber == clip_number),]
    temp_data = temp_data[which(temp_data$GazePointIndex %in% 
                                  ((frame - 1)*reduction_factor + 1):(frame*reduction_factor) ),]
    
    temp_row = data.frame(TWC = NA)
    
    kframe <- kmeans(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                               c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")], 
                     centers = 1)
    temp_row$TWC = kframe$tot.withinss
    
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
  
  # Now we put the last few rows together:
  temp_data = my_data[which(my_data$RecordingName == clip_number),]
  temp_data = temp_data[which(temp_data$GazePointIndex %in% 
                                (frame*reduction_factor + 1):(max_index)),]
  temp_row = data.frame(TWC = NA)
  
  # It is possible that we did not have enough data to perform kmeans on the remaining
  # observations, so we put in this if catch.
  if(!(length(which(!is.na(temp_data$FixationPointX..MCSpx.))) == 0 )){
    kframe <- kmeans(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                               c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")], 
                     centers = 1)
    temp_row$TWC = kframe$tot.withinss
    new_data = rbind(new_data, temp_row)
  }
  
  new_data = new_data[-1,]
  
  write.csv(new_data, current_name)
  print("starting next one")
}
library(beepr)
beep(3)

#########################################################
############## Comparing TWC between films ##############
#########################################################

data_names = c("holiday", "lumiere", "musketeer", 
               "NbyNW", "playtime1", "playtime2")

holiday = read.csv(nick_names[1])
names(holiday) = c("TimePoint", "TotalWithinSS")

lumiere = read.csv(nick_names[2])
names(lumiere) = c("TimePoint", "TotalWithinSS")

musketeer = read.csv(nick_names[3])
names(musketeer) = c("TimePoint", "TotalWithinSS")

NbyNW = read.csv(nick_names[4])
names(NbyNW) = c("TimePoint", "TotalWithinSS")

playtime1 = read.csv(nick_names[5])
names(playtime1) = c("TimePoint", "TotalWithinSS")

playtime2 = read.csv(nick_names[6])
names(playtime2) = c("TimePoint", "TotalWithinSS")

data_list = list(holiday, lumiere, musketeer, NbyNW, playtime1, playtime2)
max_index = max(nrow(holiday), nrow(lumiere), nrow(musketeer), nrow(NbyNW), 
                nrow(playtime1), nrow(playtime2))

full_data = data.frame(TimePoint = rep(1:max_index, times = 6),
                       TotalWithinSS = rep(NA, times = (max_index*6)),
                       Film = rep(data_names, each = max_index))
for(index in 1:6) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = (nrow(data_list[[index]])) + lower_index - 1
  full_data$TotalWithinSS[lower_index:upper_index] = data_list[[index]]$TotalWithinSS
}

ggplot(full_data, aes(x=TimePoint, y=TotalWithinSS, group=Film)) +
  geom_line(aes(color=as.factor(Film))) + labs(color = "Film") + 
  ggtitle("WithinSS for each film, First Viewings") + 
  xlab("TimePoint (in groups of 100)") + xlim(0,75)

# Pulling apart the time series
new_full_data_1 = data.frame(TimePoint = rep(1:75, times = 3),
                             TotalWithinSS = rep(NA, times = 3*75),
                             Film = rep(data_names[1:3], each = 75))
new_full_data_2 = data.frame(TimePoint = rep(1:75, times = 3),
                             TotalWithinSS = rep(NA, times = 3*75),
                             Film = rep(data_names[4:6], each = 75))
for(index in 1:3) {
  lower_index = ((index - 1)*75 + 1)
  upper_index = lower_index + 75 - 1
  new_full_data_1$TotalWithinSS[lower_index:upper_index] = data_list[[index]]$TotalWithinSS[1:75]
}
for(index in 1:3) {
  lower_index = ((index - 1)*75 + 1)
  upper_index = lower_index + 75 - 1
  new_full_data_2$TotalWithinSS[lower_index:upper_index] = data_list[[(index+3)]]$TotalWithinSS[1:75]
}


ggplot(new_full_data_1, aes(x = TimePoint, y = TotalWithinSS)) + 
  geom_line(aes(color = as.factor(Film))) + 
  facet_grid(Film ~ .) + theme(legend.position = "none") + ylim(0,4.5e+08)
ggplot(new_full_data_2, aes(x = TimePoint, y = TotalWithinSS)) + 
  geom_line(aes(color = (as.factor(Film)))) + 
  facet_grid(Film ~ .) + theme(legend.position = "none") + 
  scale_color_manual(values=c("turquoise3", "steelblue3", "magenta2")) + ylim(0,4.5e+08)

