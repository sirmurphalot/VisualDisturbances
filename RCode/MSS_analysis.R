###############################################
# Visual Disturbances Film Project
# Stat Analysis: MSS Between Films
# Author: Alexander Murph August-November 2019
# Bucknell University, UNC Chapel Hill
###############################################

###############################################
## Notes: Script takes in data to clean and aggregate information for every
# (reduction_factor) timepoints.  Then finds the average MSS
# variation for each aggregated time point.
# This analysis compares the raw MSS values between films.
# We use the created datasets to produce the graphs discussed in the paper (see bottom of script for graphs).
## Developer Note: "TWC" was used throughout this code due to earlier analysis.  This should
# be read as equivalent to "MSS"
###############################################


# The size of the grouping of frames we wish to consider
reduction_factor = 1

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

#################################################################################
############ Data allowing for only single clusters; MSS Calculation ############
#################################################################################

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
    
    if(nrow(as.matrix(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                                   c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")])) == 0) {
      next
    }
    kframe <- kmeans(as.matrix(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                               c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")]), 
                     centers = 1)
    temp_row$TWC = kframe$tot.withinss / nrow(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                                                        c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")])
    
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
    print("got here")
    print(length(which(!is.na(temp_data$FixationPointX..MCSpx.))))
    kframe <- kmeans(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                               c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")], 
                     centers = 1)
    temp_row$TWC = kframe$tot.withinss / nrow(temp_data[which(!is.na(temp_data$FixationPointX..MCSpx.)),
                                                        c("FixationPointX..MCSpx.", "FixationPointY..MCSpx.")])
    new_data = rbind(new_data, temp_row)
  }
  
  new_data = new_data[-1,]
  
  write.csv(new_data, current_name)
  print("starting next one")
}
library(beepr)
beep(3)

#########################################################################
############## Comparing MSS between films; visualizations ##############
#########################################################################

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

# Pulling apart the time series
new_full_data_1 = data.frame(TimePoint = rep(1:max_index, times = 3),
                             TotalWithinSS = rep(NA, times = 3*max_index),
                             Film = rep(data_names[1:3], each = max_index))
new_full_data_2 = data.frame(TimePoint = rep(1:max_index, times = 3),
                             TotalWithinSS = rep(NA, times = 3*max_index),
                             Film = rep(data_names[4:6], each = max_index))
for(index in 1:3) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = lower_index + max_index - 1
  new_full_data_1$TotalWithinSS[lower_index:upper_index] = data_list[[index]]$TotalWithinSS[1:max_index]
}
for(index in 1:3) {
  lower_index = ((index - 1)*max_index + 1)
  upper_index = lower_index + max_index - 1
  new_full_data_2$TotalWithinSS[lower_index:upper_index] = data_list[[(index+3)]]$TotalWithinSS[1:max_index]
}


ggplot(new_full_data_1, aes(x = TimePoint, y = TotalWithinSS)) +
  geom_line(aes(color = as.factor(Film))) + labs(y ="MSS") +
  facet_grid(Film ~ .) + theme(legend.position = "none", panel.background = element_blank()) + ylim(0,190000) + 
  scale_color_manual(values=c("red", "green", "blue")) 
ggplot(new_full_data_2, aes(x = TimePoint, y = TotalWithinSS)) + 
  geom_line(aes(color = (as.factor(Film)))) + labs(y ="MSS") +
  facet_grid(Film ~ .) + theme(legend.position = "none", panel.background = element_blank()) + ylim(0,190000) + 
  scale_color_manual(values=c("turquoise3", "steelblue3", "magenta2")) 


# Drawing boxplots for analysis
my.bp <- ggplot(data=rbind(new_full_data_1, new_full_data_2), aes(y= TotalWithinSS, x=as.factor(Film), fill = as.factor(Film)) ) # Creates boxplots
my.bp <- my.bp + geom_boxplot() # Adds color
my.bp <- my.bp +  ylab("MSS") + xlab("Films") # Adds kaveks
my.bp <- my.bp + scale_fill_manual(values=c("red", "green", "blue", 
                                            "turquoise3", "steelblue3", "magenta2"))
my.bp <- my.bp + theme(legend.position="none", panel.background = element_blank()) + coord_flip()
my.bp

