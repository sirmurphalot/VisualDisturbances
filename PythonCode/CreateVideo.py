# Merging image files into movie
# Author: Alexander Murph
# 2 / 14 / 2018

##########################################################################################################
##########################################################################################################
## Update 9/28: Uploading this to GitHub.  If one wishes to recreate these results, they'll need to add ##
## in their own file paths -- these are specific to my directories.  Data will NOT be released yet.     ##
## When it is I'll update the file paths so these files will work directly from GitHub                  ##
##########################################################################################################
##########################################################################################################

import cv2
import os

# frame_folder = ["hulot_frames_V1", 
#                 "hulot_frames_V2", 
#                 "lumiere_frames_V1", 
#                 "lumiere_frames_V2",
#                 "muketeer_frames_V1",
#                 "muketeer_frames_V2", 
#                 "N_by_NW_frames_V1", 
#                 "N_by_NW_frames_V2",
#                 "playtime1_frames_V1", 
#                 "playtime1_frames_V2",
#                 "playtime2_frames_V1",
#                 "playtime2_frames_V2"]

frame_folder = ["hulot_frames_V1", 
                "lumiere_frames_V1", 
                "muketeer_frames_V1",
                "N_by_NW_frames_V1", 
                "playtime1_frames_V1", 
                "playtime2_frames_V1"]

for index in range(12):

    film_number = index

    image_folder = '../FilmOutputs/Distance_by_frame/' + frame_folder[film_number]
    video_name = '../Output/DistanceVideos/' + frame_folder[film_number] + '.avi'

    images = [img for img in os.listdir(image_folder) if img.endswith(".jpeg")]

    frame = cv2.imread(os.path.join(image_folder, images[0]))
    height, width, layers = frame.shape

    video = cv2.VideoWriter(video_name, -1, 1, (width,height))

    for image in images:
        video.write(cv2.imread(os.path.join(image_folder, image)))

    cv2.destroyAllWindows()
    video.release()



    # image_folder = '../FilmOutputs/Reduced_by_frame_circ/' + frame_folder[film_number]
    # video_name = '../Output/ReducedVideos_circ/' + frame_folder[film_number] + '.avi'

    # images = [img for img in os.listdir(image_folder) if img.endswith(".jpeg")]

    # frame = cv2.imread(os.path.join(image_folder, images[0]))
    # height, width, layers = frame.shape

    # video = cv2.VideoWriter(video_name, -1, 1, (width,height))

    # for image in images:
    #     video.write(cv2.imread(os.path.join(image_folder, image)))

    cv2.destroyAllWindows()
    video.release()




