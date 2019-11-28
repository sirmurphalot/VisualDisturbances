"""
    Drawing the aggregate box and points between clips for each film.
    Author: Alexander Murph
    2/14/2018    
"""

##########################################################################################################
##########################################################################################################
## Update 9/28: Uploading this to GitHub.  If one wishes to recreate these results, they'll need to add ##
## in their own file paths -- these are specific to my directories.  Data will NOT be released yet.     ##
## When it is I'll update the file paths so these files will work directly from GitHub                  ##
##########################################################################################################
##########################################################################################################
 
# Import a library of functions called 'pygame'
import pygame
import csv
 
# Initialize the game engine
pygame.init()
 
# Define some colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)
RED = (255, 0, 0)
GREY = (127, 127, 127)
 
PI = 3.141592653

# Files Names
nick_names = ["../Data/Hulot_Holiday_Averages.csv",
               "../Data/Lumiere_Averages.csv",
               "../Data/Musketeer_Averages.csv",
               "../Data/N_by_NW_Averages.csv",
               "../Data/playtime1_Averages.csv",
               "../Data/playtime2_Averages.csv"]

names = ["Hulot_Holiday",
               "Lumiere_Averages",
               "Musketeer_Averages",
               "N_by_NW_Averages",
               "playtime1_Averages",
               "playtime2_Averages"]

film_number = 5


# Opening our data
data_text = nick_names[film_number]
with open(data_text, 'r') as csvfile:
    reader = csv.reader(csvfile)
    data_list = list(reader)

 
# Set the height and width of the screen
size = (1280, 720)
screen = pygame.display.set_mode(size)
 
pygame.display.set_caption("Film Watching Data")
 
# Loop until the user clicks the close button.
done = False
clock = pygame.time.Clock()

#data_list = [[5, 200, 300, 50, 50]]
 
# Loop as long as done == False
# while not done:
 
#     for event in pygame.event.get():  # User did something
#         if event.type == pygame.QUIT:  # If user clicked close
#             done = True  # Flag that we are done so we exit this loop
 
#     # All drawing code happens after the for loop and but
#     # inside the main while not done loop.

screen.fill(WHITE)

#for data_row in data:
for row in data_list:
    print(row)
    if row[1] == 'Participant_Number':
        continue
    average_point_x = float(row[2])
    average_point_y = float(row[3])
    sd_point_x = float(row[4])
    sd_point_y = float(row[5])

    if row[6] == "1":
        color_box = RED
        color_triangle = BLUE
    else:
        color_box = GREEN
        color_triangle = BLACK

    pygame.draw.polygon(screen, color_triangle, [[average_point_x, average_point_y - 5], \
        [average_point_x - 3, average_point_y + 5], [average_point_x + 3, average_point_y + 5]], 2)

    pygame.draw.rect(screen, color_box, [average_point_x - sd_point_x, \
     average_point_y - sd_point_y, \
     2 * sd_point_x, \
     2 * sd_point_y], 2)

# Clear the screen and set the screen background



# Creating the upper x-axis
pygame.draw.line(screen, BLACK, [0, 0], [1280, 0], 5)

pygame.draw.line(screen, BLACK, [320, 0], [320, 10], 5)
font = pygame.font.SysFont('Calibri', 25, True, False)
text = font.render("320", True, BLACK)
screen.blit(text, [308, 11])

pygame.draw.line(screen, BLACK, [640, 0], [640, 10], 5)
text = font.render("640", True, BLACK)
screen.blit(text, [618, 11])

pygame.draw.line(screen, BLACK, [960, 0], [960, 10], 5)
text = font.render("960", True, BLACK)
screen.blit(text, [938, 11])

pygame.draw.line(screen, BLACK, [1280, 0], [1280, 10], 5)
text = font.render("1280", True, BLACK)
screen.blit(text, [1230, 11])


# Creating the left-side y-axis
pygame.draw.line(screen, BLACK, [0, 0], [0, 720], 5)

pygame.draw.line(screen, BLACK, [0, 180], [10, 180], 5)
font = pygame.font.SysFont('Calibri', 25, True, False)
text = font.render("180", True, BLACK)
screen.blit(text, [11, 180])

pygame.draw.line(screen, BLACK, [0, 360], [10, 360], 5)
text = font.render("360", True, BLACK)
screen.blit(text, [11, 360])

pygame.draw.line(screen, BLACK, [0, 540], [10, 540], 5)
text = font.render("540", True, BLACK)
screen.blit(text, [11, 540])

pygame.draw.line(screen, BLACK, [0, 720], [10, 720], 5)
text = font.render("720", True, BLACK)
screen.blit(text, [11, 685])

font = pygame.font.SysFont('Calibri', 30, True, False)
text = font.render(names[film_number], True, BLUE)
screen.blit(text, [1000, 600])

font = pygame.font.SysFont('Calibri', 15, True, False)
text = font.render("Clip 1 = ", True, BLACK)
screen.blit(text, [1000, 650])
text = font.render("Blue", True, BLUE)
screen.blit(text, [1055, 650])
text = font.render("Red", True, RED)
screen.blit(text, [1100, 650])

text = font.render("Clip 2 = ", True, BLACK)
screen.blit(text, [1000, 670])
text = font.render("Black", True, BLACK)
screen.blit(text, [1055, 670])
text = font.render("Green", True, GREEN)
screen.blit(text, [1100, 670])

# Draw on the screen several lines from (0,10) to (100,110)
# 5 pixels wide using a loop
#for y_offset in range(0, 100, 10):

#   pygame.draw.line(screen, RED, [0, 10 + y_offset], [100, 110 + y_offset], 5)


# Draw a rectangle
#pygame.draw.rect(screen, BLACK, [20, 20, 250, 100], 2)

# Draw an ellipse, using a rectangle as the outside boundaries
#pygame.draw.ellipse(screen, BLACK, [20, 20, 250, 100], 2)

# Draw an arc as part of an ellipse.
# Use radians to determine what angle to draw.
#pygame.draw.arc(screen, BLACK, [20, 220, 250, 200], 0, PI / 2, 2)
#pygame.draw.arc(screen, GREEN, [20, 220, 250, 200], PI / 2, PI, 2)
#pygame.draw.arc(screen, BLUE, [20, 220, 250, 200], PI, 3 * PI / 2, 2)
#pygame.draw.arc(screen, RED, [20, 220, 250, 200], 3 * PI / 2, 2 * PI, 2)

# This draws a triangle using the polygon command
#pygame.draw.polygon(screen, BLACK, [[100, 100], [0, 200], [200, 200]], 5)

# Select the font to use, size, bold, italics
#font = pygame.font.SysFont('Calibri', 25, True, False)

# Render the text. "True" means anti-aliased text.
# Black is the color. This creates an image of the
# letters, but does not put it on the screen
#text = font.render("My text", True, BLACK)

# Put the image of the text on the screen at 250x250
#screen.blit(text, [250, 250])

# Go ahead and update the screen with what we've drawn.
# This MUST happen after all the other drawing commands.

pygame.display.flip()

pygame.image.save(screen, "../Output/BetweenClips/" + names[film_number] + ".jpeg")

# This limits the while loop to a max of 60 times per second.
# Leave this out and we will use all CPU we can.
clock.tick(60)
 
# Be IDLE friendly
pygame.quit()