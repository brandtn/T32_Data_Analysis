#Load libraries
library("tidyverse")
library("googlesheets")


student_data <- gs_read(ss = gs_title("T_32"))

glimpse(student_data)

#function to convert date
#take in x which is a 4 digit number
convertdate {
  #convert x to a character
  
  #first charater is either 0 or 1
  #0 = 19
  #1 = 20

  #Second two characters are the year
  #concate to first character transformation
  # set to x convert to intergar
  
  # Last/Fourth character conver to semseter
  # 2 = Winter
  # 4 = Spring
  # 6 = Summer
  # 8 = Fall
  #set to y
  #return x and y
}



