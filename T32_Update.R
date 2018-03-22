#Load libraries
library("tidyverse")
library("googlesheets")
library("ggpubr")
#GoogleSheets Info
#https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html



#Function to convert university date code into decipherable terms, returns the year
convertdate_year <- function(x) {
  
  if (is.na(x)) {return(x)}
  #first charater is either 0 or 1, 0 = 19, 1 = 20
  #Second two characters are the year, concate to first character transformation
  #Last character is the semester, save to second vector spot
  
  if (substr(x, 1, 1) == 0) {
    date <- paste(19, substr(x, 2, 3), sep = "")
  } else if (substr(x, 1, 1) == 1) {
    date <- paste(20, substr(x, 2, 3), sep = "")
  }
  return(as.numeric(date))
}

#Function to convert university date code into decipherable terms, returns the semester
#May not need
convertdate_semester <- function(x) {  
  if (is.na(x)) {return(x)}
  
  # Last/Fourth character conver to semseter, 2 = Winter, 4 = Spring, 6 = Summer, 8 = Fall
  if (substr(x, 4, 4)) { date <- 'Winter'
  } else if (substr(x, 4, 4)) {date <- 'Spring'
  } else if (substr(x, 4, 4)) {date <- 'Summer'
  } else if (substr(x, 4, 4)) {date <- 'Fall'
  } 
  #return date
  return(as.character(date))
}

#Reads in list of PIs
pi.list <- select(gs_read(ss = gs_title("PI_Contact_Info_Tracker")), PI)
#Adds in first
t32 <- gs_read(ss = gs_title(paste("T_32_PhD_Info_",pi.list[1, 1], sep = "")))
#goes through the rest
for(x in 2:length(pi.list$PI)) {
  t32 <- bind_rows(t32, gs_read(ss = gs_title(paste("T_32_PhD_Info_",pi.list[x, 1], sep = ""))))
}

#gs_new("T32_Updated", ws_title = "Data", input = t32)
gs_edit_cells("T32_Updated", ws = "Data", input = t32)

# Not working as intended.
#Possile Stratergy. Compile data. Use Table names to delete all but
#t32.new <- left_join(gs_read(ss = gs_title("T_32")), t32)

#read in data from Google Sheet
#First Time requires the input of an authroazation key
student_data <- gs_read(ss = gs_title("T_32"))

#Remove Unnessary Column
student_data <- student_data %>% select(-Student_Name_1)

#Convert to Usable Dates - Years only for now
student_data$First_Term <- sapply(student_data$First_Term, convertdate_year)
student_data$Most_Recent <- sapply(student_data$Most_Recent, convertdate_year)
student_data$GABIOLMS <- sapply(student_data$GABIOLMS, convertdate_year)
student_data$GABIOLPHD <- sapply(student_data$GABIOLPHD, convertdate_year)

#Corrects for later analysis
student_data$Citz_Stat_Cd <- as.factor(student_data$Citz_Stat_Cd)
#student_data$EV <- as.factor(student_data$EV)
#student_data$EQ <- as.factor(student_data$EQ)
#student_data$EVP <- as.factor(student_data$EVP)
#student_data$EQP <- as.factor(student_data$EQP)


glimpse(student_data)
