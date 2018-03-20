#Load libraries
library("tidyverse")
library("googlesheets")
#GoogleSheets Info
#https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html

#Function to convert university date code into decipherable terms
convertdate <- function(x) {

  if (is.na(x)) {return(x)}
  #first charater is either 0 or 1, 0 = 19, 1 = 20
  #Second two characters are the year, concate to first character transformation
  #Last character is the semester, save to second vector spot
  
  if (substr(x, 1, 1) == 0) {
    date <- paste(19, substr(x, 2, 3), sep = "")#, substr(x, 4, 4)
  } else if (substr(x, 1, 1) == 1) {
    date <- paste(20, substr(x, 2, 3), sep = "")#, substr(x, 4, 4)
  }
  # Last/Fourth character conver to semseter, 2 = Winter, 4 = Spring, 6 = Summer, 8 = Fall
 # if (date[2] == 2) { date[2] <- 'Winter'
 # } else if (date[2] == 4) {date[2] <- 'Spring'
 # } else if (date[2] == 6) {date[2] <- 'Summer'
 # } else if (date[2] == 8) {date[2] <- 'Fall'
 # } 
  #return date
  return(as.numeric(date))
}

pi.list <- select(gs_read(ss = gs_title("PI_Contact_Info_Tracker")), PI)
t32 <- gs_read(ss = gs_title(paste("T_32_PhD_Info_",pi.list[1, 1], sep = "")))

for(x in 2:length(pi.list$PI)) {
   t32 <- bind_rows(t32, gs_read(ss = gs_title(paste("T_32_PhD_Info_",pi.list[x, 1], sep = ""))))
}

t32.new <- left_


#read in data from Google Sheet
#First Time requires the input of an authroazation key
student_data <- gs_read(ss = gs_title("T_32"))



#Remove Unnessary Column
student_data <- student_data %>% select(-Student_Name_1)

#Convert to Usable Dates
student_data$First_Term <- sapply(student_data$First_Term, convertdate)
student_data$Most_Recent <- sapply(student_data$Most_Recent, convertdate)
student_data$GABIOLMS <- sapply(student_data$GABIOLMS, convertdate)
student_data$GABIOLPHD <- sapply(student_data$GABIOLPHD, convertdate)

student_data$Citz_Stat_Cd <- as.factor(student_data$Citz_Stat_Cd)


glimpse(student_data)


filter.year <- 2000

#How many students per year? (dot and line plot)
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = First_Term)) +
  geom_point(stat = 'count') +
  geom_line(aes(group = 1), stat = 'count') +
  ylab("# of Students") +
  xlab("Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Demographics (pie or barcharts)
  #Citizenship status
  #Citz Stat Cd	US Citizenship Status (1 = Citizen, 3 = Perm Res, 4 = Non-Resident Alien)
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Citz_Stat_Cd)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("U.S. Citizenship Status") +
  scale_x_discrete(labels = c("Citizen","Permanent Resisdent", "Non-Resident Alien")) +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Citz_Stat_Cd)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "U.S. Citizenship Status") +
  scale_fill_discrete(labels = c("Citizen","Permanent Resisdent", "Non-Resident Alien"))

  #Country of citizenship
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Cit)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Coutry of Citizenship") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Cit)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Coutry of Citizenship")

  #Ethnicity
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Nyu_Ipeds_Eth_Ld)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Ethnicity") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Nyu_Ipeds_Eth_Ld)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Ethnicity")

  #Qualifications upon entry to program - Unsure what this is? BS vs BA???
# BA = Bachelor of the Arts
# BS = Bachelor of Science
# BBA = Bachelor of Business Administration
# BSA = Bachelor or Science and Arts
# BENG = Bachelor of Engineering
# BZ = Unkonwn
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Deg)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Undergraduate Degree") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Deg)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Undergraduate Degree")

  #GRE scores - How to look at it? Have 4 columns of data. Verbal and Quantative, both old and new syle of scoring

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = )) +
  geom_bar() +
  ylab("# of Students") +
  xlab("GRE Scorces") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = )) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "GRE Scorces")

#PhD experience (histograms or boxplot with points overlayed)
  #Time to degree
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Years )) +
  geom_histogram(binwidth = 0.5) +
  ylab("# of Students") +
  xlab("Years to PhD") +
  scale_y_continuous(limits = c(0,30), expand=c(0,0)) +
  theme_bw() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Years )) +
  geom_boxplot() +
  ylab("Years to PhD") +
  theme_bw() 

  #Number of semesters of teaching
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Semesters_of_teaching )) +
  geom_histogram(binwidth = 1) +
  ylab("# of Students") +
  xlab("Semesters of Teaching") +
  scale_y_continuous(limits = c(0,30), expand=c(0,0)) +
  theme_bw() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Semesters_of_teaching )) +
  geom_boxplot() +
  ylab("Semesters of Teaching") +
  theme_bw() 


  #Number of published papers - Need to Calculate Data
    #1st author
    #co-author


  #Subsequent position (pie or barplot) _ Need to normalize Data
ggplot(student_data, aes(????)) +
  geom_bar()

+ coord_polar("y", start=0)

    #Postdoc vs not postdoc
    #Postdoc institution
    #Summary of employment

  #Test predictors of PhD experience
    #GRE vs number of papers (scatter plot)
ggplot(student_data, aes(????)) +
  geom_point()
    #citizenship/ethnicity vs number of papers (boxplots)
ggplot(student_data, aes(????)) +
  geom_boxplot()
    #Number of papers vs postdoc or not postdoc (boxplots)
ggplot(student_data, aes(????)) +
  geom_boxplot()
    #Semesters teaching vs number of papers (boxplots)

#PI commitment - Need to figure out how to calculate
  #Number of students per PI (histogram)
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(PI)) %>%
  ggplot(aes(x = PI )) +
  geom_histogram(stat = "count", binwidth = 1) +
  ylab("# of Students") +
  xlab("Students/PI") +
  scale_y_continuous(limits = c(0,30), expand=c(0,0)) +
  theme_bw() 

  #Number of student co-mentored (class within the above histogram or no plot)
  #Number of PIs in co-mentoring relationships
  #Distribution of committee membership (how many committees is each PI on) (histogram) - Student Comittees, need to figure out how to calculate into a graph
ggplot(student_data, aes(????)) +
  geom_histogram()






