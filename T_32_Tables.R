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

# Not working as intended.
#Possile Stratergy. Compile data. Use Table names to delete all but
#t32.new <- left_join(gs_read(ss = gs_title("T_32")), t32)

#read in data from Google Sheet
#First Time requires the input of an authroazation key
student_data <- gs_read(ss = gs_title("T_32_Updated_03222018"))

#Remove Unnessary Column
student_data <- student_data %>% select(-Student_Name_1)

#Convert to Usable Dates - Years only for now
student_data$First_Term <- sapply(student_data$First_Term, convertdate_year)
student_data$Most_Recent <- sapply(student_data$Most_Recent, convertdate_year)
student_data$GABIOLMS <- sapply(student_data$GABIOLMS, convertdate_year)
student_data$GABIOLPHD <- sapply(student_data$GABIOLPHD, convertdate_year)

#Corrects for later analysis
student_data$Citz_Stat_Cd <- as.factor(student_data$Citz_Stat_Cd)

faculty <- gs_read(ss = gs_title("T_32_Updated_03222018"))

glimpse(student_data)

#global filter parameter
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
  scale_y_continuous(expand=c(0,0)) +
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
  scale_y_continuous(expand=c(0,0)) +
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
  scale_y_continuous(expand=c(0,0)) +
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
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Deg)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Undergraduate Degree")

#GRE scores - How to look at it? 
#Have 4 columns of data. Verbal and Quantative, both old and new syle of scoring
student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EV)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Verbal GRE Scorces (New Scale)") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EQ)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Quantative GRE Scorces (New Scale)") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EVP)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Verbal GRE Scorces Percentage") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EQP)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Quantative GRE Scorces Percentage") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 


student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = EV)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Verbal GRE Scorces (New Scale)")

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = EQ)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Quantative GRE Scorces (New Scale)")

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = EVP)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Verbal GRE Scorces Percentage")

student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = EQP)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Quantative GRE Scorces Percentage")





#PhD experience (histograms or boxplot with points overlayed)
  #Time to degree
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Years )) +
  geom_histogram(binwidth = 0.5) +
  ylab("# of Students") +
  xlab("Years to PhD") +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Years )) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge()) +
  ylab("Years to PhD") +
  xlab("") +
  theme_bw() +
  theme(axis.ticks.x = element_blank())

  #Number of semesters of teaching
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Semesters_of_teaching )) +
  geom_histogram(binwidth = 1) +
  ylab("# of Students") +
  xlab("Semesters of Teaching") +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Semesters_of_teaching )) +
  geom_boxplot() +
  ylab("Semesters of Teaching") +
  theme_bw() 


  #Number of published papers - 
#Calculate Data
paper_data <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  select(Student_Name, Paper_1:Paper_16) %>%
  gather(Paper_1:Paper_16, key = 'Papers', value = 'Paper_Citation') %>%
  filter(!is.na(Paper_Citation)) %>%
  mutate(Last_Name = gsub(",.*", "", Student_Name), First_Author = gsub(" .*", "", Paper_Citation) )
 

first_author <-  paper_data %>%
  filter(Last_Name == First_Author) %>%
    select(Student_Name)
    
first_author <- mutate(arrange(distinct(first_author), Student_Name), First_Author = tabulate(as.factor(first_author$Student_Name)))
  
co_author <-  paper_data %>%
  filter(!(Last_Name == First_Author)) %>%
    select(Student_Name) 
    
co_author <- mutate(arrange(distinct(co_author), Student_Name), Co_Author = tabulate(as.factor(co_author$Student_Name)))

total_author <-  paper_data %>%
  select(Student_Name) 

total_author <- mutate(arrange(distinct(total_author), Student_Name), Number_Papers = tabulate(as.factor(total_author$Student_Name)))

author_data <- left_join(total_author, first_author, by = "Student_Name")
author_data <- left_join(author_data, co_author, by = "Student_Name")
names(author_data) <- c("Student_Name", "Total_Papers", "First_Author", "Co_Author")
author_data$First_Author <- replace_na(author_data$First_Author, 0)
author_data$Co_Author <- replace_na(author_data$Co_Author, 0)


#1st author
author_data %>%
  ggplot(aes(x = First_Author )) +
  geom_bar() +
  ylab("# of Students") +
  xlab("# of First Author Papers") +
  scale_y_continuous(limits = c(0,20), expand=c(0,0)) +
  theme_bw() 

#author_data %>%
#  ggplot(aes(x = "", y= First_Author )) +
 # geom_boxplot() +
#  ylab("# of First Author Papers") +
#  theme_bw()

#co-author
author_data %>%
  ggplot(aes(x = Total_Papers )) +
  geom_bar() +
  ylab("# of Students") +
  xlab("# of Co-Author Papers") +
  scale_y_continuous(limits = c(0,20), expand=c(0,0)) +
  theme_bw() 

#author_data %>%
#  ggplot(aes(x = "", y= Co_Author )) +
#  geom_boxplot() +
#  ylab("#ofPapers") +
#  theme_bw()

#Subsequent position (pie or barplot)
#Postdoc vs not postdoc
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = (Employment_upon_graduation != 'Post-Doctoral Fellowship'))) +
  geom_bar() +
  xlab("Position upon gradutation") +
  ylab("# of Students") +
  scale_x_discrete(labels = c("Post-Doctoral Fellowship","Other", "Unkown")) +
  scale_y_continuous(limits = c(0,50), expand=c(0,0)) +
  theme_bw() 

student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", fill = (Employment_upon_graduation != 'Post-Doctoral Fellowship'))) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Position upon gradutation") +
  scale_fill_discrete(labels = c("Post-Doctoral Fellowship","Other", "Unkown"))

#Postdoc institution
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  filter(Employment_upon_graduation == 'Post-Doctoral Fellowship') %>%
  ggplot(aes(x = Postdoctoral_institution)) +
  geom_bar() +
  ylab("# of Students") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Summary of employment
??????

#Test predictors of PhD experience
    #GRE vs number of papers (scatter plot)
student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(student_data, aes(x = , y = )) +
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
 #Number of student co-mentored (class within the above histogram or no plot)
 #Number of PIs in co-mentoring relationships

pi_data <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(PI)) %>%
  select(PI, CO_PI) %>%
  gather(key = 'Position') %>%
  select(value) %>%
  filter(!is.na(value))
names(pi_data) <- c("PI")

copi_data <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(PI)) %>%
  select(PI, CO_PI) %>% 
  filter(!is.na(CO_PI))

num.comentored <- count(copi_data)
num.pi_comentoring <- length(unique(c(copi_data$CO_PI, copi_data$PI)))

pi_data <- mutate(arrange(distinct(pi_data), PI), Number_Students = tabulate(as.factor(pi_data$PI)))

pi_data %>%
  ggplot(aes(x = Number_Students )) +
  geom_bar() +
  annotate("text", x = 7, y = 9, label = paste("Number of Students Co-Mentored =",num.comentored)) +
  annotate("text", x = 7, y = 8, label = paste("Number of PI's Co-Mentoring =",num.pi_comentoring)) +
  ylab("# of PIs") +
  xlab("Students/PI") +
  scale_y_continuous(limits = c(0,10), expand=c(0,0)) +
  theme_bw() 

#Distribution of committee membership (how many committees is each PI on) (histogram) - Student Comittees, need to figure out how to calculate into a graph

#Need to check if we count committee's PI is on due to student

commit_data <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(PI)) %>%
  select(PI, CO_PI, PhD_commitee_member_1, PhD_commitee_member_2, PhD_commitee_member_3, PhD_commitee_member_4_external) %>%
  gather(key = 'Position') %>%
  filter(!is.na(value)) %>%
  select(value)
names(commit_data) <- c("PI")

#Detects if faculty is on a committee not number of times
#str_detect(commit_data, faculty$Last_Name)

#Count Number of Times Faculty is on a committee
str_count(commit_data, faculty$Last_Name)

commit_data <- mutate(faculty, Number_Committees = str_count(commit_data, faculty$Last_Name))

commit_data <- mutate(arrange(distinct(commit_data), PI), Number_PIs = tabulate(as.factor(commit_data$PI)))


commit_data %>%
  ggplot(aes(x = Number_PIs )) +
  geom_bar() +
  ylab("# of PIs") +
  xlab("# of Committee Memberships") +
  scale_y_continuous(limits = c(0,10), expand=c(0,0)) +
  theme_bw() 





