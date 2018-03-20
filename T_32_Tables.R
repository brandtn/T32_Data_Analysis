#Load libraries
library("tidyverse")
library("googlesheets")


student_data <- gs_read(ss = gs_title("T_32"))

glimpse(student_data)


#How many students per year? (dot and line plot)
ggplot(student_data, aes(????)) +
  geom_point() +
  geom_line()

#Demographics (pie or barcharts)
  #Citizenship status
  #Country of citizenship
  #Ethnicity    
  #Qualifications upon entry to program
  #GRE scores

#PhD experience (histograms or boxplot with points overlayed)
  #Time to degree
  #Number of semesters of teaching
  #Number of published papers
    #1st author
    #co-author
  #Subsequent position (pie or barplot)
    #Postdoc vs not postdoc
    #Postdoc institution
    #Summary of employment
  #Test predictors of PhD experience
    #GRE vs number of papers (scatter plot)
    #citizenship/ethnicity vs number of papers (boxplots)
    #Number of papers vs postdoc or not postdoc (boxplots)
    #Semesters teaching vs number of papers (boxplots)

#PI commitment
  #Number of students per PI (histogram)
  #Number of student co-mentored (class within the above histogram or no plot)
  #Number of PIs in co-mentoring relationships
  #Distribution of committee membership (how many committees is each PI on) (histogram)


ggplot(student_data, aes(First_Term)) +
  geom_bar()

ggplot(student_data, aes(School)) +
  geom_bar()

ggplot(student_data, aes(Years)) +
  geom_bar()

ggplot(student_data, aes(First_Term, Years)) +
  geom_count()

ggplot(student_data, aes(Cit)) +
  geom_bar()

ggplot(student_data, aes(Nyu_Ipeds_Eth_Ld)) +
  geom_bar()

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



