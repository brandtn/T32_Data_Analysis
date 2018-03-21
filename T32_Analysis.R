library("tidyverse")
library("ggpubr")

load("T32.RData")

glimpse(student_data)

#Global Filter Parameters
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

ggsave("StudentsPerYear.pdf")

#Demographics (pie or barcharts)
#Citizenship status
#Citz Stat Cd	US Citizenship Status (1 = Citizen, 3 = Perm Res, 4 = Non-Resident Alien)
bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Citz_Stat_Cd)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("U.S. Citizenship Status") +
  scale_x_discrete(labels = c("Citizen","Permanent Resisdent", "Non-Resident Alien")) +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

pie <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Citz_Stat_Cd)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "U.S. Citizenship Status") +
  scale_fill_discrete(labels = c("Citizen","Permanent Resisdent", "Non-Resident Alien"))

#Save to one file
multi.page <- ggarrange(bar, pie, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Citzenship.pdf")


#Country of citizenship
bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Cit)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Country of Citizenship") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pie <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Cit)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Country of Citizenship")

#Save to one file
multi.page <- ggarrange(bar, pie, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Country_of_Citzenship.pdf")


#Ethnicity
bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Nyu_Ipeds_Eth_Ld)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Ethnicity") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

pie <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Nyu_Ipeds_Eth_Ld)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Ethnicity")

#Save to one file
multi.page <- ggarrange(bar, pie, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Ethnicity.pdf")


#Qualifications upon entry to program - Unsure what this is? BS vs BA etc.???
# BA = Bachelor of the Arts
# BS = Bachelor of Science
# BBA = Bachelor of Business Administration
# BSA = Bachelor or Science and Arts
# BENG = Bachelor of Engineering
# BZ = Unkonwn
bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = Deg)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Undergraduate Degree") +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  theme_classic() 

pie <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = "", fill = Deg)) +
  geom_bar(width = 1 ) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  labs( fill = "Undergraduate Degree")

#Save to one file
multi.page <- ggarrange(bar, pie, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Qualifications.pdf")

#GRE scores - How to look at it? 
#Have 4 columns of data. Verbal and Quantative, both old and new syle of scoring

ev.bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EV)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Verbal GRE Scorces (New Scale)") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

eq.bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EQ)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Quantative GRE Scorces (New Scale)") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

evp.bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EVP)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Verbal GRE Scorces Percentage") +
  scale_y_continuous(limits = c(0,25), expand=c(0,0)) +
  theme_classic() 

eqp.bar <- student_data %>%
  filter(First_Term >= filter.year) %>%
  ggplot(aes(x = EQP)) +
  geom_bar() +
  ylab("# of Students") +
  xlab("Quantative GRE Scorces Percentage") +
  scale_y_continuous(limits = c(0, 25), expand=c(0,0)) +
  theme_classic() 

#All graphs on one page
ggarrange(ev.bar, eq.bar, evp.bar, eqp.bar, nrow = 2, ncol = 2)
ggsave("GRE_Scores_Bar.pdf")



#PhD experience (histograms or boxplot with points overlayed)
#Filter so that only students that graduated with PhD are included
#Time to degree
hist <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Years )) +
  geom_histogram(binwidth = 0.5) +
  ylab("# of Students") +
  xlab("Years to PhD") +
  scale_y_continuous(limits = c(0,30), expand=c(0,0)) +
  theme_bw() 

box <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Years )) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge()) +
  ylab("Years to PhD") +
  xlab("") +
  theme_bw() +
  theme(axis.ticks.x = element_blank())

#Save to one file
multi.page <- ggarrange(hist, box, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Years_to_PhD.pdf")

#Number of semesters of teaching
hist <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = Semesters_of_teaching )) +
  geom_histogram(binwidth = 1) +
  ylab("# of Students") +
  xlab("Semesters of Teaching") +
  scale_y_continuous(limits = c(0,30), expand=c(0,0)) +
  theme_bw() 

box <- student_data %>%
  filter(First_Term >= filter.year) %>%
  filter(!is.na(GABIOLPHD)) %>%
  ggplot(aes(x = "", y= Semesters_of_teaching )) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge()) +
  ylab("Semesters of Teaching") +
  theme_bw() 

#Save to one file
multi.page <- ggarrange(hist, box, nrow = 1, ncol = 1)
ggexport(multi.page, filename = "Semesters_of_Teaching.pdf")
