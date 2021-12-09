df <- read.csv("data/final_df.csv")

summary(df)

str(df)

library(dplyr)
library(ggplot2)
library(plotly)

library(lubridate)


#####################RACHAELs

# read in initial data

df1 <- read.csv("data/initial_data.csv")

# create dfs for Overview

df_counts <- df1 %>% # create count df for overview
  group_by(company) %>%
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% slice(1:10)

df_counts <- as.data.frame(df_counts)

df_salaries <- df1 %>% # create average salary df for overview
  group_by(company) %>%
  summarise(avg_salary = mean(totalyearlycompensation)) %>% 
  arrange(desc(avg_salary)) %>% slice(1:10)

df_salaries <- as.data.frame(df_salaries)

# read in Charlotte company coordinates

charlotte_location <- read.csv("data/charlotte_coordinates.csv")

# drop columns

drop <- c("country.x","X","X.1","level", "tag", "gender", "otherdetails","cityid","dmaid","rowNumber",
          "Masters_Degree","Bachelors_Degree","Doctorate_Degree","Highschool","Some_College","Race_Asian","Race_White","Race_Two_Or_More","Race_Black",
          "Race_Hispanic","Race","X.2","city_local","iso2","iso3","admin_name","capital","id")

df1 <- df1[,!(names(df1) %in% drop)]

# change timestamp to date format

# df1$timestamp <- as.Date(df1$timestamp, "%m/%d/%y")

# filter data to Charlotte for map

charlotte_data <- df1 %>% filter(city == "Charlotte")

# view number of unique companies in Charlotte

# sort(unique(charlotte_data$company))



# summarize companies in Charlotte that employ at least 2 roles

company_group <- charlotte_data %>% group_by(company) %>% summarize(count = n(), avg_income = mean(totalyearlycompensation)) %>% 
  filter(count > 2) %>% arrange(desc(count))

# merge Charlotte data with the comoany coordinate data

final_df  <- charlotte_data %>% left_join(charlotte_location, by = "company")

# filter data to only include the companies that employ at least 2 roles

final_df <- final_df %>% filter(company %in% company_group$company)






############################ FORREST --------------------------------------------------------- 

#Convert Timestamp to Datetime format
df$timestamp = parse_date_time(df$timestamp, orders = "mdy")
summary(df$timestamp)

#remove Extraneous columns
df = subset(df, select = -c(X, X.1, location, state, country, otherdetails, cityid, dmaid, lat, lng))
df = subset(df, select = -c(0, 13,14,15,16,17,18,19,20,21,22,23))
df = subset(df, select = -c(13,16,17,18,19,20,21,22))
df = subset(df, select = -c(1))
str(df)
#View(df)

set = df %>% group_by(company) %>% summarize(count = n()) %>% filter(count >= 100) %>% select(company)
set


#subset(df, sum(company == ))


unique(df$title)
unique(df$company)
#Group by to gain further insights
df1 <- df %>% group_by(company) %>% summarize(count = n(), avg_income = mean(totalyearlycompensation), max_income = max(totalyearlycompensation), min_income = min(totalyearlycompensation), 
                                              avg_exp = mean(yearsofexperience), min_exp = min(yearsofexperience), max_exp = max(yearsofexperience)) %>% arrange(desc(count))
df1
#View(df1)
str(df1)

#Change to integers
df1$min_exp = as.integer(df1$min_exp)
df1$max_exp = as.integer(df1$max_exp)
unique(df1$company)
#df1[,c("min_exp", "max_exp")]<-lapply(df1[,c("min_exp","max_exp")],integer)

mean(mean(df1$avg_exp))
mean(df1$avg_exp)

#Take a small slice of data
df2 <- df1 %>% slice(1:10)

#View(df2)

ggplot(df2, aes(x = reorder(company, -count), y = count)) + geom_col()

df3 <- df %>% group_by(title) %>% summarize(count = n(), avg_income = mean(totalyearlycompensation), max_income = max(totalyearlycompensation), min_income = min(totalyearlycompensation), 
                                              avg_exp = mean(yearsofexperience), min_exp = min(yearsofexperience), max_exp = max(yearsofexperience)) %>% arrange(desc(count))

#View(df3)


#Scatter plot with color = education level
summary(df)
#df$Education
sum(is.na(df$Education))

ggplot(df, mapping = aes(yearsofexperience, basesalary,  color = Education)) + geom_smooth(alpha = .1)

  
## Time Series 
#df$timestamp = as.Date(df$timestamp)

#summary(df$timestamp)



#Base Salary and years of experience experience required based on Education
summary(df$yearsofexperience)

ggplot(df, mapping = aes(timestamp, basesalary, color = Education)) + geom_smooth(alpha = .5)
ggplot(df, mapping = aes(timestamp, totalyearlycompensation, color = Education)) + geom_smooth(alpha = .1)
ggplot(df, mapping = aes(timestamp, yearsofexperience, color = Education)) + geom_smooth(alpha = .1)

#Base Salary and years of experience experience required based on Race
ggplot(df, mapping = aes(timestamp, basesalary, color = Race)) + geom_smooth(alpha = .1)
ggplot(df, mapping = aes(timestamp, yearsofexperience, color = Race)) + geom_smooth(alpha = .1)



df %>% group_by(Education) %>% summarise(n=n(), min_sal = min(basesalary), max_sal = max(basesalary), avg_sal = mean(basesalary)) %>% arrange(desc(n))


#df %>% group_by(timestamp) %>% summarize(n = n(), avg_sal = mean(basesalary),


#View education and time
df[,c("Education", "timestamp")]

ggplot(df, aes(timestamp, Education, color = Education)) + geom_point()

#View Race and time
#df[,c("Race", "timestamp")]

ggplot(df, aes(timestamp, Race, color = Race)) + geom_point()

##Just Yearly Salary and Time
summary(df$totalyearlycompensation)
med = median(df$totalyearlycompensation)
iqr = IQR(df$totalyearlycompensation)
 
new_df <- subset(df, totalyearlycompensation < (1.5*iqr + 264000) & totalyearlycompensation > (135000 - 1.5*iqr))


ggplot(new_df, mapping = aes(timestamp, totalyearlycompensation)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "gam", color="red", alpha = .5)  
  #geom_smooth(aes(y = basesalary), color="steelblue", linetype="twodash", alpha = .1)# +
  #ylim(150000, 200000)

#Salary based on job title
ggplot(df, mapping = aes(timestamp, totalyearlycompensation, color = )) + geom_smooth(alpha = .1)


salary_makeup <- df %>%
  group_by(title) %>%
  summarise(avg_sal = mean(totalyearlycompensation))

##
ggplot(salary_makeup, aes(reorder(title, avg_sal), avg_sal, fill = avg_sal)) + geom_col() + coord_flip()


#
#salary_makeup <- df %>%
#  group_by(title) %>%
#  summarise(avg_sal = mean(totalyearlycompensation),
#            company_sal_pct = 100 * input$companies + mean(AMIN_Pop) / avg_sal)


#
#salary_makeup <- df_company() %>%
#  group_by(title) %>%
#  summarise(avg_sal = mean(totalyearlycompensation)


#
#test = df %>% group_by(title) %>% filter(title == "Data Scientist")
#mean(test$totalyearlycompensation)
df[df$title == "Data Scientist",] %>% ggplot(aes(totalyearlycompensation)) + 
  geom_histogram(aes(fill = title), bins = 15, binwidth = 100000, show.legend = FALSE) +
  facet_wrap(vars(title), scales = 'free')

df %>% group_by(title) %>% summarize(count = n(), avg_job_sal = mean(totalyearlycompensation), med_job_sal = median(totalyearlycompensation))

#filter(title == c("Data Scientist", "Solution Architect"))

df %>% filter(title == "Data Scientist")



# if salary > mean(yearly compensation) display message that states:
#"This company pays above average salary)



test = df %>% group_by(company) %>% 
  summarize(n = n()) %>% 
  mutate(company = ifelse(n<50, "Other", company)) %>%
  ungroup()


newerdf = merge(df, test, by = 'company', all.x= TRUE)

finaldata = mutate(newerdf, company = ifelse(is.na(n) == TRUE, "Other", company))

unique(finaldata$company)

#Group by to gain further insights
companydata <- finaldata %>% group_by(company) %>% summarize(count = n(), avg_income = mean(totalyearlycompensation), max_income = max(totalyearlycompensation), min_income = min(totalyearlycompensation), 
                                              avg_exp = mean(yearsofexperience), min_exp = min(yearsofexperience), max_exp = max(yearsofexperience)) %>% arrange(desc(count))
df1


# Test of Job and Company Salary comparison
q = finaldata %>% group_by(title) %>% summarize(avg_job_sal = mean(totalyearlycompensation)) %>% filter(title == "Data Scientist")

ggplot(finaldata, aes(totalyearlycompensation)) + geom_histogram(binwidth = 100000)

#abline(v=companydata$avg_income,col="blue",lwd=2)
#finaldata %>% filter(title = input$title, company = input$companies) %>% summarize(c_avg = mean(totalyearlycompensation))




######################## LOG TRANSFORM PRODUCT MANAGER AND SOFTWARE ENGINEER ROLES THAT ARE SKEWED ###############################
transform = finaldata %>% filter(title == "Product Manager") %>% 
  mutate(totalyearlycompensation = log2(totalyearlycompensation))


ggplot(transform, aes(totalyearlycompensation)) +
  geom_histogram(binwidth = 100)


#iqr = finaldata %>% filter(title == "Product Manager") %>% IQR(totalyearlycompensation)
IQR = IQR(df$totalyearlycompensation)
Q1 = quantile(df$totalyearlycompensation, .25)
Q3 = quantile(df$totalyearlycompensation, .75)
adjusted_df = subset(df, df$totalyearlycompensation > (Q1 - 5*IQR) & 
                       df$totalyearlycompensation < (Q3 + 5*IQR))

test = ggplot(adjusted_df, mapping = aes(x = totalyearlycompensation)) + 
  geom_histogram(aes(fill = title), 
                 #bins = 20, 
                 binwidth = 100000, 
                 show.legend = FALSE)

let = ggplot_build(test)
let


the_grouped = finaldata %>% group_by(title, company) %>% summarize(avg_sal = mean(basesalary))

the_grouped %>% filter(title %in% c("Data Scientist", "Software Engineer"), company %in% c("Amazon", "Spotify", "Google"))


