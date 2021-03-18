#load libraries
library(haven)
library(readr)
library (tidyverse)
library(ggplot2)
library(jtools)
library(estimatr)
library(vtable)
#load data
df <- read_dta("~/Downloads/cps_00004.dta")
indnames <- read_csv("~/Downloads/indnames.csv")
income <- read_dta("~/downloads/incomeData.dta.gz")
#join data
df_ind <- inner_join(df, indnames, by="ind")
#add covid flag, and quarter flag
df_ind<- df_ind %>% mutate(before_covid = (year < 2020 | (year = 2020 & month < 3)),
                             quarter = ceiling(month / 3)) %>% 
  relocate(c(year, month, quarter, before_covid))
# add income data
full_income_df <- merge(df_ind, income)



#only select columns needed
trends<-select(df_ind,year,serial,month,labforce,wkstat,empstat,ind,indname,before_covid,month,quarter,asecflag)

#view new data set
View(trends)
#gather, sepaarate, spread, select, filter, delete N/A, delete duplicates,view
trends %>%
  gather("columns","observations",-serial)%>%
  separate(columns,c("columns","column_number"),sep="_")%>%
  spread(columns,observations)%>%
  
  filter(!is.na())%>%
  
  distinct()%>%
  View(trends)

#view data
vtable(df_ind)
sumtable(df_ind)

#Add in dummy COVID field
df_ind <- df_ind %>% 
  #Add month year variable
  mutate(year_month = as.Date(paste(year, '-', month, '-1', sep=''))) %>% 
  #Add in dummy COVID field
  mutate(COVID=year_month >= as.Date('2020-3-1')) %>%
  #Filter out ASEC data
  filter(is.na(asecflag) | asecflag == 2)

#Create dummy variables for specific industries
df_ind <- df_ind %>%
  mutate(retail=indname=="Retail Trade") %>%
  mutate(food.ent=indname=="Arts, Entertainment, and Recreation, and Accommodation and Food Services") %>%
  mutate(edu.health=indname=="Educational Services, and Health Care and Social Assistance") %>%
  mutate(transport=indname=="Transportation and Warehousing, and Utilities") %>%
  mutate(agri=indname=="Agriculture, Forestry, Fishing, and Hunting, and Mining")

#Create df with only retail industry
df_ret <- df_ind %>% filter(retail == TRUE) 
#Summarise retail df
vtable(df_ret)

wkstat_id <- c(10,11,12,13,14,15,20,21,22,40,41,42,50,60,99)
wkstat_desc <- c('Full-time schedules','Full-time hours (35+), usually full-time'
                 ,'Part-time for non-economic reasons, usually full-time'
                 ,'Not at work, usually full-time'
                 ,'Full-time hours, usually part-time for economic reasons'
                 ,'Full-time hours, usually part-time for non-economic reasons'
                 ,'Part-time for economic reasons'
                 ,'Part-time for economic reasons, usually full-time'
                 ,'Part-time hours, usually part-time for economic reasons'
                 ,'Part-time for non-economic reasons, usually part-time'
                 ,'Part-time hours, usually part-time for non-economic reasons'
                 ,'Not at work, usually part-time'
                 ,'Unemployed, seeking full-time work'
                 ,'Unemployed, seeking part-time work'
                 ,'NIU, blank, or not in labor force')
wkstat_desc_short <- c('Full-time'
                       ,'Full-time'
                       ,'Part-time'
                       ,'Not at work'
                       ,'Full-time'
                       ,'Full-time'
                       ,'Part-time'
                       ,'Part-time'
                       ,'Part-time'
                       ,'Part-time'
                       ,'Part-time'
                       ,'Not at work'
                       ,'Unemployed'
                       ,'Unemployed'
                       ,'NIU, blank, or not in labor force')
wkstat <- data.frame(wkstat=wkstat_id, desc=wkstat_desc, desc_short=wkstat_desc_short)
#Add in work status descriptions
df_by_month <- inner_join(df_ret, wkstat)
#Create df grouped by year, month, and work status, count of employees
df_wkstat_by_month <- df_by_month %>% group_by(year_month, desc_short, COVID) %>% summarise(employee_count=n())
#Plot employee count over time
ggplot(df_wkstat_by_month, aes(y=employee_count, x=factor(year_month), color=desc_short)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(y="Employee Count", x="Year Month", title="Retail Employee Count Over Time") +
  scale_colour_discrete("Work Status")

#Create df grouped at industry level
df_ind_by_month <- df_ind %>% group_by(year_month, wkstat, COVID) %>% summarise(employee_count=n())

#How has COVID affected the health of the retail industry, as measured by employment?

ret_model <- lm(data=df_wkstat_by_month, employee_count~COVID)
export_summs(ret_model)

#How has retail fared relative to other industries?

#Create df including different industries
ind_comparison <- df_ind %>%
  filter(retail==1|food.ent==1|edu.health==1|transport==1|agri==1) %>%
  group_by(indname,retail,food.ent,edu.health,transport,agri,COVID, year_month) %>%
  summarise(employee_count=n())

#Make Retail the reference industry
ind_comparison$indname <- relevel(as.factor(ind_comparison$indname), ref=4)

#Plot employee count over time for each industry
ggplot(ind_comparison, aes(y=employee_count, x=factor(year_month), color=indname)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(y="Employee Count", x="Year Month", title="Employee Count Over Time by Industry") +
  scale_colour_discrete("Industry")

fe_model <- lm(data=ind_comparison, employee_count~indname*COVID)
export_summs(fe_model)

#How has retail fared relative to other industries?

#Create df including different industries
ind_comparison <- df_ind %>%
  filter(retail==1|food.ent==1|edu.health==1|transport==1|agri==1) %>%
  group_by(indname,retail,food.ent,edu.health,transport,agri,COVID, year_month) %>%
  summarise(employee_count=n())

#Make Retail the reference industry
ind_comparison$indname <- relevel(as.factor(ind_comparison$indname), ref=4)

#Plot employee count over time for each industry
ggplot(ind_comparison, aes(y=employee_count, x=factor(year_month), color=indname)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(y="Employee Count", x="Year Month", title="Employee Count Over Time by Industry") +
  scale_colour_discrete("Industry")


fe_model <- lm(data=ind_comparison, employee_count~indname*COVID)
export_summs(fe_model)
