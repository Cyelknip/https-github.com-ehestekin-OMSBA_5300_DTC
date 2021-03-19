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
indnames<- read_csv("~/Downloads/indnames.csv")
income <- read_dta("~/downloads/incomeData.dta.gz")

#join data
df_ind <- inner_join(df, indnames, by="ind")
full_income_df <- inner_join(df_ind, income, by = "serial")


#only select columns needed to show data from 3/19 compared to 3/20
march_trends<-select(full_income_df,serial,pernum,year,month,labforce,wkstat,empstat,ind,indname,incwage)



#filter, delete N/A, delete duplicates
march_trends <- march_trends %>%
  filter(!is.na(incwage))%>%
  distinct()

#Add month year variable
 march_trends<- march_trends %>%
    mutate(year_month = as.Date(paste(year, '-', month, '-1', sep=''))) %>% 
#Add in dummy COVID field
    mutate(COVID=year_month >= as.Date('2020-3-1'))


df_ind <- march_trends%>%
  mutate(retail=indname=="Retail Trade") %>%
  mutate(food.ent=indname=="Arts, Entertainment, and Recreation, and Accommodation and Food Services") %>%
  mutate(edu.health=indname=="Educational Services, and Health Care and Social Assistance") %>%
  mutate(transport=indname=="Transportation and Warehousing, and Utilities") %>%
  mutate(agri=indname=="Agriculture, Forestry, Fishing, and Hunting, and Mining")
vtable(df_ind)

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
df_by_month <- inner_join(march_trends, df_ind)


#Create df grouped by year, month, and work status, count of employees
df_wkstat_by_month <- df_by_month %>% group_by(indname, COVID,incwage) %>% summarise(incwage =n())

#Plot employee count over time
ggplot(df_wkstat_by_month, aes(y=incwage, x=factor(COVID), color=indname)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(y="Personal Income", x=" After COVID", title="Personal Income by Industry") +
  scale_colour_discrete(" Industry")

#Regress incwage on covid status
covid_effect_wage <- lm(incwage ~ COVID,data=df_wkstat_by_month)
export_summs(covid_effect)

```

