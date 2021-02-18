
#Package Installations

if (!require(skimr)) {
  #"skimr", "psych", "stargazer", "lubridate", "moments", "margins", "sandwich", "lmtest", "car"
  install.packages("skimr",repos="http://cran.us.r-project.org")
  require(skimr)
}
if (!require(psych)) {
  install.packages("psych",repos="http://cran.us.r-project.org")
  require(psych)  
}  
if (!require(stargazer)) {
  install.packages("stargazer",repos="http://cran.us.r-project.org")
  require(stargazer)  
} 
if (!require(lubridate)) {
  install.packages("lubridate",repos="http://cran.us.r-project.org")
  require(lubridate)  
} 
if (!require(moments)) {
  install.packages("moments",repos="http://cran.us.r-project.org")
  require(moments)  
} 
if (!require(margins)) {
  install.packages("margins",repos="http://cran.us.r-project.org")
  require(margins)  
} 
if (!require(sandwich)) {
  install.packages("sandwich",repos="http://cran.us.r-project.org")
  require(sandwich)  
} 
if (!require(lmtest)) {
  install.packages("lmtest",repos="http://cran.us.r-project.org")
  require(lmtest)  
} 
if (!require(car)) {
  install.packages("car",repos="http://cran.us.r-project.org")
  require(car)  
} 
if (!require(kableExtra)) {
  install.packages("kableExtra",repos="http://cran.us.r-project.org")
  require(kableExtra)  
} 
if (!require(tidyverse)) {
  install.packages("tidyverse",repos="http://cran.us.r-project.org")
  require(tidyverse)  
}
if (!require(pander)) {
  install.packages("pander",repos="http://cran.us.r-project.org")
  require(pander)  
}
if (!require(ggplot2)) {
  install.packages("ggplot2",repos="http://cran.us.r-project.org")
  require(ggplot2)  
}
if (!require(officer)) {
  install.packages("officer",repos="http://cran.us.r-project.org")
  require(officer)  
}
if (!require(flextable)) {
  install.packages("flextable",repos="http://cran.us.r-project.org")
  require(flextable)  
}
if (!require(foreign)) {
  install.packages("foreign",repos="http://cran.us.r-project.org")
  require(foreign)  
}
if (!require(MASS)) {
  install.packages("MASS",repos="http://cran.us.r-project.org")
  require(MASS)  
}
if (!require(mfx)) {
  install.packages("mfx",repos="http://cran.us.r-project.org")
  require(mfx)  
}
if (!require(margins)) {
  install.packages("margins",repos="http://cran.us.r-project.org")
  require(margins)  
}


#Loading Files
#===============


workdir=getwd()

samadult <-read.csv(file = "samadult.csv", header = TRUE, sep = ",")
sampers <-read.csv(file = "personsx.csv", header = TRUE, sep = ",")


# Data Extraction
#Selecting only required variables from sample adult file
samadult1<-dplyr::select(samadult,HHX, FMX, FPX
                         ,APRVTRYR,AGE_P,SEX
                         ,R_MARITL,HISPAN_I,RACERPI2
                         ,REGION,HIT1A
                         ,ALCHRONR
)

sampers1 <-dplyr::select(sampers,HHX, FMX, FPX
                         ,EDUC1,PINTRSTR
                         ,PDIVD
                         ,MCCHOICE,MEDICAID,MCPARTD
                         ,PHSTAT
)




#Joining the files
invisible((nhisdata <- inner_join(samadult1,sampers1,by = c("HHX","FMX","FPX"))))



#Filtering with age >=65
nhisdatafil65<-filter(nhisdata,AGE_P>=65)


#Variable Rename
#Adult File Variables
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="APRVTRYR"] <- "TROUBLEFINDINGDOC"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="AGE_P"]    <- "AGE"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="SEX"]      <- "SEX"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="R_MARITL"] <- "MARITALSTATUS"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="HISPAN_I"] <- "HISPANIC"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="RACERPI2"] <- "RACE"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="REGION"]   <- "REGION"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="HIT1A"]    <- "HEALTHINTERNETINFO" 
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="ALCHRONR"] <- "FUNCTIONALLIMITATION"


#Person File variables
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="EDUC1"]    <- "EDUCATIONLEVEL"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="PINTRSTR"] <- "INTERESTINCOME"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="PDIVD"]    <- "DIVIDENDINCOME"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="MCCHOICE"] <- "MEDICAREADVANTAGE"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="MEDICAID"] <- "MEDICAID"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="MCPARTD"]  <- "MEDICAREPRESCR"
colnames(nhisdatafil65)[colnames(nhisdatafil65)=="PHSTAT"]   <- "HEALTHSTATUS"


#Mutating Variables
nhisdatafil65<-mutate(nhisdatafil65,
                      TROUBLEFINDINGDOC1=ifelse(TROUBLEFINDINGDOC == 1, 1,
                                                ifelse(TROUBLEFINDINGDOC == 2, 0, NA)),
                      SEX1=ifelse(SEX==2,0,1),
                      #MARITALSTATUS.FAC=factor(ifelse(MARITALSTATUS<=8,MARITALSTATUS,NA)),
                      MARITALSTATUS.FAC=ifelse(MARITALSTATUS %in% c(1,2,3),1,
                                               ifelse(MARITALSTATUS>=4 & MARITALSTATUS<=8,0,NA)),
                      MARITALSTATUS.MARRIED=ifelse(MARITALSTATUS %in% c(1,2,3),1,0),
                      MARITALSTATUS.NOTMARRIED=ifelse(MARITALSTATUS>=4 & MARITALSTATUS<=8,1,0),
                      HISPANIC1=ifelse(HISPANIC<=11,1,0),
                      #RACE.FAC=factor(ifelse(RACE>4,NA,RACE)),
                      RACE.FAC=ifelse(RACE==1,1,
                                      (ifelse(RACE>=2 & RACE<=4,0,NA))),
                      RACE.WHITE=ifelse(RACE==1,1,0),
                      RACE.NONWHITE=ifelse(RACE>=2 & RACE<=4,1,0),
                      REGION.FAC = ifelse(REGION > 4,NA,REGION),
                      REGION.NE = ifelse(REGION == 1, 1, 0),
                      REGION.MW = ifelse(REGION == 2, 1, 0),
                      REGION.SO = ifelse(REGION == 3, 1, 0),
                      REGION.WE = ifelse(REGION == 4, 1, 0),
                      #EDUCATIONLEVEL.FAC=factor(ifelse( EDUCATIONLEVEL<=21,EDUCATIONLEVEL,NA)),
                      EDUCATIONLEVEL.FAC=ifelse(EDUCATIONLEVEL<=12,1,
                                                ifelse(EDUCATIONLEVEL>=13 & EDUCATIONLEVEL<=14,2,
                                                       ifelse(EDUCATIONLEVEL>=15 & EDUCATIONLEVEL<=17,3,
                                                              ifelse(EDUCATIONLEVEL>=18 & EDUCATIONLEVEL<=21,4,NA)))),
                      EDUCATIONLEVEL.NOORSOMESCHOOL = ifelse(EDUCATIONLEVEL<=12,1,0),
                      EDUCATIONLEVEL.HIGHSCHOOL = ifelse(EDUCATIONLEVEL>=13 & EDUCATIONLEVEL<=14,1,0),
                      EDUCATIONLEVEL.SOMECOLLEGE = ifelse(EDUCATIONLEVEL>=15 & EDUCATIONLEVEL<=17,1,0),
                      EDUCATIONLEVEL.HIGHERDEGREE = ifelse(EDUCATIONLEVEL>=18 & EDUCATIONLEVEL<=21,1,0),
                      INTERESTINCOME1 = ifelse(INTERESTINCOME == 1, 1,
                                               ifelse(INTERESTINCOME == 2, 0, NA)),
                      DIVIDENDINCOME1 = ifelse(DIVIDENDINCOME == 1, 1,
                                               ifelse(DIVIDENDINCOME == 2, 0, NA)),
                      MEDICAREADVANTAGE1 = ifelse(MEDICAREADVANTAGE == 1, 1,
                                                  ifelse(MEDICAREADVANTAGE == 2, 0, NA)),
                      MEDICAID1 = ifelse(MEDICAID <= 2, 1,
                                         ifelse(MEDICAID == 3, 0, NA)),
                      MEDICAREPRESCR1 = ifelse(MEDICAREPRESCR == 1, 1,
                                               ifelse(MEDICAREPRESCR == 2, 0, NA)),
                      HEALTHINTERNETINFO1 = ifelse(HEALTHINTERNETINFO == 1, 1,
                                                   ifelse(HEALTHINTERNETINFO == 2, 0, NA)),
                      HEALTHSTATUS.FAC = ifelse(HEALTHSTATUS>5,NA,HEALTHSTATUS),
                      HEALTHSTATUS.EXCELLENT = ifelse(HEALTHSTATUS == 1, 1, 0),
                      HEALTHSTATUS.VERYGOOD = ifelse(HEALTHSTATUS == 2, 1, 0),
                      HEALTHSTATUS.GOOD = ifelse(HEALTHSTATUS == 3, 1, 0),
                      HEALTHSTATUS.FAIR = ifelse(HEALTHSTATUS == 4, 1, 0),
                      HEALTHSTATUS.POOR = ifelse(HEALTHSTATUS == 5, 1, 0),
                      FUNCTIONALLIMITATION1=ifelse(FUNCTIONALLIMITATION == 0,0,ifelse(FUNCTIONALLIMITATION==1,1,NA)),
                      
)



#Skipping records having NA
nhisdatafil651<-na.omit(nhisdatafil65)

# Descriptive statistics after omitting incomplete observations
stargazer(nhisdatafil651[, -(1:3)],type="text",title="Table 0.2 Descriptive statistics after omitting incomplete observations",digits=3,align=TRUE,out="table2.txt")

#Variable Counts
nhisdatafil651 %>%
  count(TROUBLEFINDINGDOC1)

nhisdatafil651 %>%
  count(SEX1)

nhisdatafil651 %>%
  count(MARITALSTATUS.FAC)

nhisdatafil651 %>%
  count(HISPANIC1)

nhisdatafil651 %>%
  count(RACE.FAC)

nhisdatafil651 %>%
  count(REGION.FAC)

nhisdatafil651 %>%
  count(EDUCATIONLEVEL.FAC)

nhisdatafil651 %>%
  count(INTERESTINCOME1)

nhisdatafil651 %>%
  count(DIVIDENDINCOME1)

nhisdatafil651 %>%
  count(MEDICAREADVANTAGE1)

nhisdatafil651 %>%
  count(MEDICAID1)

nhisdatafil651 %>%
  count(MEDICAREPRESCR1)

nhisdatafil651 %>%
  count(HEALTHINTERNETINFO1)

nhisdatafil651 %>%
  count(HEALTHSTATUS.FAC)

nhisdatafil651 %>%
  count(FUNCTIONALLIMITATION1)

#Histograms
#TROUBLEFINDINGDOC
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=TROUBLEFINDINGDOC1,y=..count..,fill=factor(TROUBLEFINDINGDOC1))) +
  labs(y="count (persons)",
       x="Had trouble finding a doctor",
       title = "1.a Problems in finding a doctor/provider",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))


# AGE
ggplot(data = nhisdatafil651) + 
  geom_histogram(mapping = aes(x=AGE),binwidth=1) +
  labs(y="count (persons)",
       x="AGE (years)",
       title = "1.b Agewise Distribution of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")

#SEX
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=SEX1,y=..count..,fill=factor(SEX1))) +
  labs(y="count of persons",
       x="SEX",
       title = "1.c Genderwise Distribution of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-Female","1-Male"))

#MARITALSTATUS
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=MARITALSTATUS.FAC,y=..count..,fill=factor(MARITALSTATUS.FAC))) +
  labs(y="count of persons",
       x="MARITAL STATUS",
       title = "1.d Marital Status of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),
                    name="Marital Status",
                    labels=c("1-Married",
                             "0-Not Married"
                    ))

#HISPANIC
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=HISPANIC1,y=..count..,fill=factor(HISPANIC1))) +
  labs(y="count of persons",
       x="HISPANIC",
       title = "1.e Hispanic population Distribution",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#RACE
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=RACE.FAC,y=..count..,fill=factor(RACE.FAC))) +
  labs(y="count of persons",
       x="RACE",
       title = "1.f Race-wise Distribution of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),
                    name="Race",
                    labels=c("1-White",
                             "0-Non White"
                    ))

#REGION
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=REGION.FAC,y=..count..,fill=factor(REGION.FAC))) +
  labs(y="count of persons",
       x="REGION",
       title = "1.g Region-wise Distribution of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040", "#404040", "#404040"),
                    name="Region",
                    labels=c("1-Northeast",
                             "2-Midwest",
                             "3-South",
                             "4-West"
                    ))

#EDUCATIONLEVEL
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=EDUCATIONLEVEL.FAC,y=..count..,fill=factor(EDUCATIONLEVEL.FAC))) +
  labs(y="count of persons",
       x="EDUCATION LEVEL",
       title = "1.h Education Level Distribution of survey population",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040", "#404040", "#404040"),
                    name="Education Level",
                    labels=c("1- No or Some School",
                             "2- High School",
                             "3- Some College",
                             "4- Higher Degree"
                    ))+
  theme(legend.text=element_text(size=8),legend.key.size=unit(0,"mm"))


#INTERESTINCOME
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=INTERESTINCOME1,y=..count..,fill=factor(INTERESTINCOME1))) +
  labs(y="count of persons",
       x="Interest Income received",
       title = "1.m Survey of population receiving Interest Income",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#DIVIDENDINCOME
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=DIVIDENDINCOME1,y=..count..,fill=factor(DIVIDENDINCOME1))) +
  labs(y="count of persons",
       x="Dividend Income received",
       title = "1.n Survey of population receiving Dividend Income",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#MEDICAREADVANTAGE
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=MEDICAREADVANTAGE1,y=..count..,fill=factor(MEDICAREADVANTAGE1))) +
  labs(y="count of persons",
       x="Enrolled to Medicare Advantage",
       title = "1.r Survey of population enrolled to Medicare Advantage Plan",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))


#MEDICAID
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=MEDICAID1,y=..count..,fill=factor(MEDICAID1))) +
  labs(y="count of persons",
       x="Enrolled to Medicaid",
       title = "1.s Survey of population enrolled to Medicaid",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#MEDICAREPRESCR
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=MEDICAREPRESCR1,y=..count..,fill=factor(MEDICAREPRESCR1))) +
  labs(y="count of persons",
       x="Enrolled to Medicare Prescription Drug Plan",
       title = "1.t Survey of population enrolled to Medicare Prescription Drug Plan",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#HEALTHINTERNETINFO
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=HEALTHINTERNETINFO1,y=..count..,fill=factor(HEALTHINTERNETINFO1))) +
  labs(y="count of persons",
       x="Have used internet to schedule medical appointment",
       title = "1.v Survey of population having access to internet to schedule medical appointment",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))

#HEALTHSTATUS
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=HEALTHSTATUS.FAC,y=..count..,fill=factor(HEALTHSTATUS.FAC))) +
  labs(y="count of persons",
       x="Health status wise distribution of survey population",
       title = "1.w Health Status of survey population ",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040", "#404040", "#404040", "#404040"),
                    name="Response",
                    labels=c("1 Excellent",
                             "2 Very good",
                             "3 Good",
                             "4 Fair",
                             "5 Poor"
                    ))

#FUNCTIONALLIMITATION
ggplot(data = nhisdatafil651) + 
  geom_bar(mapping = aes(x=FUNCTIONALLIMITATION1,y=..count..,fill=factor(FUNCTIONALLIMITATION1))) +
  labs(y="count of persons",
       x="Limited by Chronic conditions ",
       title = "1.x Survey population functionally limited by Chronic conditions  ",
       subtitle = "Population 65 years or older",
       caption = "Source of Data: National Health Interview Survey 2017")+
  scale_fill_manual(values = c("#404040", "#404040"),name="Responses",labels=c("0-No","1-Yes"))


#Cross Tabulations
#TROUBLEFINDINGDOC vs AGE
attach(nhisdatafil651)
options(digits = 4)

xtab1 <-table(TROUBLEFINDINGDOC1,AGE)

prop.table(xtab1)*100

#TROUBLEFINDINGDOC VS SEX

xtab2 <-table(TROUBLEFINDINGDOC1,SEX1)

prop.table(xtab2)*100

#TROUBLEFINDINGDOC VS MARITALSTATUS
xtab31 <-table(TROUBLEFINDINGDOC1,MARITALSTATUS.MARRIED)
prop.table(xtab31)*100

xtab32 <-table(TROUBLEFINDINGDOC1,MARITALSTATUS.NOTMARRIED)
prop.table(xtab32)*100

#TROUBLEFINDINGDOC VS HISPANIC
xtab4 <-table(TROUBLEFINDINGDOC1,HISPANIC1)
prop.table(xtab4)*100

#TROUBLEFINDINGDOC VS RACE
xtab41 <-table(TROUBLEFINDINGDOC1,RACE.WHITE)
prop.table(xtab41)*100

xtab42 <-table(TROUBLEFINDINGDOC1,RACE.NONWHITE)
prop.table(xtab42)*100

#TROUBLEFINDINGDOC VS REGION
xtab51 <-table(TROUBLEFINDINGDOC1,REGION.NE)
prop.table(xtab51)*100

xtab52 <-table(TROUBLEFINDINGDOC1,REGION.MW)
prop.table(xtab52)*100

xtab53 <-table(TROUBLEFINDINGDOC1,REGION.SO)
prop.table(xtab53)*100

xtab54 <-table(TROUBLEFINDINGDOC1,REGION.WE)
prop.table(xtab54)*100

#TROUBLEFINDINGDOC VS EDUCATIONLEVEL
xtab61 <-table(TROUBLEFINDINGDOC1,EDUCATIONLEVEL.NOORSOMESCHOOL)
prop.table(xtab61)*100

xtab62 <-table(TROUBLEFINDINGDOC1,EDUCATIONLEVEL.HIGHSCHOOL)
prop.table(xtab62)*100

xtab63 <-table(TROUBLEFINDINGDOC1,EDUCATIONLEVEL.SOMECOLLEGE)
prop.table(xtab63)*100

xtab64 <-table(TROUBLEFINDINGDOC1,EDUCATIONLEVEL.HIGHERDEGREE)
prop.table(xtab64)*100



#TROUBLEFINDINGDOC VS INTERESTINCOME
xtab11 <-table(TROUBLEFINDINGDOC1,INTERESTINCOME1)
prop.table(xtab11)*100

#TROUBLEFINDINGDOC VS DIVIDENDINCOME
xtab12 <-table(TROUBLEFINDINGDOC1,DIVIDENDINCOME1)
prop.table(xtab12)*100

#TROUBLEFINDINGDOC VS MEDICAREADVANTAGE
xtab15 <-table(TROUBLEFINDINGDOC1,MEDICAREADVANTAGE1)
prop.table(xtab15)*100

#TROUBLEFINDINGDOC VS MEDICAID
xtab16 <-table(TROUBLEFINDINGDOC1,MEDICAID1)
prop.table(xtab16)*100

#TROUBLEFINDINGDOC VS MEDICAREPRESCR
xtab17 <-table(TROUBLEFINDINGDOC1,MEDICAREPRESCR1)
prop.table(xtab17)*100

#TROUBLEFINDINGDOC VS HEALTHINTERNETINFO
xtab18 <-table(TROUBLEFINDINGDOC1,HEALTHINTERNETINFO1)
prop.table(xtab18)*100

#TROUBLEFINDINGDOC VS HEALTHSTATUS
xtab191 <-table(TROUBLEFINDINGDOC1,HEALTHSTATUS.EXCELLENT)
prop.table(xtab191)*100

xtab192 <-table(TROUBLEFINDINGDOC1,HEALTHSTATUS.VERYGOOD)
prop.table(xtab192)*100

xtab193 <-table(TROUBLEFINDINGDOC1,HEALTHSTATUS.GOOD)
prop.table(xtab193)*100

xtab204 <-table(TROUBLEFINDINGDOC1,HEALTHSTATUS.FAIR)
prop.table(xtab204)*100

xtab205 <-table(TROUBLEFINDINGDOC1,HEALTHSTATUS.POOR)
prop.table(xtab205)*100

#TROUBLEFINDINGDOC VS FUNCTIONALLIMITATION
xtab21 <-table(TROUBLEFINDINGDOC1,FUNCTIONALLIMITATION1)
prop.table(xtab21)*100
library(stargazer)
library(margins)
library(mfx)
# Statistics for selected variables

nhisdatafil653 <- dplyr::select(nhisdatafil651,
                                TROUBLEFINDINGDOC1,
                                AGE,
                                SEX1,
                                MARITALSTATUS.MARRIED,
                                MARITALSTATUS.NOTMARRIED,
                                HISPANIC1,
                                RACE.WHITE,
                                RACE.NONWHITE,
                                REGION.NE,
                                REGION.MW,
                                REGION.SO,
                                REGION.WE,
                                EDUCATIONLEVEL.NOORSOMESCHOOL,
                                EDUCATIONLEVEL.HIGHSCHOOL,
                                EDUCATIONLEVEL.SOMECOLLEGE,
                                EDUCATIONLEVEL.HIGHERDEGREE,
                                INTERESTINCOME1,
                                DIVIDENDINCOME1,
                                MEDICAREADVANTAGE1,
                                MEDICAID1,
                                MEDICAREPRESCR1,
                                HEALTHINTERNETINFO1,
                                HEALTHSTATUS.EXCELLENT,
                                HEALTHSTATUS.VERYGOOD,
                                HEALTHSTATUS.GOOD,
                                HEALTHSTATUS.FAIR,
                                HEALTHSTATUS.POOR,
                                FUNCTIONALLIMITATION1)

head(nhisdatafil653)

#Descriptive statistics of merged data after renaming and mutating variables

stargazer(nhisdatafil653,type="text",title="Table 0.3 Sample Adult and Person Merged data statistics - mutated ",digits=3,align=TRUE,out="table2.txt")

#Keeping only variables Required for regression



nhisdatafil652 <- dplyr::select(nhisdatafil651,
                                TROUBLEFINDINGDOC1,
                                AGE,
                                SEX1,
                                MARITALSTATUS.FAC,
                                HISPANIC1,
                                RACE.FAC,
                                REGION.FAC,
                                EDUCATIONLEVEL.FAC,
                                INTERESTINCOME1,
                                DIVIDENDINCOME1,
                                MEDICAREADVANTAGE1,
                                MEDICAID1,
                                MEDICAREPRESCR1,
                                HEALTHINTERNETINFO1,
                                HEALTHSTATUS.FAC,
                                FUNCTIONALLIMITATION1)


#Pairwise Correlations


nhisdatafil65corr <-as.data.frame(char2numeric(nhisdatafil653))


cor.mat <- cor(nhisdatafil65corr,use="complete.obs")

stargazer(cor.mat, type = "text",
          title = "Table 2. Correlation Matrix", digits=3, align = TRUE, font.size = "tiny",
          out="table2.txt")

nhisdatafil652$TROUBLEFINDINGDOC.alt[nhisdatafil652$TROUBLEFINDINGDOC1=="1"]<-"Yes"
nhisdatafil652$TROUBLEFINDINGDOC.alt[nhisdatafil652$TROUBLEFINDINGDOC1=="0"]<-"No"
nhisdatafil652$TROUBLEFINDINGDOC_<-factor(nhisdatafil652$TROUBLEFINDINGDOC.alt)


nhisdatafil652$SEX.alt[nhisdatafil652$SEX1==1]<-"Male"
nhisdatafil652$SEX.alt[nhisdatafil652$SEX1==0]<-"Female"
nhisdatafil652$SEX_<-factor(nhisdatafil652$SEX.alt)


nhisdatafil652$MARITALSTATUS.alt[nhisdatafil652$MARITALSTATUS.FAC==1]<-"Married"
nhisdatafil652$MARITALSTATUS.alt[nhisdatafil652$MARITALSTATUS.FAC==0]<-"NotMarried"
nhisdatafil652$MARITALSTATUS_<-factor(nhisdatafil652$MARITALSTATUS.alt)

nhisdatafil652$HISPANIC.alt[nhisdatafil652$HISPANIC1=="1"]<-"Yes"
nhisdatafil652$HISPANIC.alt[nhisdatafil652$HISPANIC1=="0"]<-"No"
nhisdatafil652$HISPANIC_<-factor(nhisdatafil652$HISPANIC.alt)

nhisdatafil652$RACE.alt[nhisdatafil652$RACE.FAC==1]<-"White"
nhisdatafil652$RACE.alt[nhisdatafil652$RACE.FAC==0]<-"NonWhite"
nhisdatafil652$RACE_<-factor(nhisdatafil652$RACE.alt)

nhisdatafil652$REGION.alt[nhisdatafil652$REGION.FAC==1]<-"NorthEast"
nhisdatafil652$REGION.alt[nhisdatafil652$REGION.FAC==2]<-"MidWest"
nhisdatafil652$REGION.alt[nhisdatafil652$REGION.FAC==3]<-"South"
nhisdatafil652$REGION.alt[nhisdatafil652$REGION.FAC==4]<-"West"
nhisdatafil652$REGION_<-factor(nhisdatafil652$REGION.alt)


nhisdatafil652$EDUCATIONLEVEL.alt[nhisdatafil652$EDUCATIONLEVEL.FAC==1]<-"NoOrSomeSchool"
nhisdatafil652$EDUCATIONLEVEL.alt[nhisdatafil652$EDUCATIONLEVEL.FAC==2]<-"HighSchool"
nhisdatafil652$EDUCATIONLEVEL.alt[nhisdatafil652$EDUCATIONLEVEL.FAC==3]<-"SomeCollege"
nhisdatafil652$EDUCATIONLEVEL.alt[nhisdatafil652$EDUCATIONLEVEL.FAC==4]<-"HigherDegree"
nhisdatafil652$EDUCATIONLEVEL_<-factor(nhisdatafil652$EDUCATIONLEVEL.alt)

nhisdatafil652$INTERESTINCOME.alt[nhisdatafil652$INTERESTINCOME1=="1"]<-"Yes"
nhisdatafil652$INTERESTINCOME.alt[nhisdatafil652$INTERESTINCOME1=="0"]<-"No"
nhisdatafil652$INTERESTINCOME_<-factor(nhisdatafil652$INTERESTINCOME.alt)


nhisdatafil652$DIVIDENDINCOME.alt[nhisdatafil652$DIVIDENDINCOME1=="1"]<-"Yes"
nhisdatafil652$DIVIDENDINCOME.alt[nhisdatafil652$DIVIDENDINCOME1=="0"]<-"No"
nhisdatafil652$DIVIDENDINCOME_<-factor(nhisdatafil652$DIVIDENDINCOME.alt)


nhisdatafil652$MEDICAREADVANTAGE.alt[nhisdatafil652$MEDICAREADVANTAGE1=="1"]<-"Yes"
nhisdatafil652$MEDICAREADVANTAGE.alt[nhisdatafil652$MEDICAREADVANTAGE1=="0"]<-"No"
nhisdatafil652$MEDICAREADVANTAGE_<-factor(nhisdatafil652$MEDICAREADVANTAGE.alt)

nhisdatafil652$MEDICAID.alt[nhisdatafil652$MEDICAID1=="1"]<-"Yes"
nhisdatafil652$MEDICAID.alt[nhisdatafil652$MEDICAID1=="0"]<-"No"
nhisdatafil652$MEDICAID_<-factor(nhisdatafil652$MEDICAID.alt)

nhisdatafil652$MEDICAREPRESCR.alt[nhisdatafil652$MEDICAREPRESCR1=="1"]<-"Yes"
nhisdatafil652$MEDICAREPRESCR.alt[nhisdatafil652$MEDICAREPRESCR1=="0"]<-"No"
nhisdatafil652$MEDICAREPRESCR_<-factor(nhisdatafil652$MEDICAREPRESCR.alt)

nhisdatafil652$HEALTHINTERNETINFO.alt[nhisdatafil652$HEALTHINTERNETINFO1=="1"]<-"Yes"
nhisdatafil652$HEALTHINTERNETINFO.alt[nhisdatafil652$HEALTHINTERNETINFO1=="0"]<-"No"
nhisdatafil652$HEALTHINTERNETINFO_<-factor(nhisdatafil652$HEALTHINTERNETINFO.alt)


nhisdatafil652$HEALTHSTATUS.alt[nhisdatafil652$HEALTHSTATUS.FAC==1]<-"Excellent"
nhisdatafil652$HEALTHSTATUS.alt[nhisdatafil652$HEALTHSTATUS.FAC==2]<-"VeryGood"
nhisdatafil652$HEALTHSTATUS.alt[nhisdatafil652$HEALTHSTATUS.FAC==3]<-"Good"
nhisdatafil652$HEALTHSTATUS.alt[nhisdatafil652$HEALTHSTATUS.FAC==4]<-"Fair"
nhisdatafil652$HEALTHSTATUS.alt[nhisdatafil652$HEALTHSTATUS.FAC==4]<-"Poor"
nhisdatafil652$HEALTHSTATUS_<-factor(nhisdatafil652$HEALTHSTATUS.alt)

nhisdatafil652$FUNCTIONALLIMITATION.alt[nhisdatafil652$FUNCTIONALLIMITATION1=="1"]<-"Yes"
nhisdatafil652$FUNCTIONALLIMITATION.alt[nhisdatafil652$FUNCTIONALLIMITATION1=="0"]<-"No"
nhisdatafil652$FUNCTIONALLIMITATION_<-factor(nhisdatafil652$FUNCTIONALLIMITATION.alt)




#Model with HEALTHSTATUS and not FUNCTIONALIMITATION


model1.logit=glm(TROUBLEFINDINGDOC_~
                   AGE+
                   MARITALSTATUS_+
                   SEX_+
                   MARITALSTATUS_+
                   HISPANIC_+
                   RACE_+
                   REGION_+
                   EDUCATIONLEVEL_+
                   INTERESTINCOME_+
                   DIVIDENDINCOME_+
                   MEDICAREADVANTAGE_+
                   MEDICAID_+
                   MEDICAREPRESCR_+
                   HEALTHINTERNETINFO_+
                   HEALTHSTATUS_,
                 data = nhisdatafil652,
                 family = binomial("logit"))



stargazer(model1.logit, type = "text",
          title = "Table 3. Regression Results - with HEALTHSTATUS and no FUNCTIONALIMITATION", digits = 2, align = TRUE,
          out = "table3_P.txt")



summary(model1.logit)


#Model without HEALTHSTATUS but with FUNCTIONALIMITATION

model2.logit=glm(TROUBLEFINDINGDOC_~
                   AGE+
                   MARITALSTATUS_+
                   SEX_+
                   
                   HISPANIC_+
                   RACE_+
                   REGION_+
                   EDUCATIONLEVEL_+
                   INTERESTINCOME_+
                   DIVIDENDINCOME_+
                   MEDICAREADVANTAGE_+
                   MEDICAID_+
                   MEDICAREPRESCR_+
                   HEALTHINTERNETINFO_+
                   FUNCTIONALLIMITATION_,
                 data = nhisdatafil652,
                 family = binomial("logit"))



stargazer(model2.logit, type = "text",
          title = "Table 3. Regression Results -without HEALTHSTATUS but with FUNCTIONALIMITATION", digits = 2, align = TRUE,
          out = "table3_P.txt")



summary(model2.logit)



## Model Significance tests

# Model with HEALTHSTATUS but no FUNCTIONALIMITATION

gchisq.logit <- model1.logit$null.deviance - model1.logit$deviance
p.gchisq.logit <- pchisq(gchisq.logit,df = 20, lower.tail = FALSE)
gchisq.logit
p.gchisq.logit
fitted.logit<-fitted(model1.logit, type="response")
max(fitted.logit)
min(fitted.logit)
## average marginal effects, dydx
summary(margins(model1.logit, type="response"))

# Model with FUNCTIONALIMITATION but no HEALTHSTATUS

gchisq.logit <- model2.logit$null.deviance - model2.logit$deviance
p.gchisq.logit <- pchisq(gchisq.logit,df = 18, lower.tail = FALSE)
gchisq.logit
p.gchisq.logit
fitted.logit<-fitted(model2.logit, type="response")
max(fitted.logit)
min(fitted.logit)
## average marginal effects, dydx
summary(margins(model2.logit, type="response"))



