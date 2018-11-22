# IAN BLOMQUIST and MICHAEL MACEY

# set working directory
setwd("/Users/maceyma/Desktop/ISA 491/Project")

# supress warnings
options(warn=-1)

# libraries
library(readxl)
library(Amelia)
library(dplyr)
library(Hmisc)
library(foreign)


### Data Importation

# We will be using data from all years (2005-2014) for our analysis. We feel it is more important
# to have more observations than more it is to have more variables. Some of the variables
# record similar types of information. Thus, we want to reduce the redundancy in the data
# and keep things simple. We want to see how retention data has changed over time so keeping 
# all years in our analysis will allow us to capture this. 

# major data
maj <- read_excel("Major Codes and Descriptions.xlsx", sheet = "data")

### 2005
d05 <- read_excel("Data for Business Class.xlsx", sheet = "2005")
m05 <- merge(d05,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r05 <- read_excel("Data for Business Class.xlsx", sheet = "2005 Retained")
f05 <- merge(m05,r05,by="tag") # change Retained to 0 and 1
f05$Retained <- ifelse(f05$Retained=="Gone",0,1)

# 2005 size, high school GPA, ACT score
f05$imp_GPA <- with(f05,impute(GPA,mean))
f05$imp_ACTComposite <- with(f05,impute(ACTComposite,mean))
f05imp_GPA_mean <- mean(f05$imp_GPA)
f05imp_ACTComposite_mean <- mean(f05$imp_ACTComposite)
f05obs <- nrow(f05)

# create 2005 data frame
t05_headings <- c("2005 Class Size","2005 Mean GPA","2005 Mean ACTComposite")
t05_obs <- c(f05obs,f05imp_GPA_mean,f05imp_ACTComposite_mean)
t05 <- rbind(t05_obs)
colnames(t05) <- t05_headings
t05 <- as.data.frame(t05)


### 2006
d06 <- read_excel("Data for Business Class.xlsx", sheet = "2006")
m06 <- merge(d06,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r06 <- read_excel("Data for Business Class.xlsx", sheet = "2006 retained")
f06 <- merge(m06,r06,by="tag") # change retained to 0 and 1
f06$retained <- ifelse(f06$retained=="Gone",0,1)
colnames(f06)[65] <- "Retained"

# 2006 size, high school GPA, ACT score
f06$imp_GPA <- with(f06,impute(GPA,mean))
f06$imp_ACTComposite <- with(f06,impute(ACTComposite,mean))
f06imp_GPA_mean <- mean(f06$imp_GPA)
f06imp_ACTComposite_mean <- mean(f06$imp_ACTComposite)
f06obs <- nrow(f06)

# create 2006 data frame
t06_headings <- c("2006 Class Size","2006 Mean GPA","2006 Mean ACTComposite")
t06_obs <- c(f06obs,f06imp_GPA_mean,f06imp_ACTComposite_mean)
t06 <- rbind(t06_obs)
colnames(t06) <- t06_headings
t06 <- as.data.frame(t06)


### 2007
d07 <- read_excel("Data for Business Class.xlsx", sheet = "2007")
m07 <- merge(d07,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r07 <- read_excel("Data for Business Class.xlsx", sheet = "2007 Retained")
f07 <- merge(m07,r07,by="tag") # change retained to 0 and 1
f07$retained <- ifelse(f07$retained=="Gone",0,1)
colnames(f07)[66] <- "Retained"

# 2007 size, high school GPA, ACT score
f07$imp_GPA <- with(f07,impute(GPA,mean))
f07$imp_ACTComposite <- with(f07,impute(ACTComposite,mean))
f07imp_GPA_mean <- mean(f07$imp_GPA)
f07imp_ACTComposite_mean <- mean(f07$imp_ACTComposite)
f07obs <- nrow(f07)

# create 2007 data frame
t07_headings <- c("2007 Class Size","2007 Mean GPA","2007 Mean ACTComposite")
t07_obs <- c(f07obs,f07imp_GPA_mean,f07imp_ACTComposite_mean)
t07 <- rbind(t07_obs)
colnames(t07) <- t07_headings
t07 <- as.data.frame(t07)


### 2008
d08 <- read_excel("Data for Business Class.xlsx", sheet = "2008")
m08 <- merge(d08,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r08 <- read_excel("Data for Business Class.xlsx", sheet = "2008 retained")
f08 <- merge(m08,r08,by="tag") # change retained to 0 and 1
f08$retained <- ifelse(f08$retained=="Gone",0,1)
colnames(f08)[64] <- "Retained"

# 2008 size, high school GPA, ACT score
f08$imp_GPA <- with(f08,impute(GPA,mean))
f08$imp_ACTComposite <- with(f08,impute(ACTComposite,mean))
f08imp_GPA_mean <- mean(f08$imp_GPA)
f08imp_ACTComposite_mean <- mean(f08$imp_ACTComposite)
f08obs <- nrow(f08)

# create 2008 data frame
t08_headings <- c("2008 Class Size","2008 Mean GPA","2008 Mean ACTComposite")
t08_obs <- c(f08obs,f08imp_GPA_mean,f08imp_ACTComposite_mean)
t08 <- rbind(t05_obs)
colnames(t08) <- t08_headings
t08 <- as.data.frame(t08)


### 2009
d09 <- read_excel("Data for Business Class.xlsx", sheet = "2009")
m09 <- merge(d09,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r09 <- read_excel("Data for Business Class.xlsx", sheet = "2009 retained")
f09 <- merge(m09,r09,by="tag")
colnames(f09)[67] <- "Retained"

# 2009 size, high school GPA, ACT score
f09$imp_GPA <- with(f09,impute(GPA,mean))
f09$imp_ACTComposite <- with(f09,impute(ACTComposite,mean))
f09imp_GPA_mean <- mean(f09$imp_GPA)
f09imp_ACTComposite_mean <- mean(f09$imp_ACTComposite)
f09obs <- nrow(f09)

# create 2009 data frame
t09_headings <- c("2009 Class Size","2009 Mean GPA","2009 Mean ACTComposite")
t09_obs <- c(f09obs,f09imp_GPA_mean,f09imp_ACTComposite_mean)
t09 <- rbind(t09_obs)
colnames(t09) <- t09_headings
t09 <- as.data.frame(t09)


### 2010
d10 <- read_excel("Data for Business Class.xlsx", sheet = "2010")
m10 <- merge(d10,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r10 <- read_excel("Data for Business Class.xlsx", sheet = "2010 Retained")
f10 <- merge(m10,r10,by="tag")

# 2010 size, high school GPA, ACT score
f10$imp_GPA <- with(f10,impute(GPA,mean))
f10$imp_ACTComposite <- with(f10,impute(ACTComposite,mean))
f10imp_GPA_mean <- mean(f10$imp_GPA)
f10imp_ACTComposite_mean <- mean(f10$imp_ACTComposite)
f10obs <- nrow(f10)

# create 2010 data frame
t10_headings <- c("2010 Class Size","2010 Mean GPA","2010 Mean ACTComposite")
t10_obs <- c(f10obs,f10imp_GPA_mean,f10imp_ACTComposite_mean)
t10 <- rbind(t10_obs)
colnames(t10) <- t10_headings
t10 <- as.data.frame(t10)


### 2011
d11 <- read_excel("Data for Business Class.xlsx", sheet = "2011")
m11 <- merge(d11,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r11 <- read_excel("Data for Business Class.xlsx", sheet = "2011 Retained")
f11 <- merge(m11,r11,by="tag")

# 2011 size, high school GPA, ACT score
f11$imp_GPA <- with(f11,impute(GPA,mean))
f11$imp_ACTComposite <- with(f11,impute(ACTComposite,mean))
f11imp_GPA_mean <- mean(f11$imp_GPA)
f11imp_ACTComposite_mean <- mean(f11$imp_ACTComposite)
f11obs <- nrow(f11)

# create 2011 data frame
t11_headings <- c("2011 Class Size","2011 Mean GPA","2011 Mean ACTComposite")
t11_obs <- c(f11obs,f11imp_GPA_mean,f11imp_ACTComposite_mean)
t11 <- rbind(t11_obs)
colnames(t11) <- t11_headings
t11 <- as.data.frame(t11)


### 2012
d12 <- read_excel("Data for Business Class.xlsx", sheet = "2012")
m12 <- merge(d12,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r12 <- read_excel("Data for Business Class.xlsx", sheet = "2012 Retained")
f12 <- merge(m12,r12,by="tag")
colnames(f12)[124] <- "Retained"

# 2012 size, high school GPA, ACT score
f12$imp_GPA <- with(f12,impute(GPA,mean))
f12$imp_ACTComposite <- with(f12,impute(ACTComposite,mean))
f12imp_GPA_mean <- mean(f12$imp_GPA)
f12imp_ACTComposite_mean <- mean(f12$imp_ACTComposite)
f12obs <- nrow(f12)

# create 2012 data frame
t12_headings <- c("2012 Class Size","2012 Mean GPA","2012 Mean ACTComposite")
t12_obs <- c(f12obs,f12imp_GPA_mean,f12imp_ACTComposite_mean)
t12 <- rbind(t12_obs)
colnames(t12) <- t12_headings
t12 <- as.data.frame(t12)

### 2013
d13 <- read_excel("Data for Business Class.xlsx", sheet = "2013")
m13 <- merge(d13,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r13 <- read_excel("Data for Business Class.xlsx", sheet = "2013 Retained")
f13 <- merge(m13,r13,by="tag")

# 2013 size, high school GPA, ACT score
f13$imp_GPA <- with(f13,impute(GPA,mean))
f13$imp_ACTComposite <- with(f13,impute(ACTComposite,mean))
f13imp_GPA_mean <- mean(f13$imp_GPA)
f13imp_ACTComposite_mean <- mean(f13$imp_ACTComposite)
f13obs <- nrow(f13)

# create 2013 data frame
t13_headings <- c("2013 Class Size","2013 Mean GPA","2013 Mean ACTComposite")
t13_obs <- c(f13obs,f13imp_GPA_mean,f13imp_ACTComposite_mean)
t13 <- rbind(t13_obs)
colnames(t13) <- t13_headings
t13 <- as.data.frame(t13)

### 2014
d14 <- read_excel("Data for Business Class.xlsx", sheet = "2014")
m14 <- merge(d14,maj,by.x=c("Major"),by.y=c("STVMAJR_CODE"))
r14 <- read_excel("Data for Business Class.xlsx", sheet = "2014 Retained")
f14 <- merge(m14,r14,by="tag")

# 2014 size, high school GPA, ACT score
f14$imp_GPA <- with(f14,impute(GPA,mean))
f14$imp_ACTComposite <- with(f14,impute(ACTComposite,mean))
f14imp_GPA_mean <- mean(f14$imp_GPA)
f14imp_ACTComposite_mean <- mean(f14$imp_ACTComposite)
f14obs <- nrow(f14)

# create 2013 data frame
t14_headings <- c("2014 Class Size","2014 Mean GPA","2014 Mean ACTComposite")
t14_obs <- c(f14obs,f14imp_GPA_mean,f14imp_ACTComposite_mean)
t14 <- rbind(t14_obs)
colnames(t14) <- t14_headings
t14 <- as.data.frame(t14)

# combined table
class_table <- rbind(t05_obs,t06_obs,t07_obs,t08_obs,t09_obs,t10_obs,t11_obs,t12_obs,t13_obs,t14_obs)
colnames(class_table) <- c("Class Size","Class Mean GPA","Class Mean ACTComposite")
class_table <- as.data.frame(class_table)

# Class size varies in size between the years of 2005 and 2010, but from 2011-2014 
# there seems to be an upwards trend. Class Mean GPA has a downward trend from the 
# years of 2005 to 2008, then varies from 2008 to 2012 where it then shoots up in 2013 
# with a small decrease in 2014. Class Mean ACTComposite stays remains relatively 
# the same from 2005 to 2010, and increases dramatically from 2010 to 2014.
class_table

# Class statistics plots
# *Index 1 - 2005, Index 2 - 2006, Index 3 - 2007, ... etc.

plot(class_table$`Class Size`) # Class Size Over The Years
plot(class_table$`Class Mean GPA`) # Class Mean GPA Over The Years
plot(class_table$`Class Mean ACTComposite`) # Class Mean ACTComposite Over The Years



### Data Condensing

variables <- read_excel("Data for Business Class.xlsx", sheet = "Used Variables")
inputs <- as.list(variables)

# All variables listed below will serve as input variables for our analysis. The 'tag' variable
# will serve as a unique ID for each distinct student. The 'Retained' variable will serve 
# as our target variable.
inp <- c("tag","HomeState","NationDesc","Gender","AlumniConnection","Division","Major",
         "ApplicationType","Housing","SpecialConsideration","Question","Decision",
         "DecisionType","AcadRS","RankPercent","ClassSize","GPA","ACTBest","ACTComposite",
         "ACTEquivalent","SATComp","HsType","Math","Lang","EER","ON","Citizen","VisaType",
         "Retained")

# 2005 condense/cleanse
c05 <- f05[inp]
c05$Year <- 2005
#c05$dummy_2005 <- 1 # dummy variable for year variable

# 2006 condense/cleanse
c06 <- f06[inp]
c06$Year <- 2006
#c06$dummy_2006 <- 2006 # dummy variable for year variable

# 2007 condense/cleanse
c07 <- f07[inp]
c07$Year <- 2007
#c07$dummy_2007 <- 1 # dummy variable for year variable

# 2008 condense/cleanse
c08 <- f08[inp]
c08$Year <- 2008
#c08$dummy_2008 <- 1 # dummy variable for year variable

# 2009 condense/cleanse
c09 <- f09[inp]
c09$Year <- 2009
#c09$dummy_2009 <- 1 # dummy variable for year variable

# 20010 condense/cleanse
c10 <- f10[inp]
c10$Year <- 2010
#c10$dummy_2010 <- 1 # dummy variable for year variable

# 2011 condense/cleanse
c11 <- f11[inp]
c11$Year <- 2011
#c11$dummy_2011 <- 1 # dummy variable for year variable

# 2012 condense/cleanse
c12 <- f12[inp]
c12$Year <- 2012
#c12$dummy_2012 <- 1 # dummy variable for year variable

# 2013 condense/cleanse
c13 <- f13[inp]
c13$Year <- 2013
#c13$dummy_2013 <- 1 # dummy variable for year variable

# 2014 condense/cleanse
c14 <- f14[inp]
c14$Year <- 2014
#c14$dummy_2014 <- 1 # dummy variable for year variable

# combine data clean data sets
cc <- rbind(c05,c06,c07,c08,c09,c10,c11,c12,c13,c14)



### Data Transformation

# All NA's for interval variables are imputed with each variable's mean value.
# All NA's for nominal variables are imputed the value IMP_variableName.
# We created new variables containing the prefix IMP_ to illustrate the presence
# of an imputed variable. We are still keeping the original variables as we do not
# want to throw them out yet since we might still need them for later analysis.
# As mentioned earlier, certain years had the Retained variable coded as 'Here'
# and 'Gone'. We converted these values to 1 and 0, respectively.


### Data Imputation
# Imputed variables are constructed as follows. For each variable that needs to be imputed in some
# sort of way, the methodology can be seen below. All categorical variables are imputed with the variable
# name preceeded by the IMP_prefix. All interval variables are imputed with the variables mean. Afer we 
# created the new imputed varaibles, we also constructed indicator variables that places a value of 1 to
# indicate if a value for that observation was imputed or not. The variables are implemented with the 
# prefix IMP_ followed by the variable name followed by _IND. 

# Impute HomeState NA with IMP
cc$IMP_HomeState <- ifelse(is.na(cc$HomeState),"IMP_HomeState",cc$HomeState)
cc$IMP_HomeState_Ind <- ifelse(is.na(cc$HomeState),1,0)

# Impute NationDesc NA with USA
cc$IMP_NationDesc <- ifelse(is.na(cc$NationDesc),"USA",cc$NationDesc)
cc$IMP_NationDesc_Ind <- ifelse(is.na(cc$NationDesc),1,0)

# Impute AlumniConnection NA with IMP
cc$IMP_AlumniConnection <- ifelse(is.na(cc$AlumniConnection),"IMP_AlumniConnection",cc$AlumniConnection)
cc$IMP_AlumniConnection_Ind <- ifelse(is.na(cc$AlumniConnection),1,0)

# Impute Housing NA with IMP
cc$IMP_Housing <- ifelse(is.na(cc$Housing),"IMP_Housing",cc$Housing)
cc$IMP_Housing_Ind <- ifelse(is.na(cc$Housing),1,0)

# Impute SpecialConsideration NA with IMP
cc$IMP_SpecialConsideration <- ifelse(is.na(cc$SpecialConsideration),"IMP_SpecialConsideration",cc$SpecialConsideration)
cc$IMP_SpecialConsideration_Ind <- ifelse(is.na(cc$SpecialConsideration),1,0)

# Impute Question NA with IMP
cc$IMP_Question <- ifelse(is.na(cc$Question),"IMP_Question",cc$Question)
cc$IMP_Question_Ind <- ifelse(is.na(cc$Question),1,0)

# Impute DecisionType NA with IMP
cc$IMP_DecisionType <- ifelse(is.na(cc$DecisionType),"IMP_DecisionType",cc$DecisionType)
cc$IMP_DecisionType_Ind <- ifelse(is.na(cc$DecisionType),1,0)

# Impute AcadRS NA with mean
cc$IMP_AcadRS <- with(cc,impute(AcadRS,mean))
cc$IMP_AcadRS_Ind <- ifelse(is.na(cc$AcadRS),1,0)

# Impute RankPercent NA with mean
cc$IMP_RankPercent <- with(cc,impute(RankPercent,mean))
cc$IMP_RankPercent_Ind <- ifelse(is.na(cc$RankPercent),1,0)

# Impute ClassSize NA with mean
cc$IMP_ClassSize <- with(cc,impute(ClassSize,mean))
cc$IMP_ClassSize_Ind <- ifelse(is.na(cc$ClassSize),1,0)

# Impute GPA NA with mean
cc$IMP_GPA <- with(cc,impute(GPA,mean))
cc$IMP_GPA_Ind <- ifelse(is.na(cc$GPA),1,0)

# Impute ACTBest NA with mean
cc$IMP_ACTBest <- with(cc,impute(ACTBest,mean))
cc$IMP_ACTBest_Ind <- ifelse(is.na(cc$ACTBest),1,0)

# Impute ACTComposite NA with mean
cc$IMP_ACTComposite <- with(cc,impute(ACTComposite,mean))
cc$IMP_ACTComposite_Ind <- ifelse(is.na(cc$ACTComposite),1,0)

# Impute ACTEquivalent NA with mean
cc$IMP_ACTEquivalent <- with(cc,impute(ACTEquivalent,mean))
cc$IMP_ACTEquivalent_Ind <- ifelse(is.na(cc$ACTEquivalent),1,0)

# Impute SATComp NA with mean
cc$IMP_SATComp <- with(cc,impute(SATComp,mean))
cc$IMP_SATComp_Ind <- ifelse(is.na(cc$SATComp),1,0)

# Impute HsType NA with IMP
cc$IMP_HsType <- ifelse(is.na(cc$HsType),"IMP_HsType",cc$HsType)
cc$IMP_HsType_Ind <- ifelse(is.na(cc$HsType),1,0)

# Impute Math NA with IMP
cc$IMP_Math <- ifelse(is.na(cc$Math),"IMP_Math",cc$Math)
cc$IMP_Math_Ind <- ifelse(is.na(cc$Math),1,0)

# Impute Lang NA with IMP
cc$IMP_Lang <- ifelse(is.na(cc$Lang),"IMP_Lang",cc$Lang)
cc$IMP_Lang_Ind <- ifelse(is.na(cc$Lang),1,0)

# Impute EER NA with IMP
cc$IMP_EER <- ifelse(is.na(cc$EER),"IMP_EER",cc$EER)
cc$IMP_EER_Ind <- ifelse(is.na(cc$EER),1,0)

# Impute ON NA with IMP
cc$IMP_ON <- ifelse(is.na(cc$ON),"IMP_ON",cc$ON)
cc$IMP_ON_Ind <- ifelse(is.na(cc$ON),1,0)

# Impute ViaType NA with IMP
cc$IMP_VisaType <- ifelse(is.na(cc$VisaType),"IMP_VisaType",cc$VisaType)
cc$IMP_VisaType_Ind <- ifelse(is.na(cc$VisaType),1,0)

# Imputed data table construction
a1 <- c("HomeState","IMP_HomeState","IMP_HomeState_Ind")
a2 <- c("NationDesc","USA","IMP_NationDesc_Ind")
a3 <- c("AlumniConnection","IMP_AlumniConnection","IMP_AlumniConnection_Ind")
a4 <- c("Housing","IMP_Housing","IMP_Housing_Ind")
a5 <- c("SpecialConsideration","IMP_SpecialConsideration","IMP_SpecialConsideration_Ind")
a6 <- c("Question","IMP_Question","IMP_Question_Ind")
a7 <- c("DecisionType","IMP_DecisionType","IMP_DecisionType_Ind")
a8 <- c("AcadRS",mean(cc$IMP_AcadRS),"IMP_AcadRS_Ind")
a9 <- c("RankPercent",mean(cc$IMP_RankPercent),"IMP_RankPercent_Ind")
a10 <- c("ClassSize",mean(cc$IMP_ClassSize),"IMP_ClassSize_Ind")
a11 <- c("GPA",mean(cc$IMP_GPA),"IMP_GPA_Ind")
a12 <- c("ACTBest",mean(cc$IMP_ACTBest),"IMP_ACTBest_Ind")
a13 <- c("ACTComposite",mean(cc$IMP_ACTComposite),"IMP_ACTComposite_Ind")
a14 <- c("ACTEquivalent",mean(cc$IMP_ACTEquivalent),"IMP_ACTEquivalent_Ind")
a15 <- c("SATComp",mean(cc$IMP_SATComp),"IMP_SATComp_Ind")
a16 <- c("HsType","IMP_HsType","IMP_HsType_Ind")
a17 <- c("Math","IMP_Math","IMP_Math_Ind")
a18 <- c("Lang","IMP_Lang","IMP_Lang_Ind")
a19 <- c("EER","IMP_EER","IMP_EER_Ind")
a20 <- c("ON","IMP_ON","IMP_ON_Ind")
a21 <- c("VisaType","IMP_VisaType","IMP_VisaType_Ind")

aa <- as.data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,
            a11,a12,a13,a14,a15,a16,a17,a18,
            a19,a20,a21))
colnames(aa) <- c("VARIABLE","IMPUTED VALUE","IMPUTED INDICATOR VARIABLE NAME")

# Data frame of imputed varaibles, value that is imputed for each variable and the name
# for every indicator variable for each variable.
aa



### Logarithmic Transformation

# For our data set, there are only 3 variables that need to be transformed: GPA, ClassSize and RankPercent.
# Below, we show the distribution of each variable before the transformation process as well as after the 
# transformation proces. We use the imputed variables for the transformation process becauase there are no
# missing values. Each variable is transformed by taking the LOG of each variable. We can see that the 
# distributions for each variable become more normal after the transformation process.

# Histogram of un-transformed IMP_GPA variable
hist(cc$IMP_GPA)
cc$IMP_GPA_Trans <- log(cc$IMP_GPA)

# Histogram of transformed IMP_GPA variable
hist(cc$IMP_GPA_Trans)

# Histogram of un-transformed IMP_ClassSize variable
hist(cc$IMP_ClassSize)
cc$IMP_ClassSize_Trans <- log(cc$IMP_ClassSize)

# Histogram of transformed IMP_ClassSize variable
hist(cc$IMP_ClassSize_Trans)

# Histogram of un-transformed IMP_RankPercent variable
hist(cc$IMP_RankPercent)
cc$IMP_RankPercent_Trans <- log(cc$IMP_RankPercent)

# Histogram of transformed IMP_RankPercent variable
hist(cc$IMP_RankPercent_Trans)


### Variable Creation/Dimension Reduction

# In order to reduce the dimensionality for our future model, we create three new variables that
# minimize the amount of levels of different variables. The SC_General variable is created to
# reduce the dimensionality in the SpecialConsiderations variable. The Alumni_General variable is
# created to reduce the dimensionality in the AlumniConnection variable. Finally, the Continent 
# variable is created to reduce the dimensionality of the NationDesc variable. Outputs of the new
# variables can be seen below. The will allows us to have fewer levels in our model, which means
# fewer degrees of freedom, which ultimately will lead to better predictive power in our future models.

# variable created to reduce the amount of levels in the IMP_SpecialConsideration variable
cc$SC_General <- ifelse(cc$IMP_SpecialConsideration=="IMP_SpecialConsideration",0,1)
unique(cc$SC_General)

# variable created to reduce the amount of levels in the IMP_AlumniConnection variable
cc$Alumni_General <- ifelse(cc$IMP_AlumniConnection=="IMP_AlumniConnection",0,1)
unique(cc$Alumni_General)

# continent vectors are created to help construct the continent variable
north_america <- c("Canada","Trinidad & Tobago","USA")
south_america <- c("Argentina","Bolivia","Brazil","Chile","Ecuador","Guatemala","Guyana","Panama","Peru")
europe <- c("Albania","Austria","Bulgaria","Cyprus","Finland","Georgia","Germany","Hungary",
            "Italy","Luxembourg","Norway","Poland","Romania","Russia","Serbia and Montenegro",
            "Sweden","Switzerland","Turkey","United Kingdom")
africa <- c("Ghana","Kenya","Morocco","Namibia","Nigeria","Tanzania","Tunisia","Zambia","Zimbabwe")
australia <- c("Australia")
asia <- c("Bahrain","Bangladesh","China","Hong Kong","India","Indonesia","Japan","Kazakhstan","Kuwait","Macau",
          "Malaysia","Mauritius","Nepal","Pakistan","Philippines","Qatar","Singapore","South Korea",
          "Sri Lanka","Taiwan","Thailand","United Arab Emirates","Vietnam")

# Continent varaible created to list each student's continent of origin
cc$Continent <- ifelse(cc$IMP_NationDesc %in% north_america,"North America",
                       ifelse(cc$IMP_NationDesc %in% south_america,"South America",
                              ifelse(cc$IMP_NationDesc %in% europe,"Europe",
                                     ifelse(cc$IMP_NationDesc %in% africa,"Africa",
                                            ifelse(cc$IMP_NationDesc %in% australia,"Australia",
                                                   ifelse(cc$IMP_NationDesc %in% asia,"Asia",""))))))

# showcase the reduced amount of levels in the continent variable
unique(cc$Continent)

### Input variables in tabular format

# Frequency tables are displayed for nominal variables, while summary statistics
# are displayed for interval variables.

# IMP_HomeState frequency table
i1 <- as.data.frame(table(cc$IMP_HomeState))
i1

# IMP_NationDesc frequency table
i2 <- as.data.frame(table(cc$IMP_NationDesc))
i2

# Gender frequency table
i3 <- as.data.frame(table(cc$Gender))
i3

# IMP_AlumniConnection frequency table
i4 <- as.data.frame(table(cc$IMP_AlumniConnection))
i4

# Division frequency table
i5 <- as.data.frame(table(cc$Division))
i5

# Major frequency table
i6 <- as.data.frame(table(cc$Major))
i6

# ApplicationType frequency table
i7 <- as.data.frame(table(cc$ApplicationType))
i7

# IMP_Housing frequency table
i8 <- as.data.frame(table(cc$IMP_Housing))
i8

# IMP_SpecialConsideration frequency table
i9 <- as.data.frame(table(cc$IMP_SpecialConsideration))
i9

# IMP_Question frequency table
i10 <- as.data.frame(table(cc$IMP_Question))
i10

# Decision frequency table
i11 <- as.data.frame(table(cc$Decision))
i11

# IMP_DecisionType frequency table
i12 <- as.data.frame(table(cc$IMP_DecisionType))
i12

# IMP_AcadRS summary table
i13 <- summary(cc$IMP_AcadRS)
i13

# IMP_RankPercent summary table
i14 <- summary(cc$IMP_RankPercent)
i14

# IMP_ClassSize summary table
i15 <- summary(cc$IMP_ClassSize)
i15

# IMP_GPA summary table
i16 <- summary(cc$IMP_GPA)
i16

# IMP_ACTBest summary table
i17 <- summary(cc$IMP_ACTBest)
i17

# IMP_ACTComposite summary table
i18 <- summary(cc$IMP_ACTComposite)
i18

# IMP_ACTEquivalent summary table
i19 <- summary(cc$IMP_ACTEquivalent)
i19

# IMP_SATComp summary table
i20 <- summary(cc$IMP_SATComp)
i20

# IMP_HsType frequency table
i21 <- as.data.frame(table(cc$IMP_HsType))
i21

# IMP_Math frequency table
i22 <- as.data.frame(table(cc$IMP_Math))
i22

# IMP_Lang frequency table
i23 <- as.data.frame(table(cc$IMP_Lang))
i23

# IMP_EER frequency table - *table has too many levels to show complete table
#i24 <- as.data.frame(table(cc$IMP_EER))
#i24

# IMP_ON frequency table
i25 <- as.data.frame(table(cc$IMP_ON))
i25

# Citizen frequency table
i26 <- as.data.frame(table(cc$Citizen))
i26

# IMP_VisaType frequency table
i27 <- as.data.frame(table(cc$IMP_VisaType))
i27

# Year frequency table
i28 <- as.data.frame(table(cc$Year))
i28 

# Reverse ordering of the response variable
cc$Retained <- ifelse(cc$Retained==0,1,0)

write.csv(cc, "/Users/maceyma/Desktop/ISA 491/Project/cc.csv")