###
#
# Replication material for "What is in a number? Reconsidering the evidence on nine-ender effects on the engagement in meaning searching behavior"
# - Erik Gahner Larsen (erikgahner@gmail.com)
# 
# Data from Alter and Hershfield (2014) can be downloaded in SPSS format from http://www.openicpsr.org/repoEntity/show/18882?versionId=18882V2
# Data from World Values Survey can be downloaded in SPSS format from http://www.worldvaluessurvey.org/ (usage information required)
#
# GitHub repo with more information about the R session used to run the script: https://github.com/erikgahner/fp-meaning
#
###

# Load packages - use install.packages("") for missing packages
library(foreign)
library(Hmisc)
library(MASS)
library(ggplot2)
library(lme4)
library(BMA)
library(gridExtra)
library(tidyr)

# Study 1
## Load data
s1.ah <- read.spss("study-1-pnas-18886.sav", to.data.frame=T) 
s1.w1 <- read.spss("WV1_Data_spss_v_2014_04_28.sav", to.data.frame=T)
s1.w2 <- read.spss("WV2_Data_spss_v_2014_04_28.sav", to.data.frame=T)
s1.w3 <- read.spss("WV3_Data_spss_v_2014_09_21.sav", to.data.frame=T)
s1.w4 <- read.spss("WV4_Data_spss_v_2014-04-28.sav", to.data.frame=T)
s1.w5 <- read.spss("WV5_Data_spss_v_2014_04_28.sav", to.data.frame=T)
s1.w6 <- read.spss("WorldValuesSurvey-Wave6-2010-2014_v2014-11-07_spss.sav", to.data.frame=T)

## Create numeric dependent variables with high values equal to more search for meaning
### Wave 1
s1.w1$Meaning <- NA
s1.w1$Meaning[s1.w1$v177 == "Never"] <- 1
s1.w1$Meaning[s1.w1$v177 == "Rarely"] <- 2
s1.w1$Meaning[s1.w1$v177 == "Sometimes"] <- 3
s1.w1$Meaning[s1.w1$v177 == "Often"] <- 4

### Wave 2
s1.w2$Meaning <- NA
s1.w2$Meaning[s1.w2$v133 == "Never"] <- 1
s1.w2$Meaning[s1.w2$v133 == "Rarely"] <- 2
s1.w2$Meaning[s1.w2$v133 == "Sometimes"] <- 3
s1.w2$Meaning[s1.w2$v133 == "Often"] <- 4

### Wave 3
s1.w3$Meaning <- NA
s1.w3$Meaning[s1.w3$V177 == "Never"] <- 1
s1.w3$Meaning[s1.w3$V177 == "Rarely"] <- 2
s1.w3$Meaning[s1.w3$V177 == "Sometimes"] <- 3
s1.w3$Meaning[s1.w3$V177 == "Often"] <- 4

### Wave 4
s1.w4$Meaning <- NA
s1.w4$Meaning[s1.w4$v182 == "Never"] <- 1
s1.w4$Meaning[s1.w4$v182 == "Rarely"] <- 2
s1.w4$Meaning[s1.w4$v182 == "Sometimes"] <- 3
s1.w4$Meaning[s1.w4$v182 == "Often"] <- 4

### Wave 5
s1.w5$Meaning <- NA
s1.w5$Meaning[s1.w5$V184 == "Never"] <- 1
s1.w5$Meaning[s1.w5$V184 == "Rarely"] <- 2
s1.w5$Meaning[s1.w5$V184 == "Sometimes"] <- 3
s1.w5$Meaning[s1.w5$V184 == "Often"] <- 4

### Wave 6
s1.w6$Meaning <- NA
s1.w6$Meaning[s1.w6$V143 == "Never"] <- 1
s1.w6$Meaning[s1.w6$V143 == "Rarely"] <- 2
s1.w6$Meaning[s1.w6$V143 == "Sometimes"] <- 3
s1.w6$Meaning[s1.w6$V143 == "Often"] <- 4

## Create age and nine-ender variables
### AH
s1.ah$NineEnd <- ifelse(s1.ah$NineEnd == 9, 1, 0)

### Wave 1
s1.w1$Age <- s1.w1$v216
s1.w1 <- s1.w1[s1.w1$Age > 24 & s1.w1$Age < 65,]
s1.w1$NineEnd <- ifelse(s1.w1$Age %in% c(29,39,49,59),1,0)

### Wave 2
s1.w2$Age <- s1.w2$v355
s1.w2 <- s1.w2[s1.w2$Age > 24 & s1.w2$Age < 65,]
s1.w2$NineEnd <- ifelse(s1.w2$Age %in% c(29,39,49,59),1,0)

### Wave 3
s1.w3$Age <- s1.w3$V216
s1.w3 <- s1.w3[s1.w3$Age > 24 & s1.w3$Age < 65,]
s1.w3$NineEnd <- ifelse(s1.w3$Age %in% c(29,39,49,59),1,0)

### Wave 4
s1.w4$Age <- s1.w4$v225
s1.w4 <- s1.w4[s1.w4$Age > 24 & s1.w4$Age < 65,]
s1.w4$NineEnd <- ifelse(s1.w4$Age %in% c(29,39,49,59),1,0)

### Wave 5
s1.w5$Age <- s1.w5$V237
s1.w5 <- s1.w5[s1.w5$Age > 24 & s1.w5$Age < 65,]
s1.w5$NineEnd <- ifelse(s1.w5$Age %in% c(29,39,49,59),1,0)

### Wave 6
s1.w6$Age <- s1.w6$V242
s1.w6 <- s1.w6[s1.w6$Age > 24 & s1.w6$Age < 65,]
s1.w6$NineEnd <- ifelse(s1.w6$Age %in% c(29,39,49,59),1,0)

## Run models
### Simple linear regression
s1.ah.slr <- lm(Meaning ~ NineEnd, data=s1.ah)
s1.w1.slr <- lm(Meaning ~ NineEnd, data=s1.w1)
s1.w2.slr <- lm(Meaning ~ NineEnd, data=s1.w2)
s1.w3.slr <- lm(Meaning ~ NineEnd, data=s1.w3)
s1.w4.slr <- lm(Meaning ~ NineEnd, data=s1.w4)
s1.w5.slr <- lm(Meaning ~ NineEnd, data=s1.w5)
s1.w6.slr <- lm(Meaning ~ NineEnd, data=s1.w6)

### Simple ordered logistic regression
s1.ah.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.ah, Hess=TRUE)
s1.w1.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w1, Hess=TRUE)
s1.w2.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w2, Hess=TRUE)
s1.w3.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w3, Hess=TRUE)
s1.w4.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w4, Hess=TRUE)
s1.w5.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w5, Hess=TRUE)
s1.w6.or <- polr(as.factor(Meaning) ~ NineEnd, data = s1.w6, Hess=TRUE)

### Linear mixed effects
s1.ah.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.ah) 
s1.w1.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w1) 
s1.w2.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w2) 
s1.w3.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w3) 
s1.w4.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w4) 
s1.w5.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w5) 
s1.w6.lme <- lmer(Meaning ~ NineEnd + (1 | Age), data=s1.w6) 

# Create data frame with output from the three models
s1.results <- data.frame(
  type = c(rep(c("(A.1) Mean difference","(A.2)  Ordered logistic", "(A.3) Mixed effects"), each=7)),
  study = c(rep(1:7, 3)),
  estimate = c(
    # From simple OLS
    coef(summary(s1.ah.slr))["NineEnd","Estimate"],
    coef(summary(s1.w1.slr))["NineEnd","Estimate"],
    coef(summary(s1.w2.slr))["NineEnd","Estimate"],
    coef(summary(s1.w3.slr))["NineEnd","Estimate"],
    coef(summary(s1.w4.slr))["NineEnd","Estimate"],
    coef(summary(s1.w5.slr))["NineEnd","Estimate"],
    coef(summary(s1.w6.slr))["NineEnd","Estimate"],
    # From ordered logistic 
    coef(summary(s1.ah.or))["NineEnd","Value"],
    coef(summary(s1.w1.or))["NineEnd","Value"],
    coef(summary(s1.w2.or))["NineEnd","Value"],
    coef(summary(s1.w3.or))["NineEnd","Value"],
    coef(summary(s1.w4.or))["NineEnd","Value"],
    coef(summary(s1.w5.or))["NineEnd","Value"],
    coef(summary(s1.w6.or))["NineEnd","Value"],
    # From linear mixed effects
    coef(summary(s1.ah.lme))["NineEnd","Estimate"],
    coef(summary(s1.w1.lme))["NineEnd","Estimate"],
    coef(summary(s1.w2.lme))["NineEnd","Estimate"],
    coef(summary(s1.w3.lme))["NineEnd","Estimate"],
    coef(summary(s1.w4.lme))["NineEnd","Estimate"],
    coef(summary(s1.w5.lme))["NineEnd","Estimate"],
    coef(summary(s1.w6.lme))["NineEnd","Estimate"]
    ),
  se = c(
    # From OLS
    coef(summary(s1.ah.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w1.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w2.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w3.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w4.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w5.slr))["NineEnd","Std. Error"],
    coef(summary(s1.w6.slr))["NineEnd","Std. Error"],
    # From ordered
    coef(summary(s1.ah.or))["NineEnd","Std. Error"],
    coef(summary(s1.w1.or))["NineEnd","Std. Error"],
    coef(summary(s1.w2.or))["NineEnd","Std. Error"],
    coef(summary(s1.w3.or))["NineEnd","Std. Error"],
    coef(summary(s1.w4.or))["NineEnd","Std. Error"],
    coef(summary(s1.w5.or))["NineEnd","Std. Error"],
    coef(summary(s1.w6.or))["NineEnd","Std. Error"],
    # From linear mixed
    coef(summary(s1.ah.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w1.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w2.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w3.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w4.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w5.lme))["NineEnd","Std. Error"],
    coef(summary(s1.w6.lme))["NineEnd","Std. Error"]
    )
  )

# Create Panel A.1, A.2 and A.3 from Figure 1 and save in s1.results
s1.results <- ggplot(s1.results, aes(x = study, y = estimate)) + 
  geom_hline(yintercept=0, col="#D53E4F") +
  geom_point() +
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), colour="black", width=.2) +
  scale_x_continuous(breaks = c(1:7), labels = c("AH","W1","W2","W3","W4","W5","W6")) +
  xlab("") +
  ylab("Effect estimate") +
  facet_grid(. ~ type) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey70" ) 
  )


# Study 4
## Load data
s4 <- read.spss("study-4-pnas-18987.sav", to.data.frame=T)

## Create region code dummy variables
s4$RC.1 <- ifelse(s4$RegionCode == 1, 1, 0)
s4$RC.2 <- ifelse(s4$RegionCode == 2, 1, 0)
s4$RC.3 <- ifelse(s4$RegionCode == 3, 1, 0)
s4$RC.4 <- ifelse(s4$RegionCode == 4, 1, 0)

## Zero-order correlation betwen suicide rate and nine-ender variable (not reported in paper)
rcorr(s4$SuicideRate,s4$NineEnd, type="spearman")

## ANCOVA and ANOVA 
### AH (2014)
s4.ancova <- aov(SuicideRate~NineEnd+Age+RegionCode+DeathsAll, data=s4)
summary(s4.ancova)

### Bivariate model
s4.anova <- aov(SuicideRate~NineEnd, data=s4)
summary(s4.anova)

## Create matrix with the dependent variable, independent variable and covariates
s4.c <- s4[,c("NineEnd", "RC.1", "RC.2", "RC.3", "RC.4", "Age", "DeathsAll")]
s4.c <- as.matrix(s4.c)

## Run the Bayesian model averaging
s4.bma <- bicreg(s4.c,s4$SuicideRate)

## Posterior probability for nine-ender variable (reported in footnote 2)
s4.bma$probne0[[1]]

# Study 5
## Load data
s5 <- read.spss("study-5-pnas-19017.sav", to.data.frame=T)

## Recode data from wide to long
s5.long <- gather(s5, age, time, Time27:Time41)

## Create age variable
s5.long$age <- as.numeric(sub('Time', '', s5.long$age))

## Create nine-ender variable
s5.long$nineend <- ifelse(s5.long$age == 29 | s5.long$age == 39, 1, 0)

## Create end digit variable
s5.long$enddigit <- NA
s5.long$enddigit[s5.long$age == 27 | s5.long$age == 37] <- 1
s5.long$enddigit[s5.long$age == 28 | s5.long$age == 38] <- 2
s5.long$enddigit[s5.long$age == 29 | s5.long$age == 39] <- 3
s5.long$enddigit[s5.long$age == 30 | s5.long$age == 40] <- 4
s5.long$enddigit[s5.long$age == 31 | s5.long$age == 41] <- 5

## Run random effects models
s5.reg.nine <- lmer(time ~ nineend + (1 | Participant), data=s5.long[s5.long$Participant != 22,]) 
s5.reg.end <- lmer(time ~ as.factor(enddigit) + (1 | Participant), data=s5.long[s5.long$Participant != 22,]) 
s5.reg.age <- lmer(time ~ as.factor(age) + (1 | Participant), data=s5.long[s5.long$Participant != 22,]) 

## Create data frames with output from regressions
s5.fig.data.nine <- data.frame( nineend = c("7|8|0|1","9"), 
                       estimate = c(coef(summary(s5.reg.nine))["(Intercept)","Estimate"],
                                coef(summary(s5.reg.nine))["(Intercept)","Estimate"] + coef(summary(s5.reg.nine))["nineend","Estimate"]
                                ),
                       se = c(coef(summary(s5.reg.nine))["(Intercept)","Std. Error"],
                              coef(summary(s5.reg.nine))["nineend","Std. Error"])
                         )

s5.fig.data.end <- data.frame( digit = c(1:5), 
                                enddigit = c("7","8","9","0","1"),
                                estimate = c(coef(summary(s5.reg.end))["(Intercept)","Estimate"],
                                             coef(summary(s5.reg.end))["(Intercept)","Estimate"] + coef(summary(s5.reg.end))["as.factor(enddigit)2","Estimate"],
                                             coef(summary(s5.reg.end))["(Intercept)","Estimate"] + coef(summary(s5.reg.end))["as.factor(enddigit)3","Estimate"],
                                             coef(summary(s5.reg.end))["(Intercept)","Estimate"] + coef(summary(s5.reg.end))["as.factor(enddigit)4","Estimate"],
                                             coef(summary(s5.reg.end))["(Intercept)","Estimate"] + coef(summary(s5.reg.end))["as.factor(enddigit)5","Estimate"]
                                ),
                               se = c(coef(summary(s5.reg.end))["(Intercept)","Std. Error"],
                                            coef(summary(s5.reg.end))["as.factor(enddigit)2","Std. Error"],
                                            coef(summary(s5.reg.end))["as.factor(enddigit)3","Std. Error"],
                                            coef(summary(s5.reg.end))["as.factor(enddigit)4","Std. Error"],
                                            coef(summary(s5.reg.end))["as.factor(enddigit)5","Std. Error"]
                                      )
)

s5.fig.data.age <- data.frame( digit = c(1:10), 
                               age = c("27","28","29","30","31","37","38","39","40","41"),
                               estimate = c(coef(summary(s5.reg.age))["(Intercept)","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)28","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)29","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)30","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)31","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)37","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)38","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)39","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)40","Estimate"],
                                            coef(summary(s5.reg.age))["(Intercept)","Estimate"] + coef(summary(s5.reg.age))["as.factor(age)41","Estimate"]
                               ),
                               se = c(coef(summary(s5.reg.age))["(Intercept)","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)28","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)29","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)30","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)31","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)37","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)38","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)39","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)40","Std. Error"],
                                      coef(summary(s5.reg.age))["as.factor(age)41","Std. Error"]
                               )
)


## Create figure for Panel B.1 in Figure 1
s5.fig.nine <- ggplot(s5.fig.data.nine, aes(x = nineend, y = estimate)) + 
  geom_hline(yintercept=s5.fig.data.nine$estimate[1], col="#D53E4F") +
  geom_point() +
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), colour="black", width=.2) +
  xlab("") +
  ylab("Marathon time (Minutes)") +
  ggtitle("(B.1) Nine-ender") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey70" ),
    plot.title = element_text(size = 10)
  )

## Create figure for Panel B.2 in Figure 1
s5.fig.end <- ggplot(s5.fig.data.end, aes(x = digit, y = estimate)) + 
  geom_hline(yintercept=s5.fig.data.end$estimate[1], col="#D53E4F") +
  geom_point() +
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), colour="black", width=.2) +
  scale_x_continuous(labels = s5.fig.data.end$enddigit) +
  xlab("") +
  ylab("") +
  ggtitle("(B.2) End digit") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey70" ),
    plot.title = element_text(size = 10)
  )

## Create figure for Panel B.3 in Figure 1
s5.fig.age <- ggplot(s5.fig.data.age, aes(x = digit, y = estimate)) + 
  geom_hline(yintercept=s5.fig.data.age$estimate[1], col="#D53E4F") +
  geom_point() +
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), colour="black", width=.2) +
  scale_x_continuous(breaks=c(1:10), labels = s5.fig.data.age$age) +
  xlab("") +
  ylab("") +
  ggtitle("(B.3) Age") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey70" ),
    plot.title = element_text(size = 10)
  )

# Draw Figure 1 and save as results.jpeg
jpeg('results.jpeg', height=5, width=7, units="in",res=700)
grid.arrange(s1.results, arrangeGrob(s5.fig.nine, s5.fig.end, s5.fig.age, widths=c(0.3,0.35,0.55), ncol=3), ncol=1)
dev.off()

# Create sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")


