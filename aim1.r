# setwd("C:/Users/Xinyi/OneDrive - Yale University/Yale/BIS 687/BIS687-Capstone_Project_Team5")
library(dplyr)
# import diet data
physical <- read.csv("physical.csv")
physical <- physical[, -1]
physical <- physical %>% 
  mutate(across(everything(), ~ifelse(.<0, 0, .)))
physical$screen_time <- physical$time_spent_using_computer_f1080_0_0 + physical$time_spent_watching_television_tv_f1070_0_0
physical$screen_time <- ifelse(physical$screen_time>=4, 1, 0)

ukbiobank <- readRDS("ukbiobank.rds")
DQI_score <- read.csv("DQI_score.csv")
DQI_score <- DQI_score[,-1]
age <- ukbiobank[, c("eid", "age_at_recruitment_f21022_0_0")]

df <- merge(physical, age, by = "eid")
df <- merge(df, DQI_score, by = "eid")
df$total_screen_hours_per_day <- df$time_spent_watching_television_tv_f1070_0_0 + df$time_spent_using_computer_f1080_0_0
# df <- df %>% mutate(across(everything(), ~ ifelse(. < 0, 0, .)))
df$DQI_score <- as.numeric(df$DQI_score)
df$ipaq_activity_group_f22032_0_0 <- factor(df$ipaq_activity_group_f22032_0_0,
                                            levels = c("low","moderate","high"),
                                            labels = c(1,2,3))
df$sex_f31_0_0 <- factor(df$sex_f31_0_0,
                         levels = c("Female", "Male"),
                         labels = c(0,1))
df <- na.omit(df)

#----------------- Descriptive Statistics -------------------#
cat("\n\n#----------------- Descriptive Statistics -------------------#\n\n")
library(psych)
# subset the data by screen_time
screen0 <- subset(df, screen_time == 0)
screen1 <- subset(df, screen_time == 1)

# calculate descriptive statistics for screen_time=0
screen_time_0_desc <- describe(screen0[c("age_at_recruitment_f21022_0_0",
                                         "total_met_minites_per_day",
                                         "DQI_score",
                                         "total_screen_hours_per_day")])

# calculate descriptive statistics for screen_time=1
screen_time_1_desc <- describe(screen1[c("age_at_recruitment_f21022_0_0",
                                         "total_met_minites_per_day",
                                         "DQI_score",
                                         "total_screen_hours_per_day")])


# calculate percentage of females in each group
percent_female_0 <- round(prop.table(table(screen0$sex_f31_0_0))["0"] * 100, 2)
percent_female_1 <- round(prop.table(table(screen1$sex_f31_0_0))["0"] * 100, 2)




# calculate descriptive statistics for screen_time=1
screen_time_desc <- describeBy(df[c("age_at_recruitment_f21022_0_0",
                                    "total_met_minites_per_day",
                                    "DQI_score",
                                    "total_screen_hours_per_day")], 
                               df$screen_time, 
                               mat=TRUE, 
                               digits=2, 
                               fast=TRUE)


# print the table and percentage of females
sink("aim1_results.txt")
cat("Group 0: screen time < 4hr per day \n")
cat("Group 1: screen time > 4hr per day \n")
screen_time_desc[, c(2, 4, 5, 6)] # display only mean and sd columns
cat("Group 0: Percentage of females: ", percent_female_0, "% \n")
cat("Group 1: Percentage of females: ", percent_female_1, "% \n")



#----------------- Hypothesis Testing -------------------#
cat("\n\n#----------------- Hypothesis Testing -------------------#\n\n")

cat("\n\n#------------ Any DQI_score differences between screen_time=0,1? --------------#\n\n")
hist(df$DQI_score)
# check normality using Kolmogorov-Smirnov test
ks.test(df$DQI_score, "pnorm", mean(df$DQI_score), sd(df$DQI_score))

# calculate the mean DQI score for each group
mean_0 <- mean(df$DQI_score[df$screen_time == 0])
mean_1 <- mean(df$DQI_score[df$screen_time == 1])

# print the mean DQI score for each group
cat("Mean DQI score for screen_time = 0:", mean_0, "\n")
cat("Mean DQI score for screen_time = 1:", mean_1, "\n\n")

# conduct Mann-Whitney U test and save the results to a local file
wilcox.test(df$DQI_score[df$screen_time == 0], df$DQI_score[df$screen_time == 1], alternative = "two.sided")


cat("\n\n#------------ Any DQI_score differences between sex=0,1? --------------#\n\n")
# calculate the mean DQI score for each group
mean_0 <- mean(df$DQI_score[df$sex_f31_0_0 == 0])
mean_1 <- mean(df$DQI_score[df$sex_f31_0_0 == 1])

# print the mean DQI score for each group
cat("Mean DQI score for sex_f31_0_0 = 0:", mean_0, "\n")
cat("Mean DQI score for sex_f31_0_0 = 1:", mean_1, "\n\n")

# conduct Mann-Whitney U test and save the results to a local file
wilcox.test(df$DQI_score[df$sex_f31_0_0 == 0], df$DQI_score[df$sex_f31_0_0 == 1], alternative = "two.sided")


#----------------- Linear Regression -------------------#
# fit a multivariable linear regression model
model <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + 
              total_met_minites_per_day, data = df)

# display the regression 
cat("\n\n#----------------- Linear Regression -------------------#\n\n")
cat("Analysis Results\n")
summary(model)
sink()



############## Mediation Analysis #############
#Mediate package
library(mediation)
fitM <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0, 
           data = df)
fitY <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + 
             total_met_minites_per_day, data = df)

anova(fitM, fitY)

# Estimation via quasi-Bayesian approximation
contcont <- mediate(fitM, fitY, sims=50, 
                    treat="screen_time", 
                    mediator="total_met_minites_per_day")
summary(contcont)
plot(contcont)

# Estimation via nonparametric bootstrap
contcont.boot <- mediate(fitM, fitY, boot=TRUE, sims=50, 
                         treat="screen_time", 
                         mediator="total_met_minites_per_day")
summary(contcont.boot)

# Allowing treatment-mediator interaction
fitD <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + 
             total_met_minites_per_day + screen_time:total_screen_hours_per_day, 
           data=df)
anova(fitY, fitD)

contcont.int <- mediate(fitM, fitD, sims=50, 
                        treat="screen_time", 
                        mediator="total_met_minites_per_day")
summary(contcont.int)

# Allowing ``moderated mediation'' with respect to sex
b.int <- lm(DQI_score ~ screen_time*sex_f31_0_0 + age_at_recruitment_f21022_0_0,
            data = df)
d.int <- lm(DQI_score ~ screen_time*sex_f31_0_0*total_met_minites_per_day + 
              age_at_recruitment_f21022_0_0,
            data = df)
anova(fitM, b.int)
contcont.female <- mediate(b.int, d.int, sims=50, 
                           treat="screen_time", 
                           mediator="total_met_minites_per_day",
                           covariates = list(sex_f31_0_0 = 0))
contcont.male <- mediate(b.int, d.int, sims=50, 
                         treat="screen_time", 
                         mediator="total_met_minites_per_day",
                         covariates = list(sex_f31_0_0 = 1))
summary(contcont.female)
summary(contcont.male)



# # compare two categories of treatment --- 0 and 1
# model.cat <- mediate(fitM, fitY, 
#                      treat="screen_time", 
#                      mediator="total_met_minites_per_day", 
#                      sims=50, 
#                      control.value = 0, treat.value = 1)
# summary(model.cat)







