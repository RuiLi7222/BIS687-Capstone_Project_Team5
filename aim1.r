# setwd("C:/Users/Xinyi/OneDrive - Yale University/Yale/BIS 687/BIS687-Capstone_Project_Team5")
library(dplyr)


#----------------- Descriptive Statistics -------------------#
# import realted covariates
physical <- read.csv("physical.csv")
# remove useless columns X, redundant alcohol,ethic, leisure social activity, types of transport
physical <- physical[, -c(1,13,14,18,20)]
# change data type
physical$sex_f31_0_0 <- factor(physical$sex_f31_0_0,
                               levels = c("Female", "Male"),
                               labels = c(0,1))

physical$ipaq_activity_group_f22032_0_0 <- ifelse(physical$ipaq_activity_group_f22032_0_0=="low",1,
                                           ifelse(physical$ipaq_activity_group_f22032_0_0=="moderate",2,3))

physical$alcohol_intake_frequency_f1558_0_0 <- ifelse(physical$alcohol_intake_frequency_f1558_0_0=="Never",0,
                                               ifelse(physical$alcohol_intake_frequency_f1558_0_0=="Special occasions only",1,
                                               ifelse(physical$alcohol_intake_frequency_f1558_0_0=="One to three times a month",2,
                                               ifelse(physical$alcohol_intake_frequency_f1558_0_0=="Once or twice a week",3,
                                               ifelse(physical$alcohol_intake_frequency_f1558_0_0=="Three or four times a week",4,
                                               ifelse(physical$alcohol_intake_frequency_f1558_0_0=="Daily or almost daily",5,6))))))

physical$frequency_of_friendfamily_visits_f1031_0_0 <- ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="Never or almost never"|
                                                                physical$frequency_of_friendfamily_visits_f1031_0_0=="No friends/family outside household",0,
                                                       ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="Once every few months",1,
                                                       ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="About once a month",2,
                                                       ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="About once a week",3,
                                                       ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="2-4 times a week",4,
                                                       ifelse(physical$frequency_of_friendfamily_visits_f1031_0_0=="Almost daily",5,6))))))

physical$smoking_status_f20116_0_0 <- ifelse(physical$smoking_status_f20116_0_0=="Never",0,
                                      ifelse(physical$smoking_status_f20116_0_0=="Previous",1,
                                      ifelse(physical$smoking_status_f20116_0_0=="Current",2,NA)))
physical$smoking_status_f20116_0_0 <- as.factor(physical$smoking_status_f20116_0_0)

# negatives in sleep duration
physical <- physical %>% 
  mutate_at("sleep_duration_f1160_0_0", ~ifelse(.<0, NA, .))

# negatives in screen time
physical <- physical %>% 
  mutate_at(c("time_spent_watching_television_tv_f1070_0_0", 
              "time_spent_using_computer_f1080_0_0"), 
            ~ifelse(.<0, 0, .))
physical$screen_time <- physical$time_spent_using_computer_f1080_0_0 + physical$time_spent_watching_television_tv_f1070_0_0
physical$screen_time <- ifelse(physical$screen_time>=4, 1, 0)

# import diet data
ukbiobank <- readRDS("ukbiobank.rds")
DQI_score <- read.csv("DQI_score.csv",skip = 1)
DQI_score <- DQI_score[1:11270,-1]
age <- ukbiobank[, c("eid", "age_at_recruitment_f21022_0_0")]

df <- merge(physical, age, by = "eid")
df <- merge(df, DQI_score, by = "eid")
df$total_screen_hours_per_day <- df$time_spent_watching_television_tv_f1070_0_0 + df$time_spent_using_computer_f1080_0_0
# df <- df %>% mutate(across(everything(), ~ ifelse(. < 0, 0, .)))
df$DQI_score <- as.numeric(df$DQI_score)


df <- na.omit(df)

write.csv(df, "df.csv")

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
                                         "total_screen_hours_per_day",
                                         "sleep_duration_f1160_0_0",
                                         "alcohol_intake_frequency_f1558_0_0",
                                         "standing_height_f50_0_0",
                                         "weight_f21002_0_0",
                                         "frequency_of_friendfamily_visits_f1031_0_0")])

# calculate descriptive statistics for screen_time=1
screen_time_1_desc <- describe(screen1[c("age_at_recruitment_f21022_0_0",
                                         "total_met_minites_per_day",
                                         "DQI_score",
                                         "total_screen_hours_per_day",
                                         "sleep_duration_f1160_0_0",
                                         "alcohol_intake_frequency_f1558_0_0",
                                         "standing_height_f50_0_0",
                                         "weight_f21002_0_0",
                                         "frequency_of_friendfamily_visits_f1031_0_0")])


# calculate percentage of females and smoke status in each group
percent_female_0 <- round(prop.table(table(screen0$sex_f31_0_0))["0"] * 100, 2)
percent_female_1 <- round(prop.table(table(screen1$sex_f31_0_0))["0"] * 100, 2)
percent_never_smoke_0 <- round(prop.table(table(screen0$smoking_status_f20116_0_0))["0"] * 100, 2)
percent_never_smoke_1 <- round(prop.table(table(screen1$smoking_status_f20116_0_0))["0"] * 100, 2)
percent_previous_smoke_0 <- round(prop.table(table(screen0$smoking_status_f20116_0_0))["1"] * 100, 2)
percent_previous_smoke_1 <- round(prop.table(table(screen1$smoking_status_f20116_0_0))["1"] * 100, 2)
percent_current_smoke_0 <- round(prop.table(table(screen0$smoking_status_f20116_0_0))["2"] * 100, 2)
percent_current_smoke_1 <- round(prop.table(table(screen1$smoking_status_f20116_0_0))["2"] * 100, 2)



# calculate descriptive statistics for screen_time=1
screen_time_desc <- describeBy(df[c("age_at_recruitment_f21022_0_0",
                                    "total_met_minites_per_day",
                                    "DQI_score",
                                    "total_screen_hours_per_day",
                                    "sleep_duration_f1160_0_0",
                                    "alcohol_intake_frequency_f1558_0_0",
                                    "standing_height_f50_0_0",
                                    "weight_f21002_0_0",
                                    "frequency_of_friendfamily_visits_f1031_0_0")], 
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
cat("Group 0: Percentage of never smoke: ", percent_never_smoke_0, "% \n")
cat("Group 1: Percentage of never smoke: ", percent_never_smoke_1, "% \n")
cat("Group 0: Percentage of previous smoke: ", percent_previous_smoke_0, "% \n")
cat("Group 1: Percentage of previous smoke: ", percent_previous_smoke_1, "% \n")
cat("Group 0: Percentage of current smoke: ", percent_current_smoke_0, "% \n")
cat("Group 1: Percentage of current smoke: ", percent_current_smoke_1, "% \n")



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
mean_0 <- mean(df$DQI_score[df$sex_f31_0_0 == 0])
mean_1 <- mean(df$DQI_score[df$sex_f31_0_0 == 1])
cat("Mean DQI score for sex_f31_0_0 = 0:", mean_0, "\n")
cat("Mean DQI score for sex_f31_0_0 = 1:", mean_1, "\n\n")
wilcox.test(df$DQI_score[df$sex_f31_0_0 == 0], df$DQI_score[df$sex_f31_0_0 == 1], alternative = "two.sided")

cat("\n\n#------------ Any DQI_score differences between smoking status=0,1,2? --------------#\n\n")
mean_0 <- mean(df$DQI_score[df$smoking_status_f20116_0_0 == 0])
mean_1 <- mean(df$DQI_score[df$smoking_status_f20116_0_0 == 1])
mean_1 <- mean(df$DQI_score[df$smoking_status_f20116_0_0 == 2])
cat("Mean DQI score for smoking_status_f20116_0_0 = 0:", mean_0, "\n")
cat("Mean DQI score for smoking_status_f20116_0_0 = 1:", mean_1, "\n")
cat("Mean DQI score for smoking_status_f20116_0_0 = 1:", mean_2, "\n\n")
wilcox.test(df$DQI_score[df$smoking_status_f20116_0_0 == 0], df$DQI_score[df$smoking_status_f20116_0_0 == 1], alternative = "two.sided")


#----------------- Linear Regression -------------------#
cat("\n\n#----------------- Linear Regression -------------------#\n\n")
# fit a multivariable linear regression model
model <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + 
              total_met_minites_per_day + sleep_duration_f1160_0_0 + alcohol_intake_frequency_f1558_0_0 +
              standing_height_f50_0_0 + weight_f21002_0_0 + frequency_of_friendfamily_visits_f1031_0_0 + 
              smoking_status_f20116_0_0, data = df)

# display the regression 
cat("Analysis Results\n")
summary(model)
sink()



############## Mediation Analysis #############
# choose best predictors
dt <- df[,-c(1,2,4:9,20)]
y1 <- lm(DQI_score~.,data=dt)
summary(y1)
library(leaps)
Best_Subset <-
  regsubsets(DQI_score~.,
             data=dt,
             nbest = 1,      
             nvmax = NULL,   
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary_best_subset <- summary(Best_Subset)
# as.data.frame(summary_best_subset$outmat)
which.max(summary_best_subset$adjr2)
summary_best_subset$which[10,]
# include all the predictors in the dataset


library(mediation)
fitM <- lm(total_met_minites_per_day~.-DQI_score, 
           data = dt)
fitY <- lm(DQI_score ~ ., data = dt)

# Estimation via quasi-Bayesian approximation
contcont <- mediation::mediate(fitM, fitY, data = dt,sims=50, 
                    treat="screen_time", 
                    mediator="total_met_minites_per_day")
summary(contcont)
plot(contcont)

# Estimation via nonparametric bootstrap
contcont.boot <- mediation::mediate(fitM, fitY, boot=TRUE, sims=50, 
                         treat="screen_time", 
                         mediator="total_met_minites_per_day")
summary(contcont.boot)

# Allowing treatment-mediator interaction
fitD <- lm(DQI_score~.+screen_time:total_met_minites_per_day, 
           data=dt)
summary(fitD)

contcont.int <- mediate(fitM, fitD, sims=50,
                        treat="screen_time",
                        mediator="total_met_minites_per_day")
summary(contcont.int)

# Allowing "moderated mediation" with respect to sex
b.int <- lm(total_met_minites_per_day ~ screen_time*sex_f31_0_0+.-DQI_score,
            data = dt)
d.int <- lm(DQI_score ~ screen_time*sex_f31_0_0*total_met_minites_per_day+.,
            data = dt)

contcont.female <- mediation::mediate(b.int, d.int, sims=50, 
                           treat="screen_time", 
                           mediator="total_met_minites_per_day",
                           covariates = list(sex_f31_0_0 = 0))

contcont.male <- mediation::mediate(b.int, d.int, sims=50, 
                         treat="screen_time", 
                         mediator="total_met_minites_per_day",
                         covariates = list(sex_f31_0_0 = 1))

summary(contcont.female)
summary(contcont.male)


# Allowing "moderated mediation" with respect to smoking status
e.int <- lm(total_met_minites_per_day ~ screen_time*smoking_status_f20116_0_0+.-DQI_score,
            data = dt)
f.int <- lm(DQI_score ~ screen_time*smoking_status_f20116_0_0*total_met_minites_per_day+.,
            data = dt)

contcont.nosmoking <- mediation::mediate(e.int, f.int, sims=50,
                                         treat="screen_time",
                                         mediator="total_met_minites_per_day",
                                         covariates = list(smoking_status_f20116_0_0 = 0))

contcont.previoussmoking <- mediation::mediate(e.int, f.int, sims=50,
                                               treat="screen_time",
                                               mediator="total_met_minites_per_day",
                                               covariates = list(smoking_status_f20116_0_0 = 1))

contcont.currentsmoking <- mediation::mediate(e.int, f.int, sims=50,
                                              treat="screen_time", 
                                              mediator="total_met_minites_per_day",
                                              covariates = list(smoking_status_f20116_0_0 = 2))

summary(contcont.nosmoking)
summary(contcont.previoussmoking)
summary(contcont.currentsmoking)








