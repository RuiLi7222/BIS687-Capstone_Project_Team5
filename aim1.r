setwd("C:/Users/Xinyi/OneDrive - Yale University/Yale/BIS 687/BIS687-Capstone_Project_Team5")

#----------------- Descriptive Statistics -------------------#
# import diet data
physical <- read.csv("physical.csv")
physical <- physical[, -1]
ukbiobank <- readRDS("ukbiobank.rds")
DQI_score <- read.csv("DQI_score.csv")
DQI_score <- DQI_score[, -1]
age <- ukbiobank[, c("eid", "age_at_recruitment_f21022_0_0")]

df <- merge(physical, age, by = "eid")
df <- merge(df, DQI_score, by = "eid")
df$total_screen_hours_per_day <- df$time_spent_watching_television_tv_f1070_0_0 + df$time_spent_using_computer_f1080_0_0
library(dplyr)
df <- df %>% mutate(across(everything(), ~ ifelse(. < 0, 0, .)))
# write.csv(df, "df.csv")
df <- na.omit(df)
# names(df)

library(psych)
# subset the data by screen_time
screen0 <- subset(df, screen_time == 0)
screen1 <- subset(df, screen_time == 1)

# calculate descriptive statistics for screen_time=0
screen_time_0_desc <- describe(screen_time_0[c("age_at_recruitment_f21022_0_0",
                                               "total_met_minites_per_day",
                                               "DQI_score",
                                               "total_screen_hours_per_day")])

# calculate descriptive statistics for screen_time=1
screen_time_1_desc <- describe(screen_time_1[c("age_at_recruitment_f21022_0_0",
                                               "total_met_minites_per_day",
                                               "DQI_score",
                                               "total_screen_hours_per_day")])


# calculate percentage of females in each group
percent_female_0 <- round(prop.table(table(screen0$sex_f31_0_0))["Female"] * 100, 2)
percent_female_1 <- round(prop.table(table(screen1$sex_f31_0_0))["Female"] * 100, 2)




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
sink("desc_stats.txt")
cat("Group 0: screen time < 4hr per day \n")
cat("Group 1: screen time > 4hr per day \n")
screen_time_desc[, c(2, 4, 5, 6)] # display only mean and sd columns
cat("Group 0: Percentage of females: ", percent_female_0, "% \n")
cat("Group 1: Percentage of females: ", percent_female_1, "% \n")
sink()



#----------------- Hypothesis Testing -------------------#
# calculate the mean DQI score for each group
mean_0 <- mean(df$DQI_score[df$screen_time == 0])
mean_1 <- mean(df$DQI_score[df$screen_time == 1])

# print the mean DQI score for each group
print(paste("Mean DQI score for screen_time = 0:", mean_0))
print(paste("Mean DQI score for screen_time = 1:", mean_1))

# conduct ANOVA test and save the results to a local file
sink("ANOVA_results.txt")
fit <- aov(DQI_score ~ screen_time, data = df)
summary(fit)
sink()



#----------------- Linear Regression -------------------#
# fit a multivariable linear regression model
# model <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + 
#               ipaq_activity_group_f22032_0_0 + total_met_minites_per_day, data = df)
model <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0 + total_met_minites_per_day, data = df)

# display the regression summary
sink("LR_results.txt")
cat("Analysis Results\n")
summary(model)
sink()

# # use stratification on ipaq_activity_group_f22032_0_0
# # ---> results hard to explain. abandoned
# group1 <- df[df$ipaq_activity_group_f22032_0_0 == "low", ]
# group2 <- df[df$ipaq_activity_group_f22032_0_0 == "moderate", ]
# group3 <- df[df$ipaq_activity_group_f22032_0_0 == "high", ]
# model1 <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0, data = group1)
# model2 <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0, data = group2)
# model3 <- lm(DQI_score ~ screen_time + sex_f31_0_0 + age_at_recruitment_f21022_0_0, data = group3)
# # display the regression summary
# sink("LR_strat_by_ipaq_activity_group_results.txt")
# cat("Analysis Results\n")
# cat("Result for ipaq_activity_group_f22032_0_0: low\n")
# summary(model1)
# cat("Result for ipaq_activity_group_f22032_0_0: moderate\n")
# summary(model2)
# cat("Result for ipaq_activity_group_f22032_0_0: high\n")
# summary(model3)
# sink()


