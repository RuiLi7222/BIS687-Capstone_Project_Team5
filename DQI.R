# import diet data
df <- read.csv("/Users/jgloriouswu/Desktop/BIS687/final-project/diet.csv")
diet <- ukbiobank[,c("age_at_recruitment_f21022_0_0","sex_f31_0_0",names(df)[3:34])]

# select older patients (age>65)
diet <- subset(diet, age_at_recruitment_f21022_0_0>65)

# library(dplyr)
# factor to text
diet <- as.data.frame(apply(diet, 2, as.character))

# remove useless columns
# remove several last data (other text + pilot data)
diet <- diet[,-c(28:34)]

# text to numeric (character) based on frequency
unique(diet$oily_fish_intake_f1329_0_0)
for(j in c(7:14)){
  for(i in 1:nrow(diet)){
    if(!is.na(diet[i,j])){
      if(diet[i,j]=="Do not know"|diet[i,j]=="Prefer not to answer"){
        diet[i,j] <- NA
      }else if (diet[i,j]=="Never"){
        diet[i,j] <- 0
      }else if (diet[i,j]=="Less than once a week"){
        diet[i,j]<- 1
      }else if (diet[i,j]=="Once a week"){
        diet[i,j] <- 2
      }else if (diet[i,j]=="2-4 times a week"){
        diet[i,j] <- 3
      }else if (diet[i,j]=="5-6 times a week"){
        diet[i,j] <- 4
      }else{
        diet[i,j] <- 5
      }
    }
  }
}

# milk data
unique(diet$milk_type_used_f1418_0_0)
for (i in 1:nrow(diet)){
  if(!is.na(diet[i,15])){
    if (diet[i,15]=="Do not know"|diet[i,15]=="Prefer not to answer"){
      diet[i,15] <- NA
    }else if(diet[i,15]=="Never/rarely have milk"){
      diet[i,15] <- 0
    }else{
      diet[i,15] <- 1
    }
  }
}

# remove useless columns (food type..)
diet <- select(diet, -c("spread_type_f1428_0_0",
                        "bread_type_f1448_0_0",
                        "cereal_type_f1468_0_0",
                        "coffee_type_f1508_0_0",
                        "hot_drink_temperature_f1518_0_0",
                        "major_dietary_changes_in_the_last_5_years_f1538_0_0",
                        "salt_added_to_food_f1478_0_0"))

# character to numeric
diet[,3:19] <- lapply(diet[,3:19], as.numeric)

# negative values to NA
for (j in 3:19){
  for (i in 1:nrow(diet)){
    diet[i,j] <- ifelse(diet[i,j]<0, NA, diet[i,j])
    }
}

# Count NA for each column
miss <- diet %>%
  summarise_all(~sum(is.na(.)))

miss_c <- as.numeric(as.vector(miss[1,]))
names(miss_c) <- names(miss)

# Drop columns missing>75%
diet.col <- miss_c[miss_c<nrow(diet)*0.75]
drop75_diet <- diet[,c(names(diet.col))]
length(diet.col) #20 (keep all the columns now)

# Columns we dropped
drop.name <- miss_c[miss_c>=nrow(diet)*0.75]
drop.name #nothing dropped


# calculate sex-specific median 
median_sex <- diet[,-c(1,20)] %>% 
  group_by(sex_f31_0_0) %>% 
  summarise_all(median,na.rm=TRUE)

# create index for each diet column
index <- diet
for (i in 1:nrow(index)){
  for (j in 3:19){
    if (!is.na(index[i,j])){
      if (index$sex_f31_0_0[i]=="Female"){
        index[i,j] <- ifelse(index[i,j]>=median_sex[1,j-1], 1, 0)
      }else{
        index[i,j] <- ifelse(index[i,j]>=median_sex[2,j-1], 1, 0)
      }
    }
  }
}

# create column MDS
index$MDS <- rowSums(index[,3:19], na.rm = TRUE)

# output df: index(11270 patients)
write.csv(index, "/Users/jgloriouswu/Desktop/BIS687/final-project/MDS.csv")

# column `variation_in_diet_f1548_0_0` not used to calculate MDS,
# just put here for reference.





