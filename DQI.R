setwd("C:/Users/Xinyi/OneDrive - Yale University/Yale/BIS 687/BIS687-Capstone_Project_Team5")

# import diet data
df <- read.csv("diet.csv")
df <- df[,-1]
diet <- df[,c(1,3,4:35)]

library(dplyr)
# factor to text
# diet <- as.data.frame(apply(diet, 2, as.character))

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


# column `variation_in_diet_f1548_0_0` not used to calculate MDS,
# just put here for reference.

# output df: index(11270 patients)
write.csv(index, "MDS_index.csv")




# set variable weight
names <- c("cooked_vegetable_intake_f1289_0_0",
           "salad_raw_vegetable_intake_f1299_0_0",
           "fresh_fruit_intake_f1309_0_0",
           "dried_fruit_intake_f1319_0_0",        
           "oily_fish_intake_f1329_0_0",
           "nonoily_fish_intake_f1339_0_0",      
           "processed_meat_intake_f1349_0_0",
           "poultry_intake_f1359_0_0",     
           "beef_intake_f1369_0_0",
           "lambmutton_intake_f1379_0_0",      
           "pork_intake_f1389_0_0",
           "cheese_intake_f1408_0_0",
           "milk_type_used_f1418_0_0",
           "bread_intake_f1438_0_0",
           "cereal_intake_f1458_0_0",
           "tea_intake_f1488_0_0",
           "water_intake_f1528_0_0")
num_names <- length(names)
weights <- c(rep(1, num_names))

# calculate the weighted sum of the columns
index[is.na(index)] <- 0
weighted_mds <- apply(index[, names], 1, function(x) weighted.mean(x, w = weights) * num_names)

# add the weighted sum as a new column to the data frame
index$weighted_mds <- weighted_mds


# calculate variaty by food groups
# 5 food groups: meat/poultry/fish, dairy, grains, fruits, and vegetables
# Each food group awarded 0 or 3 pts. 3 points awarded if at least 1 item from that group was consumed
# score range: 0-15

#Variables for 5 food groups:
#Meat/Poultry/Fish/Egg: oily_fish_intake_f1329_0_0, nonoily_fish_intake_f1339_0_0, processed_meat_intake_f1349_0_0, 
#                       poultry_intake_f1359_0_0, beef_intake_f1369_0_0, lambmutton_intake_f1379_0_0, pork_intake_f1389_0_0
#Dairy/Beans: cheese_intake_f1408_0_0, milk_type_used_f1418_0_0
#Grains: bread_intake_f1438_0_0, cereal_intake_f1458_0_0
#Fruits: fresh_fruit_intake_f1309_0_0, dried_fruit_intake_f1319_0_0
#Vegetables: cooked_vegetable_intake_f1289_0_0, salad_raw_vegetable_intake_f1299_0_0

variety <- diet
variety[is.na(variety)] <- -1

variety$meat_poultry_fish_points <- ifelse(variety$oily_fish_intake_f1329_0_0 > 1 | 
                                             variety$nonoily_fish_intake_f1339_0_0 > 1 |
                                             variety$processed_meat_intake_f1349_0_0 > 1 |
                                             variety$poultry_intake_f1359_0_0 > 1 |
                                             variety$beef_intake_f1369_0_0 > 1 |
                                             variety$lambmutton_intake_f1379_0_0 > 1 |
                                             variety$pork_intake_f1389_0_0 > 1, 3, 0)

variety$dairy_points <- ifelse(variety$cheese_intake_f1408_0_0 > 1 | 
                                 variety$milk_type_used_f1418_0_0 >= 1, 3, 0)

variety$grains_points <- ifelse(variety$bread_intake_f1438_0_0 >= 1 | 
                                  variety$cereal_intake_f1458_0_0 >= 1, 3, 0)

variety$fruits_points <- ifelse(variety$fresh_fruit_intake_f1309_0_0 >= 1 | 
                                  variety$dried_fruit_intake_f1319_0_0 >= 1, 3, 0)

variety$vegetables_points <- ifelse(variety$cooked_vegetable_intake_f1289_0_0 >= 1 | 
                                      variety$salad_raw_vegetable_intake_f1299_0_0 >= 1, 3, 0)

variety$food_source_points <- rowSums(variety[,c('meat_poultry_fish_points', 'dairy_points', 'grains_points', 'fruits_points', 'vegetables_points')])

# calculate variety by protein sources
# 4 sources: meat, poultry, fish, dairy
# 3 or more sources consumed: 5 pts, 2 sources consumed: 3 pts, 1 source consumed: 1 pts, 0 sources consumed: 0 pts
# score range: 0-5


# Create a new column for meat/poultry/fish/egg points
variety$meat_intake <- ifelse(variety$processed_meat_intake_f1349_0_0 > 1 |
                                variety$beef_intake_f1369_0_0 > 1 |
                                variety$lambmutton_intake_f1379_0_0 > 1 |
                                variety$pork_intake_f1389_0_0 > 1, 1, 0)

variety$poultry_intake <- ifelse(variety$poultry_intake_f1359_0_0 > 1, 1, 0)

variety$fish_intake <- ifelse(variety$oily_fish_intake_f1329_0_0 > 1 | 
                                variety$nonoily_fish_intake_f1339_0_0 > 1 , 1, 0)

variety$dairy_intake <- ifelse(variety$cheese_intake_f1408_0_0 > 1 | 
                                 variety$milk_type_used_f1418_0_0 >= 1, 1, 0)

protein_names <- c('meat_intake', 'poultry_intake', 'fish_intake', 'dairy_intake')

variety$protein_source_points <- ifelse(rowSums(variety[, protein_names]) >= 3, 5,
                                        ifelse(rowSums(variety[, protein_names]) >= 2, 3,
                                               ifelse(rowSums(variety[, protein_names]) >= 1, 1, 0)))

# merge mds with variety
joined_df <- merge(index, variety, by = "eid")
joined_df$DQI_score <- joined_df$weighted_mds * 80 / max(joined_df$weighted_mds) + joined_df$food_source_points * 15 / max(joined_df$food_source_points) + joined_df$protein_source_points * 5 / max(joined_df$protein_source_points)
joined_df <- joined_df[,c("eid", "DQI_score")]


write.csv(joined_df, "DQI_score.csv")