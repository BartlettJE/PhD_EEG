require(tidyverse)

# Import data for non-smoker demographics 
raw_quest <- read.csv("Raw_questionnaires/non-smoker-demographics.csv")

# Basic demographic information
demo <- raw_quest %>% 
  select(Participant = X1..What.is.your.participant.code.,
         Age = X2..What.is.your.age.,
         Gender = X3..I.identify.my.gender.as....)

# Age 
demo %>% 
  summarise(mean_age = round(mean(Age), 2),
            SD_age = round(sd(Age), 2))

# Gender (count)
demo %>% 
  count(Gender)

# Select only PANAS and recode to make the variable names more manageable 
panas <- raw_quest %>% 
  select(Participant = X1..What.is.your.participant.code., 
         P1 = X6.1..Interested,
         P2 = X6.2..Distressed,
         P3 = X6.3..Excited,
         P4 = X6.4..Upset,
         P5 = X6.5..Strong,
         P6 = X6.6..Guilty,
         P7 = X6.7..Scared,
         P8 = X6.8..Hostile,
         P9 = X6.9..Enthusiastic,
         P10 = X6.10..Proud,
         P11 = X6.11..Irritable,
         P12 = X6.12..Alert,
         P13 = X6.13..Ashamed,
         P14 = X6.14..Inspired,
         P15 = X6.15..Nervous,
         P16 = X6.16..Determined,
         P17 = X6.17..Attentive,
         P18 = X6.18..Jittery,
         P19 = X6.19..Active,
         P20 = X6.20..Afraid) %>% 
  gather(PANAS_scale, score, -Participant) #Gather into long format to make recoding easier 

# Recode the responses to a numerical value 
panas$score <- panas$score %>% 
  recode("1. Very slightly or not at all" = 1,
         "2. A little" = 2,
         "3. Moderately" = 3,
         "4. Quite a bit" = 4,
         "5. Extremely" = 5)

# Convert back to wide format 
panas <- panas %>% 
  spread(key = PANAS_scale, value = score) %>% 
  mutate(Pos_Aff = (P1 + P3 + P5 + P9 + P10 + P12 + P14 + P16 + P17 + P19) / 10, # Calculate mean positive affect 
         Neg_Aff = (P2 + P4 + P6 + P7 + P8 + P11 + P13 + P15 + P18 + P20) / 10) # Calculate mean negative affect
