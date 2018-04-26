require(tidyverse)

# Import data for FTCD and WISDM 
raw <- read.csv("Raw_questionnaires/FTCD-WISDM.csv")

# Analyse FTCD data 
ftcd <- raw %>% 
  select(participant = X1..What.is.your.participant.code., # Rename variables to make them easier to manage
         F1 = X2..How.soon.after.you.wake.up.do.you.smoke.your.first.cigarette.,
         F2 = X3..Do.you.find.it.difficult.to.refrain.from.smoking.in.places.where.it.is.forbidden..e.g...in.church..at.the.library..cinema..etc.,
         F3 = X4..Which.cigarette.would.you.hate.most.to.give.up.,
         F4 = X5..How.many.cigarettes.per.day.do.you.smoke.,
         F5 = X6..Do.you.smoke.more.frequently.during.the.first.hours.of.waking.than.during.the.rest.of.the.day.,
         F6 = X7..Do.you.smoke.if.you.are.so.ill.that.you.are.in.bed.for.most.of.the.day.) %>% 
  gather(FTCD_scale, Response, -participant) %>% # Convert to long to make recoding easier 
  mutate(Score = substr(Response, 0, 1)) # Code the response to just keep the value 

# Convert from string to numeric
ftcd$Score <- as.numeric(ftcd$Score) 

# Convert back to wide format and calculate total score
ftcd <- ftcd %>% 
  select(-Response) %>% 
  spread(key = FTCD_scale, value = Score) %>% 
  mutate(total_score = F1 + F2 + F3 + F4 + F5 + F6)
