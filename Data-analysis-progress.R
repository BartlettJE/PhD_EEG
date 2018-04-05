require(tidyverse)
require(cowplot)

# Create data frame of my progress
progress <- tribble(~Group, ~Number, ~Target,
                    "Non-smokers", 4, 18,
                    #"LITS", 0, 18,
                    #"Daily", 0, 18,
                    "Smokers", 1, 36)

# Create data frame of Kieron's progress
kieron_progress <- tribble(~Group, ~Number, ~Target,
                    "Non-smokers", 5, 10,
                    #"LITS", 0, 10,
                    #"Daily", 0, 10,
                    "Smokers", 2, 20)

# Create list of repeated options - save repetition 
base_progress <- list(geom_bar(stat = "identity", colour = "black"),
                      theme(legend.position = "none"),
                      xlab("Group"),
                      facet_wrap(~Group))

# Plot progress of my study 
progress %>% 
  ggplot(aes(x = Group, y = Number, fill = Group)) + 
  base_progress + 
  expand_limits(y = c(0, 40)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5)) + 
  geom_hline(data = progress, 
             aes(yintercept = Target), 
             linetype = 2)

# Plot progress of Kieron's study
kieron_progress %>% 
  ggplot(aes(x = Group, y = Number, fill = Group)) + 
  base_progress + 
  expand_limits(y = c(0, 20)) + 
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  geom_hline(data = kieron_progress, # Change h line by group 
             aes(yintercept = Target), 
             linetype = 2) # dashed line 
