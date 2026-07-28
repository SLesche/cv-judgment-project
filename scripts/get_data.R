library(tidyverse)
cvs_list <- list.files("anonym_cvs/kontrolliert/")

subject_ids <- str_extract(cvs_list, "\\d+")

iq_data <- read.csv("../promotion_mental_speed/decision-or-response-analysis/behav_data/cleaned_data/ist_clean.csv") %>% 
  filter(Subject %in% subject_ids) %>% 
  select(-X)

big5_data <- read.csv("../promotion_mental_speed/decision-or-response-analysis/behav_data/cleaned_data/demo_clean.csv")  %>% 
  filter(Subject %in% subject_ids) %>% 
  select(-X)

full_data <- iq_data %>% 
  left_join(big5_data, by = "Subject")

write.csv(full_data, "./data/cv_scores.csv")
