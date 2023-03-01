# Import packages
library(tidyverse)


############################################################# Load data
path <- fs::path("","Volumes","Peres_Research", "Changing contraceptives R01")

clinical_data <-
  read_csv(paste0(path,"/data/recoded_iud_with_covariates.csv"))

############################################################# Recoding

clinical_data <- clinical_data %>% 
  rename(YearDX = diagyear, PID = pid,
         talc_ever = talcever,
         talc_gen = talcgen) %>% 
  mutate(histology = case_when(
    histology == 1                           ~ 1,
    histology == 2                           ~ 3,
    histology == 3                           ~ 4,
    histology == 4                           ~ 2,
    histology == 5                           ~ 6,
    histology == 6                           ~ 6,
    histology == 7                           ~ 6,
    histology == 8                           ~ 8,
    histology == 9                           ~ 0,
    histology == 88                          ~ NA_real_,
    histology == 99                          ~ 88
  )) %>% 
  mutate(talc_body = case_when(
    talcnongen == 88                         ~ 2,
    TRUE                                     ~ talcnongen
  )) %>% 
  mutate_at(c("PID", "fibroids"), ~ case_when(
    . == 99                                  ~ 8,
    TRUE                                     ~ .
  )) %>% 
  mutate(currently_using = case_when(
    refage == ocstop                          ~ "currently using",
    ocstop %in% c(888, 999)                   ~ NA_character_,
    refage < ocstop                           ~ "using after dx"
  )) %>%
  # mutate(ocfirstage = as.character(ocstart)) %>%
  mutate(oclastage = as.character(ocstop)) %>%
  mutate(ocfirstage = case_when(
    ocstart %in% c(888, 999)                  ~ ".",
    TRUE                                      ~ as.character(ocstart)
  )) %>%
  mutate(oclastage = case_when(
    ocstop %in% c(888, 999)                   ~ ".",
    currently_using == "currently using"      ~ "5",
    TRUE                                      ~ as.character(ocstop)
  )) %>% 
  mutate_at(c("iuddur", "iudfirstage", "iudlastage"), ~ case_when(
    is.na(.)       ~ ".",
    TRUE           ~ as.character(.)
  )) %>% 
  mutate_at(c("endom"), ~ case_when(
    is.na(.)       ~ 8,
    TRUE           ~ .
  )) %>% 
  mutate_at(c("stage"), ~ case_when(
    . == 8         ~ 88,
    TRUE           ~ .
  )) %>% 
  select(-c(talcnongen, currently_using, 
            ocstart, ocstop))


write_csv(clinical_data, paste0(path,"/data/clinical_data_with_covariate.csv"))


# END







