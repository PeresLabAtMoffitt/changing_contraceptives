# Import packages
library(haven)
library(tidyverse)


############################################################# Load data
path <- fs::path("","Volumes","Peres_Research", "Changing contraceptives R01")

clinical_data_short <-
  read_sas(paste0(path,"/data/iud_short.sas7bdat"))
clinical_data_long <-
  read_csv(paste0(path,"/data/iud_long.csv"))
# The data is modified directly in excel for patients who wrote "Dalk" as other contraceptives
clinical_data_long <- clinical_data_long %>% 
  mutate(QE2A_01 = case_when(
    QE2A_01 == 13 & 
      QE2OA == "Dalk"          ~ 3,
    TRUE                       ~ QE2A_01
  )) %>% 
  mutate(QE2B_02 = case_when(
    QE2B_02 == 13 & 
      str_detect(QE2OB, "tied")          ~ 10,
    TRUE                                   ~ QE2B_02
  ))


############################################################# Merge
clinical_data_short <- clinical_data_short %>% 
  select(suid)

clinical_data_long <- clinical_data_long %>% 
  bind_rows(clinical_data_short)
############################################################# Recoding
number_of_agecolumn = length(colnames(clinical_data_long %>% select(starts_with("QE3"))))
                             
clinical_data_long1 <- clinical_data_long %>% 
  # Rename age variable for easiest manipulation
  rename_with(~paste0("QE3", LETTERS[1:number_of_agecolumn], "age") , starts_with("QE3")) %>% 
  # Remove all the variable that only have NA
  purrr::keep(~!all(is.na(.))) %>% 
  # Convert years to months aka that's why we rename previous Y variables to age
  mutate(across(c(ends_with("Y") & where(is.numeric)), 
                ~ . * 12
  ))

# Add years var (newly converted in months) to months var
colvar <- LETTERS[1:number_of_agecolumn]

clinical_data_long2 <- clinical_data_long1
for (i in colvar){
  print(i)
  
  clinical_data_long2 <- clinical_data_long2 %>%
    mutate_at(c(paste0("QE4", i, "M")), 
              ~ rowSums(select(clinical_data_long2,
                               paste0("QE4", i, "M"):paste0("QE4", i, "Y")), 
                        na.rm = TRUE)) %>% 
    select(-c(paste0("QE4", i, "Y")))
  
}

clinical_data_long3 <- clinical_data_long2 %>%
  # IUD
  # everiud
  # mutate(across(c(starts_with("QE2")), ############# Noooooooo
  #               ~ case_when(
  #                 . == 3                                ~ 1,
  #                 is.na(QE2A_01)                        ~ 9,
  #                 TRUE                                  ~ 0
  #               ))) %>%
  rowwise() %>%
  mutate(everiud = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 3)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 3)   ~ 0,
    QE1 == 2                                                     ~ 0,
    is.na(QE1)                                                   ~ 9
    )) %>% 
  mutate(iudfirstage = case_when(
    any(c_across(starts_with("QE2A_")) == 3)             ~ as.character(QE3Aage),
    any(c_across(starts_with("QE2B_")) == 3)             ~ as.character(QE3Bage),
    any(c_across(starts_with("QE2C_")) == 3)             ~ as.character(QE3Cage),
    any(c_across(starts_with("QE2D_")) == 3)             ~ as.character(QE3Dage),
    any(c_across(starts_with("QE2E_")) == 3)             ~ as.character(QE3Eage),
    any(c_across(starts_with("QE2F_")) == 3)             ~ as.character(QE3Fage),
    any(c_across(starts_with("QE2G_")) == 3)             ~ as.character(QE3Gage),
    any(c_across(starts_with("QE2H_")) == 3)             ~ as.character(QE3Hage),
    any(c_across(starts_with("QE2I_")) == 3)             ~ as.character(QE3Iage),
    TRUE                                                 ~ "."
  )) %>% 
  ungroup() %>% 
  mutate(iudlastage = ".") %>% 
  mutate(everhormiud = 9) %>% 
  mutate(typeiud = 0) %>% 
  mutate(dalkon = 9) %>% 
  mutate(lippes = 9) %>% 
  mutate(copper = 9) %>% 
  mutate(coil = 9) %>% 
  mutate(inertiud = 9) %>% 
  mutate(everdiaphragm = 9) %>% 
  rowwise() %>%
  mutate(evercondoms = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 5)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 5)   ~ 0,
    QE1 == 2                                                     ~ 0,
    is.na(QE1)                                                   ~ 9
  )) %>% 
  mutate(everimplant = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 8)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 8)   ~ 0,
    QE1 == 2                                                     ~ 0,
    is.na(QE1)                                                   ~ 9
  )) %>% 
  mutate(evershot = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 7)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 7)   ~ 0,
    QE1 == 2                                                     ~ 0,
    is.na(QE1)                                                   ~ 9
  )) %>% 
  mutate(everothbar = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 13)  ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 13)  ~ 0,
    QE1 == 2                                                     ~ 0,
    is.na(QE1)                                                   ~ 9
  )) %>% 
  mutate(patch = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 2)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 2)   ~ 2,
    QE1 == 2                                                     ~ 2,
    is.na(QE1)                                                   ~ 8
  )) %>% 
  mutate(ring = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 9)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 9)   ~ 2,
    QE1 == 2                                                     ~ 2,
    is.na(QE1)                                                   ~ 8
  )) %>% 
  mutate(depo = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 7)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 7)   ~ 2,
    QE1 == 2                                                     ~ 2,
    is.na(QE1)                                                   ~ 8
  )) %>% 
  mutate(norplant = case_when(
    any(c_across(starts_with("QE2") & where(is.numeric)) == 8)   ~ 1,
    any(c_across(starts_with("QE2") & where(is.numeric)) != 8)   ~ 2,
    QE1 == 2                                                     ~ 2,
    is.na(QE1)                                                   ~ 8
  )) %>% 
  ungroup()

clinical_data_long4 <- clinical_data_long3 %>% 
  rowwise() %>%
  mutate(iuddurA = case_when(
    any(c_across(starts_with("QE2A_")) == 3)             ~ QE4AM
  )) %>% 
  mutate(iuddurB = case_when(
    any(c_across(starts_with("QE2B_")) == 3)             ~ QE4BM
  )) %>% 
  mutate(iuddurC = case_when(
    any(c_across(starts_with("QE2C_")) == 3)             ~ QE4CM
  )) %>% 
  mutate(iuddurD = case_when(
    any(c_across(starts_with("QE2D_")) == 3)             ~ QE4DM
  )) %>% 
  mutate(iuddurE = case_when(
    any(c_across(starts_with("QE2E_")) == 3)             ~ QE4EM
  )) %>% 
  mutate(iuddurF = case_when(
    any(c_across(starts_with("QE2F_")) == 3)             ~ QE4FM
  )) %>% 
  mutate(iuddurG = case_when(
    any(c_across(starts_with("QE2G_")) == 3)             ~ QE4GM
  )) %>% 
  mutate(iuddurH = case_when(
    any(c_across(starts_with("QE2H_")) == 3)             ~ QE4HM
  )) %>% 
  mutate(iuddurI = case_when(
    any(c_across(starts_with("QE2I_")) == 3)             ~ QE4IM
  )) %>% 
  ungroup() %>% 
  # Have to do in 2 step as rowsums add NAs = 0
  mutate(iudtemp = rowSums(select(.,iuddurA:iuddurI), na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(iuddur = case_when(
    any(!is.na(c_across(starts_with("iudd"))))             ~ as.character(iudtemp),
    TRUE                                                          ~ "."
  )) %>% 
  ungroup() %>% 
  select(-c(iuddurA:iuddurI, iudtemp))
  
  

# clinical_data_long2 <- clinical_data_long1
# for (i in colvar){
#   print(i)
#   
#   clinical_data_long2 <- clinical_data_long2 %>%
#     mutate_at(c(paste0("QE4", i, "M")), 
#               ~ rowSums(select(clinical_data_long2,
#                                paste0("QE4", i, "M"):paste0("QE4", i, "Y")), 
#                         na.rm = TRUE)) %>% 
#     select(-c(paste0("QE4", i, "Y")))
#   
# }
# 
# 
# clinical_data_long4 <- clinical_data_long3
# for (i in colvar){
#   print(i)
#   
#   clinical_data_long4[paste0("iuddur", i)] <- NA
#   clinical_data_long4 <- clinical_data_long4 %>%
#     mutate_at(c(paste0("iuddur", i)), ~ case_when(
#       any(c_across(starts_with(paste0("QE2", i)
#       )) == 3)             ~ 1#clinical_data_long4[paste0("QE4", i, "M")]
#     ))
#   
# }


clinical_data_long5 <- clinical_data_long4 %>% 
  rowwise() %>%
  mutate(patchmosA = case_when(
    any(c_across(starts_with("QE2A_")) == 2)             ~ QE4AM
  )) %>% 
  mutate(patchmosB = case_when(
    any(c_across(starts_with("QE2B_")) == 2)             ~ QE4BM
  )) %>% 
  mutate(patchmosC = case_when(
    any(c_across(starts_with("QE2C_")) == 2)             ~ QE4CM
  )) %>% 
  mutate(patchmosD = case_when(
    any(c_across(starts_with("QE2D_")) == 2)             ~ QE4DM
  )) %>% 
  mutate(patchmosE = case_when(
    any(c_across(starts_with("QE2E_")) == 2)             ~ QE4EM
  )) %>% 
  mutate(patchmosF = case_when(
    any(c_across(starts_with("QE2F_")) == 2)             ~ QE4FM
  )) %>% 
  mutate(patchmosG = case_when(
    any(c_across(starts_with("QE2G_")) == 2)             ~ QE4GM
  )) %>% 
  mutate(patchmosH = case_when(
    any(c_across(starts_with("QE2H_")) == 2)             ~ QE4HM
  )) %>% 
  mutate(patchmosI = case_when(
    any(c_across(starts_with("QE2I_")) == 2)             ~ QE4IM
  )) %>% 
  ungroup() %>% 
  mutate(pattemp = rowSums(select(.,patchmosA:patchmosI), na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(patchmos = case_when(
    any(!is.na(c_across(starts_with("patchmos"))))             ~ pattemp,
    TRUE                                                          ~ 888
  )) %>% 
  ungroup() %>% 
  select(-c(patchmosA:patchmosI, pattemp))

clinical_data_long6 <- clinical_data_long5 %>% 
  rowwise() %>%
  mutate(ringmosA = case_when(
    any(c_across(starts_with("QE2A_")) == 9)             ~ QE4AM
  )) %>% 
  mutate(ringmosB = case_when(
    any(c_across(starts_with("QE2B_")) == 9)             ~ QE4BM
  )) %>% 
  mutate(ringmosC = case_when(
    any(c_across(starts_with("QE2C_")) == 9)             ~ QE4CM
  )) %>% 
  mutate(ringmosD = case_when(
    any(c_across(starts_with("QE2D_")) == 9)             ~ QE4DM
  )) %>% 
  mutate(ringmosE = case_when(
    any(c_across(starts_with("QE2E_")) == 9)             ~ QE4EM
  )) %>% 
  mutate(ringmosF = case_when(
    any(c_across(starts_with("QE2F_")) == 9)             ~ QE4FM
  )) %>% 
  mutate(ringmosG = case_when(
    any(c_across(starts_with("QE2G_")) == 9)             ~ QE4GM
  )) %>% 
  mutate(ringmosH = case_when(
    any(c_across(starts_with("QE2H_")) == 9)             ~ QE4HM
  )) %>% 
  mutate(ringmosI = case_when(
    any(c_across(starts_with("QE2I_")) == 9)             ~ QE4IM
  )) %>% 
  ungroup() %>% 
  mutate(ringtemp = rowSums(select(.,ringmosA:ringmosI), na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(ringmos = case_when(
    any(!is.na(c_across(starts_with("ringmos"))))             ~ ringtemp,
    TRUE                                                          ~ 888
  )) %>% 
  ungroup() %>% 
  select(-c(ringmosA:ringmosI, ringtemp))


clinical_data_long7 <- clinical_data_long6 %>% 
  rowwise() %>%
  mutate(depomosA = case_when(
    any(c_across(starts_with("QE2A_")) == 7)             ~ QE4AM
  )) %>% 
  mutate(depomosB = case_when(
    any(c_across(starts_with("QE2B_")) == 7)             ~ QE4BM
  )) %>% 
  mutate(depomosC = case_when(
    any(c_across(starts_with("QE2C_")) == 7)             ~ QE4CM
  )) %>% 
  mutate(depomosD = case_when(
    any(c_across(starts_with("QE2D_")) == 7)             ~ QE4DM
  )) %>% 
  mutate(depomosE = case_when(
    any(c_across(starts_with("QE2E_")) == 7)             ~ QE4EM
  )) %>% 
  mutate(depomosF = case_when(
    any(c_across(starts_with("QE2F_")) == 7)             ~ QE4FM
  )) %>% 
  mutate(depomosG = case_when(
    any(c_across(starts_with("QE2G_")) == 7)             ~ QE4GM
  )) %>% 
  mutate(depomosH = case_when(
    any(c_across(starts_with("QE2H_")) == 7)             ~ QE4HM
  )) %>% 
  mutate(depomosI = case_when(
    any(c_across(starts_with("QE2I_")) == 7)             ~ QE4IM
  )) %>% 
  ungroup() %>% 
  mutate(depotemp = rowSums(select(.,depomosA:depomosI), na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(depomos = case_when(
    any(!is.na(c_across(starts_with("depomos"))))             ~ depotemp,
    TRUE                                                          ~ 888
  )) %>% 
  ungroup() %>% 
  select(-c(depomosA:depomosI, depotemp))

clinical_data_long8 <- clinical_data_long7 %>% 
  rowwise() %>%
  mutate(norplantmosA = case_when(
    any(c_across(starts_with("QE2A_")) == 8)             ~ QE4AM
  )) %>% 
  mutate(norplantmosB = case_when(
    any(c_across(starts_with("QE2B_")) == 8)             ~ QE4BM
  )) %>% 
  mutate(norplantmosC = case_when(
    any(c_across(starts_with("QE2C_")) == 8)             ~ QE4CM
  )) %>% 
  mutate(norplantmosD = case_when(
    any(c_across(starts_with("QE2D_")) == 8)             ~ QE4DM
  )) %>% 
  mutate(norplantmosE = case_when(
    any(c_across(starts_with("QE2E_")) == 8)             ~ QE4EM
  )) %>% 
  mutate(norplantmosF = case_when(
    any(c_across(starts_with("QE2F_")) == 8)             ~ QE4FM
  )) %>% 
  mutate(norplantmosG = case_when(
    any(c_across(starts_with("QE2G_")) == 8)             ~ QE4GM
  )) %>% 
  mutate(norplantmosH = case_when(
    any(c_across(starts_with("QE2H_")) == 8)             ~ QE4HM
  )) %>% 
  mutate(norplantmosI = case_when(
    any(c_across(starts_with("QE2I_")) == 8)             ~ QE4IM
  )) %>% 
  ungroup() %>% 
  mutate(norplantemp = rowSums(select(.,norplantmosA:norplantmosI), na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(norplantmos = case_when(
    any(!is.na(c_across(starts_with("norplantmos"))))             ~ norplantemp,
    TRUE                                                          ~ 888
  )) %>% 
  ungroup() %>% 
  select(-c(norplantmosA:norplantmosI, norplantemp))

clinical_data_long <- clinical_data_long8 %>% 
  select(suid, everiud, iuddur, iudfirstage, iudlastage,
         everhormiud, typeiud, dalkon, lippes, copper,
         coil, inertiud, evercondoms, everdiaphragm, 
         everimplant, evershot, everothbar, patch,
         patchmos, ring, ringmos, depo, depomos, 
         norplant, norplantmos)

write_csv(clinical_data_long, paste0(path,"/data/recoded_iud_long.csv"))


# END
