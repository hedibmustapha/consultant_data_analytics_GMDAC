# Load composite indicator definition
many_records <- read.csv("./assessment2/input/migrant_msna_composite_indicator.csv")

#Use composR to create the composite indicators
ci_data <- ci_prep_data %>% 
  recode_batch(tos = many_records$to_values,
                                         wheres = many_records$conditions,
                                         targets = many_records$target_variables,
                                         questionnaire = questionnaire) %>%
                                                    end_recoding()

composite_data <- ci_data %>% 
  mutate(preexisting_vulnerability_score = ifelse(preexisting_vulnerability_index3 > 2 & !is.na(preexisting_vulnerability_index3) | 
                                                    preexisting_vulnerability_index5 > 2 & !is.na(preexisting_vulnerability_index5), 
                                                  pmax(preexisting_vulnerability_index3,preexisting_vulnerability_index5,na.rm = T),
                                                  rounded_mean(rowMeans(select(., preexisting_vulnerability_index1:preexisting_vulnerability_index5), na.rm = T))),
         
         impact_score = ifelse(impact_index2 > 2 & !is.na(impact_index2), 
                               impact_index2, 
                               rounded_mean(rowMeans(select(., impact_index1:impact_index3), na.rm = T))),
         
         protection_score = ifelse(protection_index1 > 2 & !is.na(protection_index1), 
                                   protection_index1, 
                                   rounded_mean(rowMeans(select(., protection_index1:protection_index4), na.rm = T))),
         
         shelter_nfi_score = ifelse(shelter_nfi_index1 > 3 & !is.na(shelter_nfi_index1) | 
                                      shelter_nfi_index3 > 2  & !is.na(shelter_nfi_index3), 
                                    pmax(shelter_nfi_index1,shelter_nfi_index3,na.rm = T), 
                                    rounded_mean(rowMeans(select(., shelter_nfi_index1:shelter_nfi_index4), na.rm = T))),
         
         fs_score = ifelse(fs_index3 > 3 & !is.na(fs_index3), 
                           fs_index3, 
                           rounded_mean(rowMeans(select(., fs_index1:fs_index4), na.rm = T))),
         
         health_score = ifelse(health_index1 > 2  & !is.na(health_index1) |
                                 health_index2 > 2  & !is.na(health_index2), 
                               pmax(health_index1,health_index2,na.rm = T), 
                               rounded_mean(rowMeans(select(., health_index1:health_index3), na.rm = T))),
         
         wash_score = ifelse(wash_index1 > 2 & !is.na(wash_index1), 
                             wash_index1, 
                             rounded_mean(rowMeans(select(., wash_index1:wash_index3), na.rm = T))),
         
         education_score = ifelse(education_index1 > 2 & !is.na(education_index1), 
                                  education_index1, 
                                  rounded_mean(rowMeans(select(., education_index1:education_index3), na.rm = T))),
         
         capacity_gap_score = capacity_gap
  )
