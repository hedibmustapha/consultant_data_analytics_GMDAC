devtools::install_github("mabafaba/composr")
devtools::install_github("mabafaba/koboquest")
devtools::install_github("hedibmustapha/UpSetR")
devtools::install_github("caldwellst/Setviz")

library(UpSetR) #An R implementation of the UpSet set visualization technique
library(Setviz) #A wrapper for UpsetR
library(composr) #R package for incrementally recoding variables (with kobo extension)
library(koboquest)
library(data.table)
library(purrr)
library(dplyr)


rounded_mean <- function(...){
  round(...,digits = 0)
}

# Load data, kobo questions and choices
questions <- read.csv("./assessment2/input/kobo_questionnaire.csv", 
                      stringsAsFactors = F)
choices <- read.csv("./assessment2/input/kobo_choices.csv", 
                    stringsAsFactors = F)
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)
data <- read.csv("./assessment2/input/data.csv", stringsAsFactors = F)
data <- mutate_if(data, is.character, na_if, "")
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")

# Create new variables
ci_prep_data <- data %>% mutate(
  area_per_person = total_area_accomodation / (nb_people_share_accomodation + 1),
  rcsi = less_expensive_quality + (borrow_relatives * 2) + reduce_number_meals + (reduce_adult * 3) + shrink_meals,
  rcsi_category = case_when(
    rcsi <= 3 ~ "low",
    rcsi > 3 & rcsi <= 9 ~ "medium",
    rcsi > 9 ~ "high"
  ),
  fcs = (cereals * 2) + (legumes * 3) + veggies + fruits + (meat * 4) + (dairy * 4) + (fats * 0.5) + (sugar * 0.5),
  fcs_category = case_when(
    fcs <= 28 ~ "poor",
    fcs > 28 & fcs <= 42 ~ "borderline",
    fcs > 42 ~ "acceptable"
  ),
  fcs_index = case_when(
    fcs_category == "acceptable" ~ 1,
    fcs_category == "borderline" ~ 3,
    fcs_category == "poor" ~ 4
  ),
  
  cash_coping_stress = rowSums(select(., 
                                      sold_nonproductive_hh_assets,
                                      spent_savings,
                                      borrowed_purchased_oncredit_food,
                                      reduced_expenditures_essential_nfi) %>%
                                 mutate_all(~ .x %in% c("no_already_exhausted", "yes")),
                               na.rm = T),
  cash_coping_crisis = rowSums(select(., 
                                      sold_productive_hh_assets,
                                      borrowed_money,
                                      reduced_expenditures_health_education,
                                      took_additional_job) %>%
                                 mutate_all(~ .x %in% c("no_already_exhausted", "yes")),
                               na.rm = T),
  cash_coping_emergency = rowSums(select(., 
                                         begging,
                                         adult_accepting_degrading_illegal_work,
                                         minor_accepting_degrading_illegal_work,
                                         child_marriage) %>%
                                    mutate_all(~ .x %in% c("no_already_exhausted", "yes")),
                                  na.rm = T),
  cash_coping = case_when(
    cash_coping_emergency > 0 ~ "emergency",
    cash_coping_crisis > 0 ~ "crisis",
    cash_coping_stress > 0 ~ "stress",
    cash_coping_emergency + cash_coping_crisis + cash_coping_stress == 0 ~ "none"
    
  ),
  food_expenditure_exc_0 = ifelse(food_expenditure ==0,NA,food_expenditure),
  rent_expenditure_exc_0 = ifelse(rent_expenditure ==0,NA,rent_expenditure),
  shelter_maintenance_expenditure_exc_0 = ifelse(shelter_maintenance_expenditure ==0,NA,shelter_maintenance_expenditure),
  water_expenditure_exc_0 = ifelse(water_expenditure ==0,NA,water_expenditure),
  nfi_expenditure_exc_0 = ifelse(nfi_expenditure ==0,NA,nfi_expenditure),
  utilities_expenditure_exc_0 = ifelse(utilities_expenditure ==0,NA,utilities_expenditure),
  fuel_expenditure_exc_0 = ifelse(fuel_expenditure ==0,NA,fuel_expenditure),
  health_related_expenditure_exc_0 = ifelse(health_related_expenditure ==0,NA,health_related_expenditure),
  education_related_expenditure_exc_0 = ifelse(education_related_expenditure ==0,NA,education_related_expenditure),
  transportation_expenditure_exc_0 = ifelse(transportation_expenditure ==0,NA,transportation_expenditure),
  mobile_phone_credit_expenditure_exc_0 = ifelse(mobile_phone_credit_expenditure ==0,NA,mobile_phone_credit_expenditure),
  productive_assets_expenditure_exc_0 = ifelse(productive_assets_expenditure ==0,NA,productive_assets_expenditure),
  debt_repayment_expenditure_exc_0 = ifelse(debt_repayment_expenditure ==0,NA,debt_repayment_expenditure),
  other_expenditure_exc_0 = ifelse(other_expenditure ==0,NA,other_expenditure),
  
  total_expenditures = rowSums(select(.,food_expenditure,rent_expenditure,shelter_maintenance_expenditure,
                                      water_expenditure,
                                      nfi_expenditure,
                                      utilities_expenditure,
                                      fuel_expenditure,health_related_expenditure,education_related_expenditure
                                      ,transportation_expenditure,mobile_phone_credit_expenditure,
                                      productive_assets_expenditure,
                                      debt_repayment_expenditure,other_expenditure)
                               , na.rm=T),
  total_expenditures_exc_0 = ifelse(total_expenditures == 0, NA, total_expenditures),
  
  food_expenditure_share = food_expenditure_exc_0 / total_expenditures_exc_0,
  foodexp = case_when(
    food_expenditure_share < 0.5   ~ 1,
    food_expenditure_share < 0.65  ~ 2,
    food_expenditure_share < 0.75  ~ 3,
    food_expenditure_share >= 0.75 ~ 4
    
  ),
  
  lcs = case_when(
    cash_coping == "none" ~ 1,
    cash_coping == "stress" ~ 2,
    cash_coping == "crisis" ~ 3,
    cash_coping == "emergency" ~ 4
  ),
  
  fsi = (0.5 * fcs_index) + (0.25 * foodexp) + (0.25 * lcs)
)



ci_prep_data <- ci_prep_data  %>% 
  new_recoding(target = employement_profile) %>%
  recode_to(to = "more_than_1_job",
            where = sm_selected(type_jobs, any = c("permanent_job", "temporary_job", "other","daily_labour"))) %>%
  recode_to(to = "did_not_work",
            where = sm_selected(type_jobs, exactly = c("didnt_work"))) %>%
  recode_to(to = "daily_labour",
            where = sm_selected(type_jobs, exactly = c("daily_labour"))) %>%
  recode_to(to = "permanent_job",
            where =  sm_selected(type_jobs, exactly = c("permanent_job"))) %>%
  recode_to(to = "temporary_job",
            where =  sm_selected(type_jobs, exactly = c("temporary_job"))) %>%
  end_recoding()

ci_prep_data$date <- gsub("-","",data$arrive_libya)
ci_prep_data <- ci_prep_data %>% new_recoding(target = time_arrival_category, source = date ) %>%
  recode_to("0_2_year", where.num.larger.equal =  "20170801") %>%
  recode_to("2_4_year", where.num.smaller = "20170801") %>%
  recode_to("4_6_year", where.num.smaller = "20150801") %>%
  recode_to("6_8_year", where.num.smaller = "20130801") %>%
  recode_to("over_8years", where.num.smaller = "20110801") %>%
  end_recoding()

ci_prep_data <- ci_prep_data %>% mutate(
  arabic_speaking= ifelse(languages.arabic==1, "yes","no")
)
