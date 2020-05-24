source("./code/dependencies.R")

# Load dataset
data <- import_csv(path_tofile = "./input/data.csv")

#Load sampling frame
sampling_frame <- import_csv(path_tofile = "./input/sampling_frame.csv")

# adding new indicators
data <- data %>% mutate(
  strata.names = str_c(mantika_label,displacement_status,sep = "_"),
  fcs = (cereals * 2) + (legumes * 3) + veggies + fruits + (meat * 4) + (dairy * 4) + (fats * 0.5) + (sugar * 0.5),
  fcs_category = case_when(
    fcs <= 28 ~ "poor",
    fcs > 28 & fcs <= 42 ~ "borderline",
    fcs > 42 ~ "acceptable"
  ),
  rcsi = less_expensive_quality + (borrow_relatives * 2) + reduce_number_meals + (reduce_adult * 3) + shrink_meals,
  rcsi_category = case_when(
    rcsi <= 3 ~ "low",
    rcsi > 3 & rcsi <= 9 ~ "medium",
    rcsi > 9 ~ "high"
  ),
  total_income = rowSums(select(.,gvt_salary,gvt_social_benefits,non_gvt_salary,casual_labour,
                                own_business_income,
                                remittances,
                                family_support,humanitarian_assistance,zakat,income_other) 
                         , na.rm=T),
  total_income = ifelse(total_income ==0, NA,total_income),
  total_expenditures = rowSums(select(.,food_expenditure,rent_expenditure,shelter_maintenance_expenditure,
                                      water_expenditure,
                                      nfi_expenditure,
                                      utilities_expenditure,
                                      fuel_expenditure,health_related_expenditure,education_related_expenditure
                                      ,transportation_expenditure,mobile_phone_credit_expenditure,productive_assets_expenditure,
                                      debt_repayment_expenditure,other_expenditure)
                               , na.rm=T),
  total_expenditures = ifelse(total_expenditures == 0, NA, total_expenditures),
  lcsi_stress = rowSums(select(., 
                                      sold_nonproductive_hh_assets,
                                      spent_savings,
                                      borrowed_purchased_oncredit_food,
                                      reduced_expenditures_essential_nfi) %>%
                                 mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                               na.rm = T),
  lcsi_crisis = rowSums(select(., 
                                      sold_productive_hh_assets,
                                      borrowed_money,
                                      reduced_expenditures_health_education,
                                      took_additional_job,
                                      child_dropped_school,
                                      delayed_skipped_rent) %>%
                                 mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                               na.rm = T),
  lcsi_emergency = rowSums(select(., 
                                         begging,
                                         adult_accepting_degrading_illegal_work,
                                         minor_accepting_degrading_illegal_work,
                                         child_marriage) %>%
                                    mutate_all(~ .x %in% c("already_exhausted_this_coping_strategy", "yes")),
                                  na.rm = T),
  lcsi = case_when(
    lcsi_emergency > 0 ~ "emergency",
    lcsi_crisis > 0 ~ "crisis",
    lcsi_stress > 0 ~ "stress",
    lcsi_emergency + lcsi_crisis + lcsi_stress == 0 ~ "none"
  )
)

#create weighting fct
weights <- create_weights(sampling.frame = sampling_frame,
                          data.stratum.column = "strata.names",
                          sampling.frame.population.column = "population",
                          sampling.frame.stratum.column = "strata.names",
                          data = data)

data <- data %>% mutate(
  strata.weights = weights(data)
)
