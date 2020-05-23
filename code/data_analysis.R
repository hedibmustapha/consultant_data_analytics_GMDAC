
# create survey design
design <- data %>% as_survey(weights = c(strata.weights))

fcs_summary <- design %>% group_by(displacement_status,fcs_category) %>% 
  summarize(numbers = survey_mean())


p1 <- plot_animated_ggplot(fcs_summary,"fcs_category","numbers","displacement_status",
                             "% of HHs with an acceptable, borderline or poor FCS 
                             (Food Consumption Score) for {closest_state}")
  

rcsi_summary <- design %>% group_by(displacement_status,rcsi_category) %>% 
  summarize(numbers = survey_mean())

p2 <- plot_animated_ggplot(rcsi_summary,"rcsi_category","numbers","displacement_status",
                             "% of HHs with low, medium or high rCSI 
       (reduced Coping Strategy Index) for {closest_state}")


lcsi_summary <- design %>% group_by(displacement_status,lcsi) %>% 
  summarize(numbers = survey_mean(na.rm =T))

p3 <- plot_animated_ggplot(lcsi_summary,"lcsi","numbers","displacement_status",
                             "% of HHs with a stress, crisis or emergency LCSI 
  (Livelihood Coping Strategy Index) for {closest_state}")

foodsource_summary <- design %>%  
  summarize(Market_purchased_cash = survey_mean(food_source.market_cash),
            Market_purchased_cheque = survey_mean(food_source.market_cheque),
            Market_purchased_credit = survey_mean(food_source.market_credit)) %>%
  pivot_longer(c(Market_purchased_cash,
                 Market_purchased_cheque,
                 Market_purchased_credit))

p4 <- plot_flipped_ggplot(foodsource_summary,"name","value",
                          "Top 3 sources from which households reported acquiring food")


watersource_summary <- design %>% group_by(primary_drinkingwater_source) %>%
  summarize(numbers = survey_mean()) %>% arrange(-numbers)

p5 <- plot_flipped_ggplot(watersource_summary,"primary_drinkingwater_source","numbers",
                          "Main reported sources of drinking water")

access_publicwater_summary <- design %>% group_by(displacement_status, access_public_water_network) %>%
  summarize(numbers = survey_mean()) %>% arrange(-numbers)

p6 <- plot_animated_flipped_ggplot(access_publicwater_summary,"access_public_water_network","numbers",
                                   "displacement_status",
                                   "access to the water from the public network in the last 7 days for {closest_state}")

 income_summary <- design %>% group_by(displacement_status) %>%
  summarize(
    numbers = survey_median(total_income, na.rm = T, vartype = NULL),
    income = survey_quantile(total_income, quantile = c(0,0.25, 0.75,1), vartype = NULL, na.rm = T)
  ) %>% mutate(
    level = "income"
  )
 
 income_summary$displacement_status <- with(income_summary,
                                            reorder(income_summary$displacement_status,
                                                    numbers,function(x) - median(x, na.rm=TRUE)))
 
 p7 <- ggplot(income_summary, aes(x=displacement_status, 
                            ymin = income_q00, 
                            lower = income_q25, 
                            middle = numbers, 
                            upper = income_q75, 
                            ymax = income_q100,
                            width = 0.7, 
                            fill = displacement_status)) +
   geom_boxplot(stat = "identity") +
   theme_bw() +
   theme(legend.position = 'none',
         plot.title = element_text(hjust = 0.5),
         text = element_text('Arial Narrow'),
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line(size=.1, color="grey85"),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 0, size = 10.2, hjust = 0.5)) +
   labs(title = "Median total income by displacement groups",
        x = NULL,
        y = " Income (LYD)") +
   geom_text(data = income_summary, 
             aes(y = numbers, label = round(numbers,digits = 0)),
             size = 3.52, hjust = -2.57, vjust =  0.6)
 
 income_summary_strata <- design %>% group_by(mantika_label,displacement_status) %>%
   summarize(
     numbers = survey_median(total_income, na.rm = T, vartype = NULL)
   )
 
 expenditure_summary <- design %>% group_by(displacement_status) %>%
   summarize(
     numbers = survey_median(total_expenditures, na.rm = T, vartype = NULL),
     expenditure = survey_quantile(total_expenditures, quantile = c(0,0.25, 0.75,1), vartype = NULL, na.rm = T)
   ) %>% mutate(
     level = "expenditure"
   )
 
  expenditure_summary_strata <- design %>% group_by(mantika_label,displacement_status) %>%
   summarize(
     numbers = survey_median(total_expenditures, na.rm = T, vartype = NULL)
   )
 
 income_expenditure <- bind_rows(income_summary,expenditure_summary)
 
 p8 <- plot_ly(income_expenditure, x = ~displacement_status, y = ~numbers, type = 'bar',color = ~level) %>%
   layout(title = "Median total income VS Median Total expenditure", yaxis = list(title = "LYD"), xaxis = list(title = ""), barmode = 'group')
 
 
 # results <- list(fcs_summary=fcs_summary,
 #                 rcsi_summary=rcsi_summary,
 #                 lcsi_summary=lcsi_summary,
 #                 foodsource_summary=foodsource_summary,
 #                 watersource_summary=watersource_summary,
 #                 access_publicwater_summary=access_publicwater_summary,
 #                 income_summary=income_summary,
 #                 p7=p7,
 #                 income_expenditure=income_expenditure,
 #                 income_summary_strata=income_summary_strata,
 #                 p8=p8,
 #                 expenditure_summary_strata=expenditure_summary_strata)
 # 
 # render_report_rmd(x = results,
 #                   dir = "./output",
 #                   filename = "report.html")