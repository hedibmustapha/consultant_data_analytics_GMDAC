
Setviz_data <- composite_data %>% mutate(
  protection = ifelse(protection_score >=3,1,0),
  shelter_nfi = ifelse(shelter_nfi_score>=3,1,0),
  fs = ifelse(fs_score>=3,1,0),
  health = ifelse(health_score>=3,1,0),
  wash = ifelse(wash_score>=3,1,0),
  education = ifelse(education_score>=3,1,0),
  capacity_gap = ifelse(capacity_gap_score ==4,1,0))

setnames(x = Setviz_data,
         old = c("protection", "shelter_nfi", "fs", "health", "wash", "education", "capacity_gap"),
         new = c("Protection", "Shelter_NFI", "FSL", "Health", "WASH", "Education", "Capacity_gap"))


plots <- Setviz_data %>%
              split(.$mantika_label) %>% purrr::map(plot_set_percentages,
              varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education","Capacity_gap"),
              mutually_exclusive_sets = T,
              exclude_unique = F,
              round_to_1_percent = T,
              nintersects = 12)

filenames<- unique(Setviz_data$mantika_label)
filenames <- filenames[order(filenames)]
filenames <- str_c("./assessment2/output/", filenames, ".png")

map2(filenames,plots, function(fn,plot){
  png(filename = fn, width = 784, height = 512, res = 100)
  print(plot)
  dev.off()
})
