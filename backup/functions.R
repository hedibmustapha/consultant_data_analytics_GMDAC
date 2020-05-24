#install only packages not available in the library
install_new_packages <- function(list.of.packages){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages) 
}

#sanitize dataframe headers
to_alphanumeric_lowercase <- function(x){
  tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))
}

#read csv files
import_csv <- function(path_tofile){
  assertthat::assert_that(grepl(x = path_tofile, pattern = ".csv$"), 
                          msg = "file must end with '.csv' (..and actually be a .csv file)")
  df <- data.table::fread(file = path_tofile, 
                            stringsAsFactors = F,
                          na.strings = c("","na","n/a","NA",NA)) %>% as.data.frame
  colnames(df) <- to_alphanumeric_lowercase(colnames(df))
  return(df)
}

#Creates a weighting function from sampling frame
create_weights <- function(sampling.frame, data.stratum.column, 
                           sampling.frame.population.column = "population", 
                           sampling.frame.stratum.column = "strata.names", 
                           data = NULL){
  surveyweights::weighting_fun_from_samplingframe(sampling.frame = sampling.frame, 
                                                  data.stratum.column = data.stratum.column, 
                                                  sampling.frame.population.column = sampling.frame.population.column, 
                                                  sampling.frame.stratum.column = sampling.frame.stratum.column, 
                                                  data = data)
}


plot_animated_ggplot <- function(summary_object, categories, numbers, states,plot_title){
  categories <- sym(categories)
  numbers <- sym(numbers)
  states <- sym(states)
  ggplot(summary_object, aes(x = !!categories, 
                        y = !!numbers * 100, fill = !!categories)) +
  geom_col(position = 'dodge') + 
  labs(title = plot_title,
       x = NULL,
       y = NULL) +
  theme_minimal() + 
  theme(
    legend.position = 'none',      
    plot.title = element_text(hjust = 0.5),
    text = element_text('Arial Narrow'),
    strip.text = element_text(face = 'bold', hjust = 0),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste(round(!!numbers * 100), "%")),
            position=position_dodge(width=0.9),
            vjust = -0.5) +
  transition_states(states = !!states) +
  enter_grow() + 
  exit_fade()
}

plot_flipped_ggplot <- function(summary_object, categories, numbers, plot_title){
  categories <- sym(categories)
  numbers <- sym(numbers)
  ggplot(summary_object, aes(x = reorder(!!categories, !!numbers), 
                               y = !!numbers * 100, fill = !!categories)) +
  geom_col(position = 'dodge') + 
  labs(title = plot_title,
       x = NULL,
       y = NULL) +
  theme_minimal() + 
  theme(
    legend.position = 'none',      
    plot.title = element_text(hjust = 0.5),
    text = element_text('Arial Narrow'),
    strip.text = element_text(face = 'bold', hjust = 0),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste(round(!!numbers * 100), "%")),
            position=position_dodge(width=0.9),
            hjust = -0.5) +
  ylim(0, 100) +
  coord_flip()
}

plot_animated_flipped_ggplot <- function(summary_object, categories, numbers, states, plot_title){
  categories <- sym(categories)
  numbers <- sym(numbers)
  states <- sym(states)
  ggplot(summary_object, aes(x = reorder(!!categories, !!numbers), 
                             y = !!numbers * 100, fill = !!categories)) +
    geom_col(position = 'dodge') + 
    labs(title = plot_title,
         x = NULL,
         y = NULL) +
    theme_minimal() + 
    theme(
      legend.position = 'none',      
      plot.title = element_text(hjust = 0.5),
      text = element_text('Arial Narrow'),
      strip.text = element_text(face = 'bold', hjust = 0),
      panel.grid.minor = element_blank()
    ) +
    geom_text(aes(label = paste(round(!!numbers * 100), "%")),
              position=position_dodge(width=0.9),
              hjust = -0.5) +
    ylim(0, 100) +
    coord_flip() +
    transition_states(states = !!states) +
    enter_grow() + 
    exit_fade()
}

f <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r}

#Map results to an output template
render_report_rmd <- function(x, dir, filename) {
  
  template <- "code/report.Rmd"
  render_environment <- new.env()
  
  render_environment$x <- x
  rmarkdown::render(
    template,
    output_file = filename,
    output_dir = dir,
    intermediates_dir = dir,
    envir = render_environment,
    knit_root_dir = getwd()
  )
  
}
