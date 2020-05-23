source("./code/functions.R")

list.of.packages <- c(
  'assertthat',
  'data.table',
  'surveyweights',
  'stringr',
  'tidyverse',
  'srvyr',
  'gganimate',
  'ggthemes',
  'plotly',
  'extrafont',
  'DT',
  'magick',
  'magrittr'
)

#Install new packages
install_new_packages(list.of.packages)

library(assertthat)
library(data.table)
library(surveyweights)
library(stringr)
library(tidyverse)
library(srvyr)
library(gganimate)
library(ggthemes)
library(plotly)
library(extrafont)
library(DT)
library(magick)
library(magrittr)
fonts()
fonttable()
