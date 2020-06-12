library(readr)



data_mpg <- read_csv("Dataset/gas_mileage_comsumption_mpg.csv")

cols(
  X1 = col_double(),
  manufacturer = col_character(),
  model = col_character(),
  displ = col_double(),
  year = col_double(),
  cyl = col_double(),
  trans = col_character(),
  drv = col_character(),
  cty = col_double(),
  hwy = col_double(),
  fl = col_character(),
  class = col_character()
)



library(tidyverse)
ano99= filter(data_mpg, year=="1999")
ano08= filter(data_mpg, year=="2008")
mean(ano99$cty)
mean(ano99$hwy)
mean(ano08$cty)
mean(ano08$hwy)
