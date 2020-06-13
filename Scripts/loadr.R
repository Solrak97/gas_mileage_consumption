library(readr)
library(modeest)


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
#comienzan los filtros para comparaciones
year99    = filter(data_mpg, year=="1999")
year08    = filter(data_mpg, year=="2008")
motoresS = filter(data_mpg,  displ <= 2.5)
motoresM = filter(data_mpg,  displ > 2.5 & displ <=4.0)
motoresXL= filter(data_mpg, displ > 4.0)
transAuto= filter(data_mpg, str_extract(trans, "auto*") == "auto")
transM = filter(data_mpg, str_extract(trans, "manual*") == "manual")
delantera= filter(data_mpg, drv == "f")
trasera  = filter(data_mpg, drv == "r")
awd      = filter(data_mpg, drv == "4")
cuatro   = filter(data_mpg, cyl == 4)
cinco    = filter(data_mpg, cyl == 5)
seis     = filter(data_mpg, cyl == 6)
ocho     = filter(data_mpg, cyl == 8)

#inicializacion de las medias de consumo 
mean(ano99$cty)
mean(ano99$hwy)
mean(ano08$cty)
mean(ano08$hwy)
mean(motoresS$cty)
mean(motoresS$hwy)
mean(motoresM$cty)
mean(motoresM$hwy)
mean(motoresXL$cty)
mean(motoresXL$hwy)
mean(transMan$cty)
mean(transMan$hwy)
mean(transAuto$cty)
mean(transAuto$hwy)
mean(delantera$cty)
mean(delantera$hwy)
mean(trasera$cty)
mean(trasera$hwy)
mean(awd$cty)
mean(awd$hwy)
mean(cuatro$cty)
mean(cuatro$hwy)
mean(cinco$cty)
mean(cinco$hwy)
mean(seis$cty)
mean(seis$hwy)
mean(ocho$cty)
mean(ocho$hwy)



