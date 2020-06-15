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
transAuto99= filter(data_mpg, str_extract(trans, "auto*") == "auto" & year == "1999")
transM99 = filter(data_mpg, str_extract(trans, "manual*") == "manual" & year == "1999")
transAuto08= filter(data_mpg, str_extract(trans, "auto*") == "auto" & year == "2008")
transM08 = filter(data_mpg, str_extract(trans, "manual*") == "manual" & year == "2008")
delantera= filter(data_mpg, drv == "f")
trasera  = filter(data_mpg, drv == "r")
awd      = filter(data_mpg, drv == "4")
cuatro   = filter(data_mpg, cyl == 4)
cinco    = filter(data_mpg, cyl == 5)
seis     = filter(data_mpg, cyl == 6)
ocho     = filter(data_mpg, cyl == 8)

#inicializacion de las medias de consumo 
mean(year99$cty)
mean(year99$hwy)
mean(year08$cty)
mean(year08$hwy)

mean(motoresS$cty)
mean(motoresS$hwy)
mean(motoresM$cty)
mean(motoresM$hwy)
mean(motoresXL$cty)
mean(motoresXL$hwy)

mean(transM$cty)
mean(transM$hwy)
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

mean(transM99$cty)
mean(transAuto99$hwy)
mean(transM08$cty)
mean(transAuto08$hwy)

#Grafica 
theme_set(theme_classic())

g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histograma marcas de vehiculos")
       subtitle=("Cantidad de  Carros diferenciado por marca y tipo de tamaño ")
       
       
grafico_motores <- function() {
  tipos <- c(rep("small", 2), rep("medium", 2), rep("XL", 2))
  
  medias <- c(mean(motoresS$cty), mean(motoresS$hwy), mean(motoresM$cty), mean(motoresM$hwy), +
        mean(motoresXL$cty), mean(motoresXL$hwy))
  
  clases <- rep(c("city" , "higway"), 3)
  
  data <- data.frame(tipos, medias, clases)
  
  ggplot(data, aes(fill=clases, y = medias, x = tipos)) +
    geom_bar(position = "dodge", stat = "identity")
}



grafico_year <- function() {
  tipos <- c(rep("1999", 2), rep("2008", 2))
  
  medias <- c(mean(year99$cty), +
              mean(year99$hwy), +
              mean(year08$cty), +
              mean(year08$hwy))
  
  clases <- rep(c("city" , "higway"), 2)
  
  data <- data.frame(tipos, medias, clases)
  
  ggplot(data, aes(fill=clases, y = medias, x = tipos)) +
    geom_bar(position = "dodge", stat = "identity")
}


grafico_trans <- function() {
  tipos <- c(rep("Manual", 2), rep("Auto", 2))
  
  medias <- c(mean(transM$cty), +
              mean(transM$hwy), +
              mean(transAuto$cty), +
              mean(transAuto$hwy))
  
  clases <- rep(c("city" , "higway"), 2)
  
  data <- data.frame(tipos, medias, clases)
  
  ggplot(data, aes(fill=clases, y = medias, x = tipos)) +
    geom_bar(position = "dodge", stat = "identity")
}



grafico_traccion <- function() {
  tipos <- c(rep("Delantera", 2), rep("Trasera", 2), rep("AWD", 2))
  
  medias <- c(
    mean(delantera$cty), +
    mean(delantera$hwy), +
    mean(trasera$cty), +
    mean(trasera$hwy), +
    mean(awd$cty), +
    mean(awd$hwy))
  
  clases <- rep(c("city" , "higway"), 3)
  
  data <- data.frame(tipos, medias, clases)
  
  ggplot(data, aes(fill=clases, y = medias, x = tipos)) +
    geom_bar(position = "dodge", stat = "identity")
}



grafico_traccion <- function() {
  tipos <- c(rep("Cuatro", 2), rep("Cinco", 2), rep("Seis", 2), rep("Ocho", 2))
  
  medias <- c(mean(cuatro$cty), +
              mean(cuatro$hwy), +
              mean(cinco$cty), +
              mean(cinco$hwy), +
              mean(seis$cty), +
              mean(seis$hwy), +
              mean(ocho$cty), +
              mean(ocho$hwy))
  
  clases <- rep(c("city" , "higway"), 4)
  
  data <- data.frame(tipos, medias, clases)
  
  ggplot(data, aes(fill=clases, y = medias, x = tipos)) +
    geom_bar(position = "dodge", stat = "identity")
}