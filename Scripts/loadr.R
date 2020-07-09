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




#Pruebas de relacion - significancia displ <=> consumo

#Prueba de asociacion dspl - hwy
chisq.test(data_mpg$displ, data_mpg$hwy, simulate.p.value = TRUE)

#Prueba de asociacion dspl - cty
chisq.test(data_mpg$displ, data_mpg$cty, simulate.p.value = TRUE)

#Valores de chi cuadrado significativos, variables fuertemente relacionadas




#Pruebas de normalidad
with(data_mpg, shapiro.test(data_mpg$displ))
with(data_mpg, qqPlot(data_mpg$displ))

#Normal para diferentes niveles de dspl
with(motoresS, shapiro.test(motoresS$displ))
with(motoresS, qqPlot(motoresS$displ))

with(motoresM, shapiro.test(motoresM$displ))
with(motoresM, qqPlot(motoresM$displ))

with(motoresXL, shapiro.test(motoresXL$displ))
with(motoresXL, qqPlot(motoresXL$displ))

#Datos son normales, en el caso de motores XL apenas normales



#T tests
#motores S
t.test(motoresS$cty, data_mpg$cty, var.equal = TRUE)
t.test(motoresS$hwy, data_mpg$hwy, var.equal = TRUE)

#Motor consume muy poco en gran cantidad de millas


#motores M
t.test(motoresM$cty, data_mpg$cty, var.equal = TRUE)
t.test(motoresM$hwy, data_mpg$hwy, var.equal = TRUE)

#motores medianos son similares a la media en ciudad, ligeramente menos eficientes en carretera


#motores XL
t.test(motoresXL$cty, data_mpg$cty, var.equal = TRUE)
t.test(motoresXL$hwy, data_mpg$hwy, var.equal = TRUE)

#Los motores grandes tienen alto consumo en ciudad y carretera 



#comparacion datos M - XL
t.test(motoresM$cty, motoresXL$cty, var.equal = TRUE)
t.test(motoresM$hwy, motoresXL$hwy, var.equal = TRUE)

#El consumo de motores medianos es significativamente menor a los XL
#Principalmente en carretera


#comparacion datos S - XL
t.test(motoresS$cty, motoresXL$cty, var.equal = TRUE)
t.test(motoresS$hwy, motoresXL$hwy, var.equal = TRUE)

#El consumo de motores medianos es significativamente menor a los XL
#Principalmente en carretera


#comparacion datos S - M
t.test(motoresS$cty, motoresM$cty, var.equal = TRUE)
t.test(motoresS$hwy, motoresM$hwy, var.equal = TRUE)

#El consumo de motores medianos es significativamente menor a los XL
#Principalmente en carretera

#Por lo tanto se cumple la hipotesis de que los autos de menor dspl consumen menos combustible