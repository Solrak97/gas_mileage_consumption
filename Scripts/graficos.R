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