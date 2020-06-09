loadr <- function(){
  
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
}