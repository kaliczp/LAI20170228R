raw <- read.csv("Hidegviz4daysWhole-MCD15A3H-006-results.csv")

library(xts)
LAI.x <- xts(raw[,9],as.Date(raw[,3]))

LAIann <- function(x, year)
{
  year.char <- as.character(year)
  plot(x[year.char])  
}

LAI.loe <- lowess(index(LAI.x),as.numeric(LAI.x),f=0.001)
LAI.lt <- xts(LAI.loe$y, index(LAI.x))
lines(LAI.lt)
