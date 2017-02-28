raw <- read.csv("Hidegviz4daysWhole-MCD15A3H-006-results.csv")

library(xts)
LAI.x <- xts(raw[,9],as.Date(raw[,3]))

LAIann <- function(x, year)
{
  year.char <- as.character(year)
  plot(x[year.char])  
}

LAIann(LAI.x, 2013)

LAI.loe <- lowess(index(LAI.x),as.numeric(LAI.x),f=0.01)
LAI.lt <- xts(LAI.loe$y, index(LAI.x))
lines(LAI.lt-2, col=2)

LAI.x1 <- LAI.x
LAI.x1[LAI.x < LAI.lt-2] <- NA
LAI.x1 <- na.approx(LAI.x1)

LAIann(LAI.x1, 2010)
lines(LAI.x)

as.Date(paste0("20",rep(c(paste0("0",2:9),10:16),each=2),c("-05-01","-09-01")))


