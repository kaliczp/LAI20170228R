raw <- read.csv("Hidegviz4daysWhole-MCD15A3H-006-results.csv")

library(xts)
LAI.x <- xts(raw[,9],as.Date(raw[,3]))
