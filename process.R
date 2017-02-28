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

month.num <- as.numeric(format(index(LAI.x),"%m"))
month.recod <- ifelse(month.num < 5,13,month.num)
month.ws <- ifelse(month.recod >= 10,"winter","summer")

tapply(as.numeric(LAI.x), month.ws, max)

plot(apply.monthly(LAI.x1,max))

monthly.max <- round(apply.monthly(LAI.x1,max),1)

month.num <- as.numeric(format(index(monthly.max),"%m"))
month.recod <- ifelse(month.num < 5,13,month.num)
month.ws <- ifelse(month.recod >= 10,"winter","summer")
month.wsc <- ifelse(month.num == 4 | month.num == 10,"change",NA)
month.wsc <- ifelse(is.na(month.wsc),month.ws,month.wsc)

monthly.min=as.numeric(apply.monthly(LAI.x1,min))
monthly.mean=round(as.numeric(apply.monthly(LAI.x1,mean)),1)

LAI.df <- data.frame(month=month.wsc,max=as.numeric(monthly.max),
                     min=monthly.min,
                     mean=monthly.mean)
LAI.df[LAI.df$month == "change",c(2,3)] <- NA
LAI.df[LAI.df$month == "summer",3:4] <- NA
LAI.df[LAI.df$month == "winter",c(2,4)] <- NA

LAI.monthraw <- ifelse(is.na(LAI.df[,2]),LAI.df[,3],LAI.df[,2])
LAI.monthraw <- ifelse(is.na(LAI.monthraw),LAI.df[,4],LAI.monthraw)
summary(monthly.max)
date.new <- seq(as.Date("2002-07-15"),as.Date("2016-12-15"),by="months")
LAI.maxmin <- xts(LAI.monthraw,date.new)
plot(LAI.x, col=2)
lines(LAI.maxmin)

LAIann(LAI.x,2016)
lines(LAI.maxmin,lwd=2)

## Daily
date.new <- seq(as.Date("2002-07-15"),as.Date("2016-12-15"),by="days")
LAI.d <- xts(rep(NA,length(date.new)),date.new)
LAI.d <- round(na.approx(merge.xts(LAI.d,LAI.maxmin)[,2]),1)
