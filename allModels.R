#libraries
library(forecast)

#prepare data
#quotes part
#load quotes
load("C:/Users/Danil/Documents/investparty/data/quotes.RData")

#prepare weeks data
weekRets <- lapply(quotes, function(x){
  df <- subset(x, select = c("Date", "ret"))
  df <- df[complete.cases(df),]
  df$gf <- strftime(df$Date, format = "%W/%Y")
  df$month <- as.numeric(format(df$Date, "%m"))
  wy <- aggregate(df$ret, by = list(Category = df$gf), FUN = sum)
  wy$month <- aggregate(df$month, by = list(Category = df$gf), FUN = median)$x
  wy$week <- as.numeric(substr(wy$Category, 1, 2))
  wy$year <- as.numeric(substr(wy$Category, 4, 7))
  wy <- wy[order(wy$year, wy$week),]
  wy$lag <- c(NA, wy$x[1:(nrow(wy)-1)])
  wy$lag2 <- c(NA, NA, wy$x[1:(nrow(wy)-2)])
  wy <- wy[complete.cases(wy),]
  wy$dir <- sign(wy$x)
  wy$dir[wy$dir == (-1)] <- 0
  wy$Category <- NULL
  colnames(wy) <- c(attributes(x)$name, "month", "week", "year", "lag", "lag2", "direction")
  attributes(wy)$name <- attributes(x)$name
  
  return(wy)
})
names(weekRets) <- sapply(weekRets, function(x){attributes(x)$name})
rm(quotes)


#idx part
#cpi
cpi <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/cpi.csv", collapse = "", sep = ""), sep = ";", dec = ",", skip = 1, header = T)
cpi$Date <- as.Date(cpi$Date, format = "%d.%m.%Y")
cpi <- cpi[order(cpi$Date),]
colnames(cpi) <- c("Date", "value")
cpi$week <- as.numeric(format(cpi$Date, "%W"))
cpi$month <- as.numeric(format(cpi$Date, "%m"))
cpi$year <- as.numeric(format(cpi$Date, "%Y"))
attributes(cpi)$name <- "cpi"
cpi$value <- log(cpi$value)
cpi$ret <- c(NA, cpi$value[2:nrow(cpi)] - cpi$value[1:(nrow(cpi)-1)])
cpi <- cpi[complete.cases(cpi),]

#pmi
pmi <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/ismpmi.csv", collapse = "", sep = ""), sep = ";", dec = ",", header = T)
pmi$Date <- as.Date(pmi$Date, format = "%Y-%m-%d")
pmi <- pmi[order(pmi$Date),]
colnames(pmi) <- c("Date", "value")
pmi$week <- as.numeric(format(pmi$Date, "%W"))
pmi$month <- as.numeric(format(pmi$Date, "%m"))
pmi$year <- as.numeric(format(pmi$Date, "%Y"))
attributes(pmi)$name <- "pmi"
pmi$value <- pmi$value / 100
pmi$ret <- c(NA, pmi$value[2:nrow(pmi)] - pmi$value[1:(nrow(pmi)-1)])
pmi <- pmi[complete.cases(pmi),]

#csi
idx <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/csi.csv", collapse = "", sep = ""), sep = ";", dec = ",", skip = 1, header = T)
idx$Date <- as.Date(idx$Date, format = "%d.%m.%Y")
idx <- idx[order(idx$Date),]
colnames(idx) <- c("Date", "value")
idx$wy <- format(idx$Date, "%W%Y")
csi <- aggregate(idx$value, by = list(Category = idx$wy), FUN = function(x){ifelse(length(x) > 1, log(x[length(x)]), log(x))})
attributes(csi)$name <- "csi"
csi$week <- as.numeric(substr(csi$Category, 1, 2))
csi$year <- as.numeric(substr(csi$Category, 3, 6))
csi$Category <- NULL
csi <- csi[order(csi$year, csi$week),]
colnames(csi)[1] <- "value"
csi$ret <- c(NA, csi$value[2:nrow(csi)] - csi$value[1:(nrow(csi)-1)])
csi <- csi[complete.cases(csi),]
rm(idx)

#fedr
fedr <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/fedr.csv", collapse = "", sep = ""), sep = ";", dec = ",", skip = 1, header = T)
fedr$Date <- as.Date(fedr$Date, format = "%d.%m.%Y")
fedr <- fedr[order(fedr$Date),]
colnames(fedr) <- c("Date", "value")
fedr$week <- as.numeric(format(fedr$Date, "%W"))
fedr$month <- as.numeric(format(fedr$Date, "%m"))
fedr$year <- as.numeric(format(fedr$Date, "%Y"))
attributes(fedr)$name <- "fedr"
fedr <- fedr[complete.cases(fedr),]

#frinf
idx <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/frinf.csv", collapse = "", sep = ""), sep = ";", dec = ",", skip = 1, header = T)
idx$Date <- as.Date(idx$Date, format = "%d.%m.%Y")
idx <- idx[order(idx$Date),]
colnames(idx) <- c("Date", "value")
idx$wy <- format(idx$Date, "%W%Y")
frinf <- aggregate(idx$value, by = list(Category = idx$wy), FUN = function(x){ifelse(length(x) > 1, x[length(x)], x)})
attributes(frinf)$name <- "frinf"
frinf$week <- as.numeric(substr(frinf$Category, 1, 2))
frinf$year <- as.numeric(substr(frinf$Category, 3, 6))
frinf$Category <- NULL
frinf <- frinf[order(frinf$year, frinf$week),]
colnames(frinf)[1] <- "value"
frinf$ret <- c(NA, frinf$value[2:nrow(frinf)] - frinf$value[1:(nrow(frinf)-1)])
frinf <- frinf[complete.cases(frinf),]
rm(idx)

indices <- list(cpi, pmi, csi, fedr, frinf)
names(indices) <- c("cpi", "pmi", "csi", "fedr", "frinf")

#remove parts
rm(list=setdiff(ls(), c("weekRets", "indices")))

#tests part
#logistic regression
logit <- lapply(weekRets, function(share, cpi, pmi, csi, fedr, frinf){
  years <- c(2006:2014)
  for(y in years)
  {
    learnData <- share[share$year == y, ]
    testData <- share[share$year == (y+1), ]
    
    cpi <- cpi[cpi$year == y,]
    pmi <- pmi[pmi$year == y,]
    csi <- csi[csi$year == y,]
    fedr <- fedr[fedr$year == y,]
    frinf <- frinf[frinf$year == y,]
    
    
    
  }
}, indices$cpi, indices$pmi, indices$csi, indices$fedr, indices$frinf)




