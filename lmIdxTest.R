#load quotes
load("C:/Users/Danil/Documents/investparty/data/quotes.RData")

#prepare weeks data
weekRets <- lapply(quotes, function(x){
  df <- subset(x, select = c("Date", "ret"))
  df <- df[complete.cases(df),]
  df$gf <- strftime(df$Date, format = "%W/%Y")
  wy <- aggregate(df$ret, by = list(Category = df$gf), FUN = sum)
  wy$mean <- aggregate(df$ret, by = list(Category = df$gf), FUN = mean)$x
  wy$sd <- aggregate(df$ret, by = list(Category = df$gf), FUN = sd)$x
  wy$mean <- c(NA, wy$mean[1:(nrow(wy)-1)])
  wy$sd <- c(NA, wy$sd[1:(nrow(wy)-1)])
  wy$lag <- c(NA, wy$x[1:(nrow(wy)-1)])
  wy <- wy[complete.cases(wy),]
  wy$dir <- sign(wy$x)
  wy$dir[wy$dir == (-1)] <- 0
  wy$week <- as.numeric(substr(wy$Category, 1, 2))
  wy$year <- as.numeric(substr(wy$Category, 4, 7))
  wy$Category <- NULL
  colnames(wy) <- c(attributes(x)$name, "mean", "sd", "lag", "direction", "week", "year")
  attributes(wy)$name <- attributes(x)$name
  wy <- wy[order(wy$year, wy$week),]
  
  return(wy)
})
names(weekRets) <- sapply(weekRets, function(x){attributes(x)$name})

#load idx
idxname <- "csi"
indx <- read.csv(paste("C:/Users/Danil/Documents/investparty/data/", idxname, ".csv", collapse = "", sep = ""), sep = ";", dec = ",", skip = 1, header = T)
indx$Date <- as.Date(indx$Date, format = "%d.%m.%Y")
indx <- indx[complete.cases(indx),]
indx <- indx[order(indx$Date),]
colnames(indx) <- c("Date", "value")
attributes(indx)$name <- idxname

#select years by idx
years <- unique(format(indx$Date, "%Y"))

#idx modification function
idxfunc <- function(x){mean(x) / sd(x) / length(x)}

#prepare model data
modelData <- lapply(head(years, n = (length(years)-2)), function(year)
{
  print(year)
  byYears <- t(sapply(weekRets, function(share, idx, y){
    learnData <- share[share$year == y,]
    if(nrow(learnData) > 0)
    {
    idxData <- idx[format(idx$Date, "%Y") == as.character(y),]
    idxData$week <- as.numeric(strftime(idxData$Date, format = "%W"))
    widx <- aggregate(idxData$value, by = list(Category = idxData$week), FUN = idxfunc)
    colnames(widx) <- c("week", "value")
    widx <- widx[order(widx$week),]
    
    widx$week <- widx$week + 1
    weeks <- widx$week
    weeks <- learnData$week[learnData$week %in% weeks]
    widx <- widx[widx$week %in% weeks,]
    learnData <- learnData[learnData$week %in% widx$week,]
    learnData$idx <- widx$value
    
    model <- lm(formula(paste(attributes(share)$name, "~idx", collapse = "", sep = "")), subset(learnData, select = c(attributes(share)$name, "idx")))
    armodel <- lm(formula(paste(attributes(share)$name, "~idx+lag", collapse = "", sep = "")), subset(learnData, select = c(attributes(share)$name, "idx", "lag")))
    
    #test
    y <- y + 1
    testData <- share[share$year == y,]
    
    idxData <- idx[format(idx$Date, "%Y") == as.character(y),]
    idxData$week <- as.numeric(strftime(idxData$Date, format = "%W"))
    widx <- aggregate(idxData$value, by = list(Category = idxData$week), FUN = idxfunc)
    colnames(widx) <- c("week", "value")
    widx <- widx[order(widx$week),]
    
    widx$week <- widx$week + 1
    weeks <- widx$week
    weeks <- testData$week[testData$week %in% weeks]
    widx <- widx[widx$week %in% weeks,]
    testData <- testData[testData$week %in% widx$week,]
    testData$idx <- widx$value
    
    testData$prediction <- predict(model, subset(testData, select = c("idx")))
    modelprob <- sum(sign(testData[,1]) == sign(testData$prediction)) / nrow(testData)
    testData$arprediction <- predict(armodel, subset(testData, select = c("idx", "lag")))
    armodelprob <- sum(sign(testData[,1]) == sign(testData$arprediction)) / nrow(testData)
    
    return(c(modelprob, armodelprob))
    } else
    {
      return(c(0, 0))
    }
        
  }, idx = indx, y = as.numeric(year)))
  row.names(byYears) <- names(weekRets)
  
  return(byYears)
})
names(modelData) <- as.numeric(head(years, n = (length(years)-2)))

#aggregate results
idxModel <- t(sapply(c(1:length(quotes)), function(x){
  m <- mean(sapply(modelData, function(r){return(r[x,1])}), na.rm = T)
  arm <- mean(sapply(modelData, function(r){return(r[x,2])}), na.rm = T)
  
  return(c(m, arm))
}))
colnames(idxModel) <- c("idxModel", "arModel")
rownames(idxModel) <- names(weekRets)

#save results
write.table(idxModel, paste("C:/Users/Danil/Documents/investparty/tests/", idxname, ".csv", collapse = "", sep = ""), sep = ";", dec = ",")

