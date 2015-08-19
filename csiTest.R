library(Quandl)
Quandl.auth("7fhzpCA_h2GZobLUKwsT")

nyselist <- read.table("C:/Users/Danil/Documents/investparty/nyselist.txt", header = F, stringsAsFactors = FALSE)

quotes <- list()
for(i in 1:50)
{
  val <- tryCatch({
    dat <- Quandl(paste("YAHOO/", nyselist$V1[i], collapse = "", sep = ""), start_date = "2004-01-01", end_date = "2014-12-31", collapse = "daily", sort = "asc")
    dat$ret <- c(NA, log(dat$Close[2:nrow(dat)] / dat$Close[1:(nrow(dat)-1)]))
    attributes(dat)$name <- nyselist$V1[i]
    quotes <- append(quotes, list(dat))
  },
  error = function(err){
    print(nyselist$V1[i])
  })
}
rm(list = setdiff(ls(), "quotes"))
  
csi <- read.csv("C:/Users/Danil/Documents/investparty/data/csi.csv", sep = ";", dec = ",", skip = 1, header = T)
csi$Date <- as.Date(csi$Date, format = "%d.%m.%Y")
csi <- csi[complete.cases(csi),]
csi <- csi[order(csi$Date),]
csi$norm <- csi$Mid.Price / 200


modelData <- lapply(quotes, function(x){
  dates <- csi$Date
  dates <- x$Date[x$Date %in% dates]
  df <- as.data.frame(x$ret[x$Date %in% dates])
  df$csi <- csi$norm[csi$Date %in% dates]
  df$csi <- c(NA, df$csi[1:(nrow(df)-1)])
  df <- df[complete.cases(df),]
  colnames(df)[1] <- attributes(x)$name
  attributes(df)$name <- attributes(x)$name
  attributes(df)$cor <- cor(df[,1], df[,2])
  return(list(df))
})

