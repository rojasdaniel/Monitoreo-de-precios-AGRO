library(readxl)
library(prophet)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringi)
library(TSdist)
library(imputeTS)
library(lattice)
library(TTR)
#library(smooth)
library(forecast)
mainDir <- "C:/Users/darojas/OneDrive - Departamento Nacional de Planeacion/Documentos/AGRO/PROYECTO"
setwd(mainDir)
load("DATOS/datasetimputado.Rda")
setwd(mainDir)
dataset <- dataimputada
library(readr)
errores <- read_delim("C:/Users/darojas/OneDrive - Departamento Nacional de Planeacion/Documentos/AGRO/PROYECTO/DATOS/ERRORES.csv", 
                      ";", escape_double = FALSE, col_types = cols(Error_Profeta = col_number(), 
                                                                   Error_arima = col_number(), Error_ets = col_number(), 
                                                                   Error_meanf = col_number(), Error_naive = col_number(), 
                                                                   Error_snaive = col_number(), Error_tbats = col_number(), 
                                                                   `Minimo Error porcentaje` = col_number()), 
                      trim_ws = TRUE)
tipose <- unique(errores$`Minimo Error`)
centrales <- unique(dataset$Central)
#productos <- unique(dataset$Producto)
no_errores <- unique(errores$`Minimo Error porcentaje`)
predicc <- data.frame(Central=character(),Producto=character(), Ganador=as.character())
por <-0.8
for (central in centrales){
  tabla <- errores[errores$Central == central,]
  products <-unique(tabla$Producto)
  for (producto in products){
    show <- tabla[tabla$Central == central & tabla$Producto == producto,]
    result <- as.data.frame(table(show$`Minimo Error`))
    pos <- which.max(result$Freq)
    resultado <- as.character(result$Var1[pos])
    z <- data.frame(central, producto, resultado)
    colnames(z) <- c("Central", "Producto", "Ganador")
    predicc <- rbind(z, predicc)
  }
}
lista <- predicc[predicc$Ganador=="Error_Profeta",]
graficadora <- function(forecast, forecast2, nom_prod, ciudad, datos, funcion){
  titulo <- paste(nom_prod,"en", ciudad,"con", funcion)
  file_name = paste(titulo, ".png", sep = "") ##717
  file_name <- gsub("\\*", "", file_name)
  gra <- ggplot(aes(x = Fecha, y =  Precio), data = datos)  + ggtitle(titulo) + geom_point() +
    geom_line(aes(x = Fecha, y = yhat),stat = "identity", data = forecast, color = "red") +
    geom_line(aes(x = Fecha, y = yhat),stat = "identity", data = forecast2, color = "blue") +  
    geom_line (aes(x = Fecha, y = yhat_upper), colour='grey', linetype="dotted", data = forecast2, stat='identity')  +
    geom_line (aes(x = Fecha, y = yhat_lower), colour='grey', linetype="dotted", data = forecast2, stat='identity')  + 
    theme_bw()
  ggsave(file=file_name, dpi = 72)
  dev.off() 
}
errores_profeta <- function(dp08, dp02, dr, nom_prod, ciudad, funcion){
  x <- data.frame(dp08)
  colnames(x) <- (c("ds", "y"))
  x <- prophet(x, yearly.seasonality = TRUE)
  future <- make_future_dataframe(x, periods = length(dp02$Precio)) ##131
  forecast <- predict(x, future)
  df2 <- data.frame(dp08)
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  dr2$Precio <- as.numeric(dr2$Precio)
  df2$Precio <- as.numeric(df2$Precio)
  datos <- rbind(df2, dr2)
  forecast$ds2 <- as.Date(forecast$ds, format="%Y-%m-%d")
  file_name = paste("Producto - ", nom_prod, ".png", sep = "") ##717
  titulo <- paste(nom_prod,"en", ciudad)
  file_name <- gsub("\\*", "", file_name)
  fo <- forecast
  forecast <- fo
  forecast1 <- data.frame(forecast$ds2[1:length(dp08$Fecha)], forecast$yhat[1:length(dp08$Precio)])
  forecast2 <- data.frame(forecast$ds2[length(dp08$Fecha)+1:length(dp02$Fecha)], forecast$yhat[length(dp08$Fecha)+1:length(dp02$Fecha)])
  colnames(forecast1) <- c("ds2", "yhat")
  colnames(forecast2) <- c("ds2", "yhat")
  titulo <- paste(nom_prod,"en", ciudad,"con", funcion)
  file_name = paste(titulo, ".png", sep = "") ##717
  file_name <- gsub("\\*", "", file_name)
  gra <- ggplot(aes(x = Fecha, y =  Precio), data = datos)  + ggtitle(titulo) + geom_point() + 
    geom_line(aes(x = ds2, y = yhat),stat = "identity", data = forecast2, color = "blue") +
    geom_line(aes(x = ds2, y = yhat),stat = "identity", data = forecast1, color = "red") +  
    geom_line (aes(x = ds2, y = yhat_upper), colour='grey', linetype="dotted", data = forecast, stat='identity')  +
    geom_line (aes(x = ds2, y = yhat_lower), colour='grey', linetype="dotted", data = forecast, stat='identity')  + 
    theme_bw()
  ggsave(file=file_name, dpi = 72)
  dev.off()  

}
errores_naive <- function(dp08, dp02, dr, nom_prod, ciudad, funcion){
  dp08$Fecha <- as.Date(dp08$Fecha)
  sts <- ts(dp08$Precio,frequency = 365)
  ddd <- data.frame(dp08$Fecha, sts[1:length(dp08$Precio)])
  plot(sts)
  ar =naive(sts, h=length(dp02$Precio))
  fc.ets = forecast(ar, h=length(dp02$Precio))
  sad <- data.frame(dr$Fecha[1:length(dp08$Fecha)], fc.ets$x[1:length(dp08$Fecha)])
  sad1 <- data.frame(dp02$Fecha, fc.ets$upper, fc.ets$mean,fc.ets$lower)
  plot(fc.ets)
  df <- data.frame(sad$Fecha, sad$yhat)
  df2 <- dp08
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  dr2$Precio <- as.numeric(dr2$Precio)
  df2$Precio <- as.numeric(df2$Precio)
  datos <- rbind(dr2, df2)
  colnames(sad) <- c("Fecha","yhat")
  colnames(sad1) <- c("Fecha","yhat_upper","yhat", "yhat_lower")
  
  forecast2 <- data.frame(sad1)
  forecast <- data.frame(sad)
  graficadora(forecast, forecast2, nom_prod, ciudad, datos, funcion)

}
errores_meanf <- function(dp08, dp02, dr, nom_prod, ciudad, funcion){
  dp08$Fecha <- as.Date(dp08$Fecha)
  sts <- ts(dp08$Precio,frequency = 365)
  ddd <- data.frame(dp08$Fecha, sts[1:length(dp08$Precio)])
  plot(sts)
  ar =meanf(sts, h=length(dp02$Precio))
  fc.ets = forecast(ar, h=length(dp02$Precio))
  sad <- data.frame(dr$Fecha[1:length(dp08$Fecha)], fc.ets$x[1:length(dp08$Fecha)])
  sad1 <- data.frame(dp02$Fecha, fc.ets$upper, fc.ets$mean,fc.ets$lower)
  plot(fc.ets)
  df <- data.frame(sad$Fecha, sad$yhat)
  df2 <- dp08
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  dr2$Precio <- as.numeric(dr2$Precio)
  df2$Precio <- as.numeric(df2$Precio)
  datos <- rbind(dr2, df2)
  colnames(sad) <- c("Fecha","yhat")
  colnames(sad1) <- c("Fecha","yhat_upper","yhat", "yhat_lower")
  
  forecast2 <- data.frame(sad1)
  forecast <- data.frame(sad)
  graficadora(forecast, forecast2, nom_prod, ciudad, datos, funcion)
}
errores_arima <- function(dp08, dp02, dr, nom_prod, ciudad, funcion){
  dp08$Fecha <- as.Date(dp08$Fecha)
  sts <- ts(dp08$Precio,frequency = 7)
  ddd <- data.frame(dp08$Fecha, sts[1:length(dp08$Precio)])
  plot(sts)
  ar = auto.arima(sts)
  fc.ets = forecast(ar, h=length(dp02$Precio))
  sad <- data.frame(dr$Fecha[1:length(dp08$Fecha)], fc.ets$x[1:length(dp08$Fecha)])
  sad1 <- data.frame(dp02$Fecha, fc.ets$upper, fc.ets$mean,fc.ets$lower)
  plot(fc.ets)
  df <- data.frame(sad$Fecha, sad$yhat)
  df2 <- dp08
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  dr2$Precio <- as.numeric(dr2$Precio)
  df2$Precio <- as.numeric(df2$Precio)
  datos <- rbind(dr2, df2)
  colnames(sad) <- c("Fecha","yhat")
  colnames(sad1) <- c("Fecha","yhat_upper","yhat", "yhat_lower")
  
  forecast2 <- data.frame(sad1)
  forecast <- data.frame(sad)
  graficadora(forecast, forecast2, nom_prod, ciudad, datos, funcion)
}
errores_ets <- function(dp08, dp02, dr, nom_prod, ciudad, funcion){
  dp08$Fecha <- as.Date(dp08$Fecha)
  sts <- ts(dr$Precio,frequency = 365.25)
  ddd <- data.frame(dp08$Fecha, sts[1:length(dp08$Precio)])
  #ar <- ets(sts)
  plot(sts)
  fc.ets = forecast(sts, h=length(dp02$Precio), model ="ets")
  sad <- data.frame(dr$Fecha[1:length(dp08$Fecha)], fc.ets$x[1:length(dp08$Fecha)])
  sad1 <- data.frame(dp02$Fecha, fc.ets$upper, fc.ets$mean,fc.ets$lower)
  plot(fc.ets)
  df <- data.frame(sad$Fecha, sad$yhat)
  df2 <- dp08
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  dr2$Precio <- as.numeric(dr2$Precio)
  df2$Precio <- as.numeric(df2$Precio)
  datos <- rbind(dr2, df2)
  colnames(sad) <- c("Fecha","yhat")
  colnames(sad1) <- c("Fecha","yhat_upper","yhat", "yhat_lower")
  
  forecast2 <- data.frame(sad1)
  forecast <- data.frame(sad)
  graficadora(forecast, forecast2, nom_prod, ciudad, datos, funcion)

  
}



for (central in centrales){
  precioscsv <- dataset %>% filter(Central == central) %>% dcast(Fecha ~ Producto, 
                                                                sum, value.var= "Precio_kg",
                                                                na.rm=TRUE )
  zx <- colnames(precioscsv)
  zx <- zx[2:length(zx)]
  for(i in 1:length(zx)){
    nom_prod <- zx[i]
    vector_prod <- precioscsv[,i + 1]
    dp08 <- data.frame(Fecha = precioscsv[1:round((length(precioscsv$Fecha))*por,0),"Fecha"],
                       Precio = precioscsv[1:round((length(precioscsv$Fecha))*por,0),nom_prod])
    dr <- data.frame("Fecha" = precioscsv$Fecha, "Precio" = vector_prod)
    dp02 <-  subset(dr, !(Fecha %in% dp08$Fecha))
    colnames(dp08) <- c("Fecha", "Precio")
    colnames(dr) <- c("Fecha", "Precio")
    colnames(dp02) <- c("Fecha", "Precio")
    dp08$Fecha <- as.Date(dp08$Fecha)
    dr$Fecha <- as.Date(dr$Fecha)
    dp02$Fecha <- as.Date(dp02$Fecha)
    dp08$Precio <- ts(dp08$Precio)
    dr$Precio <- ts(dr$Precio)
    dp02$Precio <- ts(dp02$Precio)
    dates <- dp08$Fecha
    md <- (as.numeric(format(dates, "%Y%m%d")));
    lastime <- as.Date(dates[which.max(md)])+1;
    dates <- dp02$Fecha
    md <- (as.numeric(format(dates, "%Y%m%d")));
    today <- as.Date(dates[which.max(md)])+1;
    misingdays <- as.Date(as.Date(lastime):as.Date(today), origin="1970-01-01");
    misingdays <- misingdays[order(as.Date(misingdays, format="%Y-%m-%d"), decreasing = TRUE)];
    funcion <- predicc[predicc$Central==central & predicc$Producto==producto, ]
    funcion <- as.character(funcion$Ganador[1])
    ciudad <- central
    
    if (funcion=="Error_Profeta"){
      try(errores_profeta(dp08, dp02, dr,  zx[i], central, funcion))
    }else if(funcion=="Error_naive"){
      try(errores_naive(dp08, dp02, dr,  zx[i], central, funcion))
    }else if(funcion=="Error_meanf"){
      try(errores_meanf(dp08, dp02, dr,  zx[i], central, funcion))
    }else if(funcion=="Error_arima"){
      try(errores_arima(dp08, dp02, dr,  zx[i], central, funcion))
    }else if(funcion=="Error_tbats"){
      try(errores_tbats(dp08, dp02, dr,  zx[i], central, funcion))
    }else if(funcion=="Error_ets"){
      try(errores_ets(dp08, dp02, dr,  zx[i], central, funcion))
    }
  }
}

