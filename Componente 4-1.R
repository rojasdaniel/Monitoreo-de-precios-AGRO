#install.packages(c("lattice","imputeTS","TSdist"))
setwd("C:/Users/darojas/OneDrive - Departamento Nacional de Planeacion/Documentos/AGRO/PROYECTO/DATOS")
load("datasetimputado.Rda")
dataset <- dataimputada
library(reshape2)
library(lattice) 
library(imputeTS)
library(TSdist)
df <- dataset
df$Precio_kg <- as.numeric(df$Precio_kg)
df$Precio_kg <- round(df$Precio_kg, 0)
df_p <- dcast(df, formula = Central + Fecha ~ Producto, mean, value.var = "Precio_kg", na.rm = TRUE)
df_c <- dcast(df, formula = Producto + Fecha ~ Central, mean, value.var = "Precio_kg", na.rm = TRUE)
productos <- colnames(df_p)
productos <- productos[-c(1:2)]
centrales <- colnames(df_c)
centrales <- centrales[-c(1:2)]
#rta <- (readline(prompt = 'Digite 1 para  generar los mapas de calor por central o 2 para generarlos por producto: '))
rta <- "2"
if (rta=="1"){
  for (central in centrales){
    dff <- df_c[,c("Producto", "Fecha", central)]
    dff <- dcast(dff, formula = Fecha ~ Producto, mean, value.var = central)
    for (product in productos){
      mv <- (sum(is.na(dff[product]))/nrow(dff[product]))*100
      if (mv>=95){
        dff[product] <- NULL
      }
    }
    assign(central, dff)
    centr_df <- colnames(dff)
    centr_df <- centr_df[-1]
    len = length(centr_df)
    disctances = matrix(nrow = len, ncol = len, dimnames = list(centr_df))
    colnames(disctances) = centr_df
    for (p1 in centr_df){
      for (p2 in centr_df){
        jdf <- dff[,c(p1, p2)]
        jdf <- jdf[complete.cases(jdf),]
        calculation <- EuclideanDistance(jdf[,1], jdf[,2])
        disctances[p1, p2] = calculation
      }
    }
    m <- disctances
    mainDir <- "C:/Users/darojas/OneDrive - Departamento Nacional de Planeacion/Documentos/AGRO/PROYECTO"
    setwd(mainDir)
    dir.create(file.path(mainDir,"Distancia_Centrales"), showWarnings = FALSE)
    setwd(file.path(mainDir,"Distancia_Centrales"))
    #dir.create(file.path(mainDir,"Productos"), showWarnings = FALSE)
    #setwd(file.path(mainDir,"Productos"))
    #mainDir <- paste(mainDir, "/Productos/", sep="")
    #setwd(mainDir)
    #subDir <- product
    #dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    #setwd(file.path(mainDir, subDir))
    new.palette=colorRampPalette(c("black","red","yellow","white"),space="rgb") 
    png(file=paste(central," - Distancia Euclidiana",".png",sep=""),width = 1000, height = 1000)
    plot <- levelplot(m[1:ncol(m),ncol(m):1],col.regions=new.palette(20), scales=list(x=list(rot=90, cex=1),y=list(cex=1)), xlab=list(cex=0), ylab=list(cex=0),  main = paste(central, "Distancia Euclidiana",sep="-"))
    print(plot)
    dev.off()
  }
}else{
  for (product in productos){
    dff <- df_p[,c("Central", "Fecha", product)]
    dff <- dcast(dff, formula = Fecha ~ Central, mean, value.var = product)
    for (central in centrales){
      mv <- (sum(is.na(dff[central]))/nrow(dff[central]))*100
      if (mv>=95){
        dff[central] <- NULL
      }
    }
    assign(product, dff)
    centr_df <- colnames(dff)
    centr_df <- centr_df[-1]
    len = length(centr_df)
    disctances = matrix(nrow = len, ncol = len, dimnames = list(centr_df))
    colnames(disctances) = centr_df
    for (p1 in centr_df){
      for (p2 in centr_df){
        jdf <- dff[,c(p1, p2)]
        jdf <- jdf[complete.cases(jdf),]
        calculation <- EuclideanDistance(jdf[,1], jdf[,2])
        disctances[p1, p2] = calculation
      }
    }
    m <- disctances
    mainDir <- "C:/Users/darojas/OneDrive - Departamento Nacional de Planeacion/Documentos/AGRO/PROYECTO"
    setwd(mainDir)
    dir.create(file.path(mainDir,"Distancia_Productos"), showWarnings = FALSE)
    setwd(file.path(mainDir,"Distancia_Productos"))
    #dir.create(file.path(mainDir,"Productos"), showWarnings = FALSE)
    #setwd(file.path(mainDir,"Productos"))
    #mainDir <- paste(mainDir, "/Productos/", sep="")
    #setwd(mainDir)
    #subDir <- product
    #dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    #setwd(file.path(mainDir, subDir))
    new.palette=colorRampPalette(c("black","red","yellow","white"),space="rgb") 
    png(file=paste(product," - Distancia Euclidiana",".png",sep=""),width = 1000, height = 1000)
    plot <- levelplot(m[1:ncol(m),ncol(m):1],col.regions=new.palette(20), scales=list(x=list(rot=90, cex=1),y=list(cex=1)), xlab=list(cex=0), ylab=list(cex=0),  main = paste(product, "Distancia Euclidiana",sep="-"))
    print(plot)
    dev.off()
    
    }
}






