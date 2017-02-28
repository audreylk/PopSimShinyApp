
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

# runApp("/home/david/workspace/shinyrcharts/samples/electric_market_correlations_technologies_vs_price/")
library(RCurl)
library(RJSONIO)
library(shiny)
library(rCharts)

# fecha final grafica
urlStatus <- "http://priceprofor.ekergy.es/omieinfosys/status"
x <- getURLContent(urlStatus)
y <- fromJSON(x)
z <- y$StudyDataMIBEL$endrecdate
lastDate <- as.character(as.Date(z))

# fecha inicial grafica
month <- as.numeric(format(as.POSIXct(as.Date(lastDate)),"%m"))
numberDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
if (month==1){
  firstDate <- as.Date(lastDate)-numberDays[12]
} else{ 
  firstDate <- as.Date(lastDate)-numberDays[month-1]
}
firstDate <- as.character(firstDate)

# fichero de datos
omiefunc <- function (fechainicial, fechafinal) {
  urlData <- "http://priceprofor.ekergy.es/omieinfosys/DataFileGenerator"
  w <- postForm(urlData,
  # .opts = list(postfields=toJSON(list(fechaini="2011-1-1", fechafin=as.character(Sys.Date()), market="ES")),
  .opts = list(postfields=toJSON(list(fechaini=fechainicial, fechafin=fechafinal , market="ES")),
  httpheader=c('Content-Type'='application/json', Accept='application/json')
  ))
  omiemi <- read.csv(text=w, sep = ";",  header=TRUE)

  return(omiemi)
}

# inicialmente los datos en omiemi tienen un periodo de un mes
omiemi <- omiefunc(firstDate, lastDate)

corfunc <- function (omiemi, metodo) {

  test_cor_HIDCONV <- list()
  test_cor_HIDBOMB <- list()
  test_cor_NUCLEAR <- list()
  test_cor_CARB <- list()
  test_cor_CARBIMP <- list()
  test_cor_CICLOCOMB <- list()
  test_cor_TOTAL_REGIMEN_ORD <- list()
  test_cor_REGIMEN <- list()

  # calculo de las correlaciones
  for(j in 1:24) {
    aux<-omiemi[omiemi$Hora==j-1,]
    test_cor_HIDCONV[j] <- cor(aux$HIDRAULICA_CONVENCIONAL, aux$Precio, method=metodo)
    test_cor_HIDBOMB[j]<-cor(aux$HIDRAULICA_BOMBEO_PURO, aux$Precio, method=metodo)
    test_cor_NUCLEAR[j]<-cor(aux$NUCLEAR, aux$Precio, method=metodo)
    test_cor_CARB[j]<-cor(aux$CARBON_NACIONAL, aux$Precio, method=metodo)
    test_cor_CARBIMP[j]<-cor(aux$CARBON_IMPORTACION, aux$Precio, method=metodo)
    test_cor_CICLOCOMB[j]<-cor(aux$CICLO_COMBINADO, aux$Precio, method=metodo)
    test_cor_TOTAL_REGIMEN_ORD[j]<-cor(aux$TOTAL_REGIMEN_ORDINARIO_CON_PRIMA, aux$Precio, method=metodo)
    test_cor_REGIMEN[j]<-cor(aux$REGIMEN_ESPECIAL_A_MERCADO, aux$Precio, method=metodo)
  }

result <- list(test_cor_HIDCONV, test_cor_HIDBOMB, test_cor_NUCLEAR,
               test_cor_CARB, test_cor_CARBIMP, test_cor_CICLOCOMB,
               test_cor_TOTAL_REGIMEN_ORD, test_cor_REGIMEN)
return(result)

}

metodo = "spearman"
result <- corfunc(omiemi, metodo)
test_cor_HIDCONV <- result[[1]]
test_cor_HIDBOMB <- result[[2]]
test_cor_NUCLEAR <- result[[3]]
test_cor_CARB <- result[[4]]
test_cor_CARBIMP <- result[[5]]
test_cor_CICLOCOMB <- result[[6]]
test_cor_TOTAL_REGIMEN_ORD <- result[[7]]
test_cor_REGIMEN <- result[[8]]
