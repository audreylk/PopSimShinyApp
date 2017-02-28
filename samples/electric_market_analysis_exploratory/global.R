
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

# runApp("/home/david/workspace/shinyrcharts/samples/electric_market_analysis_exploratory/")
library(XML)
library(RCurl)
library(RJSONIO)
library(shiny)
library(devtools)
#install.packages('installr')
#install_github("ramnathv/rCharts")
#require("rCharts")
#library(rCharts)

# fecha de datos
urlStatus <- "http://priceprofor.ekergy.es/omieinfosys/status"
x <- getURLContent(urlStatus)
y <- fromJSON(x)
z <- y$StudyDataMIBEL$endrecdate
lastDate <- as.character(as.Date(z))

# fichero de datos
omiefunc <- function (fechainicial,fechafinal) {
  urlData <- "http://priceprofor.ekergy.es/omieinfosys/DataFileGenerator";
  w <- postForm(urlData,
       # .opts = list(postfields = toJSON(list(fechaini = "2011-1-1", fechafin = as.character(Sys.Date()), market = "ES")),
       .opts = list(postfields = toJSON(list(fechaini = fechainicial, fechafin = fechafinal , market = "ES")),
       httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
       ));
  omiemi <- read.csv(text=w, sep = ";",  header=TRUE);

  # genero la variable "time" que es la hora en formato "HH"
  for (i in 1:length(omiemi$Hora)){
    if (omiemi$Hora[i]<=9) {
      omiemi$time[i] <- paste(0, omiemi$Hora[i], sep="")
    }
    else{
      omiemi$time[i]<-omiemi$Hora[i]
    } 
  }
  
  # genero la variable "date" que es la fecha en formato "YY-mm-dd HH:MM:SS"
  Hora <- paste(omiemi$time, ":00:00", sep="")
  omiemi$date <- as.POSIXct(paste(omiemi$Fecha, Hora, sep=" "),"GMT")
  
  # elimino de la lista las variables "Fecha" y "Hora"
  omiemi <- omiemi[ ,!colnames(omiemi)=="Fecha"]
  omiemi <- omiemi[ ,!colnames(omiemi)=="Hora"]

  return(omiemi);
 }

# inicialmente omiemi es el ultimo dia con datos
omiemi <- omiefunc(lastDate,lastDate)
