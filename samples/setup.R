
# Codigo R de instalacion de librerias que permite ejecutar las aplicaciones shiny
# que contiene esta carpeta de ejemplos. La instruccion runApp funciona en rstudio

# paquetes R necesarios para trabajar con aplicacines shiny
install.packages('Rcpp',contriburl = contrib.url('http://cran.r-project.org'))
install.packages('devtools',contriburl = contrib.url('http://cran.r-project.org'))
install.packages('shiny',contriburl = contrib.url('http://cran.r-project.org'))
install.packages('plyr',contriburl = contrib.url('http://cran.r-project.org'))
library('devtools');install_git('https://github.com/ramnathv/rCharts.git')

# paquetes R utiles para descargar y manipular datos de urls
install.packages("RCurl", contriburl = contrib.url("http://cran.r-project.org"))
install.packages("RJSONIO", contriburl = contrib.url("http://cran.r-project.org"))

# codigo R para ejecutar una aplicacion shiny usando RStudio
library('shiny')
# app_folder can be the absolute route to the samples folder
app_folder = "./electric_market_correlations_technologies_vs_price"
runApp(app_folder)
