
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

shinyUI(
  fluidPage(

    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="fonts/font-awesome-4.2.0/css/font-awesome.min.css")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/menu_topexpand.css")),
    tags$head(
      tags$script(src="js/snap.svg-min.js")),
    tags$body(class="content-wrap-menu"),
    
    includeHTML("www/elastic.html"),

    column(12,
           titlePanel(""), align="center"),

    column(12,
           # titulo del panel
           titlePanel("Electric Daily Market (Spain)"), align="center"),

    column(8, 
           # lista de las variables input
           selectInput("varInput", "Select a variable:", 
                          choices=colnames(omiemi)),
           # fechas inicial y final usadas en el graficado
           dateRangeInput('dateRange',
                          label = 'Date range:',
                          # start = as.Date("2011-1-1"), end = as.Date("2011-1-1"),
                          # start = Sys.Date(), end = Sys.Date(),
                          start=as.Date(lastDate), end=as.Date(lastDate),
                          # start = as.Date("2015-1-1"), end = as.Date("2015-1-7"),
                          language = 'en'
                          # language = 'es'
                          )
           ),

    column(4,
           # tabla resumen de estadisticas
           h5(strong("Table basic statistics:")),
           verbatimTextOutput("summary"), align="right"),
    
    
    column(12,
           # nota de aviso para hacer zoom
           "Pinch the chart to zoom in", align="right"),
    
    # grafica a mostrar y libreria empleada
    showOutput("highstock", "highcharts"),
    
    # seccion del panel principal
    mainPanel(),

    tags$script(src="js/classie.js"),
    tags$script(src="js/main.js")

  )
)
