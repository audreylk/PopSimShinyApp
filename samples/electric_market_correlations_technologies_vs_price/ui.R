
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

shinyUI(fluidPage(

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
         selectInput("metodo", "Select a method:", 
                     choices=c("spearman", "kendall", "pearson"),
                     selected = metodo)
         ),

  column(8, 
         # fechas inicial y final usadas en el graficado
         dateRangeInput('dateRange',
                        label='Date range:',
                        # start=as.Date("2011-1-1"), end=as.Date("2011-1-1"),
                        # start=Sys.Date(), end=Sys.Date(),
                        start=as.Date(firstDate), end=as.Date(lastDate),
                        language = 'en'
                        # language = 'es'
                        )
  ),
  
  # seccion del panel principal
  mainPanel(showOutput("myChart", "highcharts")),
  
  tags$script(src="js/classie.js"),
  tags$script(src="js/main.js")

))
