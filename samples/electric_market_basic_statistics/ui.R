
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

  column(12, 
         # fechas inicial y final usadas en el graficado
         dateRangeInput('dateRange',
                        label = 'Date range:',
                        # start = as.Date("2011-1-1"), end = as.Date("2011-1-1"),
                        # start = Sys.Date(), end = Sys.Date(),
                        start = as.Date(lastDate), end = as.Date(lastDate),
                        # start = as.Date("2015-1-1"), end = as.Date("2015-1-7"),
                        language = 'en'
                        # language = 'es'
                        )
         ),

  
  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics Precio:")),
         verbatimTextOutput("summary_Precio"), align="left"),
    
  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics HIDRAULICA_CONVENCIONAL:")),
         verbatimTextOutput("summary_HIDRAULICA_CONVENCIONAL"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics HIDRAULICA_BOMBEO_PURO:")),
         verbatimTextOutput("summary_HIDRAULICA_BOMBEO_PURO"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics NUCLEAR:")),
         verbatimTextOutput("summary_NUCLEAR"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics CARBON_NACIONAL:")),
         verbatimTextOutput("summary_CARBON_NACIONAL"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics CARBON_IMPORTACION:")),
         verbatimTextOutput("summary_CARBON_IMPORTACION"), align="left"),
  
  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics CICLO_COMBINADO:")),
         verbatimTextOutput("summary_CICLO_COMBINADO"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics REGIMEN_ESPECIAL_A_MERCADO:")),
         verbatimTextOutput("summary_REGIMEN_ESPECIAL_A_MERCADO"), align="left"),

  column(12,
         # tabla resumen de estadisticas
         h5(strong("Table statistics TOTAL_REGIMEN_ORDINARIO_CON_PRIMA:")),
         verbatimTextOutput("summary_TOTAL_REGIMEN_ORDINARIO_CON_PRIMA"), align="left"),

#   column(12, dataTableOutput("cor")),

# seccion del panel principal
  mainPanel(),

  tags$script(src="js/classie.js"),
  tags$script(src="js/main.js")

))
