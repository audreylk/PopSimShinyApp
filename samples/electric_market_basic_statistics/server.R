
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

shinyServer(function(input, output, session) {

  # tabla resumen de estadisticas de los valores graficados
  output$summary_Precio <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$Precio))
  })
  
  # tabla resumen de estadisticas de los valores graficados
  output$summary_HIDRAULICA_CONVENCIONAL <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$HIDRAULICA_CONVENCIONAL))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_HIDRAULICA_BOMBEO_PURO <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$HIDRAULICA_BOMBEO_PURO))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_NUCLEAR <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$NUCLEAR))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_CARBON_NACIONAL <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$CARBON_NACIONAL))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_CARBON_IMPORTACION <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$CARBON_IMPORTACION))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_CICLO_COMBINADO <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$CICLO_COMBINADO))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_REGIMEN_ESPECIAL_A_MERCADO <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$REGIMEN_ESPECIAL_A_MERCADO))
  })

  # tabla resumen de estadisticas de los valores graficados
  output$summary_TOTAL_REGIMEN_ORDINARIO_CON_PRIMA <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])$TOTAL_REGIMEN_ORDINARIO_CON_PRIMA))
  })
  
#   output$cor <- renderDataTable({cor(omiemiCOR,omiemiCOR)})
  
})
