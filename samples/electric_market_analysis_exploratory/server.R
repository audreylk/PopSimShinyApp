
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

shinyServer(function(input, output, session) {

  # grafica highcharts
  output$highstock <- renderChart({
  
    # objeto highcharts
    p <- Highcharts$new()
    
    # boton de descarga del grafico
    p$exporting(enabled = T)
    
    # opcion de zoom del grafico
    p$chart(zoomType = "x")
    
    # titulo del propio grafico
    p$title(text = "Electric Market")
    
    # variables "x" e "y" que se dibujan en el grafico
    omiemi <- omiefunc(as.character(input$dateRange[1]), as.character(input$dateRange[2]))
    xInput <- as.numeric(as.POSIXct(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                                                     as.Date(date) <= input$dateRange[2])[,"date"])),"GMT") * 1000
    yInput <- c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                               as.Date(date) <= input$dateRange[2])[,input$varInput])
    
    # convierte el tipo de dato del eje X en datetime
    p$xAxis(type='datetime')
    
    # dibuja una linea vertical al seleccionar un punto
    p$set(tooltip = list(crosshairs= "true"))
    
    # definimos la linea que pintamos en el grafico
    p$series(name = input$varInput,
             var <- list(),
             for(i in 0:length(xInput)) var[i] <- list(list(xInput[i],yInput[i])),
             data = var)
    
    # a la variable indicada en el eje Y le anyado las unidades
    if (input$varInput=="Precio") {
      variable <- paste(input$varInput, "(Eur.)", sep=" ")
    }
    else{
      variable <- paste(input$varInput, "(MWh)", sep=" ")
    }  
    p$yAxis(title=list(text=variable))
    
    # parametros para dar dimensiones a la grafica
    p$addParams(height = 400, width=1000, dom="highstock")
    
    # pinta la grafica
    print(p)
  })
  
  # tabla resumen de estadisticas de los valores graficados
  output$summary <- renderPrint({
    omiemi <- omiefunc(as.character(input$dateRange[1]), as.character(input$dateRange[2]))
    summary(c(subset(omiemi, as.Date(date) >= input$dateRange[1] &
                             as.Date(date) <= input$dateRange[2])[,input$varInput]))
  })

})
