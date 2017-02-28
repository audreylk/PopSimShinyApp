
# Pagina Web para el modelado, el analisis y la explotacion de datos del Mercado Electrico
# http://priceprofor.ekergy.es/

shinyServer(function(input, output) {

  # grafica highcharts
  output$myChart <- renderChart2({
    
    # objeto highcharts
    hc <- Highcharts$new()
    
    # tipo de grafico escogido
    hc$chart(type="spline")
    
    # titulo del propio grafico
    hc$title(text="Technologies vs. Price")
    
    # boton de descarga del grafico
    hc$exporting(enabled=T)
    
    # formateo los ejes X e Y
    hc$xAxis(title=list(text="Hora"), allowDecimals=F)
    hc$yAxis(title=list(text=paste("Correlacion", input$metodo, sep=" ")))
    
    # parametros para dar dimensiones a la grafica
    hc$addParams(height=400, width=1000, dom="highstock")
    # hc$addParams(height = 500, width=1250, dom="highstock")
    
    # agrego una leyenda para identificar las lineas
    hc$legend(symbolWidth = 0)
    # hc$legend(symbolWidth = 80)

    # preselecciona los datos escogidos en la interfaz
    omiemi <- omiefunc(as.character(input$dateRange[1]),
                       as.character(input$dateRange[2]))

    # realiza el calculo de las correlaciones
    result <- corfunc(omiemi, as.character(input$metodo))
    test_cor_HIDCONV <- result[[1]]
    test_cor_HIDBOMB <- result[[2]]
    test_cor_NUCLEAR <- result[[3]]
    test_cor_CARB <- result[[4]]
    test_cor_CARBIMP <- result[[5]]
    test_cor_CICLOCOMB <- result[[6]]
    test_cor_TOTAL_REGIMEN_ORD <- result[[7]]
    test_cor_REGIMEN <- result[[8]]

    # definimos las lineas que pintamos en el grafico
    hc$series(data=test_cor_HIDCONV, name="HIDRAULICA_CONVENCIONAL")
    hc$series(data=test_cor_HIDBOMB, name="HIDRAULICA_BOMBEO_PURO")
    hc$series(data=test_cor_NUCLEAR, name="NUCLEAR")
    hc$series(data=test_cor_CARB, name="CARBON_NACIONAL")
    hc$series(data=test_cor_CARBIMP, name="CARBON_IMPORTACION")
    hc$series(data=test_cor_CICLOCOMB, name="CICLO_COMBINADO")
    hc$series(data=test_cor_TOTAL_REGIMEN_ORD, name="TOTAL_REGIMEN_ORDINARIO_CON_PRIMA")
    hc$series(data=test_cor_REGIMEN, name="REGIMEN_ESPECIAL_A_MERCADO")

    # pinta la grafica
    return(hc)
  })

})
