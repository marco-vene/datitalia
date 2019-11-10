# Define the server
shinyServer( function(input, output) {
  

 # output$trendPlot <- renderGirafe({
#    ggiraph(code = print(trendLine(dati, input$gruppo, input$metrica)))
 # })
  
  output$trendPlot <- renderPlotly({
    trendLine(dati, input$gruppo, input$metrica, input$periodo)
  }) 
  
  
  output$description <- renderUI({
    
      if(input$metrica == "Popolazione")
      {
        includeMarkdown("def_popolazione.md")
      }
    else if(input$metrica == "Occupati")
    {
      includeMarkdown("def_occupati.md")
    }
    else if(input$metrica == "Disoccupati")
    {
      includeMarkdown("def_disoccupati.md")
    }
    else if(input$metrica == "Inattivi")
    {
      includeMarkdown("def_inattivi.md")
    }
    else if(input$metrica == "Tasso_Occupazione")
    {
      includeMarkdown("def_tasso_occ.md")
    }
    else if(input$metrica == "Tasso_Disoccupazione")
    {
      includeMarkdown("def_tasso_dis.md")
    }
    else if(input$metrica == "Tasso_Inattivita")
    {
      includeMarkdown("def_tasso_ina.md")
    }
     
      else{
        NULL
      }
    })
  

    
  })
  
  
