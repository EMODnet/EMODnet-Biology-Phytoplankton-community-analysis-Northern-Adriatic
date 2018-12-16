library(shiny)

source("helper.R")

shinyServer(function(input, output,session) {

# the data used - updated whenever a new selection
        outVar1 = reactive({
            mydata = get(input$set1)
            names(mydata)[6:(length(mydata[1,]))]
            })
        observe({
            updateSelectInput(session, "spec.set1",choices = outVar1()
            )})
        # outVar2 = reactive({
        #   mydata = get(input$set2)
        #   names(mydata)[6:(length(mydata[1,]))]
        # })
        # observe({
        #   updateSelectInput(session, "spec.set2",choices = outVar2()
        #   )})
        # outVar3 = reactive({
        #   mydata = get(input$set3)
        #   names(mydata)[6:(length(mydata[1,]))]
        # })
        # observe({
        #   updateSelectInput(session, "spec.set3",choices = outVar3()
        #   )})
        
  
    output$plot1 <- renderPlot({
        plotspecs(input$set1,input$spec.set1,input$transform)
    }) 
  # output$plot2 <- renderPlot({
  #   plotspecs(input$set2,input$spec.set2,input$transform)
  # }) 
  # output$plot3 <- renderPlot({
  #   plotspecs(input$set3,input$spec.set3,input$transform)
  # }) 
  output$plot4 <- renderPlot({
    plotseas(input$set1,input$spec.set1,input$transform)
  }) 
  # output$plot5 <- renderPlot({
  #   plotseas(input$set2,input$spec.set2,input$transform)
  # }) 
  # output$plot6 <- renderPlot({
  #   plotseas(input$set3,input$spec.set3,input$transform)
  # }) 
  output$plM1 <- renderPlot({
    plotmultv(input$set1,input$spec.set1)
  })
  # output$plM2 <- renderPlot({
  #   plotmultv(input$set2,input$spec.set2)
  # })
  # output$plM3 <- renderPlot({
  #   plotmultv(input$set3,input$spec.set3)
  # })
  
    output$about <- renderUI({
      doc 
      })
})
