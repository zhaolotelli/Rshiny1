shinyServer(function(input, output) {
  output$shower <- reactive(input$shower)
  contents <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (!input$show)
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  output$text <- reactive({
    paste("成绩大于等于", input$obs, "的学生比率")
  })
  output$ave <- renderTable({
    if(is.null(contents()))
      return(NULL)
    else {
      data <- contents()
      colMeans(data[, input$ave.range[1]:input$ave.range[2]])
    }
  }, colnames = FALSE)
  output$ove <- renderTable({
    if(is.null(contents()))
      return(NULL)
    else {
      data <- contents()
      colSums(data[, input$ove.range[1]:input$ove.range[2]] >= input$obs)/nrow(data)
    }
  }, colnames = FALSE)
  output$lmsummary <- renderPrint({
    if(is.null(contents())) 
      return(NULL)
    else {
      data <- contents()
      y <- data[,input$yno]
      X <- data[,input$xno]
      fit.model <- lm(y ~ X)
      summary(fit.model)
    }
  })
  output$lmplot <- renderPlot({
    if(is.null(contents())) 
      return(NULL)
    else {
      data <- contents()
      y <- data[,input$yno]
      X <- data[,input$xno]
      fit.model <- lm(y ~ X)
      plot(X, y)
      abline(coef = fit.model$coef)
    }
  })
  output$pcasummary <- renderPrint({
    if(is.null(contents())) 
      return(NULL)
    else {
      data <- contents()
      X <- data[,input$pcano[1]:input$pcano[2]]
      pca.model <- princomp(X, cor = TRUE)
      summary(pca.model, loadings = TRUE, cutoff = 0.1, digit = 2)
    }
  })
})