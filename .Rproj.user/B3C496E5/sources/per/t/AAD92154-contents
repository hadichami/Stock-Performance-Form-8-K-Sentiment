#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# institution:  Albert-Ludwigs-University of Freiburg
# title:        Visualizing stock performance and Form 8-K sentiment using R’s shiny package
# author:       Hadi Chami
# contact:      hadichami@hotmail.com
# date:         25.06.2018
# 

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidytext)
library(tm)
library(wordcloud)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Select dashboard theme color
  skin = "blue",
  
  # Define Dashboard title
  dashboardHeader(
    title = "Shiny Dashboard"
  ),  
  
  dashboardSidebar(sidebarMenu(
    
    # Menu Item to visualize investor and stock market 
    menuItem(
      "Visualization",
      tabName = "visu", 
      icon = icon("line-chart")),
    
    # Menu Item with several information about the Dataset
    menuItem(
      "Dataset",
      tabName = "dset",
      icon = icon("table")),
    
    # Menu Item to plot a word cloud
    menuItem(
      "Word Cloud",
      tabName = "wcloud",
      icon = icon("cloud")),
    
    # Menu Item with the top 15 positive/negative words
    menuItem(
      "Top Words",
      tabName = "topwords",
      icon = icon("bar-chart"))
  )),
  
  dashboardBody(
    
    tabItems(
      
      # *** Content for the Visualization menu item *** #
      tabItem(tabName = "visu",
              fluidRow(
                
                # Insert a Date Range (from - to)
                box(
                  title = "Date Control", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  height = 142,
                  dateRangeInput("daterange", "Select Date", start = min(OLS.Data$Date), end = max(OLS.Data$Date))
                ),
                
                # Input to select between Informed and Noise Investor
                box(
                  title = "Investor Control", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("investor", label="Select Investor", choices = list("Informed" = "Informed", "Noise" = "Noise"), selected = "Informed")
                ),
                
                # Input to select between the different categories of investors
                box(
                  title = "Category Control", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("category", label="Select Category", choices = list("Full" = "Full", "Fact" = "Fact", "Fact Positive" = "Fact.Pos", "Fact Negative" = "Fact.Neg", "Emotions" = "Emotions", "Emotions Fact" = "EmotionsFact"), selected = "Full")
                ),
                
                # Insert a Alpha-transparency scale for ggplot
                box(
                  title = "Alpha Control",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  height = 142,
                  sliderInput("alpha", label = "Select Alpha", min = 0, max = 0.5, value = 0.2, step = 0.1)
                ),
                
                # Main plot - stock market with the selected date /investor / category / alpha
                box(
                  title = "Graph", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("plotVisu")
                ),   
                
                # Download button with different formats (png, eps, pdf)
                box(
                  title = "Download", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("formatVisu", "Document format", c("PNG"="png", "EPS"="eps", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("downloadVisu")
                ) 
              )
      ),
      
      # *** Content for the Dataset menu item *** #
      tabItem(tabName = "dset",
              fluidRow(
                
                # Info Box with the number of observation of the Dataset
                infoBox("Number of observation", nrow(OLS.Data), color = "light-blue", fill = TRUE, width = 6, icon = icon("arrow-circle-down")),
                
                # Info Box with the number of variables of the Dataset
                infoBox("Number of variables", ncol(OLS.Data), color = "light-blue", fill = TRUE, width = 6, icon = icon("arrow-circle-right")),
                
                # Select Informed Investor category to print the summary of the selected variable
                box(
                  title = "Variables", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 157,
                  selectInput("columnsDset", label="Select Variable", choices = colnames(OLS.Data))
                ), 

                # Print the summary of the selected Informed variable
                box(
                  title = "Summary Data Table", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 157,
                  verbatimTextOutput("summaryDset")
                ),
                
                # Plot correlation matrix table with inside scroll
                box(
                  title = "Correlation Matrix Table", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 6,
                  div(style = 'overflow-x: scroll', tableOutput('tableCmatrix'))
                ),
                
                # Plot correlation matrix for variable 1 - 12
                box(
                  title = "Correlation Matrix Graph", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 486,
                  plotOutput("plotCmatrix")
                ),
                
                # Download button with different formats (png, pdf)
                box(
                  title = "Download", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("formatDset", "Document format", c("PNG"="png", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("downloadDset")
                )
              )
      ),
      
      # *** Content for the Word Cloud menu item *** #
      tabItem(tabName = "wcloud",
              fluidRow(
               
                # Select document from 1-20
                box(
                  title = "Document Control",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("doc", label="Select Document", choices = c(1:20), selected = 1)
                ),
                
                # Slider to change frequency
                box(
                  title = "Frequency Control",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  height = 142,
                  uiOutput("slider")
                ),

                # Slider to change max words
                box(
                  title = "Number Control",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  height = 142,
                  uiOutput("slider2")
                ),
                  
                # Plot word cloud            
                box(
                  title = "Graph", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("plotWcloud")
                ),
                
                # Download button with different formats (png, pdf)
                box(
                  title = "Download", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("formatWcloud", "Document format", c("PNG"="png", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("downloadWcloud")
                )
        )
      ),
      
      # *** Content for the Top Words menu item *** #
      tabItem(tabName = "topwords",
              fluidRow(
                
                # Plot the top 15 positive words for the Informed Investor
                box(
                  title = "Informed Positive", 
                  status = "danger", 
                  solidHeader = TRUE,
                  plotOutput(outputId = "myPlotTwords1")
                ),
                
                # Plot the top 15 positive words for the Noise Investor
                box(
                  title = "Noise Positive", 
                  status = "danger", 
                  solidHeader = TRUE,
                  plotOutput(outputId = "myPlotTwords2")
                ),
                
                # Plot the top 15 negative words for the Informed Investor
                box(
                  title = "Informed Negative", 
                  status = "danger", 
                  solidHeader = TRUE,
                  plotOutput(outputId = "myPlotTwords3")
                ),
                
                # Plot the top 15 negative words for the Noise Investor
                box(
                  title = "Noise Negative", 
                  status = "danger", 
                  solidHeader = TRUE,
                  plotOutput(outputId = "myPlotTwords4")
                ),
 
                # Download selected plot in different formats (png, pdf)
                box(
                  title = "Download Single Plot", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("dSingle", label="Select Plot", choices = list("Informed Positive" = "InformedPos", "Informed Negative" = "InformedNeg", "Noise Positive" = "NoisePos", "Noise Negative" = "NoiseNeg"), selected = "Informed Positive"),
                  radioButtons("formatTwordsSingle", "Document format", c("PNG"="png", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("downloadTwordsSingle")
                ),
                  
                # Download all 4 plots in different formats (png, pdf)             
                box(
                  title = "Download All Plots", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  height = 236,
                  radioButtons("formatTwordsAll", "Document format", c("PNG"="png", "PDF"="pdf"), inline = TRUE, selected = "png"),
                  downloadButton("downloadTwordsAll")
                ) 
            )
        )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # *** Output for the Visualization menu item *** #
  
  # Define 3 variables used to concatenate 2 selected inputs
  parts <- reactiveValues(partA=NULL, partB=NULL, partC=NULL)

  # partA = selected investor
  observeEvent(input$investor, { 
    parts$partA <- as.character(input$investor)
  })
  
  # part B = selected category
  observeEvent(input$category, { 
    parts$partB <- as.character(input$category)
  })

  # Needed to transform them due an error
  OLS.Data$InformedEmotionsFact <- as.numeric(as.character(OLS.Data$InformedEmotionsFact))
  OLS.Data$NoiseEmotionsFact <- as.numeric(as.character(OLS.Data$NoiseEmotionsFact))
  
  # Output into the Visualization menu Item box
  output$plotVisu <- renderPlot({

    # partC = Concatenate partA + partB
    parts$partC <- as.character(paste(parts$partA, parts$partB,sep=""))
    
    # Filter the data due selected date range
    OLS.Data.filtered <- OLS.Data %>%

      filter(Date >= input$daterange[1], Date <= input$daterange[2])
     
    # Ensure that values are available 
    req(OLS.Data.filtered)
    
    OLS.Data.filtered %>%
      
      # ggplot filtered date + stock market data and selected investor + category
      ggplot(aes(x = Date, y = NYSE)) +
      
      # plot as line (recommended for stock market prices)
      geom_line(aes(color="S&P500")) +
      
      # plot title = selected Investor + category
      ggtitle(paste(input$investor,input$category,sep = "")) +
      
      # title bold
      theme(plot.title = element_text(hjust = 0.5,face="bold")) +
      
      # name of x and y axis
      labs(x = "Time", y = "S&P500") +
      
      # plot selected investor + category and alpha
      geom_line(aes(x = Date, y = OLS.Data.filtered[parts$partC], color = parts$partC)) +
      
      # alpha transparency 
      geom_line(alpha = rep(as.numeric(input$alpha), nrow(OLS.Data.filtered[parts$partC]))) +
      
      # neccessary for legend of the plot
      scale_colour_manual(values=c("red", "black"))
  })
  
  # Set file name with selected Investor and category (parts$partC) + format
  fn <- reactive({paste(parts$partC,input$formatVisu,sep = ".")})
  d <- reactive({input$formatVisu})
  
  # Download current plot
  output$downloadVisu <- downloadHandler(
    filename = fn,
    content = function(file) {
      
      ## ggsave saving the last plot that is displayed ; dpi for higher resolution
      ggsave(file, device=d(), dpi = 600, width = 297, height = 210, units = "mm")
    
    }
  )
  
  # *** Output for the Dataset menu item *** #

  # Print summary of selected Informed category
  output$summaryDset <- renderPrint({
    summary(OLS.Data[[input$columnsDset]]) 
  })
  
  m <- cor(OLS.Data[,c(1:12)])
  
  # Print correlation matrix table column 1-12
  output$tableCmatrix <- renderTable({
    
    m
    
  })
  
  # Plot correlation matrix column 1-12
  output$plotCmatrix <- renderPlot({
    
    colnames(m) <- c("IFull", "NFull", "IFact", "NFact", "IFactPos","NFactPos","IFactNeg","NFactNeg","IEmo","NEmo","IEmoFact","NEmoFact")
    rownames(m) <- c("IFull", "NFull", "IFact", "NFact", "IFactPos","NFactPos","IFactNeg","NFactNeg","IEmo","NEmo","IEmoFact","NEmoFact")
    
    corrplot(m,method = "circle",type = "upper",tl.col = "darkgrey")
  })
  
  fk <- reactive({paste("Dataset_Output",input$formatDset,sep = ".")})
  v  <- reactive({input$formatDset})
  
  # Download current plot
  output$downloadDset <- downloadHandler(
    filename = fk,
    content = function(file) {
      
      # check if selected format is png -> plot png
      if(input$formatDset == "png") {
        png(file)
        
        # check if selected format is pdf -> plot pdf
      } else if(input$formatDset == "pdf") {
        pdf(file)
      }

      # change col- and rownames to better fit in the plot
      colnames(m) <- c("IFull", "NFull", "IFact", "NFact", "IFactPos","NFactPos","IFactNeg","NFactNeg","IEmo","NEmo","IEmoFact","NEmoFact")
      rownames(m) <- c("IFull", "NFull", "IFact", "NFact", "IFactPos","NFactPos","IFactNeg","NFactNeg","IEmo","NEmo","IEmoFact","NEmoFact")
      corrplot(m,method = "circle",type = "upper",tl.col = "darkgrey")
      
      dev.off() 
    
    }
  )
  
  # *** Output for the Word Cloud menu item *** #
  
  # Filter by selected document
  filtered <- reactive({
    Wcloud.Data.filtered <- Wcloud.Data %>%
      filter(document == input$doc)
  }) 
  
  # Interactive slider based on selected document
  output$slider <- renderUI({
    sliderInput("minFreq", label = "Minimum Frequency", min = min(filtered()$count), max = max(filtered()$count), value = 1)
  })
  
  # Interactive slider based on selected document
  output$slider2 <- renderUI({
    sliderInput("maxNum", label = "Maximum Number of Words", min = 1, max = length(unique(filtered()$term)), value = 100)
  })
  
  # Plot word cloud based on filter and slider
  output$plotWcloud <- renderPlot({
  
    wordcloud(words = filtered()$term, freq = filtered()$count, min.freq = input$minFreq, max.words = input$maxNum, scale=c(3.5,0.25), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    
  })
  
  fw  <- reactive({paste("WordCloud_Output",input$formatWcloud,sep = ".")})
  cl  <- reactive({input$formatWcloud})
  
  # Download and save plot
  output$downloadWcloud <- downloadHandler(
    filename = fw,
    content = function(file) {
      
      # check if selected format is png -> plot png
      if(input$formatWcloud == "png") {
        png(file)
        
        # check if selected format is pdf -> plot pdf
      } else if(input$formatWcloud == "pdf") {
        pdf(file)
      }
      
      wordcloud(words = filtered()$term, freq = filtered()$count, 
                min.freq = input$minFreq, max.words = input$maxNum, random.order=FALSE, 
                rot.per=0.35, colors=brewer.pal(8, "Dark2"))
      
      # This function closes the specified plot
      dev.off() 
    }
  )

  # *** Output for the Top Words menu item *** #
  
  # Plot top 15 positive words for Informed Investor + lasso coefficient
  output$myPlotTwords1 <- renderPlot({
    barplot(TopWords$lassoInfPos, las = 2, names.arg = TopWords$informedPos, main = "Informed Investor Top 15 positive words", ylab = "Lasso coefficient")
  })
  
  # Plot top 15 positive words for Noise Investor + lasso coefficient
  output$myPlotTwords2 <- renderPlot({
    barplot(TopWords$lassoNoisePos , las = 2, names.arg = TopWords$noisePos, main = "Noise Investor Top 15 positive words", ylab = "Lasso coefficient")
  })
  
  # Plot top 15 negative words for Informed Investor + lasso coefficient
  output$myPlotTwords3 <- renderPlot({
    barplot(TopWords$lassoInfNeg, las = 2, names.arg = TopWords$informedNeg, main = "Informed Investor Top 15 negative words", ylab = "Lasso coefficient")
  })
  
  # Plot top 15 negative words for Informed Investor + lasso coefficient
  output$myPlotTwords4 <- renderPlot({
    barplot(TopWords$lassoNoiseNeg, las = 2, names.arg = TopWords$noiseNeg, main = "Noise Investor Top 15 negative words", ylab = "Lasso coefficient")
  })
  
  # Set file name with selected Investor (input$dSingle) + format
  fh <- reactive({paste(input$dSingle,input$formatTwordsSingle,sep = ".")})
  e <- reactive({input$formatTwordsSingle})
  
  # Download single barplot
  output$downloadTwordsSingle <- downloadHandler(
    filename = fh,
    content = function(file) {
      
      # check if selected format is png -> plot png
      if(input$formatTwordsSingle == "png") {
        png(file)
        
      # check if selected format is pdf -> plot pdf
      } else if(input$formatTwords == "pdf") {
        pdf(file)
      }

      # check selected investor -> if true -> plot
      if(input$dSingle == "InformedPos") {
        barplot(TopWords$lassoInfPos, las = 2, names.arg = TopWords$informedPos, main = "Informed Investor Top 15 positive words", ylab = "Lasso coefficient")
      }
      
      # check selected investor -> if true -> plot
      if(input$dSingle == "InformedNeg") {
        barplot(TopWords$lassoInfNeg, las = 2, names.arg = TopWords$informedNeg, main = "Informed Investor Top 15 negative words", ylab = "Lasso coefficient")
      }
      
      # check selected investor -> if true -> plot
      if(input$dSingle == "NoisePos") {
        barplot(TopWords$lassoNoisePos , las = 2, names.arg = TopWords$noisePos, main = "Noise Investor Top 15 positive words", ylab = "Lasso coefficient")
      }
      
      # check selected investor -> if true -> plot
      if(input$dSingle == "NoiseNeg") {
        barplot(TopWords$lassoNoiseNeg, las = 2, names.arg = TopWords$noiseNeg, main = "Noise Investor Top 15 negative words", ylab = "Lasso coefficient")
      }
    
      # This function closes the specified plot
      dev.off() 
    }
  )
  
  # Set file name + format
  fg <- reactive({paste("TopWordsAll",input$formatTwordsAll,sep = ".")})
  j <- reactive({input$formatTwordsAll})
  
  # Download all barplot in one page
  output$downloadTwordsAll <- downloadHandler(
    filename = fg,
    content = function(file) {
      
      # check if selected format is png -> plot png
      if(input$formatTwordsAll == "png") {
        png(file)
        
      # check if selected format is pdf -> plot pdf
      } else if(input$formatTwordsAll == "pdf") {
        pdf(file)
      }
      
      # set graphs in one page 2x2
      par(mfrow=c(2,2))
      barplot(TopWords$lassoInfPos, las = 2, names.arg = TopWords$informedPos, main = "Informed Investor Top 15 positive words", ylab = "Lasso coefficient")
      barplot(TopWords$lassoNoisePos , las = 2, names.arg = TopWords$noisePos, main = "Noise Investor Top 15 positive words", ylab = "Lasso coefficient")
      barplot(TopWords$lassoInfNeg, las = 2, names.arg = TopWords$informedNeg, main = "Informed Investor Top 15 negative words", ylab = "Lasso coefficient")
      barplot(TopWords$lassoNoiseNeg, las = 2, names.arg = TopWords$noiseNeg, main = "Noise Investor Top 15 negative words", ylab = "Lasso coefficient")
      
      # This function closes the specified plot
      dev.off() 
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

