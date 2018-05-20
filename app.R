## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
#library(ggpubr)
library(rbokeh)
library(RColorBrewer)
library(formattable)

#df <- readRDS("data/df.rds")
source("R/process-facs.R")

# the log transfrom
#dflog <- df %>% dplyr::mutate_at(vars(FSC.A:FL4.H), logtrans.fcs)

# the sample names to populate the input$selectSamples
#sampleNames <- as.factor(levels(df$sample)) %>% as.character()

header <- dashboardHeader()


sidebar <- dashboardSidebar(
                    fileInput("files", "Choose FCS files",
                               multiple = TRUE,
                                accept = ".fcs"),
                    
                    #selectizeInput("selectSamples", "Select samples", choices = NULL, multiple = TRUE),
                    
                    selectizeInput("sampleData", "Sampling of datapoints", 
                                   choices = c("1/1000" = "0.001", 
                                               "1/100" = "0.01", 
                                               "1/10" = "0.1",
                                               "all data" = "1"),
                                   selected = "1/1000"),
                    
                    selectizeInput("selectX", "select X", choices = NULL, multiple = FALSE), 
                    
                    selectizeInput("selectY", "select Y", choices = NULL, multiple = FALSE),
                # the slider appears only when you go to the scatterplots tab
                conditionalPanel("input.tabs == 'Scatterplots'",
                    sliderInput("alpha", "alpha", 0.1, 1, 0.5)) # interesting, note input.tabs
                    )

body <- dashboardBody(
  tabsetPanel(id = "tabs",
    tabPanel("Check flow", 
             plotOutput("plot0", height = 600),
             textOutput("getip")
             ),
    tabPanel("2D density plots", 
             #box(width = 12,
            
             selectizeInput("bins", "Number of bins", choices = c(64, 128, 256), selected = 64),
            
            
            radioButtons("transf", "Fill transformation", choices = c("sqrt", "log"), selected = "sqrt", inline = T),
            
             
                    downloadLink("plot2download"),
                    
             
             
             plotOutput("plot2", height = 600)
             
             ), 
    tabPanel("Scatterplots", 
            #box(width = 12,
            downloadLink("plot3download"),
            plotOutput("plot3", height = 600,
                        brush = brushOpts(id = "gate", fill = NA, stroke = "black")),
            h4("Selected points"),
            tableOutput("gate_info")
            )
            #)
    )
  
)
ui <- dashboardPage(header, sidebar, body) 
  

server <- function(input, output, session) {
options(shiny.maxRequestSize=100*1024^2) 
 

### read files into df and log transform
  dfX <- reactive({
    
   inFiles <- input$files$datapath # critical, see example script readmultiplefiles-shiny.R or https://itsalocke.com/upload-multiple-files-in-shiny-and-consolidate-into-a-dataset/
  # if(is.null(inFiles))
  # {return(NULL)}  
   inNames <- input$files$name
  # if(is.null(inNames))
  # {return(NULL)}
  
  validate(
    need(expr = !is.null(input$files$datapath), "Please select fcs files first")
  )
  
  map2_df(inFiles, inNames, read.fcs) %>% dplyr::mutate_at(vars(1:(ncol(.) - 3)), logtrans.fcs) # here presumed that the last 3 columns of the fcs file are channels (Time + 2 made by process-facs.R)
  })


# sample fraction based on input, note that first sampling is done, then filter on sample
dfsampled <- reactive({
  dfX() %>% sample_frac(size = as.numeric(input$sampleData), replace = F) %>% na.omit()
  
  })
# make a df using coordinates of selected points, to serve as a gate
  dfgate <- reactive({
    selectedPoints <- dfsampled() %>% brushedPoints(input$gate)
     if(nrow(selectedPoints) == 0)
       return(dfsampled()[0, ])   #if nothing brushed, return empty dfsampled() dataframe
    xmin <- min(selectedPoints[[input$selectX]])
    xmax <- max(selectedPoints[[input$selectX]])
    ymin <- min(selectedPoints[[input$selectY]])
    ymax <- max(selectedPoints[[input$selectY]])
    
    gatetmp <- dfsampled()[between(dfsampled()[[input$selectX]], xmin, xmax), ]
    gatetmp[between(gatetmp[[input$selectY]], ymin, ymax), ]
                                     
                                     
})


observe({
  x <- names(dfX())
  updateSelectizeInput(session, "selectX", choices = x, selected = x[1])
  updateSelectizeInput(session, "selectY", choices = x, selected = x[2])  
})


#plot0
   output$plot0 <- renderPlot({
     dfsampled() %>% gather(param, value, 1:5) %>%
       ggplot(aes_string("Time", "value")) +
       geom_point(aes(color = param), alpha = input$alpha, stroke = 0) +
       theme_bw() +
       facet_grid(param ~ sample, scales = "free_x") +
       scale_color_brewer(type = "div", palette = "Spectral") +
       guides(color = FALSE) +
       ggtitle(label = "Signal vs time (min)" , subtitle =  "showing the first 5 channels only")
       
   }, res = 120)
  
  
# plot1
  
# plot2  
plot2 <- function() {
    dfX() %>%
    ggplot(aes_string(x = input$selectX, y = input$selectY)) +
    geom_hex(bins = as.numeric(input$bins)) +
    theme_bw() +
    facet_wrap( ~ sample) +
    scale_fill_distiller(palette = "Spectral", trans = input$transf) +
    theme(aspect.ratio = 1) +
    guides(fill = FALSE) +
    xlab(label = paste("log10(", input$selectX, ")", sep = "")) +
    ylab(label = paste("log10(", input$selectY, ")", sep = ""))
}
  output$plot2 <- renderPlot({
    
    plot2()
    
  }, res = 120)
  
  
  
# plot3
  plot3 <- function() {
    dfsampled() %>%
      ggplot() +
      geom_point(aes_string(input$selectX, input$selectY), 
                 alpha = input$alpha, 
                 stroke = 0) +
      
      geom_point(aes_string(input$selectX, input$selectY), 
                 alpha = input$alpha*0.5, 
                 color = "red", 
                 stroke = 0,
                 data = dfgate()) + #### gated points
      
      theme_bw() +
      xlab(label = paste("log10(", input$selectX, ")", sep = "")) +
      ylab(label = paste("log10(", input$selectY, ")", sep = "")) +
      ggtitle(label = "Scatter plot", subtitle = paste("Showing ", input$sampleData, "of the events")) +
      facet_wrap( ~ sample) +
      theme(aspect.ratio = 1)
  }
  output$plot3 <- renderPlot({
   
    plot3()
    
  }, res = 120)
  
  output$gate_info <- renderTable({
     
      total <- table(dfsampled()$sample)
      selected <- table(dfgate()$sample)
      # how to deal with situations where not all samples have points selected?
      # e.g. length(selected) != length(total)
      
      totaldf <- tibble(sample = names(total), total = as.integer(total)) 
      if(length(selected) == 0)
        return()
      selecteddf <- tibble(sample = names(selected), selected = as.integer(selected)) 
      
      left_join(totaldf, selecteddf, by = "sample") %>%
      dplyr::mutate(percent = percent(selected/total))
     
               
  })
  

  
  
  # downloads
  output$plot2download <- downloadHandler(
    filename = "plot2.pdf",
    content = function(file) {
      ggsave(file, plot = plot2(), device = "pdf", width = 11, height = 8, units = "in")
    })  
  
  output$plot3download <- downloadHandler(
    filename = "plot2.pdf",
    content = function(file) {
      ggsave(file, plot = plot3(), device = "pdf", width = 11, height = 8, units = "in")
    })  
}

shinyApp(ui, server)
