# Screen shot the table

#### packages ----------------------------------------------
library(shiny)
library(shinyscreenshot)
library(DT)
library(gridExtra)
library(ggpubr)
library(tidyverse)

## data ----------------------------------------------------
dat <- mtcars %>%
  mutate(cyl = as.factor(cyl),
         car_type = rownames(.)) %>%
  relocate(car_type, .before = mpg)

######################################
ui <- fluidPage(
  
  titlePanel(
    title = h1("Cars!", align = "center")
  ),
  
  sidebarPanel(
    
    width = 2,
    
    actionButton("go", "Screenshot Report"),
    
    selectInput("cyl",
                label = "Choose Cylinder:",
                choices = sort(unique(dat$cyl)),
                selected = NULL,
                multiple = FALSE)
    
  ),
  
  mainPanel(
    
    fluidRow(
      column(width = 6,  plotOutput(outputId = "plt1")),
      column(width = 6, DTOutput(outputId = "tbl")),
    ),
    br(),
    plotOutput(outputId = "plt2")
  )
)


server <- function(input, output){
  
  ## filter cylinder
  cyl_df <- reactive({
    
    req(input$cyl)
    
    d <- dat %>%
      filter(cyl == input$cyl)
    d
    
  })
  
  
  ## output plt1
  output$plt1 <- renderPlot({
    
    cyl_df() %>%
      ggplot(aes(x = wt, y = mpg)) +
      geom_point(size = 4) +
      theme_bw() +
      labs(x = "wt",
           y = "mpg",
           title = "mpg ~ wt") +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            plot.title = element_text(size = 20))
    
  })
  
  ## output table
  output$tbl <- renderDT({
    
    cyl_df() %>%
      datatable(class = 'cell-border stripe',
                rownames = FALSE,
                filter = "top",
                options = list(pageLength = 4),
                colnames = c("Car Type", "MPG", "CYL", "DISP", "HP", "DRAT", "WT", "QSEC", "VS", "AM", "GEAR", "CARB"))
    
  })
  
  ## output plt2
  output$plt2 <- renderPlot({
    
    cyl_df() %>%
      ggplot(aes(x = disp, y = hp)) +
      geom_point(size = 4) +
      theme_bw() +
      labs(x = "disp",
           y = "hp",
           title = "hp ~ disp") +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            plot.title = element_text(size = 20))
    
    
  })
  
  observeEvent(input$go, {
    screenshot()
  })
}


shinyApp(ui, server)

