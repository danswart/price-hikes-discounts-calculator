# Price Hikes and Discounts Strategy Calculator - a Shiny app

# Shiny app for Chapter ? Exercise ?-? from the book 'Reimagining Your Business, A Personalized Guidebook'

# Deployed to shinyapps.io Nov 13, 2023

# PUSH to GitHub any time the app is updated
# REPUBLISH via RStudio any time the app is updated

# Load required libraries
library(shiny)
library(rmarkdown)
library(dplyr)
library(scales)
library(gridExtra)
library(DT)  # Load DT for datatable
library(shinyscreenshot)


# Define UI
ui <- fluidPage(
  
  ## ADD HIGHLIGHTING
  
  tags$head(
    tags$style(
      HTML(
        "
      /* Add yellow highlight to input fields */
      
        .shiny-input-container input[type='number'],
        .shiny-input-container input[type='text'] {
          background-color: yellow;
        }
        
        /* Add yellow highlight to output field */
      
        .shiny-output {
          background-color: yellow;
        }
        "
      )
    )
  ),
  
  ## DEFINE TITLES AND FORMATS
  
  titlePanel("Expected Consequences of Price Hikes or Discounts on Sales Calculator"),
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Screenshot Report"),
      ## DEFINE USER INPUTS, LABELS & DEFAULT VALUES (numericInput is for handling numeric values, not strings)
      
      numericInput("OUS",
                   "Number of Units You Sell Annually:",
                   value = 300),
      numericInput("OSP", 
                   "Your Selling Price per Unit ($):", 
                   value = 500),
      numericInput("OGPPi",
                   "Your Gross Profit % (as integer):",
                   value = 40),
      numericInput("PIPi",
                   "Proposed % Price Change (+/-):",
                   value = 5, step = 0.01),
      
      ## DEFINE BUTTONS
      
      actionButton("calculateBtn", "Calculate"),
      downloadButton("downloadPDF", "Download PDF"),
    ),
      # DEFINE WHAT APPEARS IN MAIN DISPLAY PANEL (THE PLOT AND THE CALCULATION DETAILS)
    
    mainPanel(
      h3("Percent Change in Customers to Maintain the Same Gross Profit:"),
      tableOutput("outputTable"),
      h3("Calculation Details Table:"),
      DTOutput("calculationTable"))  # Use DTOutput instead of tableOutput for DataTable
      

  )
)


##################################

server <- function(input, output) {
  
  ## COLLECT THE VARIABLES AND LABELS NEEDED USING REACTIVE() FUNCTIONS
  
  # Use the eventReactive() function to return a list of the 13 variables calculated and label result 'calculate'
  
  calculate <- eventReactive(input$calculateBtn, {
    OTR <- input$OUS * input$OSP
    OGPd <- input$OGPPi / 100
    OGP <- OTR * OGPd
    OCOGS <- OTR - OGP
    PIPd <- input$PIPi / 100
    NSP <- input$OSP * (1 + PIPd)
    OCPUS <- OCOGS / input$OUS
    NGPPi <- round(((NSP - OCPUS) / NSP) * 100, 4)
    SCFi <- round(OGP / NGPPi, 2)
    NCBi <- ((SCFi / NSP) * 100)
    PCUCLi <- ((NCBi - input$OUS) / input$OUS) * 100
    NGPPd <- round(NGPPi / 100, 4)
    NCOGS <- round((OCPUS * NCBi), 2)
    OGPPd <- (input$OGPPi/100)
    
    

    # Construct a data.frame called calc_table. This data frame contains two columns, "Variables" and "Value". The "Variables" column contains labels for different variables, and the "Value" column contains values associated with those variables.
    ## 16 variable labels in column 1
    
    calc_table <- data.frame(
      "Variables" = c(
        "Units You Sell Annually", # input
        "Your Selling Price per Unit", # input
        "Your Gross Profit % (as integer)", # input
        "Proposed % Price Change", # inmput
        "Total Revenue", # calc
        "Gross Profit", # calc
        "Cost of Goods Sold", # calc
        "New Selling Price per Unit", # calc
        "New Gross Profit % (as decimal)", # calc
        "Original Gross Profit % (as decimal)", # input
        "Scaling Factor to Convert GPM (as integer) into Sales", # calc
        "New Cost of Goods Sold", # calc
        "Cost per Unit Sold", # calc
        "New Customer Base", # calc
        "New Gross Profit % (as integer)", # calc
        "% Change in Customers (+/-) Without Changing Your Profit" # calc
      ),

      # (The values column.  16 amounts in column 2)
      
      "Value" = c(
        input$OUS, 
        dollar(input$OSP),
        paste(input$OGPPi, "%", sep = ""),
        paste(input$PIPi, "%", sep = ""),
        dollar(OTR),
        dollar(OGP), 
        dollar(OCOGS), 
        dollar(NSP), 
        percent(NGPPd), 
        percent(OGPPd),
        SCFi,
        dollar(NCOGS), 
        dollar(OCPUS),
        as.integer(NCBi), 
        (NGPPi), 
        (PCUCLi)
        
      )
    )
    

    # CONSOLIDATE INTO A LIST ALL THE NUMERIC VARIABLES, ALONG WITH THE DATA.FRAME NAMED calc_table CONTAINING 16 SELECTED VARIABLE NAMES IN COLUMN 1 AND 16 NUMERIC VALUES IN COLUMN 2
    
    list(
      OTR = OTR,
      OGP = OGP,
      OCOGS = OCOGS,
      NSP = NSP,
      NGPPd = NGPPd, 
      SCFi = SCFi, 
      NGPPd = NGPPd,
      NCOGS = NCOGS, 
      OCPUS = OCPUS, 
      NCBi = NCBi, 
      NGPPd = NGPPd,
      NGPPi = NGPPi,
      PCUCLi = PCUCLi,
      calc_table = calc_table
    )
  })
  
  # USE renderTable() TO CREATE object called 'outputTable' for use in the display
  
  output$outputTable <- renderTable({
    result <- calculate()
    data.frame(
      "Variable" = "% Change in Customers (+/-) Without Changing Your Profit",
      "Value" = percent(result$PCUCLi / 100)  
    )
  }, rownames = FALSE)
  
  # USING renderDT() function FROM THE DT PACKAGE, CREATE OBJECT CALLED calculationTable to dispaly the 16 Variable Labels and 16 Amounts found in the 'calculate' OBJECT ON MAIN PANEL
  
  output$calculationTable <- renderDT({
    result <- calculate()$calc_table
    datatable(result, escape = FALSE, options = list(dom = 't', paging = FALSE))
  })

 # DOWNLOADHANDLER contains 2 arguments as functions, namely FILENAME AND CONTENT
  
  output$downloadPDF <- downloadHandler(
    
    # BUILD THE FILENAME WITH PASTE
    
   filename =  function() {
     paste("pricing_strategy_calculator_report_",
           "pdf",
           sep = ".")
   },
   
   # CONTENT is a function with argument (file). CONTENT writes the plot or table to the appropriae downloading device
   ### I HEARD YOU CANNOT FEED A DT TABLE TO THE PDF() ENGINE !!!
   
   content = function(file) {
     if(input$var3 == "png") {
       # Specify width and height for PNG device if necessary
       png(file, width = 800, height = 600)
     } else {
       pdf(file)
     }
     
     # Create the first data frame
     
     result_outputTable <- calculate()
     df_outputTable <- data.frame(
       "Variable" = "% Change in Customers (+/-) Without Changing Your Profit",
       "Value" = percent(result_outputTable$PCUCLi / 100)
     )
     
     # Output the first table
     grid.table(df_outputTable, rows = NULL)
     
     # Create and output the second table
     
     result_calcTable <- calculate()$calc_table
     
     grid.table(result_calcTable, rows = NULL)
     
     dev.off()  # turn the device off
  
     observeEvent(input$go, {
       # Take a screenshot only when the 'go' button is pressed
       if (input$go > 0) {
         screenshot(download = TRUE,
                    filename = "pricing_strategy_calculator_report")
       }
     })
     
   }
  )
}
  

# Run the Shiny app
shinyApp(ui, server)
