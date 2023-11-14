# Price Hikes and Discounts Strategy Calculator - a Shiny app

# Shiny app for Chapter ? Exercise ?-? from the book 'Reimagining Your Business, A Personalized Guidebook'

# Deployed to shinyapps.io Nov 13, 2023

# PUSH to GitHub any time the app is updated
# REPUBLISH via RStudio any time the app is updated

# Load required libraries
library(shiny)
library(dplyr)
library(scales)
library(grid)
library(DT)  # Load DT for datatable

# Define UI
ui <- fluidPage(
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
  titlePanel("Pricing Strategy Calculator"),
  
  # Input fields
  sidebarLayout(
    sidebarPanel(
      numericInput("OUS", "Number of Units You Sell Annually:", value = 300),
      numericInput("OSP", "Your Selling Price per Unit ($):", value = 500),
      numericInput("OGPPi", "Your Gross Profit % (as integer):", value = 40),
      numericInput("PIPi", "Proposed % Price Change (+/-):", value = 5, step = 0.01),
      actionButton("calculateBtn", "Calculate"),
      downloadButton("downloadPDF", "Save to PDF")
    ),
    
    # Output fields
    mainPanel(
      h3("Percent Change in Customers to Maintain the Same Gross Profit:"),
      tableOutput("outputTable"),
      h3("Calculation Details Table:"),
      DTOutput("calculationTable")  # Use DTOutput instead of tableOutput for DataTable
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Calculate function (13 variables calculated)
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
    
    # Create a data frame called calc_table for the calculation table section (16 'Variables' labels, )
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

      # (16 'Values' consisting of 4 inputs + 12 calculated values)
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
    

    # Return the results (13 values total in list (last is 'calc_table' which holds the 16 'Variables' names))
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
  
  # Output the selected variable in the Output section
  output$outputTable <- renderTable({
    result <- calculate()
    data.frame(
      "Variable" = "% Change in Customers (+/-) Without Changing Your Profit",
      "Value" = percent(result$PCUCLi / 100)  
    )
  }, rownames = FALSE)
  
  # Output the calculation table using DT::renderDT
  output$calculationTable <- renderDT({
    result <- calculate()$calc_table
    datatable(result, escape = FALSE, options = list(dom = 't', paging = FALSE))
  })

  # Generate PDF report when the Save to PDF button is clicked
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("pricing_strategy_calculator_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Extract the content of the DT table for the PDF
      table_content <- calculate()$calc_table
      
      # Generating a PDF with the table content
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.create(tempReport)
      dateTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      writeLines(c(
        "---",
        "title: 'Pricing Strategy Calculator Report'",
        "author: 'Generated on", dateTime, "'",
        "output: pdf_document",
        "geometry: margin=1in",
        "---",
        "",
        "Here is the Pricing Strategy Calculator Report:",
        "",
        "Calculation Table:",
        "",
        "```{r, results='asis'}",
        "datatable(table_content, escape = FALSE, options = list(dom = 't', paging = FALSE))",
        "```"
      ), tempReport)
      rmarkdown::render(tempReport, output_file = file)
    },
    contentType = "application/pdf"
  )
}

# Run the Shiny app
shinyApp(ui, server)
