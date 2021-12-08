library(shiny)
# This example is adapted from
# Rundel, M.C. (2013). Tabset examples.
# URL:https://github.com/rstudio/shiny-examples/tree/main/006-tabsets

# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Tabsets"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the random distribution type ----
      radioButtons("cyp", "CYP isoform:",
                   c("1A2" = "CYP1A2", "2B6" = "CYP2B6", "2C8" = "CYP2C8",
                     "2C9" = "CYP2C9", "2C19" = "CYP2C19", "2D6" = "CYP2D6",
                     "3A4" = "CYP3A4"))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("SNP detection", plotOutput("detect")),
                  tabPanel("CYP distribution (batch)", verbatimTextOutput("dist")),
                  tabPanel("Associated Drugs", tableOutput("drugs"))
      )

    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    snpToDrug(input$cyp)
  })

  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })

  # Generate an HTML table of potentially affected drugs ----
  output$drugs <- renderTable({
    d()
  })

}

# Create Shiny app ----
shinyApp(ui, server)


# [END]
