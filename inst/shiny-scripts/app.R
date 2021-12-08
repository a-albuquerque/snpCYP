library(shiny)
# This example is adapted from
# Rundel, M.C. (2013). Tabset examples.
# URL:https://github.com/rstudio/shiny-examples/tree/main/006-tabsets

# UI for snpCYP app ----
ui <- fluidPage(

  # App title ----
  titlePanel("snpCYP"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the CYP isoform to analyze ----
      radioButtons("cyp", "CYP isoform:",
                   c("1A2" = "CYP1A2", "2B6" = "CYP2B6", "2C8" = "CYP2C8",
                     "2C9" = "CYP2C9", "2C19" = "CYP2C19", "2D6" = "CYP2D6",
                     "3A4" = "CYP3A4"), inline = TRUE),

      # Input of mutated CYP
      textInput("inputCYP", "Mutated CYP protein sequence (one-letter code),
                             e.g. a mutated CYP1A2",
                paste("AALSQSVPFSATELLLASAIFCLVFWVLKGLRPRVPKGLKSPPEPWGWPLLGHVLTLGKN",
                      "LALALSRMSQRYGDVLQIRIGSTPVLVLSRLDTIRQALVRQGDDFKGRPDLYTSTLITDG",
                      "TFSTFSTDSGPVWAARRRLAQNALNTFSIASDPASSSSCYLEEHVSKEAKALISRLQELM",
                      "GHFGHFDPYNQVVVSVANVIGAMCFGQHFPESSDEMLSLVKNTHEFVETASSGNPLDFFP",
                      "PNPALPNPALQRFKAFNQRFLWFLQKTVQEHYQKNSKNSVRDITGALFKHSKKGPRASGN",
                      "PQPQEKIVNLVNDIFGAGFDTVTTAISWSLMYLVTKPEIQRKIQKELDTVIGRERRPRLS",
                      "PQPQLPYLEAFILETFRHSSFLPFTIPHSTTRDTTLNGFYIPKKCCVFVNQWQVNHDPEL",
                      "EEDPSEFRPERFLTADGTAINKPLSEKMMLFGMGKRRCIGEVLAKWEIFLFLAILLQQLE",
                      "SSVPPGVKVDLTPIYGLTMKHARCEHVQARLRFSIN", sep="")),

      verbatimTextOutput("seq"),

      # Select groups of CYP forms for batch analysis
      checkboxGroupInput("cyps", "CYP isoforms (fill in only if batch analysis), to
                         try an example, select 1A2 and 2B6",
                         c("1A2" = "CYP1A2", "2B6" = "CYP2B6", "2C8" = "CYP2C8",
                           "2C9" = "CYP2C9", "2C19" = "CYP2C19", "2D6" = "CYP2D6",
                           "3A4" = "CYP3A4"), inline = TRUE, selected = c("CYP1A2", "CYP2B6")),
      textOutput("txt"),

      # Input of mutated CYP1A2 isoform
      textInput("input1A2", "Mutated CYP1A2 protein sequence (one-letter code)
                (fill in only if batch analysis)",
                paste("ASASQSVPFSATELLLASAIFCLVFWVLKGLRPRVPKGLKSPPEPWGWPLLGHVLTLGKN",
                      "PHLALSRMSQRYGDVLQIRIGSTPVLVLSRLDTIRQALVRQGDDFKGRPDLYTSTLITDG",
                      "QSLTFSTDSGPVWAARRRLAQNALNTFSIASDPASSSSCYLEEHVSKEAKALISRLQELM",
                      "AGPGHFDPYNQVVVSVANVIGAMCFGQHFPESSDEMLSLVKNTHEFVETASSGNPLDFFP",
                      "ILRYLPNPALQRFKAFNQRFLWFLQKTVQEHYQDFDKNSVRDITGALFKHSKKGPRASGN",
                      "LIPQEKIVNLVNDIFGAGFDTVTTAISWSLMYLVTKPEIQRKIQKELDTVIGRERRPRLS",
                      "DRPQLPYLEAFILETFRHSSFLPFTIPHSTTRDTTLNGFYIPKKCCVFVNQWQVNHDPEL",
                      "WEDPSEFRPERFLTADGTAINKPLSEKMMLFGMGKRRCIGEVLAKWEIFLFLAILLQQLE",
                      "FSVPPGVKVDLTPIYGLTMKHARCEHVQARLRFSIN", sep="")),

      verbatimTextOutput("1A2seq"),


      # Input of mutated CYP2B6 isoform
      textInput("input2B6", "Mutated CYP2B6 protein sequence (one-letter code)
                (fill in only if batch analysis)",
                paste("MEL",
                     "SVLLFLALLTGLLLLLVQRHPNTHDRLPPGPRPLPLLGNLLQMDRRGLLKSFLRFRE",
                     "KYGDVFTVHLGPRPVVMLCGVEAIREALVDKAEAFSGRGKIAMVDPFFRGYGVIFANGNR",
                     "WKVLRRFSVTTMRDFGMGKRSVEERIQEEAQCLIEELRKSKGALMDPTFLFQSITANIIC",
                      "SIVFGKRFHYQDQEFLKMLNLFYQTFSLISSVFGQLFELFSGFLKYFPGAHRQVYKNLQE",
                      "INAYIGHSVEKHRETLDPSAPKDLIDTYLLHMEKEKSNAHSENAHQNLNLNTLSLFFAGT",
                      "ETTSTTLRYGFLLMLKYPHVAERVYREIEQVIGPHRPPELHDRAKMPYTEAVIYEIQRFS",
                      "DLLPMGVPHIVTQHTSFRGYIIPKDTEVFLILSTALHDPHYFEKPDAFNPDHFLDANGAL",
                      "KKTEAFIPFSLGKRICLGEGIARAELFLFFTTILQNFSMASPVAPEDIDLTPQECGVGKI",
                      "PPTYQIRFLPR", sep="")),

      verbatimTextOutput("2B6seq"),

      # Input of mutated CYP2C8 isoform
      textInput("input2C8", "Mutated CYP2C8 protein sequence (one-letter code)
                (fill in only if batch analysis)",""),

      verbatimTextOutput("2C8seq"),

      # Input of mutated CYP2C9 isoform
      textInput("input2C9", "Mutated CYP2C9 protein sequence (one-letter code)
                (fill in only if batch analysis)",""),

      verbatimTextOutput("2C9seq"),

      # Input of mutated CYP2C19 isoform
      textInput("input2C19", "Mutated CYP2C19 protein sequence (one-letter code)
                (fill in only if batch analysis)",""),

      verbatimTextOutput("2C19seq"),

      # Input of mutated CYP2D6 isoform
      textInput("input2D6", "Mutated CYP2D6 protein sequence (one-letter code)
                (fill in only if batch analysis)",""),

      verbatimTextOutput("2D6seq"),

      # Input of mutated CYP3A4 isoform
      textInput("input3A4", "Mutated CYP3A4 protein sequence (one-letter code)
                (fill in only if batch analysis)",""),

      verbatimTextOutput("3A4seq")

    ),


    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ Associated drugs, snp distribution across mutated
      # CYP protein section, snp distribution across different CYP and the
      # position of each snp
      tabsetPanel(type = "tabs",
                  tabPanel("Associated Drugs", tableOutput("drugs")),
                  tabPanel("SNP distribution", plotOutput("dist")),
                  tabPanel("CYP detection (batch)", verbatimTextOutput("detec")),
                  tabPanel("SNP detected positions (batch)", verbatimTextOutput("positions"))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested query ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  drugList <- reactive({
    snpToDrug(input$cyp)})

  snpDistribution <- reactive({
    snpDist(input$cyp, input$inputCYP)})

  snpDetection <- reactive({
    detectSNP(input$cyps,
              list(input$input1A2, input$input2B6)
              )})

  # Generate a plot of the distribution of snp across different sections
  # of the chosen CYP isoform
  output$dist <- renderPlot({
    snpDistribution()
  })

  # Generate an HTML table of potentially affected drugs ----
  output$drugs <- renderTable({
    drugList()
  })

  # Generate a plot with the distribuiton of snp across all the provided
  # mutaded CYP isoforms, as compared to their wide-type sequences
  output$detec <- renderPlot({
    snpDetection()
  })

  # Generate the position of each snp across all the provided
  # mutated CYP isoforms, as compared to their wide-type sequences
  output$positions <- renderPrint({
    snpDetection()
  })

  # To output processed sequence input, uncomment below
  # output$seq <- renderText({ input$inputCYP })



}

# Create Shiny app ----
shinyApp(ui, server)


# [END]
