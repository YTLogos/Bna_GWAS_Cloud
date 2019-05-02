library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(colourpicker)

source("gwas_emmax_cov.R")
options(shiny.maxRequestSize = 500*1024^2)

header <- dashboardHeader(title = "GWAS-Cloud")
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("user")),
    menuItem("GWAS", tabName = "gwas", icon = icon("galactic-senate")),
    menuItem("Visualization", tabName = "vis", icon = icon("braille")),
    menuItem("Extraction",tabName = "extraction", icon = icon("accusoft")),
    menuItem("Annotation", tabName = "anno", icon = icon("list")),
    menuItem("Feedback", tabName = "feedback", icon = icon("comment-alt"))
  )
)

body <- dashboardBody(
  
  tabItems(
    
# -------------------tabItem: Introduction of GWAS------------------------------------
    tabItem(tabName = "intro",
            htmlOutput(outputId = "GWAS_Introduction")
    ),
    
# -----------------------tabitem: run gwas --------------------------------------
    tabItem(tabName = "gwas",
            #upload phenotype file
            fluidRow(
              shinydashboard::box(
                title = "Step1: Upload Your Phenotype File",
                status = "primary",
                solidHeader = TRUE,
                
                fileInput("phenotype",
                          label = "Upload Your Phenotype File: ",
                          accept = c("text/plain",
                                     ".txt"))
              ),
              
              #-----------------add trait name and select model ----------
              shinydashboard::box(
                title = "Step2: Select Model and Enter Your Trait Name",
                status = "warning",
                solidHeader = TRUE,
                
              #--------------------Enter trait name-------------------------
                textInput(inputId = "trait","Enter Your Trait Name: ", value = "Bna_trait"),
                selectInput("model",
                            label = "Select the GWAS Model: ",
                            choices = list("GLM","MLM","CMLM","EMMAX","Farm-CPU"),
                            selected = "EMMAX"
                ),
                actionButton("run_gwas",
                             "Run Analysis",
                             icon("paper-plane"),
                             style="color:#fff; background-color:#337ab7; border-color:#2e6da4")
              )
            ),
            fluidRow(
              shinydashboard::box(
                status = "primary",
                title = "Distribution Of Your Phenotype",
                withSpinner(plotOutput("phenotype_vis"), type = "4")
              ),
              shinydashboard::box(
                status = "info",
                title = "Result of GWAS",
                withSpinner(DT::dataTableOutput("gwas_res"), type = "7"),
                br(),
                br(),
                downloadLink("Download_gwas_res", "Download your results of GWAS")
              )
            ),
            br(),
            tags$strong("If you use EMMAX to publish research, please cite: "),
            tags$p("Kang HM, Sul JH, Service SK, Zaitlen NA, Kong SY, Freimer NB, Sabatti C, Eskin E. (2010) Variance component model to account for sample structure in genome-wide association studies. Nat. Genet. 42:348-54.")
            ),
# -----------------------------tabitam: vis-------------------------------------------
    tabItem(tabName = "vis",
            fluidRow(
              #------param for manhattan and qq plot------------
              shinydashboard::box(
                status = "info",
                title = "Customized the visualization of Manhattan Plot and QQ Plot",
                width = 12,
                column(3,
                       colourInput("col1","Select Color1: ","#FF8C00")),
                column(3,
                       colourInput("col2","Select Color2: ","#556B2F")),
                column(4,offset = 1,
                       sliderInput("logpvalue", "Choose -log 10 p-value: ", min = -log10(0.01), max = -log10(0.00000001), value = -log10(0.00001), step = 0.5)),
                column(6,offset = 5,
                       actionButton("run_vis",
                                    "Run Visualization",
                                    icon("magic"),
                                    style="color:#fff; background-color:#337ab7; border-color:#2e6da4"))
                
              )
            ),
            fluidRow(
              #-----manhattan plot and QQ plot------
              shinydashboard::box(
                title = "Manhattan Plot",
                status = "primary",
                width = 12,
                withSpinner(plotOutput("manhattanplot"), type = "4"),
                br(),
                br(),
                downloadLink("download_manhattanplot", "Manhattan Plot Download")
              )),
            fluidRow(
              shinydashboard::box(
                title = "QQ plot",
                status = "warning",
                width = 5,
                withSpinner(plotOutput("qqplot"), type = "4"),
                br(),
                br(),
                downloadLink("download_qqplot","QQ Plot Download")
              )
            )
            
            )
          )
)
#==================================UI part==========================
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

#===================SERVER part============================
server <- function(input, output, session){
  
  #--------------------global value setting----------------------
  global_value <- reactiveValues(
    p_data = NULL,
    samples = NULL,
    trait = NULL,
    res = NULL,
    col1 = NULL,
    col2 = NULL,
    logpvalue = NULL,
    gwas_res_emmax_vis = NULL
  )
  trait_name <- eventReactive(input$run_gwas,{
    #name <- paste0(getwd(),"/", input$trait,".txt")
    name <- paste0("/labdata/public/lab_pub_file/gwas/", input$trait,".txt")
    return(name)
  })
  #========================run gwas===========================
  pheno <- reactive({
    inFile <- input$phenotype
    
    if(is.null(inFile))
      return(NULL)
    
    df <- read.table(inFile$datapath, header = FALSE, stringsAsFactors = FALSE)
    df <- df[match(tfam$V1, df$V1),]
    df[df==-999] <- NA
    df <- cbind(df[,1],df)
    return(df)
  })
  
  observeEvent({
    input$phenotype
  }, {
    global_value$p_data <- read.table(input$phenotype$datapath, header = FALSE, stringsAsFactors = FALSE)
    global_value$samples <- global_value$p_data[,1]
    })
  #-----------output of histogram of phenotype----------------
  output$phenotype_vis <- renderPlot({
    validate(
      need( ! is.null(global_value$p_data), "Upload phenotype data first")
    )
    
    df_vis <- global_value$p_data
    
    ggplot(df_vis,aes(x=V2, y=..density..))+
      geom_histogram(fill="lightblue", color="grey60", size=0.2)+
      geom_density(color="red")+
      labs(title = paste0("Distribution of Phenotype ","(", input$trait,")"),
           x=paste0("Value of Phenotype ","(",input$trait,")"),
           y="Density",
           caption="Be carful of your phenotype data, it will affect the results of GWAS greatly!")+
      global_theme
        })
  
  observeEvent(input$run_gwas,{
    global_value$trait=input$trait
  })
  
  # ---------------gwas results-----------------
  output$gwas_res <- renderDT({
    validate(
      need( ! is.null(global_value$trait),"Enter Your Trait Name First!")
    )
    out <- output(trait = global_value$trait)
    file <- pheno()
    write.table(file, trait_name(), col.names = FALSE, row.names = FALSE, quote = FALSE)
    gwas_emmax(phenotype=trait_name(), out = out)
    res <- read.table(paste0("/labdata/public/lab_pub_file/gwas/",Sys.Date(), ".", global_value$trait,".GWAS.EMMAX.cov.ps"), header = FALSE, stringsAsFactors = FALSE)
    global_value$res <- res
    colnames(res) <- c("SNPID","beta","SE(beta)","p-value")
    DT::datatable(res,
                  rownames = FALSE,
                  filter = "top",
                  selection = "single",
                  options = list(
                    pageLength=10,
                    scrollX=TRUE,
                    columnDefs=list(list(className="dt-right", target="_all"))
                  ))
  })
  
  #=================Visualization of GWAS results: manhattan plot and qq plot=========
  observeEvent(input$run_vis,{
    global_value$col1 <- input$col1
    global_value$col2 <- input$col2
    global_value$logpvalue <- input$logpvalue
  })
  
  
  #-----------manhattan plot--------------
  source("manhattan_qq_plot.R")
  output$manhattanplot <- renderPlot({
    validate(
      need( ! is.null(global_value$col1), "Select the Colors"),
      need( ! is.null(global_value$logpvalue),"Select the logpvalue")
    )
    gwas_data <- global_value$res
    gwas_data_vis <- manhattan_data_prepare(gwas_res_emmax = gwas_data)
    global_value$gwas_res_emmax_vis <- gwas_data_vis
    ggmanhattan(gwasres = gwas_data_vis,color = c(input$col1,input$col2), p_select = input$logpvalue, title = paste0("Manhattan Plot of Phenotype ","(",input$trait,")"))
  })
  
  #----------------qqplot-------------
  output$qqplot <- renderPlot({
    validate(
      need( ! is.null(global_value$col1), "Select the Colors"),
      need( ! is.null(global_value$logpvalue),"Select the logpvalue")
    )
    qqman::qq(global_value$gwas_res_emmax_vis$P)
  })
  
}

shinyApp(ui, server)

