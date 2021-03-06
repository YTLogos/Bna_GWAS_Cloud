rm(list = ls())
source("Global.R")
options(shiny.maxRequestSize = 500 * 1024^2)

header <- dashboardHeader(title = "Bna-GWAS-Cloud")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction",
      tabName = "intro", icon = icon("user"),
      menuSubItem("Introduction", tabName = "intro1"),
      menuSubItem("Analysis Workflow", tabName = "aw"),
      menuSubItem("Q&A", tabName = "qa")
    ),
    menuItem("Run GWAS", tabName = "gwas", icon = icon("galactic-senate")),
    menuItem("Visualization", tabName = "vis", icon = icon("braille")),
    menuItem("Extraction", tabName = "extraction", icon = icon("accusoft")),
    menuItem("Annotation", tabName = "anno", icon = icon("list")),
    menuItem("Feedback", tabName = "feedback", icon = icon("comment-alt"))
  ),
  absolutePanel(
    bottom = 77,
    left = 10,
    draggable = F,
    width = "100%",
    height = "auto",
    p(a(icon("github fa-2x"), href = "https://github.com/YTLogos/Bna_GWAS_Cloud", target = "_blank"))
  ),
  absolutePanel(
    bottom = 77,
    left = 55,
    draggable = F,
    width = "100%",
    height = "auto",
    p(a(icon("paper-plane fa-2x"), href = "mailto:tyan@zju.edu.cn", target = "_blank"))
  ),
  absolutePanel(
    bottom = 77,
    left = 100,
    draggable = F,
    width = "100%",
    height = "auto",
    p(a(icon("link fa-2x"), href = "https://taoyan.netlify.app", target = "_blank"))
  ),

  absolutePanel(
    bottom = 20,
    left = 5,
    draggable = F,
    width = "100%",
    height = "auto",
    div(span("Developed by", style = "color:grey"), a("JiangLX Lab", href = "https://jianglab.netlify.app", target = "_blank"), span(a(", College of", href = "http://www.cab.zju.edu.cn/en/", target = "_blank"), style = "color:grey")),
    div(a("Agriculture and Biotechnology (CAB),", href = "http://www.cab.zju.edu.cn/en/", target = "_blank"), style = "color:grey"),
    div(a("Zhejiang University,", href = "http://www.zju.edu.cn/english/", target = "_blank"), style = "color:grey")
  )
)

body <- dashboardBody(
  tabItems(

    # -------------------tabItem: Introduction of GWAS------------------------------------
    tabItem(
      tabName = "intro1",
      includeMarkdown(file.path("text", "introduction.md"))
    ),
    tabItem(
      tabName = "aw",
      includeMarkdown(file.path("text", "workflow.md"))
    ),
    tabItem(
      tabName = "qa",
      includeMarkdown(file.path("text", "qa.md"))
    ),


    #----------------------tabitem: feedback----------------
    tabItem(
      tabName = "feedback",
      includeMarkdown(file.path("text", "feedback.md"))
    ),


    # -----------------------tabitem: run gwas --------------------------------------
    tabItem(
      tabName = "gwas",
      # upload phenotype file
      fluidRow(
        shinydashboard::box(
          title = "Step1: Upload Your Phenotype File (.txt format)",
          status = "primary",
          solidHeader = TRUE,

          fileInput("phenotype",
            label = "Upload Your Phenotype File: ",
            accept = c(
              "text/plain",
              ".txt"
            )
          )
        ),

        #-----------------add trait name and select model ----------
        shinydashboard::box(
          title = "Step2: Select Model and Enter Your Trait Name",
          status = "warning",
          solidHeader = TRUE,

          #--------------------Enter trait name-------------------------
          textInput(inputId = "trait", "Enter Your Trait Name: ", value = "Bna_trait"),
          selectInput("model",
            label = "Select the GWAS Model: ",
            choices = list("GLM", "MLM", "CMLM", "EMMAX", "Farm-CPU"),
            selected = "EMMAX"
          ),
          actionButton("run_gwas",
            "Run Analysis",
            icon("paper-plane"),
            style = "color:#fff; background-color:#337ab7; border-color:#2e6da4"
          )
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
          withSpinner(DT::dataTableOutput("gwas_res"), type = "6"),
          br(),
          br(),
          downloadButton("download_gwas_res", "Download GWAS Results")
        )
      ),
      br(),
      tags$strong("If you use EMMAX to publish research, please cite: "),
      tags$p("Kang HM, Sul JH, Service SK, Zaitlen NA, Kong SY, Freimer NB, Sabatti C, Eskin E. (2010) Variance component model to account for sample structure in genome-wide association studies. Nat. Genet. 42:348-54.")
    ),
    # -----------------------------tabitam: vis-------------------------------------------
    tabItem(
      tabName = "vis",
      fluidRow(
        #------param for manhattan and qq plot------------
        shinydashboard::box(
          status = "info",
          title = "Customized the visualization of Manhattan Plot and QQ Plot",
          solidHeader = TRUE,
          width = 12,
          column(
            3,
            colourInput("col1", "Select Color1: ", "#FF8C00")
          ),
          column(
            3,
            colourInput("col2", "Select Color2: ", "#556B2F")
          ),
          column(4,
            offset = 1,
            sliderInput("logpvalue", "Choose -log 10 p-value: ",
              min =
                -log10(0.01), max = -log10(0.0000000001), value =
                -log10(0.00001), step = 0.5
            )
          ),
          column(6,
            offset = 5,
            actionButton("run_vis",
              "Run Visualization",
              icon("magic"),
              style = "color:#fff; background-color:#337ab7; border-color:#2e6da4"
            )
          )
        )
      ),
      fluidRow(
        #-----manhattan plot and QQ plot------
        shinydashboard::box(
          title = "Manhattan Plot",
          status = "primary",
          width = 12,
          withSpinner(plotOutput("manhattanplot"), type = "6"),
          downloadButton("dm", "Manhattan Plot Download")
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "QQ plot",
          status = "warning",
          width = 5,
          withSpinner(plotOutput("qqplot"), type = "8"),
          downloadButton("download_qqplot", "QQ Plot Download")
        )
      )
    ),
    #---------------------------tabitem: extraction-------------------------
    tabItem(
      tabName = "extraction",
      fluidRow(
        #-----------select params for extraction------------
        shinydashboard::box(
          title = "Extract genes based on significant SNPs",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          column(
            6,
            sliderInput("sig_p", "Choose significant -log 10 p-value: ",
              min =
                -log10(0.01), max = -log10(0.00000000001), value =
                -log10(0.000001), step = 0.5
            )
          ),
          column(
            6,
            sliderInput("distance", "Choose the distance (kb): ",
              min = 0,
              max = 150,
              value = 75,
              step = 5
            )
          ),
          column(6,
            offset = 5,
            actionButton("run_extraction",
              "Run Extraction",
              icon("magic"),
              style = "color:#fff; background-color:#337ab7; border-color:#2e6da4"
            )
          )
        )
      ),
      fluidRow(
        #----------the extracted genes-------------
        shinydashboard::box(
          title = "Genes extracted based on significant SNPs",
          status = "info",
          width = 12,
          withSpinner(DT::dataTableOutput("related_genes"), type = "7"),
          br(),
          br(),
          downloadButton("download_genes", "Download Significant Genes")
        )
      )
    ),
    #-----------------------------tabitem: gene annotation------------------
    tabItem(
      tabName = "anno",
      #---------gene annotation-------
      fluidRow(
        shinydashboard::box(
          title = "Gene annotation based on different databases",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          tags$p("If you are sure your previous analysis are right, then clink the button: Run Annotation"),
          br(),
          column(6,
            offset = 5,
            actionButton("run_annotation",
              "Run Annotation",
              icon("magic"),
              style = "color:#fff; background-color:#337ab7; border-color:#2e6da4"
            )
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Gene annotation",
          status = "warning",
          width = 12,
          withSpinner(DT::dataTableOutput("gene_annotation"), type = "7"),
          downloadButton("gene_anno_download", "Download Genes with Annotation")
        )
      )
    )
  )
)
# ==================================UI part==========================
ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body, 
                    skin = "red"
                    )

# ===================SERVER part============================
server <- function(input, output, session) {

  options(shiny.usecairo = TRUE)

  #--------------------global value setting----------------------
  global_value <- reactiveValues(
    p_data = NULL,
    samples = NULL,
    trait = NULL,
    res = NULL,
    col1 = NULL,
    col2 = NULL,
    logpvalue = NULL,
    sig_p = NULL,
    distance = NULL,
    gwas_res_emmax_vis = NULL,
    gene_extracted = NULL,
    gene_sig_select = NULL,
    manhattan_plot = NULL,
    QQ_plot = NULL
  )
  trait_name <- eventReactive(input$run_gwas, {
    # name <- paste0(getwd(),"/", input$trait,".txt")
    name <- paste0("./tmp/", input$trait, ".txt")
    return(name)
  })
  # ========================run gwas===========================
  pheno <- reactive({
    inFile <- input$phenotype

    if (is.null(inFile)) {
      return(NULL)
    }
    df <- read.table(inFile$datapath, header = FALSE, stringsAsFactors = FALSE)
    df <- left_join(core, df, by = c("core" = "V1"))
    df <- df[match(tfam$V1, df$core), ]
    df[df == -999] <- NA
    df <- cbind(df[, 1], df)
    return(df)
  })

  observeEvent(
    {
      input$phenotype
    },
    {
      global_value$p_data <- read.table(input$phenotype$datapath, header = FALSE, stringsAsFactors = FALSE)
      global_value$samples <- global_value$p_data[, 1]
    }
  )


  #-----------output of histogram of phenotype----------------
  output$phenotype_vis <- renderPlot({
    validate(
      need(!is.null(global_value$p_data), "Upload phenotype data first")
    )

    df_vis <- global_value$p_data

    ggplot(df_vis, aes(x = V2, y = ..density..)) +
      geom_histogram(fill = "lightblue", color = "grey60", size = 0.2) +
      geom_density(color = "red") +
      labs(
        title = paste0("Distribution of Phenotype ", "(", input$trait, ")"),
        x = paste0("Value of Phenotype ", "(", input$trait, ")"),
        y = "Density",
        caption = "\n Be carful of your phenotype data, \n it will affect the results of GWAS greatly!"
      ) +
      global_theme
  })

  observeEvent(input$run_gwas, {
    global_value$trait <- input$trait
  })

  # ---------------gwas results-----------------
  output$gwas_res <- renderDT({
    validate(
      need(!is.null(global_value$trait), "Enter Your Trait Name First!")
    )
    out <- output(trait = global_value$trait)
    file <- pheno()
    write.table(file, trait_name(), col.names = FALSE, row.names = FALSE, quote = FALSE)
    withProgress(
      message = "GWAS RUN in progress",
      detail = "This may take 1 min! Please Wait! ...",
      value = 0,
      {
        for (i in 1:15) {
          incProgress(1 / 15)
          Sys.sleep(0.01)
        }
        gwas_emmax(phenotype = trait_name(), out = out)
        res <- data.table::fread(paste0("./tmp/", Sys.Date(), ".", global_value$trait, ".GWAS.EMMAX.cov.ps"), data.table = FALSE)
        colnames(res) <- c("SNPID", "beta", "SE(beta)", "p-value")
        global_value$res <- res
        DT::datatable(global_value$res,
          rownames = FALSE,
          filter = "top",
          selection = "single",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            columnDefs = list(list(className = "dt-right", target = "_all"))
          )
        )
      }
    )
  })

  #--------------download gwas_res---------

  output$download_gwas_res <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.result.txt")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }

          write.table(global_value$res, file, row.names = FALSE, col.names = TRUE, quote = FALSE)
        }
      )
    }
  )

  # =================Visualization of GWAS results: manhattan plot and qq plot=========
  observeEvent(input$run_vis, {
    global_value$col1 <- input$col1
    global_value$col2 <- input$col2
    global_value$logpvalue <- input$logpvalue
  })


  #-----------manhattan plot--------------

  manhattan <- function() {
    gwas_data <- global_value$res
    gwas_data_vis <- manhattan_data_prepare(gwas_res_emmax = gwas_data)
    global_value$gwas_res_emmax_vis <- gwas_data_vis
    ggmanhattan(gwasres = global_value$gwas_res_emmax_vis, color = c(global_value$col1, global_value$col2), p_select = global_value$logpvalue, title = paste0("Manhattan Plot of Phenotype ", "(", global_value$trait, ")"), vlinesize = 0.5)
  }
  output$manhattanplot <- renderPlot({
    validate(
      need(!is.null(global_value$col1), "Select the Colors"),
      need(!is.null(global_value$logpvalue), "Select the logpvalue")
    )
    withProgress(
      message = "Visualization in progress",
      detail = "This may take 2.5 mins! Please Wait! ...",
      value = 0,
      {
        for (i in 1:15) {
          incProgress(1 / 15)
          Sys.sleep(0.01)
        }
        p_manhattan <- manhattan()
        global_value$manhattan_plot <- p_manhattan
        print(global_value$manhattan_plot)
      }
    )
  })

  #--------------------download manhattan plot---------------
  output$dm <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.manhattan.png")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }
          png(file, width = 15 * 300, height = 7 * 300, res = 300)
          print(global_value$manhattan_plot)
          dev.off()
        }
      )
    }
  )



  #----------------qqplot-------------

  myQQ_plt <- function() {
    qqman::qq(global_value$gwas_res_emmax_vis$P)
  }
  output$qqplot <- renderPlot({
    validate(
      need(!is.null(global_value$col1), "Select the Colors"),
      need(!is.null(global_value$logpvalue), "Select the logpvalue")
    )
    qq_plot <- myQQ_plt()
    global_value$QQ_plot <- qq_plot
    print(global_value$QQ_plot)
  })

  #--------------------download QQ plot---------------
  output$download_qqplot <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), ".", input$trait, ".GWAS.EMMAX.cov.QQ_plot.png")
    },
    content <- function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while ...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }
          png(file, width = 7 * 300, height = 7 * 300, res = 300)
          myQQ_plt()
          dev.off()
        }
      )
    }
  )

  # ==========================Extraction====================================

  observeEvent(input$run_extraction, {
    global_value$sig_p <- input$sig_p
    global_value$distance <- input$distance
  })

  #----------------output of gene select based on p-value
  output$related_genes <- renderDT({
    validate(
      need(!is.null(global_value$sig_p), "Choose Significant p-value first"),
      need(!is.null(global_value$distance), "Choose Distance first")
    )
    p <- 10^-(global_value$sig_p)
    snp_select <- global_value$gwas_res_emmax_vis %>% dplyr::filter(P <= p)
    chr_select <- as.character(unique(snp_select$CHR))
    Bna_geneid_select <- Bna_geneid %>% dplyr::filter(chr %in% chr_select)
    gene_sig_select <- get_gene_from_snp(gff = Bna_geneid_select, sig.snp = snp_select, distance = global_value$distance)
    global_value$gene_sig_select <- gene_sig_select
    DT::datatable(global_value$gene_sig_select,
      rownames = FALSE,
      filter = "bottom",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      )
    )
  })

  #--------------download significant genes---------

  output$download_genes <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.txt")
    },
    content = function(file) {
      write.table(global_value$gene_sig_select, file, row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  )

  # =========================gene annotation===================
  gene_anno_data <- eventReactive(input$run_annotation, {
    gene_id <- unique(global_value$gene_sig_select$gene)
    gene_anno_select <- Bna_anno %>% dplyr::filter(geneid %in% gene_id)
    return(gene_anno_select)
  })

  output$gene_annotation <- renderDT({
    DT::datatable(gene_anno_data(),
      rownames = FALSE,
      filter = "bottom",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      ), class = "white-space: nowrap"
    )
  })

  #---------------gene annotation download---------------
  output$gene_anno_download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), ".", input$trait, ".EMMAX.cov.signloci.1e-", input$sig_p, ".", input$distance, "Kb.anno.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(gene_anno_data(), file)
    }
  )
}

shinyApp(ui, server)
