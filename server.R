shiny_env = new.env()

# Define server logic required to draw the boxplot and render metadata table
server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$run2 <- 0
  rv$init <- 0
  rv$actuallygo <- 0
  rv$old <- ""
  rv$plot_temp <- data.frame()
  rv$go <- 0
  rv$tabinit_plot <- 0
  rv$starttutorial <- 0
  rv$data_prev <- data.frame()

  # hide some checkboxes
  removeModal()
  hide("Find")
  
  # init
  observeEvent(rv$init, {
    if (rv$init == 0) {
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[["gene"]])) {
        updateSelectizeInput(session,
                             inputId = "geneID",
                             selected = query[["gene"]],
                             choices = autocomplete_list,
                             server = T
        )
      } else {
        updateSelectizeInput(session,
                             inputId = "geneID",
                             selected = "name",
                             choices = autocomplete_list,
                             server = T
        )
      }
      rv$init <- 1
      rv$run2 <- 1
    }
  })
  
  # sortable or not
  jqui_sortable(ui = "#sidediv", operation = "enable", options = list(cancel = ".datatables"))
  jqui_sortable(ui = "#tabMain", operation = "enable")
  
  observe({
    if (input$doLock == TRUE) {
      jqui_sortable(ui = "#sidediv", operation = "destroy")
      jqui_sortable(ui = "#tabMain", operation = "destroy")
    } else if (input$doLock != TRUE) {
      jqui_sortable(ui = "#sidediv", operation = "enable", options = list(cancel = ".form-control"))
      jqui_sortable(ui = "#tabMain", operation = "enable")
    }
  })
  
  observeEvent(input$geneID, {
    if (rv$run2 == 1 & input$geneID != "" & !is.null(input$geneID)) {
      rv$run2 <- 1
      shinyjs::click("Find")
    }
  })
  
  # find on clicking button
  onclick(
    "geneID",
    updateSelectizeInput(session,
                         inputId = "geneID",
                         selected = "",
                         choices = autocomplete_list,
                         server = T
    )
  )
  
  # jump to plot
  observeEvent(input$Find, {
    if ((input$geneID != "") & (input$geneID != rv$old)) {
      rv$actuallygo <- rv$actuallygo + 1
      if (rv$init == 1) {
        rv$init <<- 2
      } else {
        updateQueryString(paste0("?gene=", input$geneID), mode = "push")
      }
    }
  })
  
  # query
  inid <- eventReactive(rv$actuallygo,
                        {
                          rv$old <<- input$geneID
                          shinyjs::runjs("window.scrollTo(0, 0)")
                          input$geneID
                        },
                        ignoreNULL = T,
                        ignoreInit = T
  )
  
  # dimplot1
  dimPlot1 <- reactive({
    gene1 <- inid()
    slide1 <- input$sel1
    if (gene1 %in% cats) {
      if (slide1 == "umap") {
        g <- DimPlot(s, group.by = gene1, label = TRUE)  +
          scale_color_viridis(discrete = TRUE, option = "turbo", alpha = 0.24, guide = guide_legend(ncol = 1)) +
          ggtitle("") +
          labs(colour = gene1) +
          guides(col = guide_legend(ncol = 1))
      } else if (str_detect(slide1, "H&E")) {
        g <- SpatialDimPlot(s, group.by = gene1, images = str_remove(slide1, "_H&E"), alpha = 0, image.alpha = 0.67, stroke = 0)  +
          scale_fill_viridis(discrete = TRUE, option = "turbo", alpha = 0) +
          ggtitle("") +
          guides(col = guide_legend(ncol = 1)) +
          theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"))
      } else {
        g <- SpatialDimPlot(s, group.by = gene1, images = slide1, alpha = 0.34, image.alpha = 0.23, stroke = 0)  +
          scale_fill_viridis(discrete = TRUE, option = "turbo", alpha = 0.24) +
          ggtitle("") +
          guides(col = guide_legend(ncol = 1))
      }
    } else {
      if (slide1 == "umap") {
        g <- FeaturePlot(s, gene1, order = TRUE)  +
          scale_color_gradient(low = "#F5F5F5", high = "red") +
          ggtitle("") +
          labs(colour = gene1)
      } else if (str_detect(slide1, "H&E")) {
        g <- SpatialFeaturePlot(s, gene1, images = str_remove(slide1, "_H&E"), alpha = 0, image.alpha = 0.67, stroke = 0)  +
          theme(legend.position = "right") +
          scale_fill_gradient(low = "#F5F5F5", high = "red") +
          guides(fill = guide_legend(override.aes = list(alpha = 0))) +
          theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"))
      } else {
        g <- SpatialFeaturePlot(s, gene1, images = slide1, alpha = c(0.23, 0.67), image.alpha = 0.23, stroke = 0) +
          theme(legend.position = "right") +
          scale_fill_gradient(low = "#F5F5F5", high = "red")
      }
    }
  })
  
  dimPlotr1 <- reactive({
    g <- dimPlot1()
    output$dimPlot1 <- renderPlot(g)
    plotOutput("dimPlot1", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
  })
  
  # dimplot-plotly
  dimPlotlyr1 <- reactive({
    g <- dimPlot1()
    output$dimPlot1 <- renderPlotly(ggplotly(g, tooltip = "text") %>%
                                      layout(hovermode = "closest") %>% 
                                      config(displayModeBar = FALSE)) 
    plotlyOutput("dimPlot1", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
  })
  
  # actually draw dimplot
  output$dimPlotUI1 <- renderUI({
    if (rv$init >= 1 & rv$run2 >= 1) {
      dimPlotr1()
    } else {
      plotOutput("dimPlot1", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
    }
  })
  
  # dimplot2
  dimPlot2 <- reactive({
    gene2 <- inid()
    slide2 <- input$sel2
    if (gene2 %in% cats) {
      if (slide2 == "umap") {
        g <- DimPlot(s, group.by = gene2, label = TRUE)  +
          scale_color_viridis(discrete = TRUE, option = "turbo", alpha = 0.24, guide = guide_legend(ncol = 1)) +
          ggtitle("") +
          labs(colour = gene2) +
          guides(col = guide_legend(ncol = 1))
      } else if (str_detect(slide2, "H&E")) {
        g <- SpatialDimPlot(s, group.by = gene2, images = str_remove(slide2, "_H&E"), alpha = 0, image.alpha = 0.67, stroke = 0)  +
          scale_fill_viridis(discrete = TRUE, option = "turbo", alpha = 0) +
          ggtitle("") +
          guides(col = guide_legend(ncol = 1)) +
          theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"))
      } else {
        g <- SpatialDimPlot(s, group.by = gene2, images = slide2, alpha = 0.34, image.alpha = 0.23, stroke = 0)  +
          scale_fill_viridis(discrete = TRUE, option = "turbo", alpha = 0.24) +
          ggtitle("") +
          guides(col = guide_legend(ncol = 1))
      }
    } else {
      if (slide2 == "umap") {
        g <- FeaturePlot(s, gene2, order = TRUE)  +
          scale_color_gradient(low = "#F5F5F5", high = "red") +
          ggtitle("") +
          labs(colour = gene2)
      } else if (str_detect(slide2, "H&E")) {
        g <- SpatialFeaturePlot(s, gene2, images = str_remove(slide2, "_H&E"), alpha = 0, image.alpha = 0.67, stroke = 0)  +
          theme(legend.position = "right") +
          scale_fill_gradient(low = "#F5F5F5", high = "red") +
          guides(fill = guide_legend(override.aes = list(alpha = 0))) +
          theme(legend.text = element_text(color = "white"), legend.title = element_text(color = "white"))
      } else {
        g <- SpatialFeaturePlot(s, gene2, images = slide2, alpha = c(0.23, 0.67), image.alpha = 0.23, stroke = 0) +
          theme(legend.position = "right") +
          scale_fill_gradient(low = "#F5F5F5", high = "red")
      }
    }
  })
  
  dimPlotr2 <- reactive({
    g <- dimPlot2()
    output$dimPlot2 <- renderPlot(g)
    plotOutput("dimPlot2", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
  })
  
  # dimplot-plotly
  dimPlotlyr2 <- reactive({
    g <- dimPlot2()
    output$dimPlot2 <- renderPlotly(ggplotly(g, tooltip = "id") %>%
                                      layout(hovermode = "closest") %>% 
                                      config(displayModeBar = FALSE)) 
    plotlyOutput("dimPlot2", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
  })
  
  # actually draw dimplot
  output$dimPlotUI2 <- renderUI({
    if (rv$init >= 1 & rv$run2 >= 1) {
      dimPlotr2()
    } else {
      plotOutput("dimPlot2", width = as.numeric(input$plotw) * 100, height = as.numeric(input$ploth) * 100)
    }
  })
  
  # violinplot
  violinPlot1 <- reactive({
    gene3 <- inid()
    if (gene3 %in% cats) {
      df2 <- s@meta.data %>% select(sample, gene3) %>%
        setNames(c("sample", "gene3")) %>% 
        group_by(sample, gene3) %>% summarise(n = n()) %>% 
        group_by(sample) %>% mutate(n = n/sum(n)) %>% 
        mutate(sample = factor(sample, levels = mixedsort(unique(s$sample))))
      g <- ggplot(df2,
                  aes(x = sample, y = n, fill = gene3)) +
        geom_col() +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text = element_text(size = 8), axis.title = element_text(size = 8), legend.text = element_text(size = 8)) +
        xlab("") +
        ylab("Fraction") +
        labs(fill = "") +
        ggtitle("") +
        scale_fill_viridis(discrete = TRUE, option = "turbo")
    } else {
      g <- ggplot(FetchData(s, c("name", gene3)) %>% setNames(c("sample", "gene3")),
                  aes(x = sample, y = gene3, fill = sample)) +
        geom_violin(width = 1.2, alpha = 0.5) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text = element_text(size = 8), axis.title = element_text(size = 8), legend.text = element_text(size = 8)) +
        xlab("") +
        ylab("Expression") +
        labs(fill = "") +
        ggtitle("") +
        NoLegend()
    }
  })
  
  violinPlotr1 <- reactive({
    g <- violinPlot1()
    output$violinPlot1 <- renderPlot(g)
    plotOutput("violinPlot1", width = as.numeric(input$plotw) * 200, height = as.numeric(input$ploth) * 100)
  })
  
  # dimplot-plotly
  violinPlotly1 <- reactive({
    g <- violinPlot1()
    output$violinPlot1 <- renderPlotly(ggplotly(g, tooltip = "id") %>%
                                      layout(hovermode = "closest") %>% 
                                      config(displayModeBar = FALSE)) 
    plotlyOutput("violinPlot1", width = as.numeric(input$plotw) * 200, height = as.numeric(input$ploth) * 100)
  })
  
  # actually draw dimplot
  output$violinPlotUI1 <- renderUI({
    if (rv$init >= 1 & rv$run2 >= 1) {
      violinPlotr1()
    } else {
      plotOutput("violinPlot1", width = as.numeric(input$plotw) * 200, height = as.numeric(input$ploth) * 100)
    }
  })
  
  observeEvent(input$tutorial, {
    introjs(session,
            options = list(
              "nextLabel" = ">",
              "prevLabel" = "<",
              "skipLabel" = "skip",
              "overlayOpacity" = -1
            ) # ,events = list("onexit" = I("alert('abc')"))
    )
  })
  
  observeEvent(rv$run2, {
    if (start_tutorial & rv$starttutorial == 0) {
      rv$starttutorial <- 1
      introjs(session, options = list(
        "nextLabel" = ">",
        "prevLabel" = "<",
        "skipLabel" = "skip",
        "overlayOpacity" = -1
      )) # events = list("onexit" = I("document.getElementsByClassName('introjs-nextbutton').blur()")))
    }
  })
  
  output$intro <- renderUI({
    clean <- a(" (link) ",
               href = manuscriptL,
               target="_blank"
    )
    tagList(tags$h6(manuscriptD, clean))
  })
  
  output$intro2 <- renderUI({
    clean2 <- a("Pediatric Neuro-oncology Cell Atlas",
                href = url,
                target="_blank"
    )
    tagList(tags$h6("This is part of an ongoing effort of the ", clean2,
                    "lead by the Department of Pediatrics, the Morgan Adams Foundation Pediatric Brain Tumor Research Program, and the RNA Bioscience Initiative at the University of Colorado Anschutz Medical Campus."))
  })
  
  output$rawdata <- renderUI({
    url <- str_c(
      "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geoN
    )
    clean <- a(geoN,
               href = url,
               target="_blank"
    )
    tagList(tags$h6("Data and metadata are deposited on NCBI Gene Expression Omnibus at ", clean))
  })
  
  output$GitHub <- renderUI({
    clean <- a(paste0("v", versionN),
               href = giturl,
               target="_blank"
    )
    tagList(tags$p(icon("github-square"), clean))
  })
}