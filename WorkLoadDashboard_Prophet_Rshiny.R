



ui <- dashboardPage(
  dashboardHeader(title = "Workload Prediction"),
  
  ## Sidebar ------------------------------------
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Upload Data", tabName = "Upload Data"),
      #menuItem("Set Parameters", tabName = "Set Parameters"),
      menuItem("Start", tabName = "Start")
    )
  ),
  
  ## Body ------------------------------------
  dashboardBody(
    ### include css file --------------------
    tags$head(),
    ### include script with function openTab----------------------
    # tags$script(HTML("var openTab = function(tabName){$('a', $('.sidebar')).each(function() {
    # if(this.getAttribute('data-value') == tabName) {
    # this.click()
    #};
    #});
    # }
    # ")),
    ### use shinyjs -----------------------
    useShinyjs(),
    
    ## Tab Items ---------------------------
    tabItems(
      ### ABout ----------------------------
      tabItem(tabName = "About",
              fluidRow(
                box(width=12,
                    infoBox(width = 12,
                            title = "",
                            #value = includeHTML("./www/about.html"),
                            icon = icon("info")),
                    
                    column(width = 3,
                           a(actionButton(inputId = "start",
                                          label = "Get Started",
                                          style = "font-size: 150%'"),
                             onclick = "openTab('Start')",
                             style="cursor: pointer; font-size: 300%;")))
                
              )
      ),
      
      tabItem(tabName = "Set Parameters",
              fluidRow( )
      ),
      ### Prophet ----------------------------
      tabItem(tabName = "Start",
              fluidRow(
                box(width = 12,
                    tabsetPanel(id = "inTabset",
                                ## TAB 1 : Upload Data --------------------------
                                tabPanel(title = "Set Parameters", value = "panel1",
                                         
                                         fluidRow(
                                           ## upload main dataset -----------------
                                           sidebarPanel(
                                             selectInput(inputId = "dataset",
                                                         label = "Choose a Role:",
                                                         choices = c("AAO_DFI", "AAOBHCMIS", "AAOBPMS","PO",
                                                                     "AAOBureau",
                                                                     "AAOCHGME",
                                                                     "AAODGMO",
                                                                     "AAODGP",
                                                                     "AAODIR",
                                                                     "AAOHAB",
                                                                     "AAOHCOF",
                                                                     "AAONTDP",
                                                                     "AAOOFAM",
                                                                     "AAOOHIT",
                                                                     "AAOOIT",
                                                                     "AAOORHP",
                                                                     "AAOORO",
                                                                     "APO",
                                                                     "AUDREV",
                                                                     "CHGMEFIM",
                                                                     "CHGMEPO",
                                                                     "CHGMEPQC",
                                                                     "CHGMERC",
                                                                     "COR",
                                                                     "DFI_340B_Reviewer",
                                                                     "DFI_DMO",
                                                                     "DFI_QC",
                                                                     "DFI_Reviewer",
                                                                     "DIR",
                                                                     "Expert",
                                                                     "FDE",
                                                                     "FMO",
                                                                     "FTCA_C/A_Reviewer",
                                                                     "FTCA_PQC_Reviewer",
                                                                     "FTCAAPOD",
                                                                     "FTCAPAOD",
                                                                     "FTCAPQCD",
                                                                     "General",
                                                                     "GMO",
                                                                     "GMS",
                                                                     "HCOFANL",
                                                                     "LAO",
                                                                     "LAS",
                                                                     "PA",
                                                                     "PAO",
                                                                     "PDRANL",
                                                                     "PGA",
                                                                     "PO",
                                                                     "PQC",
                                                                     "PS",
                                                                     "PSS",
                                                                     "PSVR",
                                                                     "QC",
                                                                     "SA",
                                                                     "Scope_Administrator",
                                                                     "SVP",
                                                                     "TAC",
                                                                     "TAR",
                                                                     "UDSMNG",
                                                                     "UDSQC",
                                                                     "UDSREV",
                                                                     "Vendor_Editor",
                                                                     "Vendor_Reviewer")), width = .5
                                             
                                           ),
                                           
                                           ## upload holidays -----------------
                                           column(width = 6,
                                                  # tags$h4("Holidays (Optional)"),
                                                  # helpText("A valid dataframe contains at least 2 colums (ds, holiday)"),
                                                  fileInput("holidays_file","Processes",
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  conditionalPanel(condition = 'output.panelStatus_holidays',
                                                                   helpText("First 6 rows of the uploaded holidays ")),
                                                  tableOutput("uploaded_holidays")
                                                  
                                                  ### error msg if holidays is not valid 
                                                  # uiOutput("msg_holidays")
                                           )
                                         ),
                                         
                                         
                                         ## TAB 2 : Set Parameters -----------------------------------
                                         
                                         fluidRow(
                                           column(width = 8,
                                                  column(width = 8, offset = 2,
                                                         tags$h3("Parameters")),
                                                  column(width = 6,
                                                         
                                                         radioButtons("growth","growth",
                                                                      c('linear','logistic'), inline = TRUE),
                                                         
                                                         ### parameter: yearly.seasonality
                                                         checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                                         dateInput("Month","month", value = NULL, min = "2008-08-01", max = "2018-08-01",
                                                                   format = "yyyy-mm-dd", startview = "month"),
                                                         
                                                         ### parameter: weekly.seasonality 
                                                         # checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                                         ### parameter: n.changepoints
                                                         numericInput("n.changepoints","n.changepoints", value = 25),
                                                         
                                                         ### parameter: seasonality.prior.scale
                                                         numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                                         
                                                         ### parameter: changepoint.range
                                                         numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
                                                         numericInput("changepoint.range","changepoint.range", value = .8 , step = .1)),
                                                  column(width = 6,
                                                         
                                                         ### parameter: holidays.prior.scale
                                                        # numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                                         
                                                         ### parameter: mcmc.samples
                                                         numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                                         
                                                         ### parameter: interval.width
                                                         numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                                         ### parameter: uncertainty.samples
                                                         numericInput("uncertainty.samples","uncertainty.samples", value = 1000), 
                                                         selectInput("mode","mode", choices = c('additive', 'multiplicative'))
                                                         
                                                         
                                                  )
                                                  
                                           ),
                                           ## predict parameters --------------------
                                           column(width = 4,
                                                  tags$h3("Predict Parameters"),
                                                  ### paramater: periods
                                                  numericInput("periods","periods",value=12),
                                                  ### parameter: freq and seasonality
                                                  selectInput("freq","freq",
                                                              choices = c('month', 'quarter', 'year')),
                                                  
                                                  ### parameter: include_history
                                                  checkboxInput("include_history","include_history", value = TRUE)
                                           )
                                         ),
                                         ## Back/Next 2 --------------------------
                                         
                                         fluidRow(
                                           #column(width = 2, 
                                           # actionButton("back2", "Back",
                                           # style = "width:100%; font-size:200%")),
                                           column(width = 2, offset = 8,
                                                  actionButton("next2", "Next",
                                                               style = "width:100%; font-size:200%"))
                                         )
                                ),
                                ## TAB 3 : Fit Propher Model ----------------------
                                tabPanel(title = "Model", value = "panel3", 
                                         fluidRow(
                                           # box(width = 12, 
                                           column(width = 12,
                                                  shinyjs::disabled(actionButton("plot_btn2", "Fit Model",
                                                                                 style = "width:30%; margin-top: 25px; margin-bottom: 50px; font-size:150%; ")
                                                  )
                                           )
                                         ),
                                         
                                         ## Results Box : collapsible ------------------
                                         
                                         ## Plots Box : collapsible ------------------
                                         fluidRow( 
                                           conditionalPanel("input.plot_btn2",
                                                            box(width = 12, collapsible = T, title = "Predicted Values",
                                                                tabsetPanel(
                                                                  tabPanel("Data Table", 
                                                                           conditionalPanel("input.plot_btn2",
                                                                                            box(width = 12, collapsible = T, title = "Results",
                                                                                                
                                                                                                div(id = "output-container3",
                                                                                                    tags$img(src = "spinner.gif",
                                                                                                             id = "loading-spinner"),
                                                                                                    DT:: dataTableOutput("data")),
                                                                                                    box(width = 12, collapsible = T, title = "Evaluation Metrics",
                                                                                                        div(id = "output-container3",
                                                                                                            tags$img(src = "spinner.gif",
                                                                                                                     id = "loading-spinner"),
                                                                                                    DT:: dataTableOutput("data1"))),
                                                                                                conditionalPanel("output.data",
                                                                                                                 uiOutput("dw_button")
                                                                                                )
                                                                                            )
                                                                           )),
                                                                  
                                                                  
                                                                  tabPanel("Forecast Plot",
                                                                           
                                                                           div(id = "output-container",
                                                                               # tags$img(src = "spinner.gif",
                                                                               #          id = "loading-spinner"),
                                                                               dygraphOutput ("ts_plot")
                                                                           )
                                                                           # )
                                                                           
                                                                  ),
                                                                  tabPanel("Components Plot",
                                                                           # output.logistic_check=='no_error'
                                                                           conditionalPanel("input.plot_btn2",
                                                                                            div(id = "output-container",
                                                                                                # tags$img(src = "spinner.gif",
                                                                                                #          id = "loading-spinner"),
                                                                                                plotOutput("prophet_comp_plot"))
                                                                           )
                                                                  )
                                                                  
                                                                  
                                                                )))),
                                         ## back 3 ------------
                                         fluidRow(
                                           column(width = 2, 
                                                  actionButton("back3", "Back",
                                                               style = "width:100%; font-size:200%")))
                                ) # tab3
                    ) #
                )
                
              )))
  )
)



server <- function(input, output, session) {
  
  # addClass(selector = "body", class = "sidebar-collapse")
  
  ## Next/Back Buttons actions (to be turned into modules)---------------------------
  observeEvent(input$next1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$next2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  #observeEvent(input$back2, {
  #updateTabsetPanel(session, "inTabset",
  # selected = "panel1")
  # })
  
  observeEvent(input$back3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$back4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  ## function: duplicatedRecative values -----------------------------
  duplicatedRecative <- function(signal){
    values <- reactiveValues(val="")
    observe({
      values$val <- signal()
    })
    reactive(values$val)
  }
  
  ## read csv file main data------------------------------------------------
  dat <- reactive({
    switch(input$dataset,
           "AAO_DFI" = AAO_DFI,
           "AAOBHCMIS" = AAOBHCMIS,
           "AAOBPMS" = AAOBPMS,
           "PO" = PO,
           "AAOBureau" = AAOBureau,
           "AAOCHGME" = AAOCHGME,
           "AAODGMO" = AAOCHGME,
           "AAODGP" = AAODGP,
           "AAODIR" = AAODIR,
           "AAOHAB" =AAOHAB,
           "AAOHCOF" =AAOHCOF,
           "AAONTDP" = AAONTDP,
           "AAOOFAM" = AAOOFAM,
           "AAOOHIT" = AAOOHIT,
           "AAOOIT" = AAOOIT,
           "AAOORHP" = AAOORHP,
           "AAOORO" = AAOORO,
           "APO" = APO,
           "AUDREV" = AUDREV,
           "CHGMEFIM" = CHGMEFIM,
           "CHGMEPO" = CHGMEPO,
           "CHGMEPQC" = CHGMEPQC,
           "CHGMERC" = CHGMERC,
           "COR" = COR,
           "DFI_340B_Reviewer" = DFI_340B_Reviewer,
           "DFI_DMO" = DFI_DMO,
           "DFI_QC" = DFI_QC,
           "DFI_Reviewer" = DFI_Reviewer,
           "DIR" = DIR,
           "Expert" = Expert,
           "FDE" = FDE,
           "FMO" = FMO,
           "FTCA_C/A_Reviewer" = FTCA_C/A_Reviewer ,
           "FTCA_PQC_Reviewer" = FTCA_PQC_Reviewer,
           "FTCAAPOD" = FTCAAPOD,
           "FTCAPAOD" = FTCAPAOD,
           "FTCAPQCD" = FTCAPQCD,
           "General"= General,
           "GMO" = GMO,
           "GMS" = GMS,
           "HCOFANL" = HCOFANL,
           "LAO" = LAO,
           "LAS" = LAS,
           "PA"= PA,
           "PAO" = PAO,
           "PDRANL" = PDRANL,
           "PGA" =PGA,
           "PQC" = PQC,
           "PS" = PS,
           "PSS" = PSS,
           "PSVR" = PSVR,
           "QC" = QC,
           "SA" = SA,
           "Scope_Administrator" = Scope_Administrator,
           "SVP" = SVP,
           "TAC" = TAC,
           "TAR" = TAR,
           "UDSMNG" = UDSMNG,
           "UDSQC" = UDSQC,
           "UDSREV" = UDSREV,
           "Vendor_Editor" = Vendor_Editor,
           "Vendor_Reviewer" = Vendor_Reviewer
           
    )
  })
  
  ## Toggle submit button state according to main data -----------------------
  observe({
    if(!(c("ds","y") %in% names(dat()) %>% mean ==1))
      shinyjs::disable("next1")
    else if(c("ds","y") %in% names(dat()) %>% mean ==1)
      shinyjs::enable("next1")
  })
  
  ## output: table of 1st 6 rows of uploaded main data ------------------
  #output$uploaded_data <- renderTable({
  # req(dat)
  # head(dat())
  #})
  
  ## panel status depending on main data ------------------------
  output$panelStatus <- reactive({
    nrow(dat())>0
  })
  
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  ## read csv file of holidays ---------------------------------
  holidays_upload <- reactive({
    if(is.null(input$holidays_file)) h <- NULL
    else h <- read.csv(input$holidays_file$datapath, header = T) 
    return(h)
  })
  
  ## output: table of 1st 6 rows of uploaded holidays ------------------
  output$uploaded_holidays <- renderTable({
    req(holidays_upload)
    head(holidays_upload())
  })
  
  ## panel status depending on holidays ------------------------
  output$panelStatus_holidays <- reactive({
    !(is.null(holidays_upload()))
  })
  
  outputOptions(output, "panelStatus_holidays", suspendWhenHidden = FALSE)
  
  ## Toggle submit button state according to data ---------------
  observe({
    if(!(c("ds","y") %in% names(dat()) %>% mean ==1))
      shinyjs::disable("plot_btn2")
    else if(c("ds","y") %in% names(dat()) %>% mean ==1)
      shinyjs::enable("plot_btn2")
  })
  
  ## create prophet model --------------------------------------------------
  prophet_model <- eventReactive(input$plot_btn2,{
    
    req(dat(), 
        # ("ds" %in% dat()), "y" %in% names(dat()),
        input$n.changepoints,
        input$seasonality_scale, input$changepoint_scale,
        #input$holidays_scale, 
        input$mcmc.samples,
        input$mcmc.samples, input$interval.width,
        input$uncertainty.samples)
    
    #if(input$growth == "logistic"){
    #  validate(
    #  need(try("cap" %in% names(dat())),
    #      "Error: for logistic 'growth', the input dataframe must have a column 'cap' that specifies the capacity at each 'ds'."))
    
    # }
    
    datx <- dat() 
    #%>% 
    #mutate(y = log(y))
    
    kk <- prophet(datx,
                  growth = input$growth,
                  changepoints = NULL,
                  n.changepoints = input$n.changepoints,
                  yearly.seasonality = input$yearly,
                  # weekly.seasonality = input$monthly,
                  holidays = holidays_upload(),
                  seasonality.prior.scale = input$seasonality_scale,
                  changepoint.prior.scale = input$changepoint_scale,
                  changepoint.range = input$changepoint.range,
                  holidays.prior.scale = input$holidays_scale,
                  mcmc.samples = input$mcmc.samples,
                  interval.width = input$interval.width,
                  uncertainty.samples = input$uncertainty.samples,
                  seasonality.mode = input$mode,
                  fit = T)
    
    return(kk)
  })
  
  ## dup reactive prophet_model ------------------------------
  p_model <- duplicatedRecative(prophet_model)
  
  ## Make dataframe with future dates for forecasting -------------
  future <- eventReactive(input$plot_btn2,{
    req(p_model(),input$periods, input$freq)
    make_future_dataframe(p_model(),
                          periods = input$periods,
                          freq = input$freq,
                          include_history = input$include_history)
  })
  
  ## dup reactive future--------------------------
  p_future <- duplicatedRecative(future)
  
  ## predict future values -----------------------
  forecast <- reactive({
    req(prophet_model(),p_future())
    predict(prophet_model(),p_future())
  })
  
  ## dup reactive forecast--------------------------
  p_forecast <- duplicatedRecative(forecast)
  
  
  ## output :  datatable from forecast dataframe --------------------
  output$data <- DT::renderDataTable({
    # req(logistic_check()!="error")
    datatable(forecast(), 
              options = list(scrollX = TRUE, pageLength = 5)) %>% 
      formatRound(columns=2:17,digits=4)
  })
  
  validation <- reactive({
    req(p_model())
    prophet::  cross_validation(p_model(), horizon = 365/12, units = "days", period = 362/12,
                                initial = 2*12*(365/12))
  })
  
  p_validation <- duplicatedRecative(validation)
  
  output$data1 <- DT:: renderDataTable({
    DT::datatable(performance_metrics(p_validation()), options = list(scrollX = TRUE, pageLength = 5)) %>% 
      formatRound(columns=2:17,digits=4)
  })
  
  ## download button ----------------
  output$dw_button <- renderUI({
    req(forecast())
    downloadButton('downloadData', 'Download Data',
                   style = "width:20%;
                   margin-bottom: 25px;
                   margin-top: 25px;")
  })
  
  output$downloadData <- downloadHandler(
    filename = "forecast_data.csv",
    content = function(file) {
      write.csv(forecast(), file)
    }
  )
  
  
  #jj <- eventReactive(input$plot_btn2,{
  
  #req(p_model(),forecast())
  #df <- p_model()$history %>%
  #  dplyr::select(ds, y) %>%
  # dplyr::full_join(forecast(), by = "ds") %>%
  # dplyr::arrange(ds)
  # return(df)
  # })
  # mydata <- duplicatedRecative(jj)
  
  
  ## output:  plot forecast -------------
  output$ts_plot <- renderDygraph({
    req(p_model(), p_forecast())
    prophet::  dyplot.prophet(p_model(), p_forecast(), uncertainty = TRUE)
  })
  
  ## output:plot prophet components --------------
  output$prophet_comp_plot <- renderPlot({
    # req(logistic_check()!="error")
    prophet_plot_components(p_model(), forecast())
  })
  
  ## error msg for main dataset------------------------
  output$msg_main_data <- renderUI({
    if(c("ds","y") %in% names(dat()) %>% mean !=1)
      "Invalid Input: dataframe should have at least two columns named (ds & y)"
  })
  
  ## error msg for holidays ------------------------
  output$msg_holidays <- renderUI({
    if(c("ds","holiday") %in% names(holidays_upload()) %>% mean !=1)
      "Invalid Input: dataframe should have at least two columns named (ds & holiday)"
  })
  
  }

shinyApp(ui, server)

