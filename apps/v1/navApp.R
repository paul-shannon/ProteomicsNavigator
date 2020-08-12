library(ProteomicsNavigator)
library(shinydashboard)
library(shinyModules)
library(shinyWidgets)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
data.dir <- "data"
datasets <- file.path(data.dir, list.files(data.dir, pattern=".RData"))
#------------------------------------------------------------------------------------------------------------------------
# inspired by, and code stealing from:
#   https://roh.engineering/post/shiny-add-removing-modules-dynamically/   # linear models for mtcars mpg
#   https://www.ardata.fr/en/post/2019/07/01/dynamic-module-call/
#   https://mastering-shiny.org/scaling-modules.html
# see 2018 very similar module/shiny app:
#    https://www.blog.cultureofinsight.com/2018/01/reproducible-shiny-app-development-with-modules/
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
nav <- ProteomicsNavigator(datasets)
loadDataSets(nav)

datasetNames <- getDataSetNames(nav)
print(datasetNames)
analytes <- getAnalytes(nav)
#----------------------------------------------------------------------------------------------------
ui.dashboard <- dashboardPage(

  dashboardHeader(title = "Proteomic Assays"),
  dashboardSidebar(
      useShinyjs(),
      pickerInput(
          inputId = "experimentPicker",
          label = "Select Experiments",
          choices = datasetNames,
          options = list(`actions-box`=TRUE, size=5,`selected-text-format`="count > 3"),
          multiple = TRUE
          ),
      pickerInput(
          inputId = "analytePicker",
          label = "Select Analytes",
          choices = analytes,
          options = list(`actions-box` = TRUE, size = 5,`selected-text-format` = "count > 2"),
          multiple = TRUE
         ),
      selectInput(inputId="plotsPerRowSelector", label="Plots per Row",
                  choices=as.character(1:4), selected="3"),
      wellPanel(actionButton("displayPlotsButton", label="Plot"),
                style="background-color: lightgray; margin: 20px;"),
    sidebarMenuOutput("menu")
    ),
  dashboardBody(
    fluidRow(uiOutput("plotBoxDiv"))  # creates a div to be filled later
    )

) # ui.dashboard
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
    shinyjs::disable("displayPlotsButton")
    plotCount <- reactiveVal(0)
    plotCountBox.contents <- callModule(messageBoxServer, "plotCountBox", newContent=plotCount)

    observeEvent(input$experimentPicker, ignoreNULL=FALSE, {
        printf("--- experimentPicker")
        conditionallyEnablePlotButton()
        picked <- input$experimentPicker
        print(picked)
        })

    conditionallyEnablePlotButton <- function(){
       printf("--- calculating state of plot button")
       shinyjs::disable("displayPlotsButton")
       analytesPicked <- !is.null(input$analytePicker)
       experimentsPicked <- !is.null(input$experimentPicker)
       if(analytesPicked && experimentsPicked)
           shinyjs::enable("displayPlotsButton")
       }

    observeEvent(input$analytePicker, ignoreNULL=FALSE, {
        printf("--- analytePicker")
        analytesPicked <- input$analytePicker
        conditionallyEnablePlotButton()
        print(analytesPicked)
        })

    updatePlotCountDisplay <- function(){
       printf("--- entering updatePlotCountDisplay")
       experiments <- input$selectExperiment
       printf("experiements: %d", length(experiments))
       print(experiments)
       plotCount(0)
       if(all(experiments == "")){
           plotCount(0)
           return()
           }
       analytes <- input$selectAnalyte
       printf("analytes: %d", length(analytes))
       print(analytes)
       if(all(analytes == "")){
           plotCount(0)
           return()
           }
       tbls <- getPlotFriendlyDataFrames(nav, analytes, experiments)
       plotCount(length(tbls))
       }

    plotAnalytes <- function(tbls){
       printf("plotting %d analytes", length(tbls))
       removeUI("#temporaryDiv")
       insertUI("#plotBoxDiv", "beforeEnd", div(id="temporaryDiv"))
       tbl.names <- names(tbls)
       for(i in seq_len(length(tbl.names))){
          tbl.name <- tbl.names[i]
          tbl <- tbls[[i]]
          box.title <- tbl.name
          box.id <- tbl.name #sprintf("%s-%s", analyte.name, experiment.group)
          boxesPerRow <- as.integer(isolate(input$plotsPerRowSelector))
          calculatedBoxWidth <- as.integer(1000/boxesPerRow)
          bootstrapWidth <- as.integer(12/boxesPerRow)
          #if(boxesPerRow == 1){
          #    calculatedBoxWidth=800
          #    bootstrapWidth=12
          #    }
          insertUI("#temporaryDiv", "beforeEnd",
                   box(ExperimentalMeasuresUI(id=box.id, title=box.title, boxHeight=calculatedBoxWidth, boxWidth=calculatedBoxWidth),
                       title=box.title,
                       width=bootstrapWidth,
                       solidHeader=TRUE))
          ExperimentalMeasuresServer(id=box.id, tbl=tbl)
          } # for i
      } # plotAnalytes

    observeEvent(input$displayPlotsButton, {
       experiments <- isolate(input$experimentPicker)
       analytes <- isolate(input$analytePicker)
       print(experiments)
       print(analytes)
       legit.experiments <- !all(is.null(experiments))
       legit.analytes    <- !all(is.null(analytes))
       if(legit.experiments && legit.analytes){
          tbls <- getPlotFriendlyDataFrames(nav, analytes, experiments)
          #if(length(tbls) == 1){
          #    print(lapply(tbls[[1]], class))
          #    print(tbls)
          #    }
          if(length(tbls) < 12)
              plotAnalytes(tbls)
          else{
              plottingQuestion <- sprintf("Render %d plots?", length(tbls))
              confirmSweetAlert(session,
                                inputId="confirmPlotAlert",
                                title = "Plot?",
                                text = plottingQuestion,
                                type = "question",
                                btn_labels = c("Cancel", "Confirm"),
                                btn_colors = NULL,
                                closeOnClickOutside = FALSE,
                                showCloseButton = FALSE,
                                html = FALSE
                                )
              }
          } # if legits
        })


    observeEvent(input$confirmPlotAlert, {
       proceed <- input$confirmPlotAlert
       printf("proceed to plot: %s", proceed)
       experiments <- isolate(input$experimentPicker)
       analytes <- isolate(input$analytePicker)
       tbls <- getPlotFriendlyDataFrames(nav, analytes, experiments)
       plotAnalytes(tbls)
       })


    observeEvent(input$selectExperiment, ignoreInit=TRUE,{
       experiments <- input$selectExperiment
       print(experiments)
       # if(length(experiments) > 1 && " - " %in% experiments){

       if("All" %in% experiments){
           updateSelectInput(session, "selectExperiment", selected="All")
           printf("all experiments requested")
           }
        updatePlotCountDisplay()
        })

    observeEvent(input$selectAnalyte, ignoreInit=TRUE,{
       printf("selectAnalyte")
       analytes <- input$selectAnalyte;
       printf("analytes about to update: %s", paste(analytes, sep=","))
       updateSelectInput(session, "selectAnalyte", selected=analytes)
       updatePlotCountDisplay()
       print(analytes)
       #displayAnalyteDataByExperiment(analytes)
       })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), host="0.0.0.0", port=3838)
