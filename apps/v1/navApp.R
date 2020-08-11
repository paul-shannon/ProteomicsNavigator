library(ProteomicsNavigator)
library(shinydashboard)
library(shinyModules)
library(shinyWidgets)
library(shinyjs)
#------------------------------------------------------------------------------------------------------------------------
data.dir <- "~/github/ProteomicsNavigator/utils/prep"
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
      wellPanel(actionButton("displayPlotsButton", label="Plot"),
                style="background-color: lightgray; margin: 20px;"),
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
        }

    observeEvent(input$displayPlotsButton, {
       experiments <- isolate(input$experimentPicker)
       analytes <- isolate(input$analytePicker)
       print(experiments)
       print(analytes)
       legit.experiments <- !all(is.null(experiments))
       legit.analytes    <- !all(is.null(analytes))
       if(legit.experiments && legit.analytes){
          tbls <- getPlotFriendlyDataFrames(nav, analytes, experiments)
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
displayAnalyteDataByExperiment <- function(analyte.name)
{
   tbl.sub <- subset(tbl, analyte==analyte.name) # [, coi] #  & groupName==exoi.01)[, coi]
   experiment.groups <- sort(unique(tbl.sub$group))
   if(length(experiment.groups) == 0){
       printf("no experiments for analyte '%s'", analyte.name)
       return()
       }
   printf("--- experiment groups: %d", length(experiment.groups))
   print(experiment.groups)

   removeUI("#temporaryDiv")
   insertUI("#plotBoxDiv", "beforeEnd", div(id="temporaryDiv"))

   for(experiment.group in experiment.groups){
     coi <- c("time", "experiment", "radiation", "area", "sd")
     tbl.exp <- subset(tbl.sub, group==experiment.group)[, coi]
     printf("--- displaying %s in %s, %d rows", analyte.name, experiment.group, nrow(tbl.exp))
     if(nrow(tbl.exp) == 0) break;
     box.title <- sprintf("%s: %s", analyte.name, experiment.group)
     box.id <- sprintf("%s-%s", analyte.name, experiment.group)
     printf("--- calling insertUI, for %s, %s", analyte.name, experiment.group)
     insertUI("#temporaryDiv", "beforeEnd",
              box(ExperimentalMeasuresUI(id=box.id, title=box.title, boxWidth=400),
                  title=box.title,
                  width=4,
                  solidHeader=TRUE))
     ExperimentalMeasuresServer(id=box.id, tbl=tbl.exp)
     }

} # displayAnalyteDataByExperiment
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), host="0.0.0.0", port=3839)
