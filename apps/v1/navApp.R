library(ProteomicsNavigator)
library(shinydashboard)
library(shinyModules)
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
      div(messageBoxUI(id="plotCountBox", title=NULL, boxWidth=50, boxHeight=30,
                   fontSize=18, backgroundColor="beige"), style="margin-left: 25px;"),
      actionButton("displayPlotsButton", label="Plot"),
      div(selectInput("selectExperiment",
                  label="Choose Experiments",
                  c("None", "All", datasetNames),
                  selectize=TRUE,
                  multiple=TRUE,
                  selected="None"
                  ),
          style="margin-bottom: 150px"),

      selectInput("selectAnalyte",
                  label="Choose Analytes",
                  c("None", "All", analytes),
                  selectize=TRUE,
                  multiple=TRUE,
                  selected="None"
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
    plotCount <- reactiveVal(0)
    plotCountBox.contents <- callModule(messageBoxServer, "plotCountBox", newContent=plotCount)

    updatePlotCountDisplay <- function(){
       experiments <- input$selectExperiment
       analytes <- input$selectAnalyte
       if(!(experiments == "None" || analytes == "None")){
          tbls <- getPlotFriendlyDataFrames(nav, analytes, experiments)
          plotCount(length(tbls))
          }
       }

    observeEvent(input$displayPlotsButton, {
        printf("--- display plots")
        })

    observeEvent(input$selectExperiment, ignoreInit=TRUE,{
       experiments <- input$selectExperiment
       print(experiments)
       if(length(experiments) > 1 && "None" %in% experiments){
          deleter <- grep("None", experiments)
          experiments <- experiments[-deleter]
          updateSelectInput(session, "selectExperiment", selected=experiments)
          }

       if("All" %in% experiments){
           updateSelectInput(session, "selectExperiment", selected="All")
           printf("all experiments requested")
           }
        updatePlotCountDisplay()
        })

    observeEvent(input$selectAnalyte, ignoreInit=TRUE,{
       printf("selectAnalyte")
       analytes <- input$selectAnalyte;
       if(length(analytes) > 1 && "None" %in% analytes){
          printf("found 'None' in selected analytes")
          deleter <- grep("None", analytes)
          analytes <- analytes[-deleter]
          }
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
