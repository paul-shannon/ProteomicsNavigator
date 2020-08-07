library(shinydashboard)
library(shinyModules)
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
tbl <- get(load("tbl.39analytes.1157x6.RData"))
analytes <- sort(unique(tbl$analyte))

#----------------------------------------------------------------------------------------------------
ui.dashboard <- dashboardPage(

  dashboardHeader(title = "Proteomic Assays"),
  dashboardSidebar(
      selectInput("selectAnalyte",
                  label="Choose Analyte",
                  c(" - ", analytes),
                  selectize=TRUE
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
    observeEvent(input$selectAnalyte, ignoreInit=TRUE,{
       analyte <- input$selectAnalyte;
       if(analyte != " - "){
          printf("new analyte: %s", analyte)
          displayAnalyteDataByExperiment(analyte)
          }
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
runApp(shinyApp(ui.dashboard, server), host="0.0.0.0", port=3838)
