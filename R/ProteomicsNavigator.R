#' @import org.Hs.eg.db
#' @import yaml
#' @importFrom methods new
#' @import RColorBrewer
#'
#' @title ProteomicsNavigator
#------------------------------------------------------------------------------------------------------------------------
#' @name ProteomicsNavigator-class
#' @rdname ProteomicsNavigator-class
#' @aliases ProteomicsNavigator
#'
#' @import methods

.ProteomicsNavigator <- setClass("ProteomicsNavigator",
                          representation = representation(
                              files="character",
                              state="environment"
                             )
                          )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('loadDataSets', signature='obj', function(obj) standardGeneric ('loadDataSets'))
setGeneric('getDataSetNames', signature='obj', function(obj) standardGeneric ('getDataSetNames'))
setGeneric('getDataSet', signature='obj', function(obj, dataSetName) standardGeneric ('getDataSet'))
setGeneric('getAnalytes', signature='obj', function(obj) standardGeneric ('getAnalytes'))
setGeneric('getExperimentMetadata', signature='obj', function(obj, dataSetName)
    standardGeneric('getExperimentMetadata'))
setGeneric('getPlotFriendlyDataFrames', signature='obj', function(obj, analytes, experiments)
    standardGeneric('getPlotFriendlyDataFrames'))

#------------------------------------------------------------------------------------------------------------------------
#' Create a ProteomicsNavigator connection
#'
#' @rdname ProteomicsNavigator-class
#'
#' @param tabDelimitedFilename character
#'
#' @return An object of the ProteomicsNavigator class
#'
#' @export
#'
ProteomicsNavigator <- function(datasetFilePaths)
{
   state <- new.env(parent=emptyenv())
   obj <- .ProteomicsNavigator(files=datasetFilePaths)
   obj

} # ctor
#------------------------------------------------------------------------------------------------------------------------
#' discern the experiment names, their constant values, and the experimental manipulations
#'
#' @rdname loadDataSets
#' @aliases loadDataSets
#'
#' @export
#'
setMethod('loadDataSets', 'ProteomicsNavigator',

     function(obj){
        datasets <- list()
        for(file in obj@files){
           name <- sub(".RData", "", basename(file))
           datasets[[name]] <- get(load(file))  # a list of 2 matrices (avg, sd) and md (metadata)
           stopifnot(all(c("avg", "sd", "md") %in% names(datasets[[name]])))
           } # for file
        obj@state$datasets <- datasets
        })

#------------------------------------------------------------------------------------------------------------------------
#' discern the experiment names, their constant values, and the experimental manipulations
#'
#' @rdname getDataSetNames
#' @aliases getDataSetNames
#'
#' @export
#'
setMethod('getDataSetNames', 'ProteomicsNavigator',

     function(obj){
        names(obj@state$datasets)
        })

#------------------------------------------------------------------------------------------------------------------------
#' return 2 matrices: avg & sd
#'
#' @rdname getDataSet
#' @aliases getDataSet
#'
#' @param obj a ProteomicsNavigator instance
#' @param dataSetName a character string
#'
#' @export
#'
setMethod('getDataSet', 'ProteomicsNavigator',

     function(obj, dataSetName){
        stopifnot(dataSetName %in% names(obj@state$datasets))
        obj@state$datasets[[dataSetName]]
        })

#------------------------------------------------------------------------------------------------------------------------
#' the sorted, combined, unique list of analytes, rownames of the 'avg' matrices
#'
#' @rdname getAnalytes
#' @aliases getAnalytes
#'
#' @export
#'
setMethod('getAnalytes', 'ProteomicsNavigator',

     function(obj){
        datasets <- obj@state$datasets
        analytes <- sort(unique(unlist(lapply(datasets, function(dataset) rownames(dataset$avg)),
                                        use.names=FALSE)))
        deleters <- which(nchar(analytes) == 0)
        if(length(deleters) > 0)
          analytes <- analytes[-deleters]
        analytes
        })

#------------------------------------------------------------------------------------------------------------------------
#' get metadata for the specified experiment (aka dataset)
#'
#' @rdname getExperimentMetadata
#' @aliases getExperimentMetadata
#'
#' @export
#'
setMethod('getExperimentMetadata', 'ProteomicsNavigator',

     function(obj, dataSetName){
        if(!dataSetName %in% names(obj@state$datasets))
             return(NULL)
        obj@state$datasets[[dataSetName]]$md
        })

#------------------------------------------------------------------------------------------------------------------------
# #' avg and sd data.frames for the specified analytes, in the specified experiments
# #'
# #' @rdname getData
# #' @aliases getData
# #'
# #' @export
# #'
# setMethod('getData', 'ProteomicsNavigator',
#
#      function(obj, analytes, experiments){
#         browser();
#         xyz <- 99
#         datasets <- obj@state$datasets
#         for(experiment in experiments){
#            md <- datasets[[experiment]]$md
#            for(analyte in analytes){
#               tbl.data <- nav@state$datasets[[experiment]]$avg[analyte,,drop=FALSE]
#               tbl.ae <- .extractAnalyteTable(experiment, analyte, tbl.data, md)
#               } # for analyte
#            } # for experiment
#         })
#
#
#------------------------------------------------------------------------------------------------------------------------
#'  a list of dataframes, each ready for plotting
#'
#' @rdname getPlotFriendlyDataFrames
#' @aliases getPlotFriendlyDataFrames
#'
#' @export
#'
setMethod('getPlotFriendlyDataFrames', 'ProteomicsNavigator',

     function(obj, analytes, experiments){

         result <- list()
         if(all(tolower(experiments) == "all"))
             experiments <- getDataSetNames(obj)

         for(experimentName in experiments){
             dataset <- getDataSet(obj, experimentName)
             if(all(tolower(analytes) == "all"))
                analytes.this.experiment <- getAnalytes(obj)
             else
                analytes.this.experiment <- analytes
             analytes.this.experiment <- intersect(analytes.this.experiment, rownames(dataset$avg))
             for(analyte in analytes.this.experiment){
                 tblName <- sprintf("%s-%s", experimentName, analyte)
                 mtx.avg <- dataset$avg
                 mtx.sd  <- dataset$sd
                 colors <- rep(brewer.pal(12, "Paired"), 100)[seq_len(ncol(mtx.avg))]
                 x <- data.frame(variable=colnames(mtx.avg),
                                 mean=as.numeric(mtx.avg[analyte,]),
                                 sd=as.numeric(mtx.sd[analyte,]),
                                 analyte=rep(analyte, ncol(mtx.avg)),
                                 color=colors,
                                 stringsAsFactors=FALSE)
                 result[[tblName]] <- x
                 } # for analyte
              } # for experimentName
          result
     }) # getPlotFriendlyDataFrames

#------------------------------------------------------------------------------------------------------------------------
# create a table like this from tbl.data and md.  this can then
#                analyte experiment time radiation     area         sd group
# 141 TP53BP1_pS552_IDED       ATMi    1         0 2.096408 0.42037859  ATMi
# 142 TP53BP1_pS552_IDED       ATMi    2         5 1.914030 0.28831556  ATMi
# 143 TP53BP1_pS552_IDED       ATMi    3         5 2.195936 0.05003181  ATMi
# 144 TP53BP1_pS552_IDED       ATMi    4         5 1.919047 0.36799754  ATMi
# 145 TP53BP1_pS552_IDED       ATMi    5         5 2.370453 0.31887196  ATMi
#
# analyte [1] "TP53BP1_pS552_IDED"
# colname "G7B_none_0Gy_1"
# do.call(rbind, lapply(condition$treatments, data.frame))
#        name level   units
# 1      DMSO     1 logical
# 2 radiation     0      Gy
# 3      time     1   hours
#
# $conditions[[1]]$sampleType  # [1] "G7B"
# transformed into
#
#             analyte  sample   treatment  radiation   time   area    sd
#  TP53BP1_pS552_IDED     G7B        DMSO          0      1   x.xx  y.yy
#  ...
# which is almost ready to be plotted
# .extractAnalyteInExperimentTable <- function(experimentName, analyte, tbl.data, md)
# {
#    colnames <- colnames(tbl.data)
#
#    for(colname in colnames){ # find the corresponding metadata
#       index <- grep(colname, unlist(lapply(md$conditions, function(cond) cond$name)))
#       condition <- md$conditions[[index]]
#       tbl.md.condition <- do.call(rbind, lapply(condition$treatments, data.frame))
#       browser()
#       xyz <- 678
#       } # for colname
#
# } # .extractAnalyteInExperimentTable
#------------------------------------------------------------------------------------------------------------------------
