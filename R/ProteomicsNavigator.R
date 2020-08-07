#' @import org.Hs.eg.db
#' @import yaml
#' @importFrom methods new
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
setGeneric('getAnalytes', signature='obj', function(obj) standardGeneric ('getAnalytes'))
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
