# test_ProteomicsNavigator.R
#------------------------------------------------------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "C")
#------------------------------------------------------------------------------------------------------------------------
library(ProteomicsNavigator)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
data.dir <- "~/github/ProteomicsNavigator/utils/prep"
datasets <- file.path(data.dir, list.files(data.dir, pattern=".RData"))
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_ctor()
   test_loadDataSets()
   test_getAnalytes()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_ctor <- function()
{
   message(sprintf("--- test_ctor"))

   nav <- ProteomicsNavigator(datasets)

} # test_ctor
#------------------------------------------------------------------------------------------------------------------------
test_loadDataSets <- function()
{
   message(sprintf("--- test_loadDataSets"))
   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   checkEquals(sort(getDataSetNames(nav)), c("Glioma", "LCL57_1", "LCL57_2", "PBMC"))

} # test_loadDataSets
#------------------------------------------------------------------------------------------------------------------------
test_getAnalytes <- function()
{
   message(sprintf("--- test_getAnalytes"))
   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   x <- getAnalytes(nav)
   checkTrue(length(x) > 35)
   checkTrue("RTF1_pan_SASD" %in% x)

} # test_getAnalytes
#------------------------------------------------------------------------------------------------------------------------
test_getPlotFriendlyDataFrames_1_1 <- function()
{
   message(sprintf("--- test_getPlotFriendlyDataFrames_1_1"))

   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   x <- getAnalytes(nav)
   checkTrue(length(x) > 35)
   checkTrue("RTF1_pan_SASD" %in% x)

   x.01 <- getPlotFriendlyDataFrames(nav,
                                     analytes=c("TP53BP1_pS552_IDED"),
                                     experiments="Glioma")
   checkEquals(length(x.01), 1)
   tbl <- x.01[[1]]
   checkEquals(dim(tbl), c(12, 5))

   x.01 <- getPlotFriendlyDataFrames(nav,
                                     analytes=c("TP53BP1_pS552_IDED"),
                                     experiments="Glioma")

   x.all <- getPlotFriendlyDataFrames(nav, analytes=c("all"), experiments="Glioma")
   checkTrue(length(x.all), 33)

   plot <- FALSE
   if(plot){
      require("ggplot2")
      title <- names(x.01)
      ggplot(tbl, aes(x=variable, y=mean, fill=variable)) +
          geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.4, position=position_dodge(.9)) +
          ggtitle(title) +
          scale_fill_manual(values=tbl$color) +
          theme(text = element_text(size=20, face="bold"),
                axis.text.x = element_blank(), # element_text(angle = 45, hjust = 1.2, vjust=1.2),
                plot.title = element_text(color="navajowhite4", size=20, face="bold.italic", hjust=0.5),
                axis.title.x =  element_blank(),
                axis.title.y = element_text(color="dimgray", size=20, face="bold"),
                legend.text = element_text(colour="black", size=10, face="bold"))
          } # if plot

} # test_getPlotFriendlyDataFrames_1_1
#------------------------------------------------------------------------------------------------------------------------
test_getPlotFriendlyDataFrames_all_1 <- function()
{
   message(sprintf("--- test_getPlotFriendlyDataFrames_all_1"))

   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   x <- getAnalytes(nav)
   checkTrue(length(x) > 35)
   checkTrue("RTF1_pan_SASD" %in% x)

   x.all <- getPlotFriendlyDataFrames(nav, analytes=c("all"), experiments="Glioma")
   checkTrue(length(x.all), 33)

   plot <- FALSE
   if(plot){
      require("ggplot2")
      i <- 3
      title <- names(x.all[i])
      tbl <- x.all[[i]]
      ggplot(tbl, aes(x=variable, y=mean, fill=variable)) +
          geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.4, position=position_dodge(.9)) +
          ggtitle(title) +
          scale_fill_manual(values=tbl$color) +
          theme(text = element_text(size=20, face="bold"),
                axis.text.x = element_blank(), # element_text(angle = 45, hjust = 1.2, vjust=1.2),
                plot.title = element_text(color="navajowhite4", size=20, face="bold.italic", hjust=0.5),
                axis.title.x =  element_blank(),
                axis.title.y = element_text(color="dimgray", size=20, face="bold"),
                legend.text = element_text(colour="black", size=10, face="bold"))
          } # if plot

} # test_getPlotFriendlyDataFrames_all_1
#------------------------------------------------------------------------------------------------------------------------
test_getPlotFriendlyDataFrames_1_all <- function()
{
   message(sprintf("--- test_getPlotFriendlyDataFrames_1_all"))

   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   x <- getAnalytes(nav)
   checkTrue(length(x) > 35)
   aoi <- "CDC25B_pS323_SPSM"
   checkTrue(aoi %in% x)

   x.all <- getPlotFriendlyDataFrames(nav, analytes=aoi, experiments="all")
   length(x.all)
   checkEquals(length(x.all), length(getDataSetNames(nav)))

   plot <- FALSE
   if(plot){
      require("ggplot2")
      i <- 1
      title <- names(x.all[i])
      tbl <- x.all[[i]]
      ggplot(tbl, aes(x=variable, y=mean, fill=variable)) +
          geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.4, position=position_dodge(.9)) +
          ggtitle(title) +
          scale_fill_manual(values=tbl$color) +
          theme(text = element_text(size=20, face="bold"),
                axis.text.x = element_blank(), # element_text(angle = 45, hjust = 1.2, vjust=1.2),
                plot.title = element_text(color="navajowhite4", size=20, face="bold.italic", hjust=0.5),
                axis.title.x =  element_blank(),
                axis.title.y = element_text(color="dimgray", size=20, face="bold"),
                legend.text = element_text(colour="black", size=10, face="bold"))
          } # if plot

} # test_getPlotFriendlyDataFrames_all_1
#------------------------------------------------------------------------------------------------------------------------
test_getPlotFriendlyDataFrames_3_2 <- function()
{
   message(sprintf("--- test_getPlotFriendlyDataFrames_3_2"))

   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   analytes <- getAnalytes(nav)
   checkTrue(length(analytes) > 35)
   aoi <- analytes[1:3]
   eoi <- getDataSetNames(nav)[c(1,3)]

     # not all attributes are found in all experiements
     # expect as many data.frames as the count of attributes found
   survey <- unlist(lapply(1:2, function(e)
                       lapply(aoi, function(a) a %in% rownames(getDataSet(nav, eoi[e])$avg))))
   attribute.found.count <- length(which(survey))

   x.all <- getPlotFriendlyDataFrames(nav, analytes=aoi, experiments=eoi)
   checkEquals(length(x.all), attribute.found.count)

   plot <- FALSE
   if(plot){
      require("ggplot2")
      i <- 1
      title <- names(x.all[i])
      tbl <- x.all[[i]]
      ggplot(tbl, aes(x=variable, y=mean, fill=variable)) +
          geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.4, position=position_dodge(.9)) +
          ggtitle(title) +
          scale_fill_manual(values=tbl$color) +
          theme(text = element_text(size=20, face="bold"),
                axis.text.x = element_blank(), # element_text(angle = 45, hjust = 1.2, vjust=1.2),
                plot.title = element_text(color="navajowhite4", size=20, face="bold.italic", hjust=0.5),
                axis.title.x =  element_blank(),
                axis.title.y = element_text(color="dimgray", size=20, face="bold"),
                legend.text = element_text(colour="black", size=10, face="bold"))
          } # if plot

} # test_getPlotFriendlyDataFrames_all_1
#------------------------------------------------------------------------------------------------------------------------
test_.extractAnalyteInExperimentTable <- function()
{
   printf("--- test_.extractAnalyteInExperimentTable")

   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)

   analyte <- "TP53BP1_pS552_IDED"
   experiment <- "Glioma"
   tbl.data <- nav@state$datasets[[experiment]]$avg[analyte,,drop=FALSE]
   md <- getExperimentMetadata(nav, experiment)
   tbl.ae <- ProteomicsNavigator:::.extractAnalyteInExperimentTable(experiment, analyte, tbl.data, md)

} # test_.extractAnalyteInExperimentTable
#------------------------------------------------------------------------------------------------------------------------
test_getData <- function()
{
   message(sprintf("--- test_getAnalytes"))
   nav <- ProteomicsNavigator(datasets)
   loadDataSets(nav)
   analytes <- getAnalytes(nav)
   experiments <- getDataSetNames(nav)

   tbl.01 <- getData(nav,
                     analytes=c("TP53BP1_pS552_IDED"),
                     experiments="Glioma")

} # test_getData
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
