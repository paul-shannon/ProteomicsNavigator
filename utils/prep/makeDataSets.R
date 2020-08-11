library(ExcelProteomicsImporter)
library(yaml)

   #-----------------------------------------------------
   # test_data_LCL57cell_line_experiment_clean
   #-----------------------------------------------------

base.name <- "test_data_LCL57cell_line_experiment_clean"
tsv.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.txt", base.name))
yaml.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.yaml", base.name))
importer <- ExcelProteomicsImporter(tsv.file)
x <- getMatrices(importer, NA.to.Zero=TRUE)
x$md <- yaml.load(readLines(yaml.file))
save(x, file="LCL57_1.RData")


   #-----------------------------------------------------
   # test_data2_LCL57cell_line_experiment2
   #-----------------------------------------------------

base.name <- "test_data2_LCL57cell_line_experiment2"
tsv.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.txt", base.name))
yaml.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.yaml", base.name))
importer <- ExcelProteomicsImporter(tsv.file)
x <- getMatrices(importer, NA.to.Zero=TRUE)
x$md <- yaml.load(readLines(yaml.file))
save(x, file="LCL57_2.RData")

   #-----------------------------------------------------
   # test_data4_Glioma_cell_line_experiment
   #-----------------------------------------------------

base.name <- "test_data4_Glioma_cell_line_experiment"
tsv.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.txt", base.name))
yaml.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.yaml", base.name))
importer <- ExcelProteomicsImporter(tsv.file)
x <- getMatrices(importer, NA.to.Zero=TRUE)
x$md <- yaml.load(readLines(yaml.file))
save(x, file="Glioma.RData")

   #-----------------------------------------------------
   # test_data3_PBMC_experiment
   #-----------------------------------------------------

base.name <- "test_data3_PBMC_experiment"
tsv.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.txt", base.name))
yaml.file <- system.file(package="ExcelProteomicsImporter", "extdata", sprintf("%s.yaml", base.name))
importer <- ExcelProteomicsImporter(tsv.file)
x <- getMatrices(importer, NA.to.Zero=TRUE)
x$md <- yaml.load(readLines(yaml.file))
save(x, file="PBMC.RData")


