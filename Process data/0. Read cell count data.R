#0. Read cell count data excel file
#Input
#excel.file.name = path to excel file

readExcelCountTable <- function (excel.file.name, tree.description.sheet.name = "Tree description", 
          sampling.description.sheet.name = "Sampling description") 
{
  library(gdata)
  message("--> Reading of tree description sheet... \r")
  TreeDF <- read.xls(excel.file.name, sheet = tree.description.sheet.name)
  TreeDF.col.names <- names(TreeDF)
  if ("Site" %in% TreeDF.col.names == FALSE) {
    stop("Site index not found in tree description sheet --> Reading aborted!")
  }
  TreeDF$Site <- as.factor(TreeDF$Site)
  if ("Tree" %in% TreeDF.col.names == FALSE) {
    stop("Tree column not found in tree description sheet --> Reading aborted!")
  }
  TreeDF$Tree <- as.factor(TreeDF$Tree)
  if ("Species" %in% TreeDF.col.names == FALSE) {
    stop("Species column not found in tree description sheet --> Reading aborted!")
  }
  TreeDF$Species <- as.factor(TreeDF$Species)
  if ("Age" %in% TreeDF.col.names == FALSE) {
    warning("Age column not found in tree description sheet -->\n            corresponding column is filled with NA, reading continue...")
    TreeDF$Age <- as.integer(NA)
  }
  TreeDF$Age <- as.integer(TreeDF$Age)
  if ("Diameter" %in% TreeDF.col.names == FALSE) {
    warning("Diameter column not found in tree description sheet -->\n            corresponding column is filled with NA, reading continue...")
    TreeDF$Diameter <- as.numeric(NA)
  }
  TreeDF$Diameter <- as.numeric(TreeDF$Diameter)
  if ("Height" %in% TreeDF.col.names == FALSE) {
    warning("Height column not found in tree description sheet -->\n            corresponding column is filled with NA, reading continue...")
    TreeDF$Height <- as.numeric(NA)
  }
  TreeDF$Height <- as.numeric(TreeDF$Height)
  if ("Remark" %in% TreeDF.col.names == TRUE) {
    message("RM. Please note that remark column from tree description sheet will not be incorporated\n             in the final table...")
  }
  TreeDF <- TreeDF[c("Site", "Tree", "Species", "Age", "Diameter", 
                     "Height")]
  message("...Reading of tree description sheet done. \n")
  message("--> Reading of sampling description sheet... \r")
  SamplingDF <- read.xls(excel.file.name, sheet = sampling.description.sheet.name)
  SamplingDF.col.names <- names(SamplingDF)
  if ("Year" %in% SamplingDF.col.names == FALSE) {
    stop("Year column not found in sampling description sheet --> Reading aborted!")
  }
  SamplingDF$Year <- as.integer(SamplingDF$Year)
  if ("Sample" %in% SamplingDF.col.names == FALSE) {
    stop("Sample column not found in sampling description sheet --> Reading aborted!")
  }
  SamplingDF$Sample <- as.integer(SamplingDF$Sample)
  if ("DY" %in% SamplingDF.col.names == FALSE) {
    stop("DY column not found in sampling description sheet --> Reading aborted!")
  }
  SamplingDF$DY <- as.integer(SamplingDF$DY)
  if ("Remark" %in% TreeDF.col.names == TRUE) {
    message("RM. Please note that remark column from sampling description sheet will not be incorporated in the final table...")
  }
  SamplingDF <- SamplingDF[c("Year", "Sample", "DY")]
  message("... Reading of sampling description sheet done. \n")
  AggDF <- data.frame()
  cell.count.sheet.names <- as.factor(paste("Tree", TreeDF$Tree))
  message("--> Reading cell count data sheets... \r")
  for (i in 1:nlevels(cell.count.sheet.names)) {
    message(paste(i, ".", "\t --> Reading data for tree ", 
                  TreeDF$Tree[i], " in cell count sheet ", cell.count.sheet.names[i], 
                  "... \r", sep = ""))
    InDF <- read.xls(excel.file.name, sheet = cell.count.sheet.names[i])
    InDF.col.names <- names(InDF)
    if ("Sample" %in% InDF.col.names == FALSE) {
      stop(paste("Sample column not found in", cell.count.sheet.names[i], 
                 " cell count sheet --> reading aborted!", sep = ""))
    }
    stc <- sum(ifelse(c("C1", "C2", "C3") %in% InDF.col.names, 
                      0, 1))
    if (stc > 0) {
      stop(paste("C1, C2 or C3 (i.e. cambial cell count) not found in", 
                 cell.count.sheet.names[i], " cell count sheet --> reading aborted!", 
                 sep = ""))
    }
    ste <- sum(ifelse(c("E1", "E2", "E3") %in% InDF.col.names, 
                      0, 1))
    if (ste > 0) {
      stop(paste("E1, E2 or E3 (i.e. enlarging cell count) not found in", 
                 cell.count.sheet.names[i], " cell count sheet --> reading aborted!", 
                 sep = ""))
    }
    stt <- sum(ifelse(c("W1", "W2", "W3") %in% InDF.col.names, 
                      0, 1))
    if (stt > 0) {
      stop(paste("W1, W2 or W3 (i.e. thickening cell count) not found in", 
                 cell.count.sheet.names[i], " cell count sheet --> reading aborted!", 
                 sep = ""))
    }
    stm <- sum(ifelse(c("M1", "M2", "M3") %in% InDF.col.names, 
                      0, 1))
    if (stm > 0) {
      stop(paste("M1, M2 or M3 (i.e. mature cell count) not found in", 
                 cell.count.sheet.names[i], " cell count sheet --> reading aborted!", 
                 sep = ""))
    }
    stp <- sum(ifelse(c("P1", "P2", "P3") %in% InDF.col.names, 
                      0, 1))
    if (stp > 0) {
      stop(paste("P1, P2 or P3 (i.e. previous ring cell count) not found in", 
                 cell.count.sheet.names[i], " cell count sheet --> reading aborted!", 
                 sep = ""))
    }
    IDF <- merge(SamplingDF, InDF)
    Site <- as.factor(rep.int(TreeDF$Site[i], 3 * nrow(IDF)))
    Tree <- as.factor(rep.int(TreeDF$Tree[i], 3 * nrow(IDF)))
    Species <- as.factor(rep.int(TreeDF$Species[i], 3 * nrow(IDF)))
    Year <- as.integer(rep.int(IDF$Year, 3))
    Sample <- as.integer(rep.int(IDF$Sample, 3))
    DY <- as.integer(rep.int(IDF$DY, 3))
    RF <- as.factor(c(rep.int(1, nrow(IDF)), rep.int(2, nrow(IDF)), 
                      rep.int(3, nrow(IDF))))
    CZ <- as.integer(c(IDF$C1, IDF$C2, IDF$C3))
    EZ <- as.integer(c(IDF$E1, IDF$E2, IDF$E3))
    WZ <- as.integer(c(IDF$W1, IDF$W2, IDF$W3))
    MZ <- as.integer(c(IDF$M1, IDF$M2, IDF$M3))
    PR <- as.numeric(c(IDF$P1, IDF$P2, IDF$P3))
    TempDF <- data.frame(Site, Year, Species, Tree, Sample, 
                         DY, RF, CZ, EZ, WZ, MZ, PR)
    AggDF <- rbind(AggDF, TempDF)
  }
  message(paste("... Reading of ", i, " cell count sheets done. \n"))
  removeTest <- function(X) {
    is.na(X[1]) & is.na(X[2]) & is.na(X[3]) & is.na(X[4])
  }
  AggDF$Remove <- apply(AggDF[, 8:11], 1, removeTest)
  ODF <- AggDF[AggDF$Remove == FALSE, ]
  ODF <- ODF[, 1:(ncol(ODF) - 1)]
  ODF <- ODF[order(ODF$Site, ODF$Year, ODF$Species, ODF$Tree, 
                   ODF$Sample, ODF$RF), ]
  return(ODF)
}
