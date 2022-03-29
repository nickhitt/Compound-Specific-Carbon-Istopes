## New Correlation Analysis of CSIAA and bulk C data

library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(gridExtra)
library(ggbiplot)

## setting directory

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_C")

## Functions

csiaa_corr <- function(coral_data) {
  all_aa <- c("Ala", "Asp", "Glu", "Gly", "Ile", "Leu", "Phe", "Pro", "Thr", "Val")
  bulk <- coral_data[,1]
  coral_data <- coral_data[,2:11]
  mat <- matrix(nrow = 10, ncol = 2)
  for (i in 1:10) {
    res <- cor.test(bulk, coral_data[,i], 
                    method = "pearson")
    mat[i,1] <- res$estimate
    mat[i,2] <- res$p.value
    rm(res)
  }
  mat <- as.data.frame(mat)
  mat <- cbind(all_aa, mat)%>%
    dplyr::mutate(significance = case_when(V2 < 0.1 ~ "significant"))
  return(mat)
}

## Reading Data
all_corals_bulkAA <- data.frame(read_excel("AllCoralBulkandAA.xlsx", sheet = "Sheet1"))
all_corals <- data.frame(read_excel("csiaadataclean.xlsx", sheet = "All Data"))
all_corals_transpose <- data.frame(read_excel("csiaadataclean_transpose.xlsx", sheet = "All Data"))

## Subsetting M

coral_m <- all_corals %>%
  dplyr::filter(Coral == "M") %>%
  dplyr::select(-sample.id, -depth, -Coral)

## Subsetting P

coral_p <- all_corals %>%
  dplyr::filter(Coral == "P") %>%
  dplyr::select(-sample.id, -depth, -Coral)

## Subsetting T

coral_t <- all_corals %>%
  dplyr::filter(Coral == "T") %>%
  dplyr::select(-sample.id, -depth, -Coral)

## Subsetting All Corals

all_corals_corr <- all_corals %>%
  dplyr::select(-sample.id, -depth, -Coral)

## Correlations
corr_all <- csiaa_corr(all_corals_corr)
corr_m <- csiaa_corr(coral_m)
corr_p <- csiaa_corr(coral_p)
corr_t <- csiaa_corr(coral_t)
