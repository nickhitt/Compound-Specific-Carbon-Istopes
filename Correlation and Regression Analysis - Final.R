## New Correlation Analysis of CSIAA and bulk C data

##### Note: Does not include Lys - to be updated when Lys data comes through

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
  all_aa <- c("Ala", "Asp", "Glu", "Gly", "Ile", "Leu", 
              "Phe", "Pro", "Thr", "Val")
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
all_corals <- data.frame(read_excel("csiaadataclean_with_lys.xlsx", sheet = "All Data"))

## Subsetting M

coral_m <- all_corals %>%
  dplyr::filter(Coral == "M") %>%
  dplyr::select(-sample.id, -depth, -Coral, -age, -Lys)

## Subsetting P

coral_p <- all_corals %>%
  dplyr::filter(Coral == "P") %>%
  dplyr::select(-sample.id, -depth, -Coral, -age, -Lys)

## Subsetting T

coral_t <- all_corals %>%
  dplyr::filter(Coral == "T") %>%
  dplyr::select(-sample.id, -depth, -Coral, -age, -Lys)

## Subsetting F

coral_f <- all_corals %>%
  dplyr::filter(Coral == "F") %>%
  dplyr::select(-sample.id, -depth, -Coral, -age, -Lys)

## Subsetting All Corals

all_corals_corr <- all_corals %>%
  dplyr::select(-sample.id, -depth, -Coral, -age, -Lys)

## Correlations
corr_all <- csiaa_corr(all_corals_corr)
corr_m <- csiaa_corr(coral_m)
corr_p <- csiaa_corr(coral_p)
corr_t <- csiaa_corr(coral_t)
corr_f <- csiaa_corr(coral_f)

#################### Regression Figures by Coral

all_corals_bulkAA <- all_corals %>%
  dplyr::select(-depth, -age)

all_aa <- c("Ala", "Asp", "Glu", "Gly", "Ile", "Leu", "Lys", "Phe", "Pro", "Thr", "Val")

## Formatting Data

data_reshaped <- reshape(all_corals_bulkAA, 
                         varying = c(all_aa), 
                         v.names = "Value",
                         timevar = c("AA"), 
                         times = c(all_aa), 
                         direction = "long") %>%
  dplyr::select(-id) %>%
  dplyr::mutate(Value = round(as.numeric(Value), 2))

row.names(data_reshaped) <- 1:nrow(data_reshaped)

## Y Axis Rounding Function
scaleFUN <- function(x) {sprintf("%.2f", x)}

## Plotting function
regress_plot_coral <- function(data, coral){
  regress_plot <- data %>%
    dplyr::filter(Coral == coral) %>%
    ggplot(mapping = aes(bulk13c, Value, group = AA)) +
    facet_wrap(~ AA, scales = "free") + 
    geom_point() + geom_smooth(method = "lm") +
    theme(text = element_text(size=20))
}

p <- regress_plot_coral(data_reshaped, "P")
p

m <- regress_plot_coral(data_reshaped, "M")
m

t <- regress_plot_coral(data_reshaped, "T")
t

f <- regress_plot_coral(data_reshaped, "F")
f

