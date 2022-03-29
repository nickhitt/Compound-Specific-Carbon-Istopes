## Correlation Analysis of CSIAA and bulk C data

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
library(siar)

## setting directory

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_C")

## Functions

## Reading Data
all_corals_bulkAA <- data.frame(read_excel("AllCoralBulkandAA.xlsx", sheet = "Sheet1"))
all_corals <- data.frame(read_excel("csiaadataclean.xlsx", sheet = "All Data"))
all_corals_transpose <- data.frame(read_excel("csiaadataclean_transpose.xlsx", sheet = "All Data"))

## Bulk, Essential and Non-Essential IDs 
non_essential <- c("Ala", "Asp", "Glu", "Ile", "Leu", "Pro", "Val")
essential <- c("Gly", "Leu", "Ile", "Phe")
bulk <- c("bulk13c")
all_aa <- c("Ala", "Asp", "Glu", "Gly", "Ile", "Leu", "Phe", "Pro", "Val")

all_corals_transpose <- all_corals_transpose %>% 
  dplyr::select(sample.id, depth, Value, Isotope, Coral.ID, Time) %>%
  mutate(Coral_name = case_when(Coral.ID == "M" ~ "47996",
                                Coral.ID == "T" ~ "35104",
                                Coral.ID == "P" ~ "64344",
                                Coral.ID == "F" ~ "15131")) %>%
  mutate(type = case_when(Isotope %in% essential ~ "Essential",
                          Isotope %in% non_essential ~ "Non Essential",
                          Isotope %in% bulk ~ "Bulk")) %>%
  mutate(location = case_when(Coral.ID == "M" ~ "STF",
                              Coral.ID == "T" ~ "BOP",
                              Coral.ID == "P" ~ "BOP",
                              Coral.ID == "F" ~ "STF"))

## Limiting to 35104
code <- c(1,2,3,4,5,6,7)
bayes_35104 <- all_corals_bulkAA %>%
  filter(Coral == "35104") %>%
  dplyr::select(Thr, Leu, Phe, Ile) %>%
  dplyr::filter(!is.na(Phe)) 

bayes_35104 <- cbind(code, bayes_35104)

## Source End Member Data

source_end_members <- data.frame(read_excel("Source End Member Data_Bayesian.xlsx", sheet = "Normalised EAAs"))

## Running the model

model1<- siarmcmcdirichletv4(bayes_35104,source_end_members,corrections = 0,concdep=0,500000,50000)

siarplotdata(model1)
