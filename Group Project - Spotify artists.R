
rm(list=ls())
library(igraph)
library(tidyverse)                                                     
library(readxl)

setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI/NAI Group project") # adapt as necessary

tracklist <- read_xlsx("FILTERED DB.xlsx", sheet = "DATA")
View(tracklist)


setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI")


library(readxl)
data_from_excel <- read_xlsx("data.xlsx", sheet = "data")
View(data_from_excel)


#> NExt : create edgelist prior to matrix (see Seminar 4 script)

#lets make a loop to create the edgelist

tracks_number = cotracklist

for(i in 1:tracks_number) {                                                    
  gr     <- erdos.renyi.game(N, E, type = "gnm")
  apl[i] <- mean_distance(gr)
  cl[i]  <- transitivity(gr, type = "average")
}