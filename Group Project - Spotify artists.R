
rm(list=ls())
library(igraph)
library(tidyverse)                                                     
library(readxl)

setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI/NAI Group project") # adapt as necessary

tracklist <- read_xlsx("FILTERED DB.xlsx", sheet = "DATA")
View(tracklist)


setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI")


tracklist2 <- tracklist %>%
  select("id", "CORRECTED_ARTISTS") %>% separate_rows("CORRECTED_ARTISTS", sep = ";") 

 # check later if unique ID



PO <- as.matrix(table(tracklist2$id, tracklist2$CORRECTED_ARTISTS))       #Obtain project-organisation matrix from dataframe
OO <- t(PO)%*%PO                                                        #Adjacency matrix (artists-artists)
#PP <- PO%*%t(PO)                                                        #Adjacency matrix (project-project)



#.Artists network  ----------
diag(OO) <- 0                                                          #Set diagonal to 0 (no self-loops)
OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
plot(OO_g)




