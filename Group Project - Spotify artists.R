rm(list=ls())
library(igraph)
library(tidyverse)                                                     
library(readxl)

setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI/NAI Group project") # adapt as necessary

tracklist <- read_xlsx("FILTERED DB.xlsx", sheet = "DATA")
View(tracklist)


setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI")


tracklist %>%
  select("id", "CORRECTED_ARTISTS") %>% separate_rows("CORRECTED_ARTISTS", sep = ";") 

 # chekc later if unique ID



PO <- as.matrix(table(tracklist$id, tracklist$CORRECTED_ARTISTS))                         #Obtain project-organisation matrix from dataframe
OO <- t(PO)%*%PO                                                        #Adjacency matrix (organisation-organisation)
PP <- PO%*%t(PO)                                                        #Adjacency matrix (project-project)



#12.Organisation network  ----------
diag(OO) <- 0                                                           #Set diagonal to 0 (no self-loops)
OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
plot(OO_g)  

#13.Project network  ----------
diag(PP) <- 0                                                           #Set diagonal to 0 (no self-loops)
PP_g <- graph_from_adjacency_matrix(PP, mode = "undirected")            #Obtain graph from adjacency matrix
plot(PP_g)                                                              #Plot network





#> NExt : create edgelist prior to matrix (see Seminar 4 script)

#lets make a loop to create the edgelist

tracks_number = cotracklist
