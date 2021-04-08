#. 0 Retrieve librairy, environnent and main dataset

rm(list=ls())
library(igraph)
library(tidyverse)                                                     
library(readxl)

setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI/NAI Group project") # adapt as necessary

tracklist <- read_xlsx("FILTERED DB.xlsx", sheet = "DATA")
View(tracklist)

tracklist2 <- tracklist %>%
  filter(year > 1990 & year < 2000)  %>%
  select("id", "CORRECTED_ARTISTS") %>% separate_rows("CORRECTED_ARTISTS", sep = ";") 
tracklist2 <- tracklist2[!(tracklist2$CORRECTED_ARTISTS==""),]
tracklist2

#. 1 Transform into appropriate network table

PO <- as.matrix(table(tracklist2$id, tracklist2$CORRECTED_ARTISTS))       #Obtain project-organisation matrix from dataframe
OO <- t(PO)%*%PO                                                        #Adjacency matrix (artists-artists)
#PP <- PO%*%t(PO)                                                        #Adjacency matrix (project-project)

#. 2 Retrieve a few attributes

   #uniquecollab
   uniquecollab <- as.data.frame(rowSums(OO == 0, na.rm = TRUE))
   n <- as.numeric(count(uniquecollab))
   n
   uniquecollab <- n - uniquecollab
   colnames(uniquecollab) <- c("uniquecollab")
   uniquecollab

   #artist_tracks
   artist_tracks = as.data.frame(diag(OO))
   colnames(artist_tracks) <- c("uniquetracks")
   artist_tracks
  
   #artist_location (many n/a)
 
         metadata <- read_csv("top10k-spotify-artist-metadata.csv")
         metadata = as.data.frame(metadata)
         artist_location = as.data.frame(row.names(artist_tracks))
         colnames(artist_location) <- c("artist")
      
         artist_loc = artist_location 
         artist_loc <- artist_loc %>%
            group_by(artist) %>%
            left_join(metadata) %>% 
            select ("city_2")
         artist_loc
      
         summary(artist_location)
         summary(metadata)

#. 3 Lets plot that  ----------
diag(OO) <- 0                                                          #Set diagonal to 0 (no self-loops)
OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
V(OO_g)$size <- as.numeric(log(artist_tracks$uniquetracks))
plot(OO_g, vertex.label=NA)

l_random <- layout.random(OO_g)                                        #Random layout
plot(OO_g, layout = l_random, vertex.label=NA)  

l_kk <- layout_with_kk(OO_g)                                           #Kamada-Kawai layout
plot(OO_g, layout = l_kk, vertex.label=NA) 

#. 4 Lets look at the usual network stats:
   
      #. 4.1.Diameter = 1023
      d_g <- diameter(OO_g, directed = FALSE, unconnected = FALSE)           #Diameter
      get.diameter(OO_g)                                                     #Nodes on the diameter
      d_g
      
      #. 4.2.APL  ---------- ?
      #Unconnected network (apl is evalauted on the largest component)
      apl_g2  <- mean_distance(OO_g, directed = FALSE, unconnected = TRUE)  #APL
      dist_g2 <- distances(OO_g)                                            #Get the distance matrix
      dist_g2
      
      
      #. 4.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
      ed_g <- edge_density(OO_g)                                             #Calculate density
      ed_g
      
      #. 4.4.Components  ---------- 29 components, including one huge 964 component, 1 x 13, and the rest <6
      #Unconnected network
      comp_g2 <- components(OO_g)                                           #Calculate the number of components
      comp_g2                                                             #Components                                                  
      
      #. 4.5.Cutpoints/Bridges  ---------- 98 cutpoints
      cp_g <- articulation_points(OO_g)                                      #Cutpoints
      cp_g
      
      #'NOTE: There is no specific function in igraph to identify bridges,
      
      #. 4.6.Point/line connectivity  > 0 > its already an unconnected component
      #Unconnected network
      pointc_g2 <- min_cut(OO_g)                                           #Point connectivity
      pointc_g2
      
      #. 4.7.Cliques ---------- there a 788 3-cliques
      #help("cliques")                                                    #Explore this function
      #help("count_max_cliques")                                          #Explore this function
      
      
      #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
      #cliques_g
      numcliques_g <- count_max_cliques(OO_g, min = 3)                      #Number of cliques
      numcliques_g
      
      
      #. 4.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
      
      numisolates_g <- sum(degree(OO_g)==0)                                 #Number of isolates
      numisolates_g
      isolates_g <- V(OO_g)[degree(OO_g)==0]                                   #List of isolates
      isolates_g
      inclusiveness_g <- (vcount(OO_g)-numisolates_g)/vcount(OO_g)             #Calculate inclusiveness
      inclusiveness_g
            
      #. 4.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
      #Display the network
      dist_g <- distances(OO_g)                                             #Get the distance matrix
      dist_g
      observed_pairs_g <- (sum(!is.infinite(distances(OO_g)))-vcount(OO_g))/2  #Observed reachable pairs
      observed_pairs_g
      potentail_pairs_g <- vcount(OO_g)*(vcount(OO_g)-1)/2                     #Potential reachable pairs
      potentail_pairs_g
      reach_g <- observed_pairs_g/potentail_pairs_g                      #Proportion of reachable pairs
      reach_g
            
      #. 4.10.Transitivity ---------- transitivity at 0.1550432
      
      transitivity_g <- transitivity(OO_g, type = "globalundirected")       #Calculate transitivity
      transitivity_g
          

      
# WHAT NEXT ?  ## >> THIS IS TOO MESSY - WE MUST DROP SOME ARTISTS
   #1 LETS DROP THE ARTISTS WITH NO or less than x COLLABORATIONs
   #2 narrow down the BBC artists to those with a certain amount of tracks
   #3 limit the period further by decades / just look 90s
   #4 6 degrees of Tupac ?
   #5 select one artist for east coast and one for west coast and find the overlap 
   #6 DECIDE WHAT TO DO WITH ARTIST_LOCATION > DROP OR COMPLEMENT
   #7 data quality is still poor - There are obviously missint tracks in our DB and that should be a concern
  
