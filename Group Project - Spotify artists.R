#. 0 Retrieve librairy, environnent and main dataset

rm(list=ls())
library(igraph)
library(tidyverse)                                                     
library(readxl)
library(sna)
library(influenceR)

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

#. 4 Describe network 

   #. 4.1 Lets  look at the usual network stats:
   
      #. 4.1.1.Diameter = 1023
      d_g <- diameter(OO_g, directed = FALSE, unconnected = FALSE)           #Diameter
      get.diameter(OO_g)                                                     #Nodes on the diameter
      d_g
      
      #. 4.1.2.APL  ---------- ?
      #Unconnected network (apl is evalauted on the largest component)
      apl_g2  <- mean_distance(OO_g, directed = FALSE, unconnected = TRUE)  #APL
      dist_g2 <- distances(OO_g)                                            #Get the distance matrix
      dist_g2
      
      
      #. 4.1.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
      ed_g <- edge_density(OO_g)                                             #Calculate density
      ed_g
      
      #. 4.1.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
      #Unconnected network
      comp_g2 <- components(OO_g)                                           #Calculate the number of components
      comp_g2                                                             #Components                                                  
      
      #. 4.1.5.Cutpoints/Bridges  ---------- 98 cutpoints
      cp_g <- articulation_points(OO_g)                                      #Cutpoints
      cp_g
      
      #'NOTE: There is no specific function in igraph to identify bridges,
      
      #. 4.1.6.Point/line connectivity  > 0 > its already an unconnected component
      #Unconnected network
      pointc_g2 <- min_cut(OO_g)                                           #Point connectivity
      pointc_g2
      
      #. 4.1.7.Cliques ---------- there a 788 3-cliques
      #help("cliques")                                                    #Explore this function
      #help("count_max_cliques")                                          #Explore this function
      
      
      #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
      #cliques_g
      numcliques_g <- count_max_cliques(OO_g, min = 3)                      #Number of cliques
      numcliques_g
      
      
      #. 4.1.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
      
      numisolates_g <- sum(degree(OO_g)==0)                                 #Number of isolates
      numisolates_g
      isolates_g <- V(OO_g)[degree(OO_g)==0]                                   #List of isolates
      isolates_g
      inclusiveness_g <- (vcount(OO_g)-numisolates_g)/vcount(OO_g)             #Calculate inclusiveness
      inclusiveness_g
      
      
      #. 4.1.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
      #Display the network
      dist_g <- distances(OO_g)                                             #Get the distance matrix
      dist_g
      observed_pairs_g <- (sum(!is.infinite(distances(OO_g)))-vcount(OO_g))/2  #Observed reachable pairs
      observed_pairs_g
      potentail_pairs_g <- vcount(OO_g)*(vcount(OO_g)-1)/2                     #Potential reachable pairs
      potentail_pairs_g
      reach_g <- observed_pairs_g/potentail_pairs_g                      #Proportion of reachable pairs
      reach_g
      
      
      #. 4.1.10.Transitivity ---------- transitivity at 0.1550432
      
      transitivity_g <- transitivity(OO_g, type = "globalundirected")       #Calculate transitivity
      transitivity_g
      
      
   #. 4.2 - Let's generate a random network to compare
      
      # 4.2.0 Initiate 
      
      g_rand <- erdos.renyi.game(nrow(uniquecollab), nrow(tracklist2), type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
      #V(g_rand)$size  <- 5                                                     #Change the size of nodes
      #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
      #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
      
      #. 4.2.1.Diameter = 1023
      d_g_rand <- diameter(g_rand, directed = FALSE, unconnected = FALSE)           #Diameter
      get.diameter(g_rand)                                                     #Nodes on the diameter
      d_g_rand
      
      #. 4.2.2.APL  ---------- ?
      #Unconnected network (apl is evalauted on the largest component)
      apl_g_rand  <- mean_distance(g_rand, directed = FALSE, unconnected = TRUE)  #APL
      apl_g_rand
      #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
      #dist_g_rand
      
      #. 4.2.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
      ed_g_rand <- edge_density(g_rand)                                             #Calculate density
      ed_g_rand
      
      #. 4.2.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
      #Unconnected network
      comp_g_rand <- components(g_rand)                                           #Calculate the number of components
      comp_g_rand                                                             #Components                                                  
      
      #. 4.2.5.Cutpoints/Bridges  ---------- 98 cutpoints
      cp_g_rand <- articulation_points(g_rand)                                      #Cutpoints
      cp_g_rand
      
      #'NOTE: There is no specific function in igraph to identify bridges,
      
      #. 4.2.6.Point/line connectivity  > 0 > its already an unconnected component
      #Unconnected network
      pointc_g_rand <- min_cut(g_rand)                                           #Point connectivity
      pointc_g_rand
      
      #. 4.2.7.Cliques ---------- there a 788 3-cliques

      #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
      #cliques_g
      numcliques_g_rand <- count_max_cliques(g_rand, min = 3)                      #Number of cliques
      numcliques_g_rand
      
      
      #. 4.2.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
      
      numisolates_g_rand <- sum(degree(g_rand)==0)                                 #Number of isolates
      numisolates_g_rand
      isolates_g_rand <- V(g_rand)[degree(g_rand)==0]                                   #List of isolates
      isolates_g_rand
      inclusiveness_g_rand <- (vcount(g_rand)-numisolates_g)/vcount(g_rand)             #Calculate inclusiveness
      inclusiveness_g_rand
      
      
      #. 4.2.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
      #Display the network
      dist_g_rand <- distances(g_rand)                                             #Get the distance matrix
      dist_g_rand
      observed_pairs_g_rand <- (sum(!is.infinite(distances(g_rand)))-vcount(g_rand))/2  #Observed reachable pairs
      observed_pairs_g_rand
      potentail_pairs_g_rand <- vcount(g_rand)*(vcount(g_rand)-1)/2                     #Potential reachable pairs
      potentail_pairs_g_rand
      reach_g_rand <- observed_pairs_g_rand/potentail_pairs_g_rand                      #Proportion of reachable pairs
      reach_g_rand
      
      
      #. 4.2.10.Transitivity ---------- transitivity at 0.1550432
      
      transitivity_g_rand <- transitivity(g_rand, type = "globalundirected")       #Calculate transitivity
      transitivity_g_rand

      
   #. 4.3 Lets put this in a table 
      
      statistic <- c("Name", "Nodes", "Edges", "Components", "Diameter", "APL", "Density", "Cliques", "Inclusiveness", "Reachable Pairs", "Transitivity")
      values <- c("Dataset", nrow(uniquecollab), nrow(tracklist2), comp_g2$no, d_g[1], "TBCAPL", round(ed_g[1],4), numcliques_g[1], round(inclusiveness_g[1],4), round(reach_g[1],4), round(transitivity_g[1],4))
      random <- c("Random set", round(nrow(uniquecollab),1), nrow(tracklist2), comp_g_rand$no, d_g_rand[1], round(apl_g_rand,4), round(ed_g_rand[1],4), numcliques_g_rand[1], round(inclusiveness_g_rand[1],4), round(reach_g_rand[1],4), round(transitivity_g_rand[1],4))
      df <- data.frame(statistic, values, random)
      df
   
   #. 4.4 Lets get degree distribution histogram
      
      g2.deg <- degree(OO_g)
      g2.deg.histogram <- as.data.frame(table(g2.deg))
      g2.deg.histogram[,1] <- as.numeric(g2.deg.histogram[,1])
      ggplot(g2.deg.histogram, aes(x = g2.deg, y = Freq)) +
         geom_col() +
         scale_x_continuous("Degree") +
         scale_y_continuous("Frequency", trans = "log10") +
         ggtitle("Degree Distribution (log-log)")

   #. 4.5 Lets get betweenness distribution histogram
      
      g2.betw <- betweenness(OO_g)
      g2.betw.histogram <- as.data.frame(table(g2.betw))
      g2.betw.histogram[,1] <- as.numeric(g2.betw.histogram[,1])
      ggplot(g2.betw.histogram, aes(x = g2.betw, y = Freq)) +
         geom_col() +
         scale_x_continuous("Betweenness") +
         scale_y_continuous("Frequency", trans = "log10") +
         ggtitle("Betweenness Distribution (log-log)")
      
   #. 4.6 Node-level Measures
      
      #Degree/Closeness/Betweenness/Constraint/ENS
      
      df.deg <- as.data.frame(g2.deg)
      df.betw <- as.data.frame(g2.betw)
      g2.close <- closeness(OO_g, normalized = FALSE)
      df.close <- as.data.frame(g2.close) 
      g2.const <- 1 - constraint(OO_g)
      df.const <- as.data.frame(g2.const)
      g2.ens <- influenceR::ens(OO_g) 
      df.ens <- as.data.frame(g2.ens)

      nodelevelmeasures <- data.frame(round(df.deg,0), round(df.betw,2), round(df.close,4), round(df.const,3), round(df.ens,3))
      nodelevelmeasures_sorted <- nodelevelmeasures %>%
         arrange(desc(g2.deg))
      nodelevelmeasures_sorted


    #. 4.7 Observed brokerage properties
      
      OO_g_adj <- get.adjacency(OO_g, sparse = F)                                   #Get the adjacency matrix
      OO_g_adj
      
      ## Pending a better classification:
      V(OO_g)$type <- c("east","west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east","west","east","west","east","west","east","west","east","west", "east","west","east","west", "east","west", "east",
                           "west","east","west","east","west","east", "west")
      V(OO_g)$type
      
      br <- sna::brokerage(OO_g_adj, V(OO_g)$type)                                  #Calculate brokerage measures, 
      summary(br) 

# WHAT NEXT ?  ## >> THIS IS TOO MESSY - WE MUST DROP SOME ARTISTS
   #1 LETS DROP THE ARTISTS WITH NO or less than x COLLABORATIONs
   #2 narrow down the BBC artists to those with a certain amount of tracks
   #3 limit the period further by decades / just look 90s
   #4 6 degrees of Tupac ?
   #5 select one artist for east coast and one for west coast and find the overlap 
   #6 DECIDE WHAT TO DO WITH ARTIST_LOCATION > DROP OR COMPLEMENT
   #7 data quality is still poor - There are obviously missint tracks in our DB and that should be a concern
  
