#. 0 Retrieve librairy, environnent and main dataset

   rm(list=ls())
   library(igraph)
   library(tidyverse)                                                     
   library(readxl)

   setwd("C:/Users/Gildas/OneDrive/MSC SUSDEV/NAI/NAI Group project") # adapt as necessary
   
   tracklist <- read_xlsx("FILTERED DB.xlsx", sheet = "DATA")
   tracklist2 <- tracklist %>%
      filter(year > 1990 & year < 2000)  %>%
      select("id", "CORRECTED_ARTISTS") %>% separate_rows("CORRECTED_ARTISTS", sep = ";") 
   tracklist2 <- tracklist2[!(tracklist2$CORRECTED_ARTISTS==""),]
   tracklist2


   out_list <- read_xlsx("Artists_filtered_out.xlsx", sheet = "OUT")
   out_list

      tracklist2 <- tracklist2 %>%
      left_join(out_list, by = c("CORRECTED_ARTISTS" = "ARTIST 1")) %>%
      filter(keep == "In") %>%
      select("id", "CORRECTED_ARTISTS")
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
   
   #artist_location & artist_zone
   #
   #      metadata <- read_csv("top10k-spotify-artist-metadata.csv")
   #      metadata = as.data.frame(metadata)
   #      artist_location = as.data.frame(row.names(artist_tracks))
   #      colnames(artist_location) <- c("artist")
   #   
   #      artist_loc = artist_location 
   #      artist_loc <- artist_loc %>%
   #         group_by(artist) %>%
   #         left_join(metadata) %>% 
   #         select ("city_2")
   #      artist_loc
   #   
   #      # summary(artist_location)
   #      # summary(metadata)
   
#. 3 Lets plot that

   diag(OO) <- 0                                                          #Set diagonal to 0 (no self-loops)
   OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
   V(OO_g)$size <- as.numeric(log(artist_tracks$uniquetracks))
   
   
   #list = c("West","East, ..404.... "West"")
   #V(OO_g)$Source < list
   
   plot(OO_g, vertex.label=NA)
   
   #NOT REALLY NICE:
   #l_random <- layout.random(OO_g)                                        #Random layout
   #plot(OO_g, layout = l_random, vertex.label=NA)  

   #l_circle <- layout.circle(OO_g)                                        #Circle layout
   #plot(OO_g, layout = l_circle, vertex.label=NA)           
   
   l_kk <- layout_with_kk(OO_g)                                           #Kamada-Kawai layout
   plot(OO_g, layout = l_kk, vertex.label=NA) 

#. 4 Describe network 
   
   summary(OO_g)
   
   #. 4.1 Total network:
      #. 4.1.1 Lets  look at the usual network stats:
   
         #. 4.1.1.1.Diameter = 1023
         d_g2 <- diameter(OO_g, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(OO_g)                                                     #Nodes on the diameter
         d_g2
      
         #. 4.1.1.2.APL  ---------- ?

         apl_g2 <- mean_distance(OO_g, directed = FALSE, unconnected = FALSE)    #APL
         apl_g2
         dist_g2 <- distances(OO_g)                                            #Get the distance matrix
         dist_g2
      
         #. 4.1.1.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
         ed_g2 <- edge_density(OO_g)                                             #Calculate density
         ed_g2
      
         #. 4.1.1.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
         #Unconnected network
         comp_g2 <- components(OO_g)                                           #Calculate the number of components
         comp_g2                                                             #Components                                                  
      
         #. 4.1.1.5.Cutpoints/Bridges  ---------- 98 cutpoints
         cp_g2 <- articulation_points(OO_g)                                      #Cutpoints
         cp_g2
      
         #. 4.1.1.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g2 <- min_cut(OO_g)                                           #Point connectivity
         pointc_g2
         
         #. 4.1.1.7.Cliques ---------- there a 788 3-cliques
         #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g2 <- count_max_cliques(OO_g, min = 3)                      #Number of cliques
         numcliques_g2
         
         #. 4.1.1.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g2 <- sum(degree(OO_g)==0)                                 #Number of isolates
         numisolates_g2
         isolates_g2 <- V(OO_g)[degree(OO_g)==0]                                   #List of isolates
         isolates_g2
         inclusiveness_g2 <- (vcount(OO_g)-numisolates_g2)/vcount(OO_g)             #Calculate inclusiveness
         inclusiveness_g2
         
         
         #. 4.1.1.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g2 <- distances(OO_g)                                             #Get the distance matrix
         dist_g2
         observed_pairs_g2 <- (sum(!is.infinite(distances(OO_g)))-vcount(OO_g))/2  #Observed reachable pairs
         observed_pairs_g2
         potentail_pairs_g2 <- vcount(OO_g)*(vcount(OO_g)-1)/2                     #Potential reachable pairs
         potentail_pairs_g2
         reach_g2 <- observed_pairs_g2/potentail_pairs_g2                    #Proportion of reachable pairs
         reach_g2
         
         #. 4.1.1.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g2 <- transitivity(OO_g, type = "globalundirected")       #Calculate transitivity
         transitivity_g2
   
      #. 4.1.2 - Let's generate a random network to compare
      
         # 4.1.2.0 Initiate 

         g_rand <- erdos.renyi.game(nrow(uniquecollab), sum(uniquecollab), type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
         #V(g_rand)$size  <- 5                                                     #Change the size of nodes
         #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
         #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
         
         #. 4.1.2.1.Diameter = 1023
         d_g_rand <- diameter(g_rand, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(g_rand)                                                     #Nodes on the diameter
         d_g_rand
         
         #. 4.1.2.2.APL  ---------- ?
         #Unconnected network (apl is evalauted on the largest component)
         apl_g_rand  <- mean_distance(g_rand, directed = FALSE, unconnected = TRUE)  #APL
         apl_g_rand
         #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
         #dist_g_rand
         
         #. 4.1.2.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
         ed_g_rand <- edge_density(g_rand)                                             #Calculate density
         ed_g_rand
         
         #. 4.1.2.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
         #Unconnected network
         comp_g_rand <- components(g_rand)                                           #Calculate the number of components
         comp_g_rand                                                             #Components                                                  
         
         #. 4.1.2.5.Cutpoints/Bridges  ---------- 98 cutpoints
         cp_g_rand <- articulation_points(g_rand)                                      #Cutpoints
         cp_g_rand
         
         #'NOTE: There is no specific function in igraph to identify bridges,
         
         #. 4.1.2.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g_rand <- min_cut(g_rand)                                           #Point connectivity
         pointc_g_rand
         
         #. 4.1.2.7.Cliques ---------- there a 788 3-cliques
         
         #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g_rand <- count_max_cliques(g_rand, min = 3)                      #Number of cliques
         numcliques_g_rand
         
         
         #. 4.1.2.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g_rand <- sum(degree(g_rand)==0)                                 #Number of isolates
         numisolates_g_rand
         isolates_g_rand <- V(g_rand)[degree(g_rand)==0]                                   #List of isolates
         isolates_g_rand
         inclusiveness_g_rand <- (vcount(g_rand)-numisolates_g_rand)/vcount(g_rand)             #Calculate inclusiveness
         inclusiveness_g_rand
         
         
         #. 4.1.2.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g_rand <- distances(g_rand)                                             #Get the distance matrix
         dist_g_rand
         observed_pairs_g_rand <- (sum(!is.infinite(distances(g_rand)))-vcount(g_rand))/2  #Observed reachable pairs
         observed_pairs_g_rand
         potentail_pairs_g_rand <- vcount(g_rand)*(vcount(g_rand)-1)/2                     #Potential reachable pairs
         potentail_pairs_g_rand
         reach_g_rand <- observed_pairs_g_rand/potentail_pairs_g_rand                      #Proportion of reachable pairs
         reach_g_rand
      
         #. 4.1.2.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g_rand <- transitivity(g_rand, type = "globalundirected")       #Calculate transitivity
         transitivity_g_rand
         
      
      #. 4.1.3 Lets put this in a table 
      
         statistic <- c("Name", "Nodes", "Edges", "Components", "Diameter", "APL", "Density", "Cliques", "Inclusiveness", "Reachable Pairs", "Transitivity")
         values <- c("Dataset", nrow(uniquecollab), sum(uniquecollab), comp_g2$no, d_g2[1], round(apl_g2,4), round(ed_g2[1],4), numcliques_g2[1], round(inclusiveness_g2[1],4), round(reach_g2[1],4), round(transitivity_g2[1],4))
         random <- c("Random set", round(nrow(uniquecollab),1), sum(uniquecollab), comp_g_rand$no, d_g_rand[1], round(apl_g_rand,4), round(ed_g_rand[1],4), numcliques_g_rand[1], round(inclusiveness_g_rand[1],4), round(reach_g_rand[1],4), round(transitivity_g_rand[1],4))
         df <- data.frame(statistic, values, random)
         df
      
      #. 4.1.4 Lets get degree distribution histogram
      
      g2.deg <- as.numeric(degree(OO_g, normalized = FALSE))
      g2.deg.histogram <- as.data.frame(table(g2.deg))
      g2.deg.histogram[,1] <- as.numeric(g2.deg.histogram[,1])
      ggplot(g2.deg.histogram, aes(x = g2.deg, y = Freq)) +
         geom_col() +
         scale_x_continuous("Degree") +
         scale_y_continuous("Frequency", trans = "log10") +
         ggtitle("Degree Distribution (log-log)")
      
      #. 4.1.5 Lets get betweenness distribution histogram
      
      g2.betw <- betweenness(OO_g, normalized = FALSE)
      g2.betw.histogram <- as.data.frame(table(g2.betw))
      g2.betw.histogram[,1] <- as.numeric(g2.betw.histogram[,1])
      ggplot(g2.betw.histogram, aes(x = g2.betw, y = Freq)) +
         geom_col() +
         scale_x_continuous("Betweenness") +
         scale_y_continuous("Frequency", trans = "log10") +
         ggtitle("Betweenness Distribution (log-log)")
      
            
   #. 4.2 - large component only
      
      #. 4.2.0 - generate large component only
         
         membership1 <- as.data.frame(row.names(OO))
         membership1
         membership2 <- as.data.frame(comp_g2$membership)
         colnames(membership2) <- c("cohort")
         membership2
         membership <- cbind(membership1, membership2$cohort)
         
         colnames(membership) <- c("artist", "cohort")
         membership
         
         tracklist3 <- tracklist2 %>%
            left_join(membership, by = c("CORRECTED_ARTISTS" = "artist")) %>%
            filter(cohort == "1") %>%
            select("id", "CORRECTED_ARTISTS")
         tracklist3
         
         
            
      
      
      #. 4.2.1 - generate attributes, network and graph
         #. 4.2.1.1 - generate attributes
      
            PO3 <- as.matrix(table(tracklist3$id, tracklist3$CORRECTED_ARTISTS))       #Obtain project-organisation matrix from dataframe
            OO3 <- t(PO3)%*%PO3       

            #uniquecollab
            uniquecollab3 <- as.data.frame(rowSums(OO3 == 0, na.rm = TRUE))
            n <- as.numeric(count(uniquecollab3))
            n
            uniquecollab3 <- n - uniquecollab3
            colnames(uniquecollab3) <- c("uniquecollab")
            uniquecollab3
            
            #artist_tracks
            artist_tracks3 = as.data.frame(diag(OO3))
            colnames(artist_tracks3) <- c("uniquetracks")
            artist_tracks3
            
      
      
         #. 4.2.1.1 - generate network and graph
      
      
      
            diag(OO3) <- 0                                                          #Set diagonal to 0 (no self-loops)
            OO_g3 <- graph_from_adjacency_matrix(OO3, mode = "undirected")            #Obtain graph from adjacency matrix
            V(OO_g3)$size <- as.numeric(log(artist_tracks3$uniquetracks))
            
            plot(OO_g3, vertex.label=NA)
            summary(OO_g3)
            
            
            # Maybe add 
               # E(OO_g3)$size = number of collaborations
               # V(OO_g3)$color = location
            
      #. 4.2.2 Lets  look at the usual network stats:
         
         #. 4.2.2.1.Diameter = 1023
            d_g3 <- diameter(OO_g3, directed = FALSE, unconnected = FALSE)           #Diameter
            get.diameter(OO_g3)                                                     #Nodes on the diameter
            d_g3
            
         #. 4.2.2.2.APL  ---------- ?
            #Unconnected network (apl is evalauted on the largest component)
            #apl_g2  <- mean_distance(OO_g, directed = FALSE, unconnected = TRUE)  #APL
            
            apl_g3 <- mean_distance(OO_g3, directed = FALSE, unconnected = TRUE)    #APL
            apl_g3
            
            
            dist_g3 <- distances(OO_g3)                                            #Get the distance matrix
            dist_g3
            
         #. 4.2.2.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
            ed_g3 <- edge_density(OO_g3)                                             #Calculate density
            ed_g3
         
         #. 4.2.2.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
            #Unconnected network
            comp_g3 <- components(OO_g3)                                           #Calculate the number of components
            comp_g3                                                            #Components                                                  
            
         #. 4.2.2.5.Cutpoints/Bridges  ---------- 98 cutpoints
            cp_g3 <- articulation_points(OO_g3)                                      #Cutpoints
            cp_g3
  
         #. 4.2.2.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
            pointc_g3 <- min_cut(OO_g3)                                           #Point connectivity
            pointc_g3
            
         #. 4.2.2.7.Cliques ---------- there a 788 3-cliques

            #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
            #cliques_g
            numcliques_g3 <- count_max_cliques(OO_g3, min = 3)                      #Number of cliques
            numcliques_g3
            
         
         #. 4.2.2.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
            numisolates_g3 <- sum(degree(OO_g3)==0)                                 #Number of isolates
            numisolates_g3
            isolates_g3 <- V(OO_g3)[degree(OO_g3)==0]                                   #List of isolates
            isolates_g3
            inclusiveness_g3 <- (vcount(OO_g3)-numisolates_g3)/vcount(OO_g3)             #Calculate inclusiveness
            inclusiveness_g3
         
         
         #. 4.2.2.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
            dist_g3 <- distances(OO_g3)                                             #Get the distance matrix
            dist_g3
            observed_pairs_g3 <- (sum(!is.infinite(distances(OO_g3)))-vcount(OO_g3))/2  #Observed reachable pairs
            observed_pairs_g3
            potentail_pairs_g3 <- vcount(OO_g3)*(vcount(OO_g3)-1)/2                     #Potential reachable pairs
            potentail_pairs_g3
            reach_g3 <- observed_pairs_g3/potentail_pairs_g3                      #Proportion of reachable pairs
            reach_g3
            
         
         #. 4.2.2.10.Transitivity ---------- transitivity at 0.1550432
            
            transitivity_g3 <- transitivity(OO_g3, type = "globalundirected")       #Calculate transitivity
            transitivity_g3
            
      #. 4.2.3 - Let's generate a random network to compare
         
         # 4.2.3.0 Initiate 
            
         g_rand3 <- erdos.renyi.game(nrow(uniquecollab3), sum(uniquecollab3), type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
         #V(g_rand)$size  <- 5                                                     #Change the size of nodes
         #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
         #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
         
         #. 4.2.1.Diameter = 1023
         d_g_rand3 <- diameter(g_rand3, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(g_rand3)                                                     #Nodes on the diameter
         d_g_rand3
         
         #. 4.2.2.APL  ---------- ?
         #Unconnected network (apl is evalauted on the largest component)
         apl_g_rand3  <- mean_distance(g_rand3, directed = FALSE, unconnected = TRUE)  #APL
         apl_g_rand3
         #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
         #dist_g_rand
         
         #. 4.2.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
         ed_g_rand3 <- edge_density(g_rand3)                                             #Calculate density
         ed_g_rand3
         
         #. 4.2.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
         #Unconnected network
         comp_g_rand3 <- components(g_rand3)                                           #Calculate the number of components
         comp_g_rand3                                                            #Components                                                  
         
         #. 4.2.5.Cutpoints/Bridges  ---------- 98 cutpoints
         cp_g_rand3 <- articulation_points(g_rand3)                                      #Cutpoints
         cp_g_rand3
         
         #'NOTE: There is no specific function in igraph to identify bridges,
         
         #. 4.2.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g_rand3 <- min_cut(g_rand3)                                           #Point connectivity
         pointc_g_rand3
         
         #. 4.2.7.Cliques ---------- there a 788 3-cliques
   
         #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g_rand3 <- count_max_cliques(g_rand3, min = 3)                      #Number of cliques
         numcliques_g_rand3
         
         
         #. 4.2.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g_rand3 <- sum(degree(g_rand3)==0)                                 #Number of isolates
         numisolates_g_rand3
         isolates_g_rand3 <- V(g_rand)[degree(g_rand3)==0]                                   #List of isolates
         isolates_g_rand3
         inclusiveness_g_rand3 <- (vcount(g_rand3)-numisolates_g3)/vcount(g_rand3)             #Calculate inclusiveness
         inclusiveness_g_rand3
         
         
         #. 4.2.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g_rand3 <- distances(g_rand3)                                             #Get the distance matrix
         dist_g_rand3
         observed_pairs_g_rand3 <- (sum(!is.infinite(distances(g_rand3)))-vcount(g_rand3))/2  #Observed reachable pairs
         observed_pairs_g_rand3
         potentail_pairs_g_rand3 <- vcount(g_rand3)*(vcount(g_rand3)-1)/2                     #Potential reachable pairs
         potentail_pairs_g_rand3
         reach_g_rand3 <- observed_pairs_g_rand3/potentail_pairs_g_rand3                      #Proportion of reachable pairs
         reach_g_rand3
         
         
         #. 4.2.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g_rand3 <- transitivity(g_rand3, type = "globalundirected")       #Calculate transitivity
         transitivity_g_rand3
   
         
      #. 4.3 Lets put this in a table 
         
         statistic <- c("Name", "Nodes", "Edges", "Components", "Diameter", "APL", "Density", "Cliques", "Inclusiveness", "Reachable Pairs", "Transitivity")
         values <- c("Dataset", nrow(uniquecollab3), sum(uniquecollab3), comp_g3$no, d_g3[1], round(apl_g3,4), round(ed_g3[1],4), numcliques_g3[1], round(inclusiveness_g3[1],4), round(reach_g3[1],4), round(transitivity_g3[1],4))
         random <- c("Random set", round(nrow(uniquecollab3),1), sum(uniquecollab3), comp_g_rand3$no, d_g_rand3[1], round(apl_g_rand3,4), round(ed_g_rand3[1],4), numcliques_g_rand3[1], round(inclusiveness_g_rand3[1],4), round(reach_g_rand3[1],4), round(transitivity_g_rand3[1],4))
         df3 <- data.frame(statistic, values, random)
         df3
   
   
         
      
      
      
      
      
      
   #. 4.4 Lets get degree distribution histogram
      
      g2.deg <- as.numeric(degree(OO_g, normalized = FALSE))
      g2.deg.histogram <- as.data.frame(table(g2.deg))
      g2.deg.histogram[,1] <- as.numeric(g2.deg.histogram[,1])
      ggplot(g2.deg.histogram, aes(x = g2.deg, y = Freq)) +
         geom_col() +
         scale_x_continuous("Degree") +
         scale_y_continuous("Frequency", trans = "log10") +
         ggtitle("Degree Distribution (log-log)")

   #. 4.5 Lets get betweenness distribution histogram
      
      g2.betw <- betweenness(OO_g, normalized = FALSE)
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
      
      library(sna)
      library(influenceR)
      
      OO_g_adj <- get.adjacency(OO_g, sparse = F)                                   #Get the adjacency matrix
      OO_g_adj
      
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
      help("brokerage")                                                   #Explore this function
      
      br <- sna::brokerage(OO_g_adj, V(OO_g)$type)                                  #Calculate brokerage measures, 
      summary(br)  
      
      

## >> THIS IS TOO MESSY - WE MUST DROP SOME ARTISTS
   #1 LETS DROP THE ARTISTS WITH NO or less than x COLLABORATIONs
   #2 narrow down the BBC artists to those with a certain amount of tracks
   #3 limit the period further by decades / just look 90s
   #4 6 degrees of Tupac ?
   #5 select one artist for east coast and one for west coast and find the overlap 
   #6 DECIDE WHAT TO DO WITH ARTIST_LOCATION > DROP OR COMPLEMENT
   #7 data quality is still poor - There are obviously missint tracks in our DB and that should be a concern
      
   
   
   
# WHAT NEXT ?

      # find an independent list of top 10 90s rappers > we assume they should be central - are they ? - 
      
      
summary(OO_g)
artist_list = OO_g


# add some attributes:

   # size nodes by number of tracks
   # region of artist

# data quality (change in R code directly) 
   # duplicate names, typos, ..
   # should we limit the database ?
   # do we keep this database


