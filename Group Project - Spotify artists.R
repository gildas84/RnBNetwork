
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

   #number_collab
   
   OObis <- OO
   diag(OObis) <- 0 
   
   eList <- NULL
   eList1 <- NULL
   eList2 <- NULL
   eList3 <- NULL
   
   for ( i in 1:nrow(OObis) ){
      for ( j in 1:ncol(OObis)) {
         eList1 <- c(eList1, rep(paste(dimnames(OObis)[[1]][i])))
         eList2 <- c(eList2, rep(paste(dimnames(OObis)[[2]][j] )))
         eList3 <- c(eList3, rep(paste(OObis[i,j])))
      }
   }
   eList1 <- as.data.frame(eList1)
   eList2 <- as.data.frame(eList2)
   eList3 <- as.data.frame(eList3)
   
   number_collab = bind_cols(eList1, eList2, eList3)
   number_collab <- number_collab[!(is.na(number_collab$eList3) | number_collab$eList3==0),] 
   number_collab <- as.data.frame(number_collab)
   

#. 3  Try to plot
   
   diag(OO) <- 0                                                          #Set diagonal to 0 (no self-loops)
   OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
   plot(OO_g, vertex.label=NA)
   
   
#. 4 Describe network 
   
   summary(OO_g)
   
   #. 4.1 Total network:
      #. 4.1.1 Lets  look at the usual network stats:
   
         #. 4.1.1.1.Diameter 
         d_g2 <- diameter(OO_g, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(OO_g)                                                     #Nodes on the diameter
         d_g2
      
         #. 4.1.1.2.APL  ----------

         apl_g2 <- mean_distance(OO_g, directed = FALSE, unconnected = FALSE)    #APL
         apl_g2
         dist_g2 <- distances(OO_g)                                            #Get the distance matrix
         dist_g2
      
         #. 4.1.1.3.Density  
         ed_g2 <- edge_density(OO_g)                                             #Calculate density
         ed_g2
      
         #. 4.1.1.4.Components  
         #Unconnected network
         comp_g2 <- components(OO_g)                                           #Calculate the number of components
         comp_g2                                                             #Components                                                  
      
         #. 4.1.1.5.Cutpoints/Bridges  
         cp_g2 <- articulation_points(OO_g)                                      #Cutpoints
         cp_g2
      
         #. 4.1.1.6.Point/line connectivity  
         #Unconnected network
         pointc_g2 <- min_cut(OO_g)                                           #Point connectivity
         pointc_g2
         
         #. 4.1.1.7.Cliques
         #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g2 <- count_max_cliques(OO_g, min = 3)                      #Number of cliques
         numcliques_g2
         
         #. 4.1.1.8.Inclusiveness
         
         numisolates_g2 <- sum(degree(OO_g)==0)                                 #Number of isolates
         numisolates_g2
         isolates_g2 <- V(OO_g)[degree(OO_g)==0]                                   #List of isolates
         isolates_g2
         inclusiveness_g2 <- (vcount(OO_g)-numisolates_g2)/vcount(OO_g)             #Calculate inclusiveness
         inclusiveness_g2
         
         
         #. 4.1.1.9.Reachable pairs 
         #Display the network
         dist_g2 <- distances(OO_g)                                             #Get the distance matrix
         dist_g2
         observed_pairs_g2 <- (sum(!is.infinite(distances(OO_g)))-vcount(OO_g))/2  #Observed reachable pairs
         observed_pairs_g2
         potentail_pairs_g2 <- vcount(OO_g)*(vcount(OO_g)-1)/2                     #Potential reachable pairs
         potentail_pairs_g2
         reach_g2 <- observed_pairs_g2/potentail_pairs_g2                    #Proportion of reachable pairs
         reach_g2
         
         #. 4.1.1.10.Transitivity
         
         transitivity_g2 <- transitivity(OO_g, type = "globalundirected")       #Calculate transitivity
         transitivity_g2
   
      #. 4.1.2 - Let's generate a random network to compare
      
         # 4.1.2.0 Initiate 
         
         g_rand <- erdos.renyi.game(nrow(uniquecollab), sum(uniquecollab)/2, type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
         #V(g_rand)$size  <- 5                                                     #Change the size of nodes
         #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
         #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
         
         #. 4.1.2.1.Diameter
         d_g_rand <- diameter(g_rand, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(g_rand)                                                     #Nodes on the diameter
         d_g_rand
         
         #. 4.1.2.2.APL
         #Unconnected network (apl is evalauted on the largest component)
         apl_g_rand  <- mean_distance(g_rand, directed = FALSE, unconnected = TRUE)  #APL
         apl_g_rand
         #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
         #dist_g_rand
         
         #. 4.1.2.3.Density 
         ed_g_rand <- edge_density(g_rand)                                             #Calculate density
         ed_g_rand
         
         #. 4.1.2.4.Components 
         #Unconnected network
         comp_g_rand <- components(g_rand)                                           #Calculate the number of components
         comp_g_rand                                                             #Components                                                  
         
         #. 4.1.2.5.Cutpoints/Bridges
         cp_g_rand <- articulation_points(g_rand)                                      #Cutpoints
         cp_g_rand
         
         #. 4.1.2.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g_rand <- min_cut(g_rand)                                           #Point connectivity
         pointc_g_rand
         
         #. 4.1.2.7.Cliques
         
         #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g_rand <- count_max_cliques(g_rand, min = 3)                      #Number of cliques
         numcliques_g_rand
         
         
         #. 4.1.2.8.Inclusiveness
         
         numisolates_g_rand <- sum(degree(g_rand)==0)                                 #Number of isolates
         numisolates_g_rand
         isolates_g_rand <- V(g_rand)[degree(g_rand)==0]                                   #List of isolates
         isolates_g_rand
         inclusiveness_g_rand <- (vcount(g_rand)-numisolates_g_rand)/vcount(g_rand)             #Calculate inclusiveness
         inclusiveness_g_rand
         
         
         #. 4.1.2.9.Reachable pairs
         #Display the network
         dist_g_rand <- distances(g_rand)                                             #Get the distance matrix
         dist_g_rand
         observed_pairs_g_rand <- (sum(!is.infinite(distances(g_rand)))-vcount(g_rand))/2  #Observed reachable pairs
         observed_pairs_g_rand
         potentail_pairs_g_rand <- vcount(g_rand)*(vcount(g_rand)-1)/2                     #Potential reachable pairs
         potentail_pairs_g_rand
         reach_g_rand <- observed_pairs_g_rand/potentail_pairs_g_rand                      #Proportion of reachable pairs
         reach_g_rand
      
         #. 4.1.2.10.Transitivity
         
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

      
      #. 4.1.6 Lets plot network
      
      diag(OO) <- 0                                                          #Set diagonal to 0 (no self-loops)
      OO_g <- graph_from_adjacency_matrix(OO, mode = "undirected")            #Obtain graph from adjacency matrix
      V(OO_g)$tracks <- as.numeric(log(artist_tracks$uniquetracks))
      
      ### V(OO_g)$location <- tbc
      
      V(OO_g)$degree <- degree(OO_g, normalized = FALSE)                        #Degree (as anode attribute)
      V(OO_g)$closeness   <- closeness(OO_g, normalized = FALSE)                #Closeness (as anode attribute)
      V(OO_g)$betweenness <- betweenness(OO_g, normalized = FALSE)                #betweenness (as anode attribute)
      V(OO_g)$constraint <- - 1 - constraint(OO_g) 
      V(OO_g)$effective_net <- influenceR::ens(OO_g)   
      # + .Brokerage measure cf seminar 6
      V(OO_g)$size <- V(OO_g)$tracks
      
      
      E(OO_g)$size <- V(OO_g)$tracks


      
            
      
      
      plot(OO_g, vertex.label=NA)
      
      ### 6 types of graphs 
      #   l_random <- layout.random(OO_g)                                        #Random layout
      #   plot(OO_g, layout = l_random, vertex.label=NA)  
      
      #   l_circle <- layout.circle(OO_g)                                        #Circle layout
      #   plot(OO_g, layout = l_circle, vertex.label=NA)           
      
      #   l_kk <- layout_with_kk(OO_g)                                           #Kamada-Kawai layout
      #   plot(OO_g, layout = l_kk, vertex.label=NA) 
      
      #   l_fr <- layout_with_fr(OO_g)                                           #Fruchterman Reingold layout
      #   plot(OO_g, layout = l_fr, vertex.label=NA)                                              #Display the network
      
      l_nicely <- layout_nicely(OO_g)                                        #igraph finds the best layout
      plot(OO_g, layout = l_nicely, vertex.label=NA)                                          #Display the network
      
      #   l_opt <- layout_with_graphopt(OO_g, niter = 500, charge = 0.0001, mass = 25)     #Customised force-directed algorithm (useful for very large graphs)
      #   plot(OO_g, layout = l_opt, vertex.label=NA) 
      
      
      
      
      
      
      
      
      
      
      
      
      
            
   #. 4.2 - large component only (WEIGHTED)
      
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
            
            #artist_location
            artist_location3 = as.data.frame(row.names(uniquecollab3))
            colnames(artist_location3) <- c("artist")
            artist_location3 <- artist_location3 %>%
               left_join(out_list, by = c("artist" = "ARTIST 1")) %>%
               select("artist", "Coast", "Birthplace")
            artist_location3

            
            #number_collab
            
            OO3bis <- OO3
            diag(OO3bis) <- 0 
            
            eList1 <- NULL
            eList2 <- NULL
            eList3 <- NULL
            
            for ( i in 1:nrow(OO3bis) ){
               for ( j in 1:ncol(OO3bis)) {
                  eList1 <- c(eList1, rep(paste(dimnames(OO3bis)[[1]][i])))
                  eList2 <- c(eList2, rep(paste(dimnames(OO3bis)[[2]][j] )))
                  eList3 <- c(eList3, rep(paste(OO3bis[i,j])))
               }
            }
            eList1 <- as.data.frame(eList1)
            eList2 <- as.data.frame(eList2)
            eList3 <- as.data.frame(eList3)
            
            number_collab3 = bind_cols(eList1, eList2, eList3)
            number_collab3 <- number_collab3[!(is.na(number_collab3$eList3) | number_collab3$eList3==0),] 
            number_collab3 <- as.data.frame(number_collab3)
            
            write.csv(number_collab3,"number_collab3testtt.csv")
            
                        
         #. 4.2.1.2 - generate network and graph
      
      
      
            diag(OO3) <- 0                                                          #Set diagonal to 0 (no self-loops)
            OO_g3 <- graph_from_adjacency_matrix(OO3, mode = "undirected")            #Obtain graph from adjacency matrix
            plot(OO_g3, vertex.label=NA)
            summary(OO_g3)
            
         

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
            
            g_rand3 <- erdos.renyi.game(nrow(uniquecollab3), sum(uniquecollab3)/2, type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
         #V(g_rand)$size  <- 5                                                     #Change the size of nodes
         #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
         #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
         
         #. 4.2.3.1.Diameter = 1023
         d_g_rand3 <- diameter(g_rand3, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(g_rand3)                                                     #Nodes on the diameter
         d_g_rand3
         
         #. 4.2.3.2.APL  ---------- ?
         #Unconnected network (apl is evalauted on the largest component)
         apl_g_rand3  <- mean_distance(g_rand3, directed = FALSE, unconnected = TRUE)  #APL
         apl_g_rand3
         #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
         #dist_g_rand
         
         #. 4.2.3.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
         ed_g_rand3 <- edge_density(g_rand3)                                             #Calculate density
         ed_g_rand3
         
         #. 4.2.3.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
         #Unconnected network
         comp_g_rand3 <- components(g_rand3)                                           #Calculate the number of components
         comp_g_rand3                                                            #Components                                                  
         
         #. 4.2.3.5.Cutpoints/Bridges  ---------- 98 cutpoints
         cp_g_rand3 <- articulation_points(g_rand3)                                      #Cutpoints
         cp_g_rand3
         
         #'NOTE: There is no specific function in igraph to identify bridges,
         
         #. 4.2.3.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g_rand3 <- min_cut(g_rand3)                                           #Point connectivity
         pointc_g_rand3
         
         #. 4.2.3.7.Cliques ---------- there a 788 3-cliques
   
         #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g_rand3 <- count_max_cliques(g_rand3, min = 3)                      #Number of cliques
         numcliques_g_rand3
         
         
         #. 4.2.3.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g_rand3 <- sum(degree(g_rand3)==0)                                 #Number of isolates
         numisolates_g_rand3
         isolates_g_rand3 <- V(g_rand)[degree(g_rand3)==0]                                   #List of isolates
         isolates_g_rand3
         inclusiveness_g_rand3 <- (vcount(g_rand3)-numisolates_g3)/vcount(g_rand3)             #Calculate inclusiveness
         inclusiveness_g_rand3
         
         
         #. 4.2.3.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g_rand3 <- distances(g_rand3)                                             #Get the distance matrix
         dist_g_rand3
         observed_pairs_g_rand3 <- (sum(!is.infinite(distances(g_rand3)))-vcount(g_rand3))/2  #Observed reachable pairs
         observed_pairs_g_rand3
         potentail_pairs_g_rand3 <- vcount(g_rand3)*(vcount(g_rand3)-1)/2                     #Potential reachable pairs
         potentail_pairs_g_rand3
         reach_g_rand3 <- observed_pairs_g_rand3/potentail_pairs_g_rand3                      #Proportion of reachable pairs
         reach_g_rand3
         
         
         #. 4.2.3.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g_rand3 <- transitivity(g_rand3, type = "globalundirected")       #Calculate transitivity
         transitivity_g_rand3
   
         
         #. 4.2.4 Lets put this in a table 
         
         
         
         statistic <- c("Name", "Nodes", "Edges", "Components", "Diameter", "APL", "Density", "Cliques", "Inclusiveness", "Reachable Pairs", "Transitivity")
         values <- c("Dataset", nrow(uniquecollab3), sum(uniquecollab3)/2, comp_g3$no, d_g3[1], round(apl_g3,4), round(ed_g3[1],4), numcliques_g3[1], round(inclusiveness_g3[1],4), round(reach_g3[1],4), round(transitivity_g3[1],4))
         random <- c("Random set", round(nrow(uniquecollab3),1), sum(uniquecollab3)/2, comp_g_rand3$no, d_g_rand3[1], round(apl_g_rand3,4), round(ed_g_rand3[1],4), numcliques_g_rand3[1], round(inclusiveness_g_rand3[1],4), round(reach_g_rand3[1],4), round(transitivity_g_rand3[1],4))
         df3 <- data.frame(statistic, values, random)
         df3
   
         #. 4.2.5 Lets plot network and add attributes
         
         V(OO_g3)$tracks <- as.numeric(log(artist_tracks3$uniquetracks))
         V(OO_g3)$degree <- degree(OO_g3, normalized = FALSE)                        #Degree (as anode attribute)
         V(OO_g3)$degree_w <- strength(OO_g3, mode = "all", loops = F)              #weighted Degree (as anode attribute)
         V(OO_g3)$closeness   <- closeness(OO_g3, normalized = FALSE)                #Closeness (as anode attribute)
         V(OO_g3)$betweenness <- betweenness(OO_g3, normalized = FALSE)                #betweenness (as anode attribute)
         V(OO_g3)$constraint <- 1 - constraint(OO_g3) 
         V(OO_g3)$effective_net <- influenceR::ens(OO_g3)            
         V(OO_g3)$Coast <- as.array(artist_location3$Coast)
         V(OO_g3)$Birthplace <- c(artist_location3$Birthplace)         

         
         V(OO_g3)$size <- V(OO_g3)$degree
         
         plot(OO_g3, vertex.label=NA)
         summary(OO_g3)

         
         
         OO_df3 <- igraph::as_data_frame(OO_g3, what = "both")                     
         #OO_df3$vertices                                                          #Network as list of nodes with attributes
         #OO_df3$edges                                                             #Network as list of edges 
         
         write.csv(OO_df3$edges,"Edges_list.csv")
         write.csv(OO_df3$vertices ,"Vertices_list.csv")
         
            # + .Brokerage measure cf seminar 6

         ### 6 types of graphs 
         #   l_random <- layout.random(OO_g3)                                        #Random layout
         #   plot(OO_g3, layout = l_random, vertex.label=NA)  
         
         #   l_circle <- layout.circle(OO_g3)                                        #Circle layout
         #   plot(OO_g3, layout = l_circle, vertex.label=NA)           
         
         #   l_kk <- layout_with_kk(OO_g3)                                           #Kamada-Kawai layout
         #   plot(OO_g3, layout = l_kk, vertex.label=NA) 
         
         #   l_fr <- layout_with_fr(OO_g3)                                           #Fruchterman Reingold layout
         #   plot(OO_g3, layout = l_fr, vertex.label=NA)                                              #Display the network
         
         l_nicely <- layout_nicely(OO_g3)                                        #igraph finds the best layout
         plot(OO_g3, layout = l_nicely, vertex.label=NA)                                          #Display the network
         
         #   l_opt <- layout_with_graphopt(OO_g3, niter = 500, charge = 0.0001, mass = 25)     #Customised force-directed algorithm (useful for very large graphs)
         #   plot(OO_g3, layout = l_opt, vertex.label=NA) 
         
         
         
         
         
         

         #. 4.3 - large component only (UNWEIGHTED)
         
         #. 4.3.1 - generate am alternative network based on large component only, attributes, network and graph
                  
         PO4 <- as.matrix(table(tracklist3$id, tracklist3$CORRECTED_ARTISTS))
         OO4 <- t(PO4)%*%PO4       
         
         OO4 <- OO4%/%OO4
         OO4[is.nan(OO4)] = 0
         
         
         #uniquecollab
         uniquecollab4 <- as.data.frame(rowSums(OO4 == 0, na.rm = TRUE))
         n <- as.numeric(count(uniquecollab4))
         n
         uniquecollab4 <- n - uniquecollab4
         colnames(uniquecollab4) <- c("uniquecollab")
         uniquecollab4
         
         #artist_tracks
         artist_tracks4 = as.data.frame(diag(OO4))
         colnames(artist_tracks4) <- c("uniquetracks")
         artist_tracks4
         
         #artist_location
         artist_location4 = as.data.frame(row.names(uniquecollab4))
         colnames(artist_location4) <- c("artist")
         artist_location4 <- artist_location4 %>%
            left_join(out_list, by = c("artist" = "ARTIST 1")) %>%
            select("artist", "Coast", "Birthplace")
         artist_location4
         
         
         
         #number_collab
         
         OO4bis <- OO4
         diag(OO4bis) <- 0 
         
         eList1 <- NULL
         eList2 <- NULL
         eList3 <- NULL
         
         for ( i in 1:nrow(OO4bis) ){
            for ( j in 1:ncol(OO4bis)) {
               eList1 <- c(eList1, rep(paste(dimnames(OO4bis)[[1]][i])))
               eList2 <- c(eList2, rep(paste(dimnames(OO4bis)[[2]][j] )))
               eList3 <- c(eList3, rep(paste(OO4bis[i,j])))
            }
         }
         eList1 <- as.data.frame(eList1)
         eList2 <- as.data.frame(eList2)
         eList3 <- as.data.frame(eList3)
         
         number_collab4 = bind_cols(eList1, eList2, eList3)
         number_collab4 <- number_collab4[!(is.na(number_collab4$eList3) | number_collab4$eList3==0),] 
         number_collab4 <- as.data.frame(number_collab4)
         
  
         
         #. 4.3.1.2 - generate network and graph
         
         
         
         diag(OO4) <- 0                                                          #Set diagonal to 0 (no self-loops)
         OO_g4 <- graph_from_adjacency_matrix(OO4, mode = "undirected")            #Obtain graph from adjacency matrix
         plot(OO_g4, vertex.label=NA)
         summary(OO_g4)
         
         
         
         #. 4.3.2 Lets  look at the usual network stats:
         
         #. 4.3.2.1.Diameter
         d_g4 <- diameter(OO_g4, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(OO_g4)                                                     #Nodes on the diameter
         d_g4
         
         #. 4.3.2.2.APL  ---------- ?
         #Unconnected network (apl is evalauted on the largest component)
         #apl_g2  <- mean_distance(OO_g, directed = FALSE, unconnected = TRUE)  #APL
         
         apl_g4 <- mean_distance(OO_g4, directed = FALSE, unconnected = TRUE)    #APL
         apl_g4
         
         
         dist_g4 <- distances(OO_g4)                                            #Get the distance matrix
         dist_g4
         
         #. 4.3.2.3.Density  ---------- 0.007812485 - really not a lot of edges vs all possible edges
         ed_g4 <- edge_density(OO_g4)                                             #Calculate density
         ed_g4
         
         #. 4.3.2.4.Components  ---------- 29 components, including one huge 964 components, 1 x 13, and the rest <6
         #Unconnected network
         comp_g4 <- components(OO_g4)                                           #Calculate the number of components
         comp_g4                                                            #Components                                                  
         
         #. 4.3.2.5.Cutpoints/Bridges  ---------- 98 cutpoints
         cp_g4 <- articulation_points(OO_g4)                                      #Cutpoints
         cp_g4
         
         #. 4.3.2.6.Point/line connectivity  > 0 > its already an unconnected component
         #Unconnected network
         pointc_g4 <- min_cut(OO_g4)                                           #Point connectivity
         pointc_g4
         
         #. 4.3.2.7.Cliques ---------- there a 788 3-cliques
         
         #cliques_g <- cliques(Advanced_Materials, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g4 <- count_max_cliques(OO_g4, min = 3)                      #Number of cliques
         numcliques_g4
         
         
         #. 4.3.2.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g4 <- sum(degree(OO_g4)==0)                                 #Number of isolates
         numisolates_g4
         isolates_g4 <- V(OO_g4)[degree(OO_g4)==0]                                   #List of isolates
         isolates_g4
         inclusiveness_g4 <- (vcount(OO_g4)-numisolates_g4)/vcount(OO_g4)             #Calculate inclusiveness
         inclusiveness_g4
         
         
         #. 4.3.2.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g4 <- distances(OO_g4)                                             #Get the distance matrix
         dist_g4
         observed_pairs_g4 <- (sum(!is.infinite(distances(OO_g4)))-vcount(OO_g4))/2  #Observed reachable pairs
         observed_pairs_g4
         potentail_pairs_g4 <- vcount(OO_g4)*(vcount(OO_g4)-1)/2                     #Potential reachable pairs
         potentail_pairs_g4
         reach_g4 <- observed_pairs_g4/potentail_pairs_g4                      #Proportion of reachable pairs
         reach_g4
         
         
         #. 4.3.2.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g4 <- transitivity(OO_g4, type = "globalundirected")       #Calculate transitivity
         transitivity_g4
         
         #. 4.3.3 - Let's generate a random network to compare
         
         # 4.3.3.0 Initiate 
         
         tstt <- sum(as.numeric(number_collab3$eList3))
         tstt
         tstt1 <- sum(as.numeric(number_collab4$eList3))/2
         tstt1         
         
         g_rand4 <- erdos.renyi.game(nrow(uniquecollab4), sum(as.numeric(number_collab4$eList3))/2, type = "gnm")   # (gnm since we set the number of edges, not the probability of edges which would require "gnp")      #Erdos-Renyi random network: N=same as our network, E=same as our network
         #V(g_rand)$size  <- 5                                                     #Change the size of nodes
         #V(g_rand)$color <- "lightblue"                                           #Change the color of nodes
         #plot(g_rand, layout=layout_nicely, vertex.label=NA)   
         
         #. 4.3.3.1.Diameter
         d_g_rand4 <- diameter(g_rand4, directed = FALSE, unconnected = FALSE)           #Diameter
         get.diameter(g_rand4)                                                     #Nodes on the diameter
         d_g_rand4
         
         #. 4.3.3.2.APL  
         #Unconnected network (apl is evalauted on the largest component)
         apl_g_rand4  <- mean_distance(g_rand4, directed = FALSE, unconnected = TRUE)  #APL
         apl_g_rand4
         #dist_g_rand <- distances(g_rand)                                            #Get the distance matrix
         #dist_g_rand
         
         #. 4.3.3.3.Density 
         ed_g_rand4 <- edge_density(g_rand4)                                             #Calculate density
         ed_g_rand4
         
         #. 4.3.3.4.Components 
         #Unconnected network
         comp_g_rand4 <- components(g_rand4)                                           #Calculate the number of components
         comp_g_rand4                                                            #Components                                                  
         
         #. 4.3.3.5.Cutpoints/Bridges
         cp_g_rand4 <- articulation_points(g_rand4)                                      #Cutpoints
         cp_g_rand4
         
         #'NOTE: There is no specific function in igraph to identify bridges,
         
         #. 4.3.3.6.Point/line connectivity 
         #Unconnected network
         pointc_g_rand4 <- min_cut(g_rand4)                                           #Point connectivity
         pointc_g_rand4
         
         #. 4.3.3.7.Cliques 
         
         #cliques_g <- cliques(g_rand, min = 3)                                   #List of cliques
         #cliques_g
         numcliques_g_rand4 <- count_max_cliques(g_rand4, min = 3)                      #Number of cliques
         numcliques_g_rand4
         
         
         #. 4.3.3.8.Inclusiveness ---------- 18 isolates, and an inclusiveness of 0.9824
         
         numisolates_g_rand4 <- sum(degree(g_rand4)==0)                                 #Number of isolates
         numisolates_g_rand4
         isolates_g_rand4 <- V(g_rand)[degree(g_rand4)==0]                                   #List of isolates
         isolates_g_rand4
         inclusiveness_g_rand4 <- (vcount(g_rand4)-numisolates_g4)/vcount(g_rand4)             #Calculate inclusiveness
         inclusiveness_g_rand4
         
         
         #. 4.3.3.9.Reachable pairs ---------- 464283 pairs, 522753 potential pairs > 0.8881499 reach
         #Display the network
         dist_g_rand4 <- distances(g_rand4)                                             #Get the distance matrix
         dist_g_rand4
         observed_pairs_g_rand4 <- (sum(!is.infinite(distances(g_rand4)))-vcount(g_rand4))/2  #Observed reachable pairs
         observed_pairs_g_rand4
         potentail_pairs_g_rand4 <- vcount(g_rand4)*(vcount(g_rand4)-1)/2                     #Potential reachable pairs
         potentail_pairs_g_rand4
         reach_g_rand4 <- observed_pairs_g_rand4/potentail_pairs_g_rand4                      #Proportion of reachable pairs
         reach_g_rand4
         
         
         #. 4.3.3.10.Transitivity ---------- transitivity at 0.1550432
         
         transitivity_g_rand4 <- transitivity(g_rand4, type = "globalundirected")       #Calculate transitivity
         transitivity_g_rand4
         
         
         #. 4.3.4 Lets put this in a table 
         
         statistic <- c("Name", "Nodes", "Edges", "Components", "Diameter", "APL", "Density", "Cliques", "Inclusiveness", "Reachable Pairs", "Transitivity")
         values <- c("Dataset", nrow(uniquecollab4), sum(as.numeric(number_collab4$eList3))/2, comp_g4$no, d_g4[1], round(apl_g4,4), round(ed_g4[1],4), numcliques_g4[1], round(inclusiveness_g4[1],4), round(reach_g4[1],4), round(transitivity_g4[1],4))
         random <- c("Random set", round(nrow(uniquecollab4),1), sum(as.numeric(number_collab4$eList3))/2, comp_g_rand4$no, d_g_rand4[1], round(apl_g_rand4,4), round(ed_g_rand4[1],4), numcliques_g_rand4[1], round(inclusiveness_g_rand4[1],4), round(reach_g_rand4[1],4), round(transitivity_g_rand4[1],4))
         df4 <- data.frame(statistic, values, random)
         df4
         
         #. 4.3.5 Lets plot network and add attributes
         
         V(OO_g4)$tracks <- as.numeric(log(artist_tracks4$uniquetracks))
         V(OO_g4)$degree <- degree(OO_g4, normalized = FALSE)                        #Degree (as anode attribute)
         V(OO_g4)$degree_w <- strength(OO_g4, mode = "all", loops = F)              #weighted Degree (as anode attribute)
         V(OO_g4)$closeness   <- closeness(OO_g4, normalized = FALSE)                #Closeness (as anode attribute)
         V(OO_g4)$betweenness <- betweenness(OO_g4, normalized = FALSE)                #betweenness (as anode attribute)
         V(OO_g4)$constraint <- 1 - constraint(OO_g4) 
         V(OO_g4)$effective_net <- influenceR::ens(OO_g4)            
         V(OO_g4)$Coast <- as.array(artist_location4$Coast)
         V(OO_g4)$Birthplace <- c(artist_location4$Birthplace)         
         
         
         V(OO_g4)$size <- V(OO_g4)$tracks
         
         plot(OO_g4, vertex.label=NA)
         summary(OO_g4)
         
         
         # + .Brokerage measure cf seminar 6
         
         ### 6 types of graphs 
         #   l_random <- layout.random(OO_g4)                                        #Random layout
         #   plot(OO_g4, layout = l_random, vertex.label=NA)  
         
         #   l_circle <- layout.circle(OO_g4)                                        #Circle layout
         #   plot(OO_g4, layout = l_circle, vertex.label=NA)           
         
         #   l_kk <- layout_with_kk(OO_g4)                                           #Kamada-Kawai layout
         #   plot(OO_g4, layout = l_kk, vertex.label=NA) 
         
         #   l_fr <- layout_with_fr(OO_g4)                                           #Fruchterman Reingold layout
         #   plot(OO_g4, layout = l_fr, vertex.label=NA)                                              #Display the network
         
         l_nicely <- layout_nicely(OO_g4)                                        #igraph finds the best layout
         plot(OO_g4, layout = l_nicely, vertex.label=NA)                                          #Display the network
         
         #   l_opt <- layout_with_graphopt(OO_g4, niter = 500, charge = 0.0001, mass = 25)     #Customised force-directed algorithm (useful for very large graphs)
         #   plot(OO_g4, layout = l_opt, vertex.label=NA) 
         
         
         #. 4.3.6 Do we have a small world ?
         
         # if smallworldness > 1 it has a smallworld effect 
         
         library(qgraph)
         smallworldness1 = smallworldness(OO_g, B = 1000, up = 0.995, lo = 0.005)
         smallworldness1
         
         smallworldness3 = smallworldness(OO_g3, B = 1000, up = 0.995, lo = 0.005)
         smallworldness3
         
         smallworldness4 = smallworldness(OO_g4, B = 1000, up = 0.995, lo = 0.005)
         smallworldness4
         
         
         smallworldness5 = smallworldness(g_rand4, B = 1000, up = 0.995, lo = 0.005)
         smallworldness5
         
         
         
         #. 4.7 Bonus  simulation 1000x 
         
         
   
            mynetwork.apl <- mean_distance(OO_g4)                               #APL between nodes
            mynetwork.cl  <- transitivity(OO_g4, type="average")                #Clustering coefficent
            mynetwork.compo <- components(OO_g4)
            mynetwork.diams <- diameter(OO_g4, directed = FALSE, unconnected = FALSE) 
            
            N <- vcount(OO_g4)                                                  #Number of nodes in the network we created
            E <- ecount(OO_g4)                                                  #Number of edges in the network we created
            
            sim <- 1000                                                          #How many simulations we want to run
            apl <- numeric(sim)                                                  #APL
            cl  <- numeric(sim)                                                  #Clustering coefficent
            compo <- numeric(sim)   
            diams <- numeric(sim)
            degress <- numeric(sim)
            numcli <- numeric(sim)
            inclusivy <- numeric(sim)
            reachpair <- numeric(sim)
            smallwordy <- numeric(sim)
            
            for(i in 1:sim) {                                                    #We run the simulation
               gr     <- erdos.renyi.game(N, E, type = "gnm")
               apl[i] <- mean_distance(gr)
                  y <- components(gr)
               compo[i] <- y$no
               cl[i]  <- transitivity(gr, type = "average")
               diams[i] <- diameter(gr, directed = FALSE, unconnected = TRUE)
               degress[i] <- edge_density(gr)
               numcli[i] <- count_max_cliques(gr, min = 3)                      #Number of cliques
               inclusivy[i] <- (vcount(gr)-sum(degree(gr)==0) )/(vcount(gr))
               reachpair[i] <- ((sum(!is.infinite(distances(gr)))-vcount(gr))/2)/(vcount(gr)*(vcount(gr)-1)/2)
               smallwordy[i] <- smallworldness(gr, B = 1000, up = 0.995, lo = 0.005)
            }
            
            randomnet.apl <- mean(apl)
            randomnet.cl  <- mean(cl)
            randomnet.compo  <- mean(compo)
            randomnet.diams <- mean(diams)
            randomnet.degress <- mean(degress)
            randomnet.numcli <- mean(numcli)
            randomnet.inclusivy <-mean(inclusivy)
            randomnet.reachpair <- mean(reachpair) 
            randomnet.smallwordy <- mean(smallwordy)
            
            
            N
            E
            randomnet.compo 
            randomnet.diams
            randomnet.apl
            randomnet.cl
            randomnet.degress
            randomnet.numcli
            randomnet.inclusivy
            randomnet.reachpair
            randomnet.smallwordy
            
            #Compare observed and random measures: some evidence of small-world effect:
            gamma.apl <- mynetwork.apl/randomnet.apl
            gamma.cl  <- mynetwork.cl/randomnet.cl
            gamma.apl
            gamma.cl
            
            
            
            
         
         
         
         
   #. 4.4 Lets get degree distribution histogram
      
      g4.deg <- as.numeric(degree(OO_g4, normalized = FALSE))
      g4.deg.histogram <- as.data.frame(table(g4.deg))
      
      g4.deg.histogram <- g4.deg.histogram
      
      g4.deg.histogram[,1] <- as.numeric(g4.deg.histogram[,1])
      ggplot(g4.deg.histogram, aes(x = g4.deg, y = Freq)) +
         geom_col() +
         scale_x_continuous("Degree") +
         scale_y_continuous("Frequency") +
         ggtitle("Degree Distribution")

      
      write.csv(g4.deg.histogram,"g4deghistogram.csv")
      
      
   #. 4.5 Lets get betweenness distribution histogram
      
      g4.betw <- betweenness(OO_g4, normalized = FALSE)
      g4.betw.histogram <- as.data.frame(table(g4.betw))
      g4.betw.histogram[,1] <- as.numeric(g4.betw.histogram[,1])
      ggplot(g4.betw.histogram, aes(x = g4.betw, y = Freq)) +
         geom_col() +
         scale_x_continuous("Betweenness") +
         scale_y_continuous("Frequency", trans = "log10") +
         #scale_y_continuous("Frequency") +
         ggtitle("Betweenness Distribution")


      #write.csv(g4.betw,"ggplot_g4.betw.csv")
      
      
      
      g4.betw <- betweenness(OO_g4, normalized = FALSE)
      
      
      OO_g4thrid <- delete_node_attr(OO_g4, "degree", "degree_w", "closeness", "betweenness", "constraint", "effective_net", "Coast", "Birthplace", "size")
      betweenness(OO_g4thrid, normalized = F)
      closeness(OO_g4thrid, normalized = F)
      
      
   
   #. 4.6 Node-level Measures
      
      #Degree/Closeness/Betweenness/Constraint/ENS
      
      df4.deg <- as.data.frame(g4.deg)
      df4.betw <- as.data.frame(g4.betw)
      g4.close <- closeness(OO_g4, normalized = FALSE)
      df4.close <- as.data.frame(g4.close) 
      g4.const <- 1 - constraint(OO_g4)
      df4.const <- as.data.frame(g4.const)
      g4.ens <- influenceR::ens(OO_g4) 
      df4.ens <- as.data.frame(g4.ens)

      nodelevelmeasures <- data.frame(round(df4.deg,0), round(df4.betw,2), round(df4.close,4), round(df4.const,3), round(df4.ens,3))
      nodelevelmeasures_sorted <- nodelevelmeasures %>%
         arrange(desc(g4.deg))
      nodelevelmeasures_sorted
   
      
      write.csv(nodelevelmeasures_sorted ,"nodelevelmeasures_sorted.csv")
      
      
    #. 4.7 Observed brokerage properties
      
      library(sna)
      library(influenceR)
      
      OO_g4_adj <- get.adjacency(OO_g4, sparse = F)                                   #Get the adjacency matrix
      OO_g4_adj
      

      help("brokerage")                                                   #Explore this function
      
      br <- sna::brokerage(OO_g4_adj, V(OO_g4)$Coast)                                  #Calculate brokerage measures, 
      summary(br)  

      
#. 5. exports the data as a csv file for Gephi/GML

   #. 5.1  for Gephi nodes

      #. 5.1.1 for the network
      OO_df <- igraph::as_data_frame(OO_g, what = "both")                     
      OO_df$vertices                                                          #Network as list of nodes with attributes
      OO_df$edges                                                             #Network as list of edges 

      write.csv(OO_df$edges,"Edges_list.csv")
      write.csv(OO_df$vertices ,"Vertices_list.csv")


      
      #. 5.1.2 for the large component weighted
      OO_df3 <- igraph::as_data_frame(OO_g3, what = "both")                     
      OO_df3$vertices                                                          #Network as list of nodes with attributes
      OO_df3$edges                                                             #Network as list of edges 
      
      write.csv(OO_df3$edges,"Edges_list_large_comp.csv")
      write.csv(OO_df3$vertices ,"Vertices_list_large_comp.csv")
      
      
      
      #. 5.1.2 for the large component unweighted
      OO_df4 <- igraph::as_data_frame(OO_g4, what = "both")                     
      OO_df4$vertices                                                          #Network as list of nodes with attributes
      OO_df4$edges                                                             #Network as list of edges 
      
      write.csv(OO_df4$edges,"Edges_list_large_comp_unweighted.csv")
      write.csv(OO_df4$vertices ,"Vertices_list_large_comp_unweighted.csv")
      
               
   #. 5.2  for GML file
      #. 5.2.1 
            
      write_graph(OO_g, "network_with_names.gml", format = "gml")          #Save network data in GML
      
      write_graph(OO_g3, "network_large_comp_with_names_weighted.gml", format = "gml")          #Save network data in GML
      
      write_graph(OO_g4, "network_large_comp_with_names_unweighted.gml", format = "gml")          #Save network data in GML
      
      
