dijkstra <- function(graph, init_node){
  stopifnot(length(init_node) == 1, 
            is.numeric(init_node) == TRUE, 
            is.data.frame(graph) == TRUE, 
            length(graph[1,]) == 3, 
            names(graph) == c("v1", "v2", "w"), 
            is.numeric(graph[,1]), 
            is.numeric(graph[,2]), 
            is.numeric(graph[,3]), 
            init_node %in% unique(graph$v1))
  
  Q <- c()
  dist <- c()
  prev <- c()
  uniqueVertices <- unique(graph[[1]])
  
  for (v in 1:length(uniqueVertices)){
    dist[v] <- Inf
    prev[v] <- NA
    Q[v] <- uniqueVertices[v]
  }
  
  dist[init_node] <- 0
  
  while (length(Q) > 1){
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q!=u]
    
    neighbors <- graph[graph$v1 == u,]
    remNbors  <- neighbors[neighbors$v2 %in% Q,]$v2
    
    for(v in 1:length(remNbors)){
      
      alt <- dist[u] + neighbors[neighbors$v2 %in% Q,]$w[v]
      
      if (alt < dist[remNbors[v]]){
        dist[remNbors[v]] <- alt
        prev[remNbors[v]] <- u
      }
    }
  }
  return (dist)
}

wiki_graph <- data.frame(v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w  = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)




