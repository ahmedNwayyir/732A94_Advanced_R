dijkstra <- function(df, src) {
  v1 <- df[["v1"]]
  v2 <- df[["v2"]]
  w  <- df[["w"]]
  
  nodes <- union(v1,v2)
  Q <- c()
  dist <- rep(Inf,length(nodes))
  dist[src] <- 0
  
  min <- Inf

    for(i in nodes) {

      for(j in nodes) {
        dist[i] <- df[df$v1 == i, df$v2 == j, 'w']
        if (dist[i] < min) {
        min <- dist[i]
        }
      }
      Q <- c(Q,node[i])
    }
}
  
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)



function Dijkstra(Graph, source):

  create vertex set Q

  for each vertex v in Graph:             
      dist[v] ??? INFINITY                  
      prev[v] ??? UNDEFINED                 
      add v to Q                      
  dist[source] ??? 0 
  
  while Q is not empty:
      u ??? vertex in Q with min dist[u]    
      
      remove u from Q 
      
      for each neighbor v of u:           // only v that are still in Q
        alt ??? dist[u] + length(u, v)
        if alt < dist[v]:               
        dist[v] ??? alt 
        prev[v] ??? u 
  
    return dist[], prev[]
  
}

wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))










dijkstra <- function(df, src) {
  
### List of input parameters for function
v1 <- df[["v1"]]
v2 <- df[["v2"]]
w  <- df[["w"]]
nodes <- union(v1,v2) #vertices in df
n=length(nodes)       #number of nodes
dest=n                #destination node

###distance matrix
cost = function(df,n,nodes) { 
  S = matrix(Inf, n, n)
  for(i in nodes){
    for(j in nodes){
      if(i == j)
        S[i,j] = 0
      else{
        weight = df[df$v1 == i & df$v2 == j,'w'];
        if(length(weight) != 0)
        {
          S[i,j] = weight
          S[j,i] = weight
        }
      }
      
    }
  }
  return(S)
}

### Dijkstra's algorithm
short_path = function(n,src,S,dest){
  
  #create empty variables to store data
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # for every node in the network
  for(i in 1:n){
    prev[i] = -1
    # distance from start node src to every other node i in the network
    dest[i] = S[src,i] 
  }
  
  #initialise counter which keeps track of number of steps through network
  count=2
  
  # until we have reached our destination node n
  while(count <= n){
    min=Inf
    
    # loop over each node
    for(t in 1:n){
      #if the new path is less long than the existing smallest one and flag[t] is equal to zero 
      #(aka we've not already incuded that node in route)
      if(dest[t] < min && !flag[t]){
        # overwrite the minimum with the new shortest path and update counter
        min=dest[t]
        u=t
      }
    }
    #indicate that we go to this site
    flag[u] = 1 
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(t in 1:n){
      #if the new route is shorter than the previous route
      if((dest[u]+S[u,t] < dest[t]) && !flag[t]){
        #update the distance to destination
        dest[t]=dest[u]+S[u,t] 
        #keep track of the node visited
        prev[t]=u 
      }
    }
  }
  return(prev)
}

### create function which returns path
savepath = function(f,x){
  path=x
  while(f[x] != -1){
    path=c(path,f[x])
    x=f[x]
    savepath(f,x)
  }
  path=c(path,1)
  return(path)
}

### Run Dijkstra's algorithm with our distance matrix
prev = short_path(n,src,S,dest)
savepath(prev,dest) ### Print path

}
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
V2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
