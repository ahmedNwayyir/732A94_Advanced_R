# Problem 2

data_object <- setRefClass("data_object",
                           fields = list(p = "numeric",
                                         number_cluster = "numeric",
                                         cluster = "data.frame",
                                         observation_collection = "data.frame"),
                           methods = list(
                             distance = function(x, y){
                               stopifnot(x >= 0 && y >= 0)
                               result <- abs(x - y) ^ p
                               return(result)
                             },
                             build_data_object = function(p){
                               stopifnot(p >= 0)
                               p <<- p
                               number_cluster <<- 0
                               cluster <<- data.frame(id = character(),
                                                      center = integer(),
                                                      number_point = integer(),
                                                      stringsAsFactors = FALSE)
                               observation_collection <<- data.frame(id = character(),
                                                                     value = numeric(),
                                                                     cluster_id = character())
                               result <- list("clusters" = cluster, "observations" = observation_collection)
                               return(result)
                             },
                             create_cluster = function(center){
                               stopifnot(center >= 0)
                               check_list <- which(cluster$center == center)
                               if(length(check_list) == 0){
                                 new_cluster <- data.frame("id" = sprintf("%08d", center), "center" = center, "number_point" = 0)
                                 cluster <<- rbind(cluster, new_cluster)
                                 number_cluster <<- number_cluster + 1
                                 return("Cluster have been added!")
                               }else{
                                 return("This centre exists!")
                               }
                             },
                             add_observation = function(observation){
                               stopifnot(observation >= 0)
                               shortest_distance <- NULL
                               cluster_index <- 0
                               for(i in cluster$center){
                                 value <- distance(observation, i)
                                 if(is.null(shortest_distance)){
                                   shortest_distance <- value
                                 }else if(value < shortest_distance){
                                   shortest_distance <- value
                                   cluster_index <- which(cluster$center == i)
                                 }
                               }
                               observation_id <- sprintf("%08d", nrow(observation_collection) + 1)
                               new_observation <- data.frame("id" = observation_id, "value" = observation, "cluster_id" = cluster[cluster_index,]$id)
                               observation_collection <<- rbind(observation_collection, new_observation)
                               cluster[cluster_index,]$number_point <<- cluster[cluster_index,]$number_point + 1
                             },
                             print = function(){
                               result <- list("number_cluster" = number_cluster, "clusters" = cluster, "observations" = observation_collection)
                               return(result)
                             }
                           ))

my_object <- data_object$new()
my_object$build_data_object(2)

for(i in 1:10){
  my_object$create_cluster(i)
}

random_number <- runif(50, min = 1, max = 10)
for(i in random_number){
  my_object$add_observation(random_number[i])
}


my_object$print()

my_object




# Problem 3
b <- function(n, k){
  stopifnot(n  > k && k > 0)
  result <- factorial(n)/factorial(n - k)/factorial(k)
  return(result)
}

b(5, 2)

test_that("Check the output", {
  expect_that(b(5, 2), is_equivalent_to(choose(5, 2)))
})





