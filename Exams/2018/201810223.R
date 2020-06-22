# Problem 2
batory <- setRefClass("batory", 
                            fields = list(fuel = "numeric",
                                          max_cargo_weight = "numeric",
                                          cargo_weight = "numeric",
                                          entity = "data.frame"),
                          methods = list(
                            build_Batory = function(fuel, max_cargo_weight){
                              fuel <<- fuel
                              max_cargo_weight <<- max_cargo_weight
                              if(fuel > max_cargo_weight){
                                return("The fuel sould not greater than max_cargo_weight!")
                              }
                              cargo_weight <<- fuel
                              entity <<- data.frame("name" = "fuel", "weight" = fuel, stringsAsFactors = FALSE )
                              return(list("entity" = entity, "cargo_weight" = cargo_weight))
                            },
                            equip_Batory = function(name, equip_weight){
                              stopifnot(is.character(name) && is.numeric(equip_weight))
                              if(equip_weight + cargo_weight > max_cargo_weight){
                                return("This object is too heavy!")
                              }else{
                                new_entity = c(name, equip_weight)
                                entity <<- rbind(entity, new_entity)
                                cargo_weight <<- cargo_weight + equip_weight
                                
                                return(list("entity" = entity, "cargo_weight" = cargo_weight))
                              }
                            },
                            embark_sailor = function(sailor_weight){
                              if(sailor_weight > max_cargo_weight - fuel){
                                return("This sailor is too heavy!")
                              }else{
                                i <- 1
                                list <- c()
                                while(sailor_weight + cargo_weight > max_cargo_weight){
                                  if(i > nrow(entity)){
                                    return("Sorry, this sailor can not embark.")
                                  }
                                  if(entity$name[i] != "fuel" && entity$name[i] != "sailor"){
                                    list <- c(list, i)
                                    cargo_weight <<- cargo_weight - as.numeric(entity$weight[i])
                                  }
                                  i <- i + 1
                                }
                                if(length(list) != 0){
                                  entity <<- entity[-list,]
                                }
                                equip_Batory("sailor", sailor_weight)
                                return(list("entity" = entity, "cargo_weight" = cargo_weight))
                              }
                            },
                            print = function(){
                              print_result <- list(max_cargo_weight, cargo_weight, fuel, entity)
                              names(print_result) <- c("max_cargo_weight", "cargo_weight", "fuel", "entity")
                              return(print_result)
                            },
                            plot = function(){
                              entity_names <- unique(entity$name)
                              weight_list <- vector()
                              for(i in entity_names){
                                total_weight <- sum(as.numeric(entity[which(entity$name == i),]$weight))
                                weight_list <- c(weight_list, total_weight)
                              }
                              barplot(weight_list, 
                                      names.arg = entity_names, 
                                      xlab = "Entity Names",
                                      ylab = "Total Weight",
                                      col = "blue",
                                      main = "Entity chart",
                                      border = "red")
                            }
                          ))

ORP_Batory <- batory$new()
ORP_Batory$build_Batory(fuel=1.5,max_cargo_weight=10)

for(i in 1:10){
  weight <- rexp(n = 1, rate = 5)
  ORP_Batory$equip_Batory("food", round(weight, 5))
}

for(i in 1:20){
  sailor_weight <- runif(n = 1, min = 0.08, max = 0.2)
  ORP_Batory$embark_sailor(round(sailor_weight, 5))
}

ORP_Batory$plot()

# Problem 3
# a
multiplication <- function(A, B){
  v_long <- max(nrow(A), ncol(A))
  V <- matrix(nrow = nrow(A), ncol = ncol(B))
  for(i in 1:nrow(A)){
    for(j in 1:ncol(B)){
      elements <- A[i, ] * B[, j]
      V[i, j] <- sum(elements)
    }
  }
  return(V)
}

# A <- matrix(1:20, nrow = 5, byrow = TRUE)
# B <- t(A)
# multiplication(A, B)
# print(A%*%B)

# b
# A <- matrix(1:20, nrow = 5, byrow = TRUE)
# B <- t(A)
# test_that("The output is correct", {
#   expect_that(multiplication(A, B), is_equivalent_to(A%*%B))
# })













