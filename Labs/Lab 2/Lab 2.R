name <- "Ahmed_AlHasan"
liuid <- "ahmal787"

#1.1.1
sheldon_game <- function(player1, player2) {
  a <- c("rock", "paper", "scissors", "lizard", "spock")
  x <- which(a == player1)
  y <- which(a == player2)
  
  if(!(player1 %in% a)) stop("wrong entry")
  if(!(player2 %in% a)) stop("wrong entry") 
  
    if(((y == 5) && (x == 1 || x == 3)) || ((y == 4) && (x == 5 || x == 2)) || ((y == 3) && (x == 4 || x == 2)) || ((y == 2) && (x == 5 || x == 1)) || ((y == 1) && (x == 3 || x == 4))) {
    return ("Player 2 wins!")
  }
    else if(((y == 5) && (x == 2 || x == 4)) || ((y == 4) && (x == 3 || x == 1)) || ((y == 3) && (x == 5 || x == 1)) || ((y == 2) && (x == 4 || x == 3)) || ((y == 1) && (x == 2 || x == 5))) {
    return ("Player 1 wins!")
  }
    else {
    return ("Draw!")
  }
}


#1.2.1
my_moving_median <- function(x, n, ...) {
  if(!is.vector(x, mode = "numeric") || (!is.numeric(n)) || length(n) > 1) {stop("wrong entry")}
    y <- c()
     for(t in 1:(length(x)-n)){
      vec <- x[t:(t+n)]
      y <- c(y, median(vec, ...))
    }
  return(y)
}


# 1.2.2
for_mult_table <- function(from, to) {
  if((!is.numeric(from)) || (length(from) > 1) || (!is.numeric(to)) || (length(to) > 1)) {stop("wrong entry")}
  x <- as.numeric()
  for(i in from:to) {
    x <- append(x,i)
  }
  m <- matrix("%o%"(x,x), nrow = to-from+1)
  rownames(m) <- from:to
  colnames(m) <- from:to
  return(m)
}


# 1.3.1
find_cumsum <- function(x, find_sum) {
  if(!is.vector(x, mode = "numeric") || (!is.numeric(find_sum)) || length(find_sum) > 1) {stop("wrong entry")}
  total <- 0
  i <- 1
  while(i <= length(x)) {
      total <- total + x[i]
      i <- i + 1
      if(total > find_sum) break
  }
  return(total)
}


# 1.3.2
while_mult_table <- function(from, to) {
  if((!is.numeric(from)) || (length(from) > 1) || (!is.numeric(to)) || (length(to) > 1)) {stop("wrong entry")}
  i <- from
  j <- from
  y <- c()
  
  while(i <= to) {
    while(j <= to) {
      y <- c(y, i*j)
      j <- j + 1
    }
    i <- i + 1
    j <- from
  }
  y <- matrix(y, nrow = to-from+1)
  rownames(y) <- from:to
  colnames(y) <- from:to
  return(y)
}


# 1.4.1
repeat_find_cumsum <- function(x, find_sum) {
  if(!is.vector(x, mode = "numeric") || (!is.numeric(find_sum)) || length(find_sum) > 1) {stop("wrong entry")}
  total <- 0
  i <- 1
  repeat{
    total <- total + x[i]
    i <- i + 1
  if(i > length(x) || total >= find_sum) break
  }
  return(total)
}


#1.4.2
repeat_my_moving_median <- function(x, n, ...) {
  if(!is.vector(x, mode = "numeric") || (!is.numeric(n)) || length(n) > 1) {stop("wrong entry")}
  y<-c()
  t <- 1
  repeat{
    s <- x[t:(t+n)]
    y <- c(y, median(s, ...))
    t <- t + 1
    if(t > (length(x)-n)) break
  }
  return(y)
}


# 1.5.1
in_environment <- function(env) {
  as.vector(ls.str(env))
}


# 1.6.1
cov <- function(X) {
  if(length(X) <= 1){stop("wrong entry")}
  unlist(lapply(X, function(x) sd(x) / mean(x)))
}


# 1.7.1
moment <- function(i) {
  if(!is.vector(i, mode = "numeric")){stop("wrong entry")}
  function(x) {
    m <- (sum((x-mean(x))^i))/length(x)
    return(m)
  }
}


library(markmyassignment) 
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml" 
set_assignment(lab_path)
mark_my_assignment()