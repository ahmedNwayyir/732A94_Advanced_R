name <- "Ahmed_AlHasan"
liuid <- "ahmal787"

# 1.1.1
my_num_vector <- function() {
  x <- c(log10(11),cos(pi/5),exp(pi/3),(1173%%7)/19) 
  return(x)
}


# 1.1.2
filter_my_vector <- function(x, leq) {
  return(ifelse (x < leq, x, NA))
}


# 1.1.3
dot_prod <- function(a,b) {
  return(as.vector(a%*%b))
}


# 1.1.4

approx_e <- function(N){
  largeN <- 0
  for(n in 0:N){
    e <- sum(1/factorial(0:n))
    if (round(e, digits = 5) != round(exp(1), digits = 5)){
      Nfound <- FALSE
      largeN <- largeN + 1
    }
    else{
      Nfound <- TRUE
    }
  }
  
  if(Nfound){
    cat("N need to be: ", largeN, "\n")
  }
  else{
    print("N is not large enough")
  }
  
  cat("approx_e: ", round(e, digits = 5), "actual_e: ", round(exp(1), digits = 5))
  return(e)
}


# 1.2.1
my_magic_matrix <- function() {
  matrix(c(4,3,8,9,5,1,2,7,6), nrow=3)
}


# 1.2.2
calculate_elements <- function(A) {
  nrow(A)*ncol(A)
}


# 1.2.3
row_to_zero <- function(A,i) {
  A[i,] <- 0
  return(A)
}


# 1.2.4
add_elements_to_matrix <- function(A,x,i,j) {
  A[i,j] = A[i,j]+x
  return(A)
}

 
# 1.3.1
my_magic_list <- function() {
  res <- list(info="my own list", my_num_vector(), my_magic_matrix())
  return(res)
}


# 1.3.2
change_info <- function(x,text) {
  x$info = text
  return(x)
}


# 1.3.3
add_note <- function(x,note) {
  x$note <- note
  return(x)
}


#1.3.4
sum_numeric_parts <- function(x) {
  x <- as.numeric(unlist(x))
  res <- sum(x,na.rm = TRUE)
  return(res)
}


# 1.4.1
my_data.frame <- function() {
  data.frame(
    id = c(1:3),
    name = c("John", "Lisa", "Azra"),
    income = c(7.30, 0.00, 15.21),
    rich = c(FALSE, FALSE, TRUE)
  )
}


# 1.4.2
sort_head <- function(df, var.name, n) {
  sorted_var <- order(df[,var.name], decreasing = TRUE)
  sorted_df  <- df[sorted_var,]
  return(sorted_df[1:n,])
}


# 1.4.3
add_median_variable <- function(df,j) {
  l <- length(df[,j])
  m <- median(df[,j])
  df$compared_to_median <- as.character(1:l)
  df$compared_to_median <- ifelse(df[,j] < m, df$compared_to_median = "Smaller", 
                                  ifelse (df[,j] > m, df$compared_to_median = "Greater", 
                                                      df$compared_to_median = "Median"))
  return(df)
}


# 1.4.4
analyze_columns <- function(df,j) {
  mu_1  <- mean(df[,j[1]])
  med_1 <- median(df[,j[1]])
  sd_1  <- sd(df[,j[1]])
  
  v1 <- c(mu_1, med_1, sd_1)
  names(v1) <- c("mean", "median", "sd")
  
  mu_2  <- mean(df[,j[2]])
  med_2 <- median(df[,j[2]])
  sd_2  <- sd(df[,j[2]])
  
  v2 <- c(mu_2, med_2, sd_2)
  names(v2) <- c("mean", "median", "sd")
  
  m <- matrix(cor(data.frame(df[j[1]], df[j[2]])), nrow = 2)
  
  l <- list(v1, v2, m)
  
  names(l)[1] <- names(df)[j[1]]
  names(l)[2] <- names(df)[j[2]]
  names(l)[3] <- "correlation_matrix"
  
  return(l)
}


library(markmyassignment) 
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml" 
set_assignment(lab_path)
mark_my_assignment()