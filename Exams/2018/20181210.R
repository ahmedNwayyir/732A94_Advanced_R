# Problem 2

library <- setRefClass("ibrary",
                       
   fields = list(book_capacity = "numeric",
                 number_of_books = "numeric",
                 number_of_loaned = "numeric",
                 books = "data.frame"),
   
   methods = list(
     
     build_library = function(book_capacity){
       stopifnot(book_capacity > 0)
       book_capacity <<- book_capacity
       number_of_books <<- 0
       number_of_loaned <<- 0
       books <<- data.frame(title = character(),
                            author = character(),
                            id = character(),
                            status = character(),
                            stringsAsFactors = FALSE)
       names(books) <<- c("title", "author", "id", "status")
       },
     
     acquire_book = function(title, author){
       if(number_of_books == book_capacity){
         return("This library is full, can not add book anymore.")
       }else{
        id <- sprintf("%05d", nrow(books)+1)
        new_book <- list("title" = title, "author" = author, "id" = id, "status" = "Available")
        books <<- rbind(books, new_book)
        number_of_books <<- number_of_books + 1
        result <- paste(title, "(", author,") have been added in library!")
         return(result)
       }
     },
     
     borrow_book = function(id){
       if(id %in% books$id){
         index <- which(books$id == id)
         if(books[index,]$status == "Available"){
           number_of_loaned <<- number_of_loaned + 1
           books[index,]$status <<- "Loaned"
           result <- "Borrow process succeed."
         }else{
           result <- "Is book is not avaliable now!" 
         }
         return(result)
       }else{
         return("Does not find this book, please check the ID.")
       }
     },
     
     return_book = function(id){
       if(id %in% books$id){
         index <- which(books$id == id)
         if(books[index,]$status == "Loaned"){
           number_of_loaned <<- number_of_loaned - 1
           books[index,]$status <<- "Available"
           result <- "Return process succeed."
         }else{
           result <- "Sorry, this book have not been borrowed." 
         }
         return(result)
       }else{
         return("Does not find this book, please check the ID.")
       }
     },
     
    print = function(){
      available_list <- books[which(books$status == "Available"),]
      loaned_list <- books[which(books$status == "Loaned"),]
      print_list <- list(nrow(available_list), available_list, nrow(loaned_list), loaned_list)
      names(print_list) <- c("Number of Available", "Available", "Number of Loaned", "Loaned")
      return(print_list)
    },
    
    plot = function(){
      status_list <- c(length(which(books$status == "Available")), length(which(books$status == "Loaned")))
      barplot(status_list,
              names.arg = c("Available", "Loaned"),
              xlab = "Book Status",
              ylab = "Number",
              main = "Book Possession")
    }
     ))

my_library <- library$new()
my_library$build_library(book_capacity = 100)

for(i in 1:10){
  
  my_library$acquire_book("R Exam 2018/10/23", "Krzysztof Bartoszek")
  my_library$acquire_book("R Exam 2018/12/10", "Krzysztof Bartoszek")
}

my_library$borrow_book("00001")
my_library$borrow_book("00002")

my_library$print()
my_library$plot()




# Problem 3
# a
m <- matrix(1:25, nrow = 5)
x <- c(1:5)
quadratic_form = function(m, x){
  result <- 0
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      result <- x[i]*m[i,j]*x[j] + result
    }
  }
  return(result)
}
quadratic_form(m, x)


result <- x%*%m%*%x
print(result)

# c
test_that("The output is incorrect.", {
  expect_that(quadratic_form(m, x), is_equivalent_to(x%*%m%*%x))
})











