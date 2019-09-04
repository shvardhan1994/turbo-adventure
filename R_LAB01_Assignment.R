

name <- "Harshavardhan Subramanian"
liuid <- "harsu008"

#1.1.1

my_num_vector <- function(){
  v1 <- c(log(11,10),cos(pi/5),exp(pi/3),(1173%%7)/19)
  return(v1)
}



#1.1.2
filter_my_vector <- function(x,leq){
  x[x >= leq] <- NA 
  return(x)
}



#1.1.3
dot_prod <- function(a,b){
  c <- a*b
  d <- sum(c)
  return(d)
  
}



#1.1.4

approx_e <- function(N){
  res <- 0
  for(i in 0:N){
    res <- res + 1/factorial(i)
    
  }
  return(res)
}

# As N is increased to higher numbers, the order of the summation to find 
#exponential of a given number tends to match the value of actual exponential function. 
#In this case if N=7, the exponential value found by summation matches the exponential value 
#of the actual function upto 4 decimal points.

#Matrix

#1.2.1

my_magic_matrix <- function(){
  
  a <- matrix(c(4,3,8,9,5,1,2,7,6),3,3)
  return(a)
}


# The sumof individual columns,rows and diagonals are the same.

#1.2.2

calculate_elements <- function(A){
  
  len <- length(A)
  return(len)
}


#1.2.3

row_to_zero <- function(A,i){
  
  A[i,] <- 0
  return(A)
}



#1.2.4

add_elements_to_matrix <- function(A,x,i,j){
  
  A[i,j] <- A[i,j] + x
  return(A)
  
}


# Lists

#1.3.1 

my_magic_list <- function(){
  
  lst <- list("info" = "my own list",my_num_vector(),my_magic_matrix())
  lst[1]
  return(lst)
  
}



#1.3.2

change_info <- function(x,text){
  x["info"] <- text
  return(x)
}



#1.3.3
add_note <- function(x,note){
  x[["note"]] <- note
  return(x)
}


#1.3.4

sum_numeric_parts <- function(x){
  
 
  
  sum <- sum()
  num <- sum(sapply(as.numeric(unlist(x),na.rm = TRUE),sum),na.rm = TRUE)
  return(num)
  
}

#DataFrames

#1.4.1

my_data.frame <- function(){
  
  mydf <- data.frame(c(1,2,3),c("John","Lisa","Azra"),c(7.30,0.00,15.21),c(FALSE,FALSE,TRUE))
  colnames(mydf) <- c("id","name","income","rich")
  rownames(mydf) <- c("1","2","3")
  return(mydf)
  
}



#1.4.2

sort_head <- function(df, var.name, n){
  df <- head(df[order(df[,var.name], decreasing = TRUE),],n)
  
  return(df)
  
}



#1.4.3

add_median_variable <- function(df, j){
  
  med <- median(df[,j])
  df$compared_to_median <- "Median"
  a <- which(df[,j] > med)
  b <- which(df[,j] < med)
  df$compared_to_median[a] = "Greater"
  df$compared_to_median[b] = "Smaller"
  
  return(df)
}




#1.4.4

analyze_columns <- function(df,j){
  v1 <- c("mean" = mean(df[,j[1]]),"median" = median(df[,j[1]]),"sd" = sd(df[,j[1]]))
  v2 <- c("mean" = mean(df[,j[2]]),"median" = median(df[,j[2]]),"sd" = sd(df[,j[2]]))
  v3 <- cor(df[j])
  lst <- list(v1, v2, v3)
  names(lst) <- c(colnames(df[j[1]]),colnames(df[j[2]]),"correlation_matrix")
  return(lst)
  
}




