ds <- read.csv("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt", header=FALSE)

#We can ignore the last column
ds <- ds[,c(1:10)]

#1.Compute the multivariate mean vector
#ds$multivariateMean <- rowMeans(ds)
mu <- colMeans(ds) #SHOULD THIS BE ROW MEANS?

#2. Compute the sample covariance matrix as inner products between the columns of the centered data matrix, 
#without using the covariance function in R. Compare and contrast your results with that obtained using 
#the covariance function in R.

dot_product <- function(x,y) {
  
  if (length(x) != length(y)) {
   # print("Vectors must be same size")
    stop("Vectors must be same size")
  }
  return(sum(x*y))
}

center_matrix <- function(dataset) {
  mu <- colMeans(dataset)
  
  Z <- data.matrix(dataset) #initialize matrix with dimensions of input
  
  ### Center Matrix ###
  for (i in 1:dim(ds)[1]) {   #rows
    for (j in 1:dim(ds)[2]) { #col
      Z[i,j] <- (ds[i,j] - mu[j])
    }
  }
  
  return(Z)
}

covariance_f <- function(x,y) {
  return(dot_product(x,y) / (length(x)-1)) #matches cov function in R when (n - 1))
}

#(Z(T)*Z)/N)