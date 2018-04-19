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
    return("Vectors must be same size")
  }
  return(sum(x*y))
}

covariance_f <- function(dataset) {
  
  mu <- colMeans(dataset)
  
  Z <- data.frame()
  
  ### Center Matrix ###
  for (i in 1:dim(ds)[1]) {   #rows
    for (j in 1:dim(ds)[2]) { #col
      Z[i,j] <- (ds[i,j] - mu[j])
    }
  }
  print(dot_product(Z[,1],Z[,2]) / length(Z[,1]))
  return(Z)
}
