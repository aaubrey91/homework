ds <- read.csv("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt", header=FALSE)

#We can ignore the last column
ds <- ds[,c(1:10)]

#1.Compute the multivariate mean vector
#ds$multivariateMean <- rowMeans(ds)
mu <- colMeans(ds)

#z <- data.frame()
z <- ds

for (i in dim(ds)[1]) {   #rows
  for (j in dim(ds)[2]) { #col
    z[i,j] <- (ds[i,j] - mu[j])
  }
}

#2. Compute the sample covariance matrix as inner products between the columns of the centered data matrix, 
#without using the covariance function in R. Compare and contrast your results with that obtained using 
#the covariance function in R.

covariance_f <- function(dataset) {
  
  #mu <- colMeans(dataset)
  
  #tmp <- data.frame()
  #for (i in names(dataset)) {
  #  
  #}
  #return(colMeans(dataset))
}
