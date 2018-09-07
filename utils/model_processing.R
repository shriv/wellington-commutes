library(dplyr)

CalcRSquared <- function(observed,estimated){
    # Calculate the R2 metric
    # [0, 1]: fraction explained variance
    #
    # Args:
    #  observed: vector of observed values
    #  estimated: vector of estimated values
    #
    # Returns:
    #  R2
    
    r <- cor(observed,estimated)
    R2 <- r^2
    R2
}

