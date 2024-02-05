vector_to_matrices <- function(vc, m,q=q ) {
  # Calculate the size of gamma
  p=(m)*(m-1)
  # Create gamma matrix
  gamma <- matrix(vc[1:p], nrow = m, ncol = m-1,byrow=TRUE)
  gamma <- cbind(gamma, 1-rowSums(gamma))
  # Extract mean0, sd0, and delta0
  mean0 <- vc[(p+1):(p+m*q)]
  mean0 <-matrix(mean0,nrow = q,byrow=TRUE)
  sd0 <- vc[((p+m*q)+1):(p+m*q+m*q)]
  sd0 <- matrix(sd0,nrow = q,byrow=TRUE)
  delta0 <- vc[((p+m*q+m*q)+1):((p+m*q+m*q)+m-1)]
  delta0 <- c(delta0, 1-sum(delta0))
  return(list(gamma = gamma, mean = mean0, sd = sd0, delta = delta0))
}

# Example usage:
# Assuming you have 4x4 matrices
#Each column represent a state (m) and row is variable (q)
# gamma <- matrix(c(0.2, 0.3, 0.4, 0.1, 0.5, 0.1, 0.2, 0.2, 0.1, 0.2, 0.5, 0.2, 0.3, 0.1, 0.1, 0.5), nrow = 4,byrow=TRUE)
#
# mean0 <- c(1, 1, 1, 1,3,3,3,3)
# sd0 <- c(2, 2, 2, 2,4,4,4,4)
# delta0 <- c(0.3, 0.3,0.3,0.1)
#
# vc <- matrices_to_vector(gamma, mean0, sd0, delta0)
# m<-4
# matrices_example <- vector_to_matrices(vc, m = m,q=2)
