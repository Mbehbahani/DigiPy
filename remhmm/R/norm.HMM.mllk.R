norm.HMM.mllk <- function(vc, x, m,q) {
  matrices <- vector_to_matrices(vc, m = m,q)

  gamma0<-matrices$gamma
  mean<-matrices$mean
  sd<-matrices$sd
  delta<-matrices$delta
  n <- length(x[,1])

  n_dep<-q
  inp <- rep(list(NULL), n_dep)

  for(i in 1:n_dep){
    inp[[i]] <- outer(x[,i], Y = mean[i,], FUN = stats::dnorm,
                      sd = rep((sd[i,]), each = dim(x)[1]))
  }

  allprobs <- Reduce("*", inp)

  allprobs <- ifelse(!is.na(allprobs), allprobs, 1)
  lscale <- 0
  foo <- delta

  for (i in 1:n) {
    foo <- foo %*% gamma0 * allprobs[i, ]
    sumfoo <- sum(foo)
    lscale <- lscale + log(sumfoo)
    foo <- foo / sumfoo
  }

  mllk <- -lscale
  mllk
}
