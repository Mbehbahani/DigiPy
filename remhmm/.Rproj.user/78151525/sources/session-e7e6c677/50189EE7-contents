norm.HMM.viterbi<-function(x,Result,m)
{
  n              <- nrow(x)
  xi             <- matrix(0,n,m)
  foo            <- Result$delta*dnorm(x[1],Result$mean,Result$sd)

  xi[1,]         <- foo/sum(foo)
  for (i in 2:n)
  {
    foo<-apply(xi[i-1,]*Result$gamma,2,max)*dnorm(x[i],Result$mean,Result$sd)
    xi[i,] <- foo/sum(foo)
  }
  iv<-numeric(n)
  iv[n]     <-which.max(xi[n,])
  for (i in (n-1):1){
    iv[i] <- which.max(Result$gamma[,iv[i+1]]*xi[i,])}
  return(iv)
}
