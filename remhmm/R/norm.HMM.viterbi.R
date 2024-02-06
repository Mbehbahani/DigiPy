norm.HMM.viterbi<-function(x,Result,m)
{
  n              <- nrow(x)
  xi             <- matrix(0,n,m)

  foo=Result$delta

  for(i in 1:q){
    foo <- dnorm(x[1,i],Result$mean[i,],Result$sd[i,])*foo
  }

  xi[1,]         <- foo/sum(foo)

  for (j in 2:n){
    foo<-apply(xi[j-1,]*Result$gamma,2,max)

    for(i in 1:q){
      foo <- dnorm(x[j,i],Result$mean[i,],Result$sd[i,])*foo
    }
    xi[j,] <- foo/sum(foo)
  }


  iv<-numeric(n)
  iv[n]     <-which.max(xi[n,])
  for (i in (n-1):1){
    iv[i] <- which.max(Result$gamma[,iv[i+1]]*xi[i,])}
  return(iv)
}
