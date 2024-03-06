norm.HMM.mle <- function(x, m,q, vc,lower_bounds, upper_bounds,...){
  opt_problem_nloptr <- nloptr(x0 = vc,eval_f = norm.HMM.mllk,lb = lower_bounds,ub = upper_bounds,opts = list(algorithm = "NLOPT_LN_NELDERMEAD"),x=x1,m=m,q=q)
  # Run the optimization
  opt_problem_nloptr
  Result<-vector_to_matrices(opt_problem_nloptr$solution,m,q)
  return(Result)

  mllk <- norm.HMM.mllk(vc, x=x, m,q)
  np <- 2*m
  AIC <- 2 * (mllk + np)
  n <- nrow(x1)
  BIC <- 2 * mllk + np * log(n)
  list(mean = Result$mean, sd = Result$sd, gamma = Result$gamma, delta = Result$delta, mllk = mllk, AIC = AIC, BIC = BIC)
}
