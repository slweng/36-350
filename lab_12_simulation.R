generate_data = function(n, p) {
  covariates = rnorm(n*p)
  covariates = matrix(covariates, nrow = n, ncol = p)
  
  responses = rnorm(n)
  
  return(list(covariates = covariates, responses = responses))
}