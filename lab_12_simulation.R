generate_data = function(n, p) {
  # create an n x p matrix of random draws from the standard normal distribution
  covariates = rnorm(n*p)
  covariates = matrix(covariates, nrow = n, ncol = p)
  
  # draw n responses from the standard normal distribution
  responses = rnorm(n)
  
  return(list(covariates = covariates, responses = responses))
}
