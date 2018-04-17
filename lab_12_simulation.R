generate_data = function(n, p) {
  # create an n x p matrix of random draws from the standard normal distribution
  covariates = rnorm(n*p)
  covariates = matrix(covariates, nrow = n, ncol = p)
  
  # draw n responses from the standard normal distribution
  responses = rnorm(n)
  
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  # runs a linear regression of responses ~ covariates
  lm = lm(responses ~ covariates)
  
  # save summary data
  lm.coefficients = summary(lm)$coefficients

  # p-values
  lm.pvalues = lm.coefficients[,"Pr(>|t|)"]
  
  # find significant covariates
  significant.cov = which(lm.pvalues <= cutoff) - 1
  
  # if no covariates are significant, return an empty vector
  ifelse(length(significant.cov) == 0, return(c()),
         ifelse((length(significant.cov) == 1 & significant.cov == 0), return(c()), TRUE)) 
  
  # otherwise...if intercept is significant, keep names
  ifelse((significant.cov[1] == 0), # test
         (sig.names = names(significant.cov)), # if TRUE
         (sig.names = c("(Intercept)", names(significant.cov))) # if FALSE
         )
    
  # run a linear regression using only significant covariates
  significant.cov = significant.cov[which(significant.cov != 0)] # only keep covariates, not intercept
  lm = lm(responses ~ covariates[,significant.cov])
  p.values = summary(lm)$coefficients[,"Pr(>|t|)"]
  names(p.values) = sig.names
  
  return(p.values)
}
