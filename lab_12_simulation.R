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

run_simulation = function(n_trials, n, p, cutoff) {
  # collecting all pvalues from different trials, initialize vector
  p.values = c()
  
  # calculate pvalues
  for (i in 1:n_trials) {
    dat = generate_data(n, p)
    pval = model_select(dat[[1]], dat[[2]], cutoff)
    p.values = c(p.values, pval)
  }
  
  # save p.values to a file
  # file name is "sim_ntrials_n_p_cutoff"
  save(p.values,
       file = paste("sim", 
                    as.character(n_trials), as.character(n), as.character(p), as.character(cutoff), ".RData",
                    sep = "_"))
}

make_plot = function(datapath) {
  # loads vector of pvalues: "p.values"
  load(datapath)
  # set local environment variable pvalues to ensure function does not plot global variable p.values
  pvalues = p.values
  # set variable values
  variables = strsplit(datapath, split = "_")[[1]]
  variables = variables[-c(1,length(variables))] # removes first and last items: "sim" and ".RData"
  n_trials = variables[1]
  n = variables[2]
  p = variables[3]
  cutoff = variables[4]
  
  # plot histogram
  hist(pvalues, main = paste(n_trials, " Simulations for n = ", n, ", p = ", p, sep = ""))
}
