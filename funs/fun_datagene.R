# This file contain functions that will generate scenarios and models 
## sc_gene #######
# generate scenarios 
# input: p0, p1, nvec: the sample size of each basket
# output: pmat
sc_gene <- function(p0,
                    p1,
                    nvec = rep(16, 5)){
  ## set up the basket trial: 
  ## multiple indications, multiple endpoints
  #B: the number of indications 
  B <- length(p0)
  ## set up the simulation scenarios:
  # pmat(matrix): elements are the response rate of each basket
  # each row of pmat is a different simulation scenario
  # assume the treatment has half the hypothesized effect
  p1_low <- p0+0.075
  # assume the treatment is better than the hypothesized effect
  p1_high <- p1+0.2
  
  pmat1 <- pmat2 <- pmat3 <- matrix(NA, B+1, B)
  for(b in 1:(B+1)){
    pmat1[b, ] <- pmat2[b, ] <- pmat3[b, ] <- p0
  }
  # different numbers of effected indication (normal, low, high)
  for(b in 1:B){
    # create scenarios same as hypothesized p1
    pmat1[-1, ][b, 1:b] <- p1[1:b]
    # create scenarios different from hypothesized p1
    pmat2[-1, ][b, 1:b] <- p1_low[1:b]
    pmat3[-1, ][b, 1:b] <- p1_high[1:b]
  }
  
  pmat <- rbind(pmat1, pmat2[-1, ], pmat3[-1, ])
  return(pmat)
}

## init.mod #####

## set up all the models: 
## B: number of indications
# function init.mod: 
# this function give back all the combination of prior setting of B indications
# example: init.mod(3,0:2)
init.mod <- function(B, bma_method, ind_name){
  if(bma_method == "bma"){
    prior_indicator <- c(0, 1)
  } else if (bma_method == "robust"){
    prior_indicator <- c(0, 1, 2)
  }
  zero_one <- list()
  #1 is promising, 0 is not promising
  for(b in 1:B) zero_one[[b]] <- prior_indicator
  mods <- expand.grid(zero_one)
  names(mods) <- ind_name
  mods
}
