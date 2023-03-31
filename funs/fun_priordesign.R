# This file contains functions for prior design. 
# prior weight design and prior beta distribution design

## pr_weight ###### 
# function to calculate prior weight for each model under different gamma in weight setting
# input: mods, weight setting, other parameters: 
# 
# algorithm of weight models: 
# lin.ALT: gamma = 1
# qdr.ALT: gamma = 2
# nega.ALT.1: gamma = -1
# nega.ALT.2: gamma = -2
# output: prior a vector of prior weight
pr_weight <- function(mods = mods,setting = c("EQL", "lin.ALT","qdr.ALT",
                                              "nega.ALT.1","nega.ALT.2", "non.info.bench"),
                      bma_method){ # function start
  B <- ncol(mods)
  sc_posi <- factor(rowSums(mods==1))
  sc_posi <- data.frame(sc_posi)
  
  # give back prior weight:  
  if (setting == "nega.ALT.1"){
    tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)^(-1)/sum((1+0:B)^(-1)))
    prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
    } else if (setting == "EQL"){
      sc_posi <- rowSums(mods==1)
      tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)^(0)/sum((1+0:B)^(0)))
      prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
      }else if (setting == "nega.ALT.2") {
      tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)^(-2)/sum((1+0:B)^(-2)))
      prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
    }else if (setting == "non.info.bench") {
      # this weight is a fake-weight to let the program run
      tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)^(-2)/sum((1+0:B)^(-2)))
      prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
    }else if (setting == "lin.ALT"){
    tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)/sum(1+0:B))
    prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
  } else if (setting == "qdr.ALT"){
    tab0 <- rbind(table(rowSums(mods==1)), (1+0:B)^2/sum((1+0:B)^2))
    prior <- tab0[2, 1+rowSums(mods==1)]/tab0[1, 1+rowSums(mods==1)]
  } 
  fit_p <- cbind(mods, prior)
  return(fit_p)
}

## design.prior ######
# set the design prior for binary data
# theta: the hypothesized response rate
# n: the ess of theta
# half_width of the 95% CI
design.prior.bin <- function(theta, pt, lci, uci){
  fit <- get.beta.par(p = pt, q = c(lci, theta, uci),
                      show.output = FALSE, plot = FALSE)
  a <- fit[[1]]
  b <- fit[[2]]
  c(a, b)
}

# function to give back pairwise prob of getting same component of prior ####
prob_corr <- function(B, prior, mods){
  corrmat <- probmat <- matrix(NA, nrow = B, ncol = B)
  fit_p <- cbind(mods, prior)
  # calculate 
    for (i in 1:B){ # row
      for (j in 1:B) { # col
        probmat[i, j] <- probmat[j, i] <- sum((mods[, i]==1 & mods[, j] ==1)*prior)  
      }
    }
    return(probmat)
}


