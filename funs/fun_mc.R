#### functions of running mc simulations #######

## wt_mc #######
# this function run simulation by different prior weight 
# input: p0: null hypothesis p; pmat: scenario matrix; mods: membership matrix; 
#        bma_method: "bma" or "robust"; 
#'       par0, par1: parameter of pessimistic and enthusiastic prior;
#'       tag.fwer: target FWER; 
#'       prior_mat: prior weight matrix, with COLUMN NAME as weight name
#'       A: MC simulation times 
# output: result of pmat listed by each scenario, includes: 
#         parameter setting (p0, pmat, mods, bma_method)
#         weight setting (wt_method, wt_name, pr_weight function to see details)
#         prior-ESS for each weight
#         FWER&FWP for each scenario in each weight
#         indication-wise ER and Power for each scenario in each weight

wt_mc <- function(p0, pmat, mods, nvec, 
                    bma_method = c("bma", "robust"), 
                    par0, par1, # pessimistic and enthusiastic prior
                    tag.fwer = 0.1, 
                    prior_mat,  A = 1000){
  # setup parameters
  B = ncol(pmat) # indication  
  S = nrow(pmat) # scenario
  
  wt_name <- colnames(prior_mat)
  if(is.null(wt_name)) stop("check colnames of prior_mat")
  n_wt <- ncol(prior_mat) 
  
  # MC simulation
  # array to store results from S scenarios
  bma_pp <- array(NA, c(S, A, B)) # S scenario, A, B indication
  # Matrix for saving the power of each basket in each scenario
  bma_pow <- matrix(NA, nrow = S, ncol = B) 
  pow_tab <- fwer_tab <-  NULL
  ind_er_tab <- array(NA, dim = c(n_wt, S, B))
  for(m in 1:n_wt){ 
    
    prior <- prior_mat[,m]
    
    if (names(prior_mat)[m] == "non.info.bench"){
      for (s in 1:S){ # loop by scenario
        pval.pp <- mclapply(1:A, function(seed){
          mc_iter_bench(seed, s, B, nvec, pmat, mods, par0, par1)
        })
        bma_pp[s, ,] <- do.call(rbind, mclapply(1:A, function(i){
          pval.pp[[i]]}))
        }# end of loop by s
    } else {
    for (s in 1:S) { # loop by scenario
      pval.pp <- mclapply(1:A, function(seed){
        mc_iter(seed, s, B, nvec, pmat, mods, prior, par0, par1)
      })
      bma_pp[s, ,] <- do.call(rbind, mclapply(1:A, function(i){
        pval.pp[[i]]}))
      # mc_iter # foreach
      # give back bma_pp
      
    }# end of s in 1:S
    }# end of else (not == non.info.bench) 
    
    ## FAMILY WISE:
    # find the gam that control FWER = tag.fwer under null hypothesis
    # browser()
    gam_bma <- fwer1(post_prob = bma_pp[1, , ], fwer_target = tag.fwer)$gam
    
    fwer_bma <- NULL
    fwp_bma <- NULL
    
    ind_er_bma <- matrix(NA, nrow = S, ncol = B)
    
    for(s in 1:S){ # S scenario: normal H1, low H1, high H1

      # FWER
      idx <- pmat[s, ]==pmat[1, ] 
      tmp_bma <- matrix(bma_pp[s, , idx], nrow = A)
      fwer_bma[s] <- fwer1(post_prob = tmp_bma, gam = gam_bma)$FWER
      
      ## family-wise power
      idx <- pmat[s, ] > pmat[1, ] # all the indication with truth are not null
      tmp_bma <- matrix(bma_pp[s, , idx], nrow = A)
      fwp_bma[s] <- mean(apply(tmp_bma > gam_bma, 1, any))
    
      
      ## INDICATION WISE: 
      # find indication wise power and error rate in each scenario
      
      for (idx in 1:B) { # loop for basket
        tmp_bma <- matrix(bma_pp[s, , idx], nrow = A)
        ind_er_bma[s,idx] <- mean(tmp_bma > gam_bma)
      } # end of loop of basket
      
      } # end of loop s

    pow_tab <- cbind(pow_tab, fwp_bma)
    fwer_tab <- cbind(fwer_tab, fwer_bma)
    ind_er_tab[m,,] <- ind_er_bma
  }# end of m in 1:n_wt
  
  # browser()
  
  pow_tab <- pow_tab
  fwer_tab <-fwer_tab
  colnames(pow_tab) <- wt_name
  colnames(fwer_tab) <- wt_name
  fwer_tab <- round(fwer_tab, 3)
  pow_tab <- round(pow_tab, 3)
  # indication-wise
  dimnames(ind_er_tab) <- list(wt_name)
  
  result = list(power = pow_tab, 
                fwer = fwer_tab,
                ind_er = ind_er_tab)
  return(result)
}

## function: mc_iter #####
mc_iter <- function(seed, s, B, nvec, pmat, 
                      mods, prior, par0, par1){
  set.seed(seed)
  #simulate data for the ith trial under scenario s:
  xvec <- rbinom(B, size = nvec, prob = pmat[s, ]) # nvec is the sample size of each indication
  # xvec is simulated responded subjects in each indication
  
  #fit: posterior weight for each M_k
  fit <- bma2(mods = mods, prior = prior, x = xvec, n = nvec, 
             a0 = par0[, 1], b0 = par0[, 2],
             a1 = par1[, 1], b1 = par1[, 2])
  # column p is the posterior weight of M_k
  
  #get marginal posterior probabilities of each design prior
  wmat <- apply(fit[, 1:B], 2, function(x){
    aggregate(fit$p, by = list(x), sum)$x
  })
  
  # sum together
  
  bma_pp <- foreach(b = 1:B, .combine = cbind, .packages = 'RBesT') %do% {
    # pval use binomial test with fixed value of null hypo
    if(bma_method=="robust"){
      bmix <- mixbeta("pessimistic"=c(wmat[1, b], xvec[b] + par0[b, 1], 
                                      nvec[b] - xvec[b] + par0[b, 2]), 
                      "optimistic"=c(wmat[2, b], xvec[b] + par1[b, 1], 
                                     nvec[b] - xvec[b] + par1[b, 2]),
                      "rob"=c(wmat[3, b], xvec[b]+1, 1+nvec[b] - xvec[b]))
    }else{
      bmix <- mixbeta("pessimistic"=c(wmat[1, b], xvec[b] + par0[b, 1], 
                                      nvec[b] - xvec[b] + par0[b, 2]), 
                      "optimistic"=c(wmat[2, b], xvec[b] + par1[b, 1], 
                                     nvec[b] - xvec[b] + par1[b, 2]))
    }
    1-pmix(bmix, p0[b]) # posterior probability of > p0 of each indication
  } # end of foreach b
  return(bma_pp = bma_pp)
} # end of function

## function: mc_iter_bench #####
mc_iter_bench <- function(seed, s, B, nvec, pmat, 
                            mods, par0, par1){
  set.seed(seed)
  #simulate data for the ith trial under scenario s:
  xvec <- rbinom(B, size = nvec, prob = pmat[s, ]) # nvec is the sample size of each indication
  # xvec is simulated responded subjects in each indication
  
  bma_pp <- foreach(b = 1:B, .combine = cbind, .packages = 'RBesT') %do% {
    # pval use binomial test with fixed value of null hypo
    btrt = c(xvec[b] + 1, nvec[b] - xvec[b] + 1)
    # calculate and output 
    1-pbeta(p0[b], btrt[1], btrt[2])
  }
  return(bma_pp = bma_pp)
} # end of function

