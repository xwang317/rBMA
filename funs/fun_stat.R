# functions for statistics calculation ######

# gam is the threshold of probability of rejection
# tune epsilon to adjust the gap between gam
# FWER: reject NULL if there is one indication of N indication reject null
fwer1 <- function(post_prob, fwer_target = 0.10, epsilon = 0.001, gam = NULL){
  if(is.null(gam)){
    FWER <- 0
    gam <- 1
    while(FWER<fwer_target){
      gam <- gam - epsilon
      FWER <- mean(apply(post_prob>gam, 1, any))
    }
    gam <- gam + epsilon
    FWER <- mean(apply(post_prob>gam, 1, any))
  }else{
    FWER <- mean(apply(post_prob>gam, 1, any))
  }
  return(list("gam"=gam, "FWER"=FWER))
}

# function: bma: ######
# this function calculate posterior weight for each M_k
#a0, b0 (vector of length B): pessimistic beta priors
#a1, b1 (vector of length B): optimistic beta priors
bma <- function(mods, prior, x, n, a0, b0, a1, b1){
  B <- length(x)
  a <- b <- rep(1, B)
  p <- NULL
  
  for(k in 1:nrow(mods)){
    idx <- mods[k, 1:B]
      a[idx==0] <- a0[idx==0]
      b[idx==0] <- b0[idx==0]
      a[idx==1] <- a1[idx==1]
      b[idx==1] <- b1[idx==1]
      #calculate marginal probs m(s_j)
    p[k] <- prod((beta(a+x, b+n-x)/beta(a,b)))*prior[k]  
  }
  ## calculate posterior prob of each grouping structure
  # p(M_k|D, D^p) 
  # normalizing
  mods$p <- p/sum(p)
  
  mods
}

# function: bma2: ######
# this function calculate posterior weight for each M_k
#a0, b0 (vector of length B): pessimistic beta priors
#a1, b1 (vector of length B): optimistic beta priors
#try to add a non-infomative posterior probablity of D
bma2 <- function(mods, prior, x, n, a0, b0, a1, b1){
  B <- length(x)
  a <- b <- rep(1, B)
  p <- NULL
  
  for(k in 1:nrow(mods)){
    idx <- mods[k, 1:B]
    a[idx==0] <- a0[idx==0] 
    b[idx==0] <- b0[idx==0] 
    a[idx==1] <- a1[idx==1] 
    b[idx==1] <- b1[idx==1] 
    a[idx==2] <- 1
    b[idx==2] <- 1
    #calculate marginal probs m(s_j)
    p[k] <- prod((beta(a+x, b+n-x)/beta(a,b)))*prior[k]  
  }
  ## calculate posterior prob of each grouping structure
  # p(M_k|D, D^p) 
  # normalizing
  mods$p <- p/sum(p)
  mods
}
