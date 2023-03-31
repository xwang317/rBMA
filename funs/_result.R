## This file mainly contain the simulation process of each scenario
## scenario matrix will be generated and then do MC simulation
rm(list = ls())
setwd("C:/Users/xw455/OneDrive - Yale University/proj-Wei-clinical trial/rBMA/funs")
# setwd("/gpfs/ysm/project/esserman/xw455/rBMA/funs")
source("library.R")

# CONFIG ####
n.cores = parallel::detectCores()
options(mc.cores = n.cores)
# rstan_options(auto_write = TRUE)


# Choose simulation for reproduce results in section 3.2
# simulation
# sen = FALSE # indicator of sensitivity analysis
# p0 = c(0.05, 0.25, 0.20, 0.05, 0.1)
# p1 = c(0.25, 0.5, 0.45, 0.25, 0.35)

# Choose sensitivity analysis for reproduce results in section 3.3
# sensitivity analysis
sen = TRUE # indicator of sensitivity analysis
p0 = rep(0.25,5)
p1 = rep(0.5,5)

B = length(p0)
ind_name <- paste0("#", 1:B)
nvec = rep(20, B)
tag.fwer = 0.1
tag.beta = 0.2
tag.power = 1-tag.beta
tag.margin = 0
A = 10000
## generate scenarios: all H0, 1~5 H1, 1~5 low H1, 1~5 high H1
pmat <- sc_gene(p0, p1, nvec)
S <- nrow(pmat)
print(pmat)

## choose bma_method #####
# bma_method = "bma"
bma_method = "robust"

# set weight 
wt_name <- c("non.info.bench","lin.ALT", "qdr.ALT","nega.ALT.1", "nega.ALT.2")
n_wt <- length(wt_name)

## save location #####
savefile <- "C:/Users/xw455/OneDrive - Yale University/proj-Wei-clinical trial/rBMA/simulation/"
# savefile <- "/gpfs/ysm/project/esserman/xw455/rBMA/simulation/"
# if folder does not exist, need to create it first before calling it. 

# END of CONFIG ########

# get result #####

source("_do_by_method.R")

# SAVE list of result ######
sim_result <- list(p0 = p0, 
                   p1 = p1, 
                   nvec = nvec,
                   pmat = pmat, 
                   bma_method = bma_method, 
                   par0 = par0,
                   par1 = par1,
                   wt_name = wt_name, 
                   mods = mods,
                   prior_mat = prior_mat, 
                   simmat = simmat, 
                   fwer = fwer,
                   power = power,
                   ind_er = ind_er_df,
                   A = A,
                   design = "SINGLE")

# SAVE as Rdata ######
if(sen){
  save(sim_result, file = paste0(savefile,"sen-", bma_method,"_", A,".RData"))
}else{
  save(sim_result, file = paste0(savefile, bma_method,"_", A,".RData"))
}


