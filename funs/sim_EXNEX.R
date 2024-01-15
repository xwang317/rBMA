# simulation for EXNEX
setwd("~/Documents/rBMA/funs")
source("fun_datagene.R")
library("bhmbasket")
# number of baskets
# sample size of baskets
n_basket <- 5
nvec <- c(20, 15, 20, 25, 30)
nlist <- rep(list(nvec), times = 16)

# response rate
p0 = c(0.05, 0.25, 0.20, 0.05, 0.1)
p1 = c(0.25, 0.5, 0.45, 0.25, 0.35)
pmat <- sc_gene(p0, p1, nvec)
plist <- split(pmat, seq(nrow(pmat)))

# Number of simulation
A = 1e4 # 10000 times
doFuture::registerDoFuture()
future::plan(future::multisession)
# set scenarios
scenarios_list <- simulateScenarios(nlist,
                                    plist,
                                    n_trials = A)
# prior models
prior_parameters <- setPriorParametersExNex(
  mu_mean   = c(logit(0.1), logit(0.3)),
  mu_sd     = c(3.18, 1.94),
  tau_scale = 1, #
  mu_j      = rep(logit(0.2), 5), # mu_j for NEX
  tau_j     = rep(2.5, 5), # tau_j for NEX
  w_j       = c(0.25, 0.25, 0.5)) # weight for two EX distributions and one NEX distribution

set.seed(316)
# uncomment if wanna run on the machine
# analyses_list <- performAnalyses(
#   scenario_list         = scenarios_list,
#   method_names          = "exnex",
#   prior_parameters_list = prior_parameters,
#   # seed                  = 42,
#   n_mcmc_iterations     = 5e4,
#   # n_cores               = 2L
#   )
# 
savefile <- "~/Documents/rBMA/simulation_data/"
# saveRDS(analyses_list, paste0(savefile, "analyses_list_10000_ss.rds"))
# load the pre-ran results
analyses_list <- readRDS(paste0(savefile, "analyses_list_10000.rds"))

estimates <- getEstimates(
  analyses_list = analyses_list,
  point_estimator = "mean"
)

scaleRoundList(
  list = estimates,
  scale_param = 1, 
  round_digits = 2
)

decisions_list <- getGoDecisions(
  analyses_list = analyses_list, 
  cohort_names = c("p_1",
                   "p_2",
                   "p_3",
                   "p_4",
                   "p_5"),
  evidence_levels = c(0.975,
                      0.975,
                      0.975,
                      0.975,
                      0.95),
  boundary_rules = quote(c(x[1] > 0.05,
                           x[2] > 0.25,
                           x[3] > 0.2,
                           x[4] > 0.05,
                           x[5] > 0.1))
)

go_probabilities <- getGoProbabilities(decisions_list)
go_probabilities$exnex$scenario_1[1]
# [1] 0.0992
# so that FWER under S0 is 0.1

# 'overall' column will be TRUE if there is any TRUE in 5 indications
# View(decisions_list$scenario_1$decisions_list$exnex)
# 0.099 in BMA and rBMA
# FWER under S0

# calculate FWER and indication wise rejecting rate 

# FWER ######
# help function: 
# calculate FWER for each scenario
# scenario_num: the number of scenario, if S0, then 1, if S1, then 2, and so on. 
calculate_exnex_fwer <- function(scenario_num){
  if (scenario_num %in% c(6, 11,16)){
    return(0)
  } else if (scenario_num %in% c(5,10,15)){
    decision_table <- decisions_list[[scenario_num]]$decisions_list$exnex[,6]
    return(mean(decision_table))
  } else {
  start_row <- ((scenario_num) %% 5)+1
  decision_table <- decisions_list[[scenario_num]]$decisions_list$exnex[,start_row:6]
  return(mean(unlist(apply(decision_table, 1, any))))
  }
}


# create exnex_er
exnex_fwer <- data.frame()


for (i in 1:nrow(pmat)) {
  new_row <- c(i, "EXNEX", calculate_exnex_fwer(i), "EXNEX")
  exnex_fwer <- rbind(exnex_fwer, new_row)
}

colnames(exnex_fwer) <- c("Scenario",
                          "name",
                          "value",
                          "method")

# Indication-wise rejecting rate ######
# exnex_ind

# helper function: 
# calculate indication-wise rejecting rate for each scenario and each indication
# scenario_num: the number of scenario, if S0, then 1, if S1, then 2, and so on. 
calculate_exnex_ind_rej <- function(scenario_num){
  decision_table <- decisions_list[[scenario_num]]$decisions_list$exnex[,2:6]
  return(apply(decision_table, 2, mean))
}



exnex_ind <- data.frame()
for (i in 1:nrow(pmat)) {
  scenario_ind_df <- data.frame(pr_wt = rep("EXNEX", n_basket), 
                                ind = LETTERS[1:n_basket],
                                Scenario = rep(i, n_basket), 
                                method = rep("EXNEX", n_basket),
                                er = calculate_exnex_ind_rej(i)
                                )
  exnex_ind <- rbind(exnex_ind, scenario_ind_df)
}

sim_result <- list(fwer = exnex_fwer, 
                   ind_er = exnex_ind)
  
save(sim_result, file = paste0(savefile, "exnex", "_", A,".RData"))

