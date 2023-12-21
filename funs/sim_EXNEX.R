# simulation for EXNEX
setwd("D:/OneDrive - Yale University/proj-Wei-clinical trial/rBMA/funs")
source("fun_datagene.R")
library("bhmbasket")
# number of baskets
n_basket <- 5
# sample size of baskets
nvec <- rep(20,5)
nlist <- rep(list(nvec), times = 16)

# response rate
p0 = c(0.05, 0.25, 0.20, 0.05, 0.1)
p1 = c(0.25, 0.5, 0.45, 0.25, 0.35)
pmat <- sc_gene(p0, p1, nvec)
plist <- split(pmat, seq(nrow(pmat)))

# Number of simulation
A = 1e3 # 1000 times
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
  tau_scale = 1,
  mu_j      = rep(logit(0.2), 5), # mu_j for NEX
  tau_j     = rep(2.5, 5), # tau_j for NEX
  w_j       = c(0.25, 0.25, 0.5)) # weight for two EX distributions and one NEX distribution 

set.seed(316)

# analyses_list <- performAnalyses(
#   scenario_list         = scenarios_list,
#   method_names          = "exnex",
#   prior_parameters_list = prior_parameters,
#   # seed                  = 42,
#   n_mcmc_iterations     = 5e4,
#   # n_cores               = 2L
#   )
# 
# saveRDS(analyses_list, "analyses_list.rds")

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
  evidence_levels = c(0.9,
                      0.9,
                      0.9,
                      0.9,
                      0.9),
  boundary_rules = quote(c(x[1] > 0.05,
                           x[2] > 0.25,
                           x[3] > 0.2,
                           x[4] > 0.05,
                           x[5] > 0.1))
)

go_probabilities <- getGoProbabilities(decisions_list)

scaleRoundList(
  list = go_probabilities,
  scale_param = 1,
  round_digits = 3
)
