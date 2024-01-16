# rBMA
scripts for 'rBMA: A robust Bayesian Model Averaging Method for phase II basket trials based on informative mixture priors'

# Abstract:   
Oncology drug research in the last few decades has been driven by the development of targeted agents. In the era of targeted therapies, basket trials are often used to test the anti-tumor activity of a novel treatment in multiple indications sharing the same genomic alteration. As patient population are further fragmented into biomarker-defined subgroups in basket trials, novel statistical methods are needed to facilitate cross-indication learning to improve the statistical power in basket trial design. Here we propose a robust Bayesian model averaging (rBMA) technique for the design and analysis of phase II basket trials. We consider the posterior distribution of each indication (basket) as the weighted average of three different models which only differ in their priors (enthusiastic, pessimistic and non-informative). The posterior weights of these models are determined based on the effect of the experimental treatment in all the indications tested. In early phase oncology trials, different binary endpoints might be chosen for different indications (objective response, disease control or PFS at landmark times), which makes it even more challenging to borrow information across indications. Compared to previous approaches, the proposed method has the flexibility to support cross-indication learning in the presence of mixed endpoints. We evaluate and compare the performance of the proposed rBMA approach to competing approaches in simulation studies. R scripts to implement the proposed method are available at https://github.com/xwang317/rBMA.

# subfolders  
-- funs: scripts to reproduce simulation and sensetivity analysis in the article  
-- plots: scripts to generate figures appears in the article with the same sample size of 20  
-- plots_s: scripts to generate figures appears in the Supplementary with the different sample size 
-- simulation_data: folder to save results of simulation and sensitivity analysis  
  
# Code Overview:   
  
## funs: 
-- \_result.R: main scripts for setup and saving results  
-- \_do_by_method.R: scripts of running simulations  
-- library.R: scripts to call R packages  
-- fun\_*.R: help functions used to run simulation and sensitivity analysis   

