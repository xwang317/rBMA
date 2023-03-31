# rBMA
scripts for 'A robust Bayesian model averaging method for basket trials with different endpoints'

# Abstract:   
Since it could be difficult for some specific rare oncology diseases to recruit subjects in clinical trial, oncology research is increasingly focused on precision medicine cancer treatment which targeted at identified genetic changes.  In such a trial, people receive treatment based on their gene mutation.  
As basket trials are becoming increasingly important nowadays, here we propose a robust Bayesian model averaging (rBMA) method for basket trial for multiple endpoints and multiple indications. Information borrowing is performed based on the possibility that any subsets of baskets may be effective, which will be updated after data observed. Mixture prior of each indication is consists of conjugate enthusiastic and pessimistic priors stand for the truth in our null and alternative hypotheses, in addition to a non-informative prior which stands for neither. We make inference based on our mixture posterior distribution combined with posterior possibility of subsets of active indications. This method provide a robust framework of multiple endpoints based on different data type. Simulations show that our method has promising operating characteristics compared with previous methods.
  
# subfolders  
-- funs: scripts to reproduce simulation and sensetivity analysis in the article  
-- plots: scripts to generate figures appears in the article and figures  
-- simulation: folder to save results of simulation and sensitivity analysis  
  
# Code Overview:   
  
## funs: 
-- \_result.R: main scripts for setup and saving results  
-- \_do_by_method.R: scripts of running simulations  
-- library.R: scripts to call R packages  
-- fun\_*.R: help functions used to run simulation and sensitivity analysis   

