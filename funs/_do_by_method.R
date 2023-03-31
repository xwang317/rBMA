# do simulation by different method ######
# after CONFIG is setting

# generate membership matrix #####
mods <- init.mod(B, bma_method, ind_name)
# print(mods)

# P/E Prior setting ######
par0 <- par1 <- matrix(NA, B, 2)
for(b in 1:B){
  tau <- (p1[b]-p0[b])/2
  par0[b, ] <- design.prior.bin(theta = p0[b], pt = c(0.05, 0.5, 0.95),
                                lci = max(p0[b]-tau, 0), uci = p0[b]+tau)
  par1[b, ] <- design.prior.bin(theta = p1[b], pt = c(0.05, 0.5, 0.95),
                                lci = p1[b]-tau, uci = min(p1[b]+tau, 1))
}



# prior weight ######

## calculate prior weight 
prior_mat <- data.frame(do.call(cbind, lapply(wt_name, function(setting){
  pr_weight(mods, setting = setting, bma_method)$prior
})))
####
colnames(prior_mat) <- wt_name
print("check weight sum")
print(apply(prior_mat, 2,sum)) # check if weights has sum of 1


# probability of same component of prior #####
simmat <- lapply(1:n_wt, function(m){
  prob_corr(B, prior_mat[,m], mods)
})
names(simmat) <- wt_name
print(simmat)

# Run MC simulation by weight configuration:  #####

result <- wt_mc(p0, pmat, mods, nvec, 
                   bma_method = bma_method, 
                   par0, par1, 
                   tag.fwer = tag.fwer, 
                   prior_mat,  A = A)

# output dataframe
power <- data.frame(result$power)%>%
  mutate(Scenario = factor(rownames(.))) %>%
  pivot_longer(cols = !Scenario) %>%
  mutate(method = paste0(bma_method))

fwer <- data.frame(result$fwer)%>%
  mutate(Scenario = factor(rownames(.))) %>%
  pivot_longer(cols = !Scenario) %>%
  mutate(method = paste0(bma_method))

power$Scenario <- factor(power$Scenario, levels = 1:S)
fwer$Scenario <- factor(fwer$Scenario, levels = 1:S)

ind_er_df <- data.frame()
wt_name_temp <- c(wt_name)
for (m in 1:(n_wt)) {
  for (b in 1:B) {
    for (s in 1:S) {
      ind_er_df <- rbind(ind_er_df,
                         c(wt_name_temp[m], LETTERS[b], s, bma_method, 
                           result$ind_er[wt_name_temp[m],s,b]))
    }
  }
}
colnames(ind_er_df) <- c("pr_wt","ind","Scenario","method","er")
ind_er_df$er <- as.numeric(ind_er_df$er)
ind_er_df$Scenario <- factor(ind_er_df$Scenario, levels = 1:S)
ind_er_df$pr_wt <- factor(ind_er_df$pr_wt, levels = c(wt_name))


# automatically show power/fwer table
print("fw-power")
print(result$power)
print("fwer")
print(result$fwer)
#  also show indicatino-wise er: 
print("ind-er/power")
print(ind_er_df)
