rm(list = ls())
setwd("C:/Users/xw455/OneDrive - Yale University/Desktop/rBMA-main/funs")
source("library.R")
library(kableExtra)
library(viridisLite)
library(viridis)
library(paletteer)
library(ggsci)
library(xml2)
library(gridExtra)
library(ggpubr)
library(cowplot)

# Draw plots for method comparison #######
# FWER FWP ######
# load result data 
setwd("C:/Users/xw455/OneDrive - Yale University/Desktop/rBMA-main/simulation")


load("bma_10000.RData")
bma_res <- sim_result
load("robust_10000.RData")
rob_res <- sim_result

rm(sim_result)

A <- bma_res$A
S <- nrow(bma_res$pmat)
n <- length(bma_res$p0)
B <- J <- 5  #number of indication
wt_name <- bma_res$wt_name

## save location #####
savefile <- "C:/Users/xw455/OneDrive - Yale University/Desktop/rBMA-main/plots"
# if folder does not exist, need to create it first before calling it. 

# FWER ####
bma_er <- bma_res$fwer
rob_er <- rob_res$fwer
df.er <- rbind(bma_er, rob_er)
df.er$name <- factor(df.er$name, levels = c(rob_res$wt_name))
df.er <- df.er[df.er$name %in% c("non.info.bench","lin.ALT"), ]
df.er <- df.er[!(df.er$name == "non.info.bench" & df.er$method == "robust"),] 
df.er$method[df.er$name == "non.info.bench"] <- "Reference"
df.er$method[df.er$method == "bma"] <- "BMA"
df.er$method[df.er$method == "robust"] <- "rBMA"
df.er$method <- factor(df.er$method, levels = c("Reference", "BMA", "rBMA"))
df.er$Scenario <- factor(df.er$Scenario, levels = 1:S, 
                         labels = paste0("S",0:(S-1)) )

## plot #####

p_er1 <- ggplot(data = df.er[df.er$Scenario %in% c("S0", paste0("S",1:5)),], 
                aes(x = Scenario, y = value, fill = method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_hline(yintercept = 0.1, color="gray", linetype="dashed") + 
  # annotate("text", x = 1.75, y = 0.95, label = "Hypothesis", size = 3) +
  # annotate("text", x = 1.95, y = 0.92, label = "Equal to truth", size = 3) +
  # ggtitle("FWER for Scenarios that Hypothesis Effectiveness is Equal to Truth")+ 
  scale_x_discrete(breaks=c("S0", paste0("S",1:5)),
                   label = paste0(0:J, " \n S", c(0:5))) +
  ylim(c(0,0.2)) + ylab("Family-Wise Type I Error Rate") + theme_classic() + 
  theme(legend.position = "none",
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(color = "white"))

# theme(axis.text.x=element_text(angle=45))

p_er2 <- ggplot(data = df.er[df.er$Scenario %in% c("S0", paste0("S",6:10)),], 
                aes(x = Scenario, y = value, fill = method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_hline(yintercept = 0.1, color="gray", linetype="dashed") + 
  # annotate("text", x = 1.75, y = 0.95, label = "Hypothesis", size = 3) +
  # annotate("text", x = 2.25, y = 0.92, label = "Lower than truth", size = 3) +
  # ggtitle("FWER for Scenarios that Hypothesis Effectiveness is Lower than Truth")+ 
  scale_x_discrete(breaks=c("S0", paste0("S",6:10)),
                   label = paste0(0:J, " \n S", c(0, 6:10))) +  
  xlab("Number of Positive Indication \n Scenario") + 
  ylim(c(0,0.2)) + ylab("Family-Wise Type I Error Rate") + theme_classic()  + 
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(vjust = -0.75))

# theme(axis.text.x=element_text(angle=45))
p_er3 <- ggplot(data = df.er[df.er$Scenario %in% c("S0", paste0("S",11:15)),], 
                aes(x = Scenario, y = value, fill = method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_hline(yintercept = 0.1, color="gray", linetype="dashed") + 
  # annotate("text", x = 1.75, y = 0.95, label = "Hypothesis", size = 3) +
  # annotate("text", x = 2.25, y = 0.92, label = "Higher than truth", size = 3) +
  scale_x_discrete(breaks=c("S0", paste0("S",11:15)),
                   label = paste0(0:J, " \n S", c(0, 11:15))) + 
  ylim(c(0,0.2)) + ylab("Family-Wise Type I Error Rate") + theme_classic()  + 
  theme(legend.title=element_blank(),
        legend.position = c(0.75, 0.8),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = "white",vjust = -0.75))

# p_er <- ggplot(data = df.er, aes(x = Scenario, y = value, fill = method)) + 
#   geom_bar(position = "dodge", stat = "identity") + 
#   geom_hline(yintercept = 0.1) + 
#   ggtitle("FWER Over All Scenarios")+ ylim(c(0,1)) + 
#   ylab("Family-Wise Type I error") + theme_classic() + 
#   theme(axis.text.x=element_text(angle=45))

# setEPS()
# postscript(file = paste0(savefile,  "/fwer-",A,".eps"))
print(plot_grid(p_er1, p_er2, p_er3, align = "h", nrow = 1, rel_widths  = c(0.38, 0.31, 0.31)))
# dev.off()

# INDICATION - WISE plots:   #############
bma_ind <- bma_res$ind_er
rob_ind <- rob_res$ind_er
ind_df <- rbind(bma_ind, rob_ind)
ind_df$er <- round(ind_df$er, 3)
ind_df$pr_wt <- factor(ind_df$pr_wt, levels = c(rob_res$wt_name))
ind_df <- ind_df[ind_df$pr_wt %in% c("non.info.bench","lin.ALT"), ]
ind_df <- ind_df[!(ind_df$pr_wt == "non.info.bench" & ind_df$method == "robust"),]
ind_df$method[ind_df$pr_wt == "non.info.bench"] <- "Reference"
ind_df$method <- factor(ind_df$method, levels = c("Reference", "bma", "robust"), 
                        labels = c("Reference", "BMA", "rBMA"))
ind_df$Scenario <- factor(ind_df$Scenario, levels = 1:S, 
                          labels = c("Global Null", 
                                     paste0(rep(1:J,3), " Positive", " - ", rep(c("Equal", "Lower", "Higher"),each = J))))
names(ind_df)[4] <- "Method"
# change indication name
ind_df$ind[ind_df$ind == "A"] <- "#1"
ind_df$ind[ind_df$ind == "B"] <- "#2"
ind_df$ind[ind_df$ind == "C"] <- "#3"
ind_df$ind[ind_df$ind == "D"] <- "#4"
ind_df$ind[ind_df$ind == "E"] <- "#5"

# end of data management

g_ind_equal <- ggplot(data = ind_df[ind_df$Scenario %in% c("Global Null", paste0(1:J, " Positive", " - ", rep("Equal",J))),], 
                     aes(x = ind, y = er, fill = Method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  # two Reference line of power = 0.8 and type1er 0.025
  # geom_hline(yintercept = 0.8) + 
  # geom_hline(yintercept = 0.025) +
  # ggtitle("Indication-wise Rejection Rate")+ 
  ylim(c(0,1)) +
  xlab("Indication") + ylab("Probability of Rejecting Null Hypothesis") + 
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.title=element_blank(),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -0.75),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g_ind_lower <- ggplot(data = ind_df[ind_df$Scenario %in% c("Global Null", paste0(1:J, " Positive", " - ", rep("Lower",J))),], 
                     aes(x = ind, y = er, fill = Method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  # two Reference line of power = 0.8 and type1er 0.025
  # geom_hline(yintercept = 0.8) + 
  # geom_hline(yintercept = 0.025) +
  # ggtitle("Indication-wise Rejection Rate")+ 
  ylim(c(0,1)) +
  xlab("Indication") + ylab("Probability of Rejecting Null Hypothesis") + 
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.title=element_blank(),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -0.75),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g_ind_high <- ggplot(data = ind_df[ind_df$Scenario %in% c("Global Null", paste0(1:J, " Positive", " - ", rep("Higher",J))),], 
                aes(x = ind, y = er, fill = Method)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  # two Reference line of power = 0.8 and type1er 0.025
  # geom_hline(yintercept = 0.8) + 
  # geom_hline(yintercept = 0.025) +
  # ggtitle("Indication-wise Rejection Rate")+ 
  ylim(c(0,1)) +
  xlab("Indication") + ylab("Probability of Rejecting Null Hypothesis") + 
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.title=element_blank(),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -0.75),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

scenario_equal_label <- c("Global Null" = "S0", 
                         "1 Positive - Equal" = "S1", 
                         "2 Positive - Equal" = "S2", 
                         "3 Positive - Equal" = "S3", 
                         "4 Positive - Equal" = "S4", 
                         "5 Positive - Equal" = "S5")

scenario_lower_label <- c("Global Null" = "S0", 
                         "1 Positive - Lower" = "S6", 
                         "2 Positive - Lower" = "S7", 
                         "3 Positive - Lower" = "S8", 
                         "4 Positive - Lower" = "S9", 
                         "5 Positive - Lower" = "S10")

scenario_high_label <- c("Global Null" = "S0", 
                    "1 Positive - Higher" = "S11", 
                    "2 Positive - Higher" = "S12", 
                    "3 Positive - Higher" = "S13", 
                    "4 Positive - Higher" = "S14", 
                    "5 Positive - Higher" = "S15")


# print
# setEPS()
# postscript(paste0(savefile, "/ind-eq-",A,".eps"))
print(g_ind_equal +
        facet_wrap(~ Scenario,ncol = 3, nrow = 2, labeller = labeller(Scenario = scenario_equal_label)))+
  theme(panel.spacing.y = unit(0.4, "in"))
# dev.off()

# setEPS()
# postscript(paste0(savefile, "/ind-e-",A,".eps"))
print(g_ind_lower +
        facet_wrap(~ Scenario,ncol = 3, nrow = 2, labeller = labeller(Scenario = scenario_lower_label))) +
  theme(panel.spacing.y = unit(0.4, "in"))
# dev.off()

# setEPS()
# postscript(paste0(savefile, "/ind-p-",A,".eps"))
print(g_ind_high +
        facet_wrap(~ Scenario,ncol = 3, nrow = 2, labeller = labeller(Scenario = scenario_high_label)))+
  theme(panel.spacing.y = unit(0.4, "in"))
# dev.off()

# SENSITIVITY ######

# sen - FWER ####
load("sen-robust_10000.RData")
rob_sen_res <- sim_result
df.er_s <- rob_sen_res$fwer
df.er_s$name <- factor(df.er_s$name, levels = c("non.info.bench", "lin.ALT", "qdr.ALT", "nega.ALT.1", "nega.ALT.2"), 
                       labels = c("Reference", "gamma = 1", "gamma = 2", "gamma = -1", "gamma = -2"))
df.er_s$Scenario <- factor(df.er_s$Scenario, levels = 1:S, 
                           labels = c("Global Null", 
                                      paste0(rep(1:5,3), " Positive", " - ", rep(c("Equal", "Lower", "Higher"),each = 5))) )

# sensitivty - INDICATION - WISE plots:   #############
rob_sen_ind <- rob_sen_res$ind_er
ind_df_s <- rob_sen_ind
ind_df_s$er <- round(as.numeric(ind_df_s$er),3)
ind_df_s$Scenario <- factor(ind_df_s$Scenario, levels = 1:S)
ind_df_s$pr_wt <- factor(ind_df_s$pr_wt, levels = c(rob_res$wt_name))
ind_df_s$pr_wt <- factor(ind_df_s$pr_wt, levels = c("non.info.bench", "lin.ALT", "qdr.ALT","nega.ALT.1","nega.ALT.2"),
                         labels = c("Reference", "gamma = 1", "gamma = 2","gamma = -1","gamma = -2"))
ind_df_s$Scenario <- factor(ind_df_s$Scenario, levels = 1:S, 
                            labels = c("Global Null", 
                                       paste0(rep(1:5,3), " Positive", " - ", rep(c("Equal", "Lower", "Higher"),each = 5))))
# end of data management

# merge table of FWER & indication-wise result in one table: 
names(df.er_s) <- c("Scenario", "pr_wt", "er", "method")
df.er_s$ind <- "FWER"
sens_tbl <- bind_rows(df.er_s, ind_df_s)


## tables #######
ind_df_tbl <-
  sens_tbl[, colnames(sens_tbl) != ("method")] %>%
  pivot_wider(names_from  = ind, 
              values_from = er) %>%
  arrange(Scenario)
ind_df_tbl <- data.frame(ind_df_tbl) %>% arrange(Scenario)
names(ind_df_tbl)[2] <- "Weight"

pmat <- rob_res$pmat[1:6,]
p0 <- rob_res$p0
p1 <- rob_res$p1

# make a truth table
truth <- data.frame(t(apply(pmat, 1, function(truth){
  param = NULL
  param[truth == p0] <- 0
  param[truth == p1] <- 1
  return(param)
})))
truth <- truth[rep(seq_len(nrow(truth)), each = J), ]
truth <- rbind(0,truth)
truth <- data.frame(truth)

# ind_df_tbl[,3:7] <- cell_spec(ind_df_tbl[,3:7], background = "yellow")
ind_df_tbl_eq <- ind_df_tbl[ind_df_tbl$Scenario %in% c("Global Null", "1 Positive - Equal",
                                                     "2 Positive - Equal", "3 Positive - Equal",
                                                     "4 Positive - Equal","5 Positive - Equal"),] 
ind_df_tbl_eq$Scenario <- factor(ind_df_tbl_eq$Scenario, 
                                 levels = c("Global Null", "1 Positive - Equal",
                                            "2 Positive - Equal","3 Positive - Equal",
                                            "4 Positive - Equal","5 Positive - Equal"),
                                 labels = c("Global Null", "1 Positive",
                                            "2 Positive", "3 Positive",
                                            "4 Positive", "5 Positive"))
ind_df_tbl_eq <- rbind(c(NA,NA,NA,
                         "(0.25, 0.5)",
                         "(0.25, 0.5)",
                         "(0.25, 0.5)", 
                         "(0.25, 0.5)", 
                         "(0.25, 0.5)"), ind_df_tbl_eq)
### a tabel with 1 color ####
sen_eq <- 
  ind_df_tbl_eq %>%
  kbl(format = "latex",booktabs = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(4, background = ifelse(truth[,1] > 0, "lightgray", "white")) %>%
  column_spec(5, background = ifelse(truth[,2] > 0, "lightgray", "white")) %>%
  column_spec(6, background = ifelse(truth[,3] > 0, "lightgray", "white")) %>%
  column_spec(7, background = ifelse(truth[,4] > 0, "lightgray", "white")) %>%
  column_spec(8, background = ifelse(truth[,5] > 0, "lightgray", "white")) 

#### less effective than expected
ind_df_tbl_le <- ind_df_tbl[ind_df_tbl$Scenario %in% c("Global Null", "1 Positive - Lower",
                                                       "2 Positive - Lower", "3 Positive - Lower",
                                                       "4 Positive - Lower","5 Positive - Lower"),] %>%
  arrange(Scenario)
ind_df_tbl_le$Scenario <- factor(ind_df_tbl_le$Scenario, 
                                 levels = c("Global Null", "1 Positive - Lower",
                                            "2 Positive - Lower","3 Positive - Lower",
                                            "4 Positive - Lower","5 Positive - Lower"),
                                 labels = c("Global Null", "1 Positive",
                                            "2 Positive", "3 Positive",
                                            "4 Positive", "5 Positive"))
ind_df_tbl_le <- rbind(c(NA,NA,NA,
                         "(0.25, 0.325)",
                         "(0.25, 0.325)",
                         "(0.25, 0.325)", 
                         "(0.25, 0.325)", 
                         "(0.25, 0.325)"), ind_df_tbl_le)

sen_le <- 
  ind_df_tbl_le %>%
  kbl(format = "latex", booktabs = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(4, background = ifelse(truth[,1] > 0, "lightgray", "white")) %>%
  column_spec(5, background = ifelse(truth[,2] > 0, "lightgray", "white")) %>%
  column_spec(6, background = ifelse(truth[,3] > 0, "lightgray", "white")) %>%
  column_spec(7, background = ifelse(truth[,4] > 0, "lightgray", "white")) %>%
  column_spec(8, background = ifelse(truth[,5] > 0, "lightgray", "white")) 


#### more effective than expected 

ind_df_tbl_ge <- ind_df_tbl[ind_df_tbl$Scenario %in% c("Global Null", "1 Positive - Higher",
                                                       "2 Positive - Higher", "3 Positive - Higher",
                                                       "4 Positive - Higher","5 Positive - Higher"),] %>%
  arrange(Scenario)
ind_df_tbl_ge$Scenario <- factor(ind_df_tbl_ge$Scenario, 
                                 levels = c("Global Null", "1 Positive - Higher",
                                            "2 Positive - Higher","3 Positive - Higher",
                                            "4 Positive - Higher","5 Positive - Higher"),
                                 labels = c("Global Null", "1 Positive",
                                            "2 Positive", "3 Positive",
                                            "4 Positive", "5 Positive"))
ind_df_tbl_ge <- rbind(c(NA,NA,NA,
                         "(0.25, 0.7)",
                         "(0.25, 0.7)",
                         "(0.25, 0.7)", 
                         "(0.25, 0.7)", 
                         "(0.25, 0.7)"), ind_df_tbl_ge)
sen_ge <- 
  ind_df_tbl_ge %>%
  kbl(format = "latex", booktabs = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(4, background = ifelse(truth[,1] > 0, "lightgray", "white")) %>%
  column_spec(5, background = ifelse(truth[,2] > 0, "lightgray", "white")) %>%
  column_spec(6, background = ifelse(truth[,3] > 0, "lightgray", "white")) %>%
  column_spec(7, background = ifelse(truth[,4] > 0, "lightgray", "white")) %>%
  column_spec(8, background = ifelse(truth[,5] > 0, "lightgray", "white")) 

# print tables in latex
sen_eq
sen_le
sen_ge

# WEIGHT: ###########
## plots:####  
# for robust
mods <- rob_res$mods
prior_mat <- rob_res$prior_mat
n_posi <- rowSums(mods==1)
w_map <- data.frame(cbind(n_posi, prior_mat))
w_map$n_posi <- factor(w_map$n_posi)
# make it long format
w_map <- pivot_longer(w_map, cols = !n_posi)
w_map <- w_map[w_map$name != "non.info.bench",]
w_map$name <- factor(w_map$name, levels = wt_name[-1], 
                     labels = c("1","2","-1","-2"))
w_map$method = "rBMA"
w_map_rob <- w_map

w_map_sum_rob <- w_map_rob %>%
  group_by(n_posi,name) %>%
  summarise(weight = sum(value))%>%
  mutate(method = "rBMA")
w_map_rob <- unique(w_map_rob)

# for BMA: 

mods <- bma_res$mods
prior_mat <- bma_res$prior_mat
n_posi <- rowSums(mods==1)
w_map <- data.frame(cbind(n_posi, prior_mat))
w_map$n_posi <- factor(w_map$n_posi)
# make it long format
w_map <- pivot_longer(w_map, cols = !n_posi)
w_map <- w_map[w_map$name == wt_name[2],]
w_map$name <- factor(w_map$name, levels = wt_name[2], 
                     labels = "1")
w_map$method = "BMA"
w_map_bma <- w_map

w_map_sum_bma <- w_map_bma %>%
  group_by(n_posi,name) %>%
  summarise(weight = sum(value))%>%
  mutate(method = "BMA")
w_map_bma <- unique(w_map_bma)

# combine
w_map <- rbind(w_map_rob, w_map_bma)
w_map_sum <- rbind(w_map_sum_rob,w_map_sum_bma)

# plot
weight_sum <- ggplot(data = w_map_sum_rob, aes(x = n_posi)) + 
  geom_point(aes(y = weight, color = name), size = 2) + 
  geom_line(aes(x = as.numeric(w_map_sum_rob$n_posi), y = weight, color = name)) + 
  # ggtitle("Weight for each kind of Model") + 
  ylim(c(0,1)) + 
  xlab("Number of Enthusiastic Components") + 
  ylab("Prior Weight") +
  labs(color = expression(gamma)) + 
  theme_classic() + 
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -0.75))

weight_single <- ggplot(data = w_map_rob, aes(x = n_posi,y = value, fill = name)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  # geom_line(aes(x = as.numeric(n_posi), y = value, color = name)) + 
  # ggtitle("Weight for each single Model") + 
  ylim(c(0,1)) + 
  xlab("Number of Enthusiastic Components") + 
  ylab("Prior Weight") +
  labs(fill = expression(gamma)) +
  theme_classic() + 
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -0.75))

# setEPS()
# postscript(file = paste0(savefile,  "/weight.eps"))
print(grid.arrange(weight_sum, weight_single, nrow= 1))
# dev.off()

# rm(n_posi, w_map, w_map_sum)
# 
## heatmap #####
library(ggcorrplot)
# heatmap with gamma = -2, 0, 1, 2

# heatmap for rBMA
wt_name_ht <- c("nega.ALT.2", "EQL","lin.ALT","qdr.ALT")
n_wt <- length(wt_name_ht)
mods <- rob_res$mods
bma_method <- "robust"

prior_mat <- data.frame(do.call(cbind, lapply(wt_name_ht, function(setting){
  pr_weight(mods, setting = setting, bma_method)$prior
})))
####
names(prior_mat) <- wt_name_ht
# print("check weight sum")
# print(apply(prior_mat, 2,sum))

# probability of get same configuration of hypothesis
simmat_rob <- lapply(1:n_wt, function(m){
  prob_corr(B, prior_mat[,m], mods)
})
names(simmat_rob) <- wt_name_ht
simmat_rob <- lapply(simmat_rob, function(mat){
  colnames(mat) <-  paste0("#", 1:ncol(mat))
  rownames(mat) <-  paste0("#", 1:nrow(mat))
  return(mat)
})

# heatmap for BMA
# wt_name_ht <- c("nega.ALT.2", "EQL","lin.ALT","qdr.ALT")
n_wt <- length(wt_name_ht)
mods <- bma_res$mods
bma_method <- "bma"

prior_mat <- data.frame(do.call(cbind, lapply(wt_name_ht, function(setting){
  pr_weight(mods, setting = setting, bma_method)$prior
})))
####
names(prior_mat) <- wt_name_ht
# print("check weight sum")
# print(apply(prior_mat, 2,sum))

# probability of get same configuration of hypothesis
 simmat_bma <- lapply(1:n_wt, function(m){
  prob_corr(B, prior_mat[,m], mods)
})
names(simmat_bma) <- wt_name_ht
simmat_bma <- lapply(simmat_bma, function(mat){
                colnames(mat) <- paste0("#", 1:ncol(mat))
                rownames(mat) <- paste0("#", 1:nrow(mat))
                return(mat)
              })

plotkit <-  list(arrangeGrob(grobs = lapply(1:n_wt, function(x){
  ggcorrplot(simmat_bma[[x]], hc.order = FALSE, 
             type = "lower", lab = TRUE, show.legend = FALSE) 
}), ncol = 2, nrow = 2), # end of first plot 
arrangeGrob(grobs = lapply(1:n_wt, function(x){
  ggcorrplot(simmat_rob[[x]], hc.order = FALSE, 
             type = "lower", lab = TRUE, show.legend = FALSE) 
}), ncol = 2, nrow = 2)) # end of second plot 

# print plots
# setEPS()
# postscript(file = paste0(savefile,  "/prob-bma.eps"))
grid.arrange(grobs = lapply(1:n_wt, function(x){
  ggcorrplot(simmat_bma[[x]], hc.order = FALSE, 
             type = "lower", lab = TRUE, show.legend = FALSE) 
})
, ncol = 2, nrow = 2
)
# dev.off()

# setEPS()
# postscript(file = paste0(savefile,  "/prob-robust.eps"))
grid.arrange(grobs = lapply(1:n_wt, function(x){
  ggcorrplot(simmat_rob[[x]], hc.order = FALSE, 
             type = "lower", lab = TRUE, show.legend = FALSE) 
})
, ncol = 2, nrow = 2
)
# dev.off()

# plot of priors #####
par(mfrow=c(2, 3), oma=c(2,3,0.1,1), mar=c(2,2,2,2))
par0 <- rob_res$par0
par1 <- rob_res$par1
taxis <- seq(0, 1, 0.001)

for(b in 1:B){
  tau <- (p1[b]-p0[b])/2
  par0[b, ] <- design.prior.bin(theta = p0[b], pt = c(0.05, 0.5, 0.95),
                                lci = max(p0[b]-tau, 0), uci = p0[b]+tau)
  par1[b, ] <- design.prior.bin(theta = p1[b], pt = c(0.05, 0.5, 0.95),
                                lci = p1[b]-tau, uci = min(p1[b]+tau, 1))

  den0 <- dbeta(taxis, par0[b, 1], par0[b, 2])
  den1 <- dbeta(taxis, par1[b, 1], par1[b, 2])
  
  plot(taxis, den0, type = "l", las = 1, lwd = 3, xlab = "", ylab = "", 
       main = "", ylim = c(0, max(den0)+1), cex.axis = 1.5)
  lines(taxis, den1, col = 2, lwd = 3)
  
  if(b==1){
    title(xlab = "", ylab = "Prior Density", cex.lab= 1.5, 
          outer = T, line = 1)
    legend("topright", lty = 1, lwd = 2, col = 1:2, 
           c("Pessimistic", "Enthusiastic"), bty = "n", cex = 1.5)
  }
  if(b==4){
    title(xlab = "Rate of desired outcome", ylab = "", cex.lab= 1.5, 
          outer = T, line = 0.6)
  }
}
