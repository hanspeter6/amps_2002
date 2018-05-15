# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set02_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min.rds")

# LEVEL 2

# Subsetting only on the variable I intend to use in this section:
set02_min <- set02_min[,-c(1:2,8:12,14:19)]

# ## Determine Number of Factors to Extract
# ev <- eigen(cor(set02_min[,7:ncol(set02_min)]))
# ap <- parallel(subject=nrow(set02_min[,7:ncol(set02_min)]),var=ncol(set02_min[,7:ncol(set02_min)]),
#                rep=100,cent=.02)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# jpeg("nScree_02_min")
# plotnScree(nS, main = "National") # optimal = 6
# dev.off()
# 
# # will set them at six for both Jhb and CT for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_02_min <- PCA(set02_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# saveRDS(pca_02_min, "pca_02_min.rds")

# pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_02 <- fa(set02_min[7:ncol(set02_min)], nfactors = 6, fm = "pa") # default rotation oblimin, so does allow correlation between factors
fact_02_loadings <- fact_02$loadings
fact_02_scores <- fact_02$scores

# save model
saveRDS(fact_02, "fact_02.rds")

# save loadings:
saveRDS(fact_02_loadings, "fact_02_loadings.rds")

# save scores:
saveRDS(fact_02_scores, "fact_02_scores.rds")

