# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)

#  read in dataset
set02 <- readRDS("set02.rds")
set02_simple <- readRDS("set02_simple.rds")

# consider some correlations
png('corTypePlot2002.png')
corrplot(cor(set02[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set02[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2002.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(12)
kmeans02 <- kmeans(set02[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)
set.seed(12)
kmeans02_simple <- kmeans(set02_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)


# Comparing 2002 with 2005... will change colours if necessary to reflect meaning based on 2012:

# red becomes green:  1 becomes 2
# green becomes lilac: 2 becomes 4
# blue stays blue:   3 stays 3
# lilac becomes red: 4 becomes 1
kmeans02$cluster <- ifelse(kmeans02$cluster == 1, 7, kmeans02$cluster)
kmeans02$cluster <- ifelse(kmeans02$cluster == 2, 9, kmeans02$cluster)
kmeans02$cluster <- ifelse(kmeans02$cluster == 3, 8, kmeans02$cluster)
kmeans02$cluster <- ifelse(kmeans02$cluster == 4, 6, kmeans02$cluster)
kmeans02$cluster <- kmeans02$cluster - 5


# add cluster labels to the dataset
set02c <- set02 %>%
        mutate(cluster = factor(kmeans02$cluster))
set02c_simple <- set02_simple %>%
        mutate(cluster = factor(kmeans02_simple$cluster))

saveRDS(set02c, "set02c.rds")
saveRDS(set02c_simple, "set02c_simple.rds")


# some plots
# boxplots of clusters and media types
p1 <- ggplot(set02c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set02c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set02c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set02c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set02c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set02c, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_02.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5, p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set02c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set02c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set02c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set02c, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_02.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set02c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set02c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set02c, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
# d8 <- ggplot(set02c, aes(lifestyle, cluster, fill = cluster)) +
#         geom_col() +
#         labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_02.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, ncol=2, nrow = 2)
dev.off()





# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub02 <- set02[sample(nrow(set02), size = 0200),]

# distance matrix and MDS
sub02_dist <- dist(sub02[,c("newspapers","magazines","radio", "tv", "internet")])
mds02 <- cmdscale(sub02_dist)
plot(mds02, col = as.numeric(sub02$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub02[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2002.jpeg')
plot(mds02, col = as.numeric(sub02$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2002.jpeg')
scatterplot3d(mds3, color = as.numeric(sub02$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2002.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub02$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 02, ydim = 02, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub02[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 02000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub02['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub02['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub02['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub02['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub02['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub02$cluster ) # not very good organising??



# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set02$cluster, p = 0.7, list = FALSE)
training <- set02[ind_train,]
testing <- set02[-ind_train,]

# # using random forest:
forest02_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest02_type <- predict(forest02_type, newdata = testing)

confusionMatrix(pred_forest02_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda02 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda02)

pred_lda02 <- predict(lda02, newdata = testing)
confusionMatrix(pred_lda02$class, testing$cluster) # 

# using only demographic information
forest02_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest02_demogr <- predict(forest02_demogr, newdata = testing)

confusionMatrix(pred_forest02_demogr, testing$cluster)

# with lda
set.seed(56)
lda02_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda02_demogr <- predict(lda02_demogr, newdata = testing)
confusionMatrix(pred_lda02_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree02 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set02,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree02, uniform = TRUE, margin = 0.2)
text(tree02, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree02, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set02$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_02.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set02$radio ~ set02$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set02$tv ~ set02$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set02$newspapers ~ set02$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set02$magazines ~ set02$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set02$internet ~ set02$cluster, col = c(2,3,4,6), main = "internet", xlab = "cluster", ylab = '')
plot(set02$all ~ set02$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_02.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set02$cluster ~ factor(set02$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-02")), col = c(2,3,4,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_02.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set02$cluster ~ factor(set02$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=02000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
plot(set02$cluster ~ set02$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "")
dev.off()
