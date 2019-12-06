# Compile dimension reduction coordinates with clinical data

untar <- read.csv("../data/zscore_untargeted_annotatedions.csv")
train <- untar[untar$group <= 2,]
names(train)[1] <- "patients"

clin <- read.csv("../data/preprocessing_results.csv")
names(clin)[1] <- "patients"

pca_clust <- read.csv("../data/pca_clusters.csv")
names(pca_clust)[2:4] <- c("PCA_X", "PCA_Y", "PCA_cluster")
tsne_clust <- read.csv("../data/tsne_clusters.csv")
names(tsne_clust)[2:4] <- c("TSNE_X", "TSNE_Y", "TSNE_cluster")
som_clust <- read.csv("../data/som_clusters.csv")
names(som_clust)[2:4] <- c("SOM_X", "SOM_Y", "SOM_cluster")


summary <- merge(train[1:3], clin, by = "patients")
summary <- data.frame(summary[1:7], pca_clust[-1], tsne_clust[-1], som_clust[-1], summary[8:ncol(summary)])

write.csv(summary, "../data/clustering_summary.csv", row.names = FALSE)
