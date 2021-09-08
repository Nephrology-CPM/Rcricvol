

# after clustering patients, combine cluster information and coordinates with patient clinical data and outcomes 

untar <- read.csv("../data/zscore_untargeted_annotatedions.csv")
train <- untar[untar$group <= 2,]
names(train)[1] <- "patients"

clin <- read.csv("../data/preprocessing_results.csv")
names(clin)[1] <- "patients"

pca_clust <- read.csv("../data/pca_train_clusters.csv")
names(pca_clust)[2:4] <- c("PCA_X", "PCA_Y", "PCA_cluster")
tsne_clust <- read.csv("../data/tsne_train_clusters.csv")
names(tsne_clust)[2:4] <- c("TSNE_X", "TSNE_Y", "TSNE_cluster")
som_clust <- read.csv("../data/som_train_clusters.csv")
names(som_clust)[2:4] <- c("SOM_X", "SOM_Y", "SOM_cluster")


summary <- merge(train[1:3], clin, by = "patients")
summary <- data.frame(summary[1:7], pca_clust[-1], tsne_clust[-1], som_clust[-1], summary[8:ncol(summary)])

write.csv(summary, "../data/clustering_train_summary.csv", row.names = FALSE)





val <- untar[untar$group == 3,]
names(val)[1] <- "patients"

pca_val_clust <- read.csv("../data/pca_val_clusters.csv")
names(pca_val_clust)[2:4] <- c("PCA_X", "PCA_Y", "PCA_cluster")
tsne_val_clust <- read.csv("../data/pca_val_clusters.csv")
names(tsne_val_clust)[2:4] <- c("TSNE_X", "TSNE_Y", "TSNE_cluster")
# we dont have validation data for som yet

val_summary <- merge(val[1:3], clin, by = "patients")
val_summary <- data.frame(val_summary[1:7], pca_val_clust[-1], tsne_val_clust[-1], val_summary[8:ncol(val_summary)])
write.csv(val_summary, "../data/clustering_val_summary.csv", row.names = FALSE)
