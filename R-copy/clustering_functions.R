

#' plot clusters
#' @param data data frame of (x,y) coordinates
#' @param var_cluster vector of cluster labels
#' @param point_size size of points (default 10)

plot_cluster=function(data, var_cluster, point_size = 10) {
  names(data)[1:2] <- c("V1", "V2")
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=point_size) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.box = "horizontal")
}

#------------------------------------------------------------------

#' cluster data using hierarchical clustering
#' @param coords data frame of (x,y coordinates)
#' @param h height of tree cuts
cluster_data <- function(coords, h) {
  names(coords) = c("V1", "V2")
  fit_cluster_hierarchical=hclust(dist(scale(coords)))
  plot(fit_cluster_hierarchical)
  coords$clusters = factor(cutree(fit_cluster_hierarchical, h=h))
  return(coords)
}

#------------------------------------------------------------------

#' Compile dimension reduction coordinates with clinical data (result = summary)
#' @param untar preprocessed untargeted data
#' @param clin preprocessed clinical data
#' @param pca_clust pca clusters (x, y, clust)
#' @param tnse_clust tsne clusters (x, y, clust)
#' @param som_clust som clusters (x, y, clust)

summarize_dr_clustering <- function(untar, clin, pca_clust, tsne_clust, som_clust) {
  train <- untar[untar$group <= 2,]
  names(train)[1] <- "patients"
  names(clin)[1] <- "patients"
  names(pca_clust)[2:4] <- c("PCA_X", "PCA_Y", "PCA_cluster")
  names(tsne_clust)[2:4] <- c("TSNE_X", "TSNE_Y", "TSNE_cluster")
  names(som_clust)[2:4] <- c("SOM_X", "SOM_Y", "SOM_cluster")
  summary <- merge(train[1:3], clin, by = "patients")
  summary <- data.frame(summary[1:7], pca_clust[-1], tsne_clust[-1], som_clust[-1], summary[8:ncol(summary)])
  return(summary)
}

#------------------------------------------------------------------------------------------------------





