
# needs Rtsne and ggplot2
#' test several perplexity options for tSNE and plot
#' @param perplexities list of perplexities to test
#' @param data data to perform tsne on

find_optimal_p_tSNE <- function(perplexities, data) {
  for (p in perplexities) {
    tsne_out <- Rtsne(as.matrix(train), perplexity = p)
    plot(tsne_out$Y)
  }
}

#---------------------------------------------------------

# needs Rtsne
#' run tSNE algorithm and return data frame of (x,y) coordinates
#' @param data data to perform tsne on
#' @param optimal_p perplexity

run_tSNE <- function(data, optimal_p) {
  all_tsne_out <- Rtsne(as.matrix(data), perplexity = optimal_p)
  tsne_coords <- as.data.frame(all_tsne_out$Y)
  return(tsne_coords)
}

#---------------------------------------------------------------------

# PCA
#' @param data data to run PCA on
# returns matrix of (x,y) coordinates

run_PCA <- function(data) {
  res <- prcomp(data)$x
  return(res[,c(1,2)])
}

#----------------------------------------------------------------







