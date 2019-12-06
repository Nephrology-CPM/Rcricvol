
#' calculate enrichment score
#' @param data ordered dataframe where data$x_array consists of weights and data$in_path is vector of 1 or 0 depending on if ion is in pathway
#' result$data is original data with additional column for e_score
#' reults$es is enrichment score

escore <- function(data) {
  sum_in <- sum((data[data$in_path == 1,]$x_array)^2)
  sum_out <- sum((data[data$in_path == 0,]$x_array)^2)

  w <- sum_out / sum_in
  data$e_score = 0
  amp <- 0

  for (ion in 1:nrow(data)) {
    if (data$in_path[ion] == 1) {
      amp <- amp + (w * (data$x_array[ion])^2)
    } else if (data$in_path[ion] == 0) {
      amp <- amp - (data$x_array[ion])^2
    }
    data$e_score[ion] <- amp
  }

  if (abs(min(data$e_score)) > max(data$e_score)) {es <- min(data$e_score)} else {es <- max(data$e_score)}
  res <- list("data" = data,
              "es" = es)
  return(res)
}

#--------------------------------------------------------------------------------------------------------

#' calculate enrichment score
#' @param x_array vector of ion weights
#' @param pathway string describing pathway name of interest
#' @param ions_inpath binary matrix of ions by pathways
#' @param plot plot enrichment score? (default = TRUE)
#' @param cluster_description used to label plot

calc_ES <- function(x_array, pathway, ions_inpath, plot = TRUE, cluster_description = "") {

  in_path <- ions_inpath[,grep(paste0(pathway, "$"), names(ions_inpath))]
  if (!is.integer(in_path)) {
    print("pathway not found")
  } else {
    data <- data.frame(x_array, in_path)
    data <- data[order(data$x_array),]

    copy <- data
    escore_data <- escore(data)
    data <- escore_data$data
    es <- escore_data$es
    data$x_step <- seq(2:(nrow(data)+1))

    if (plot == TRUE) {
      par(mar = c(5,5,2,5))
      plot(c(1,data$x_step), c(0,data$e_score), type = "l", ylab = "enrichment score", xlab = "index")
      par(new = T)
      plot(data$x_step, (data$x_array^2), xlab = NA, ylab = NA, axes = F, type = "l", col = "red")
      axis(side = 4)
      mtext(side = 4, line = 3, "squared ion means")
      title(paste0("pathway: ", pathway, " | cluster: ", cluster_description))
    }


    # jumble weights 1000 times and store min and max values
    j_es <- c()

    for (i in 1:100) {
      jumbled <- copy
      jumbled$in_path <- sample(copy$in_path, size = length(copy$in_path), replace = FALSE)
      escore_jumbled<- escore(jumbled)
      jumbled <- escore_jumbled$data
      j_es <- c(j_es, escore_jumbled$es)
    }

    extreme <- copy
    extreme$in_path <- c(rep(1, sum(extreme$in_path)), rep(0, length(extreme$in_path) - sum(extreme$in_path)))
    extreme_val <- escore(extreme)$es

    if (es < quantile(j_es, .05)) {pmin = TRUE} else {pmin = FALSE}
    if (es > quantile(j_es, .95)) {pmax = TRUE} else {pmax = FALSE}

    prob_func <- approxfun(density(j_es, n = 1024))

    ret <- list(x_array = data$x_array,
                e_score = data$e_score,
                Q = es,
                Q_extreme = extreme_val,
                probability = prob_func(es)
    )

    return(ret)
  }

}

#-------------------------------------------------------------------------------------------------------------

# needs hmdbQuery
#' return dataframe of ions and which pathways they are in
#' @param ann annotated ion data

find_ions_in_pathways <- function(ann) {

  pathway_df <- data.frame("init" = c(1:(nrow(ann)*100)),
                           "metabolite" = 0,
                           "ion" = 0,
                           "HMDBID" = 0,
                           "pathway" = 0)
  pathway_df <- pathway_df[-1]
  line <- 1

  metabolites <- c()

  # for every metabolite in ann (i)
  for (i in 1:nrow(ann)) {
    met_IDs <- unlist(strsplit(as.character(ann$'ï..id'[i]), '; '))
    met_IDs <- met_IDs[grepl("HMDB", met_IDs)]
    met_IDs <- sub("HMDB", "HMDB00", met_IDs)
    metabolites <- c(metabolites, gsub("[[:punct:]]", "", ann$name[i]))

    for (id in met_IDs) {

      entry <- tryCatch(HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/", id = id), error = function(e) entry <- NA)

      if (!is.na(entry)) {
        pathways <- store(entry)$biological_properties$pathways

        if (pathways != "\n    ") {
          prev_paths <- c()

          for(path in 1:length(pathways)){
            if (!gsub(" ", "", unlist(as.character(pathways[path]$pathway$name))) %in% prev_paths){
              pathway_df$metabolite[line] <- gsub("[[:punct:]]", "", ann$name[i])
              pathway_df$ion[line] <- ann$ion[i]
              pathway_df$HMDBID[line] <- id
              pathway_df$pathway[line] <- gsub(" ", "", unlist(as.character(pathways[path]$pathway$name) ))
              line <- line + 1
              prev_paths <- c(prev_paths, gsub(" ", "", unlist(as.character(pathways[path]$pathway$name))))
            }
          }
        }
      }
    }
    print(paste0("met ", i, ": ", as.character(ann$name[i])))
  }

  pathway_df <- pathway_df[pathway_df$metabolite != 0,]

  return(pathway_df)

}

#-------------------------------------------------------------------------------------------------------------------------

#' Re format result from find_ions_in_pathways as binary matrix
#' @param pathway_df result of find_ions_in_pathways

binary_pathways <- function(pathway_df) {
  ion_inpath <- as.data.frame(matrix(nrow = length(unique(ann$ion)), ncol = length(unique(pathway_df$pathway)), 0))
  rownames(ion_inpath) <- unique(ann$ion)
  colnames(ion_inpath) <- unique(pathway_df$pathway)


  for (path in 1:ncol(ion_inpath)){
    ion_inpath[[path]] <- as.numeric(c(rownames(ion_inpath)) %in% unique(pathway_df[pathway_df$pathway == names(ion_inpath)[path],]$ion))
  }

  return(ion_inpath)
}

#------------------------------------------------------------------------------------------------------------------------

#' find important pathways (those with most ions in them)
#' @param ion_inpath result from binary_pathways
#' @param n number of important pathways to choose (default = 5)

find_important_pathways <- function(ion_inpath, n = 5){
  pathway_summary <-  data.frame("pathway" = names(ion_inpath), "total_ions" = colSums(ion_inpath))
  pathway_summary <- pathway_summary[order(pathway_summary$total_ions, decreasing = TRUE),]
  return(as.character(pathway_summary$pathway[1:n]))
}

#------------------------------------------------------------------------------------------------------------------------


#' Create enrichment score summary
#' @param pathways list of pathways (names) of interest
#' @param clusters coordinates and clusters (results of cluster_data)
#' @param inpath_matrix which ions are in pathways (result of binary_pathways)
#' returns summary of pathway, cluster, Q, Q_extreme, probability

es_summary <- function(pathways, clusters, inpath_matrix) {
  summary <- data.frame(matrix(ncol = 5, nrow = length(pathways)*length(unique(clusters$clusters))))
  names(summary) <- c("pathway", "cluster", "Q", "Q_extreme", "probability")
  i <- 1
  for (p in pathways) {
    for (c in unique(clusters$clusters)){
      df <- train[which(clusters$clusters == c),]
      res <- calc_ES(x_array = colMeans(df), pathway = p, ions_inpath = inpath_matrix, plot = TRUE, cluster_description = c)
      summary$pathway[i] <- p
      summary$cluster[i] <- c
      summary$Q[i] <- res$Q
      summary$Q_extreme[i] <- res$Q_extreme
      summary$probability[i] <- res$probability
      i <- i + 1
    }
  }

  return(summary)

}



























