
#' log transformation and z score untargeted data. result is data frame including patient, visit, and group information
#' @param original_untar data frame of untargeted ions where rows = patients and columns = ions
#' @param ann annotated ion data
#' @param untarmet untargeted metadata
#' @param partitioning dataframe where first column contains patient ids and second contains groups

preprocess_untargeted_data <- function(original_untar, ann, untarmet, partitioning) {
  untar <- original_untar[,names(original_untar) %in% paste0("V", ann$ion)]
  log_untar <- log(untar)
  zscore_untar <- as.data.frame(apply(log_untar, MARGIN = 2, function(x) {
    (x - mean(x, na.rm = TRUE)) / var(x, na.rm = TRUE)
  }))
  names(partitioning)[1] <- "patientid"
  zscore_untar$patientid <- substring(untarmet$V2, 1, 7)
  zscore_untar$visit <- substring(untarmet$V2, 9, 15)
  zscore_untar <- merge(zscore_untar, partitioning[c(1,2)], by = "patientid")
  zscore_untar <- zscore_untar[c(1,(length(zscore_untar)-1), length(zscore_untar), 2:(length(zscore_untar)-2))]
  return(zscore_untar)
}
