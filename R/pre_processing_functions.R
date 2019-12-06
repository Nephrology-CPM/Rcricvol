
#' check if a factor is continuous (has greater than 10 levels)
#' @param df data
#' @param f column number where factor is located

is.continuous <- function(df, f){
  return(length(table(df[,f])) > 10)
}

#--------------------------------------------------------------

#' convert binary variables in a data frame to factors
#' @param df data

factorize <- function(df){
  for (factor in 1:ncol(df)) {
    if (length(table(df[,factor])) == 2) {
      df[,factor] <- as.factor(df[,factor])
    }
  }
  return(df)
}

#--------------------------------------------------------------

#' convert categorical features to binary and add them to the dataset
#' @param df data

cat_to_binary <- function(df) {
  df$sex_v3y0 <- df$sex_v3y0-1 # make sex binary
  n_factors <- length(df)
  colnames <- colnames(df)
  for (factor in 2:ncol(df)) {
    # if factor is categorical and not already binary
    if (!is.continuous(df, factor) && (length(table(df[,factor])) > 2)) {
      # save category names as new_names
      new_names <- levels(as.data.frame(table(df[,factor]))$Var1)
      for (name in 1:length(new_names)) {
        # add a column to df with binary form of the cat
        df[,n_factors + 1] <- as.integer(grepl(name,df[,factor]))
        # update df column names
        colnames <- c(colnames, paste0(colnames[factor], "_", name))
        # update number of factors in df
        n_factors <- n_factors + 1
      }
      # remove original categorical factor
      df <- df[-factor]
      # update colnames
      colnames <- colnames[-factor]
      # update number of factors in df
      n_factors <- n_factors - 1
    }
  }
  colnames(df) <- colnames
  return(df)
}

#-------------------------------------------------------------

# needs knittr
#' drop variables that do not predict a certain factor at baseline well
#'


df_cont <- data.frame("init" = c(1:ncol(df_init)), "name" = 0,
                      "B" = 0, "p" = 0, "r_squared" = 0)

signif <- c()
cont_drop <- c()

line <- 1
for (factor in 2:ncol(df_init)) {
  if (is.continuous(df_init, factor)) {
    df_cont$name[line] <- colnames(df_init)[factor]
    lm.res <- lm(df_init$egfr_ckd_epi_v3y0 ~ df_init[,factor])

    df_cont$B[line] <- lm.res$coefficients[2]
    df_cont$p[line] <- summary(lm.res)$coefficients[,4][2]
    df_cont$r_squared[line] <- summary(lm.res)$r.squared

    if (df_cont$p[line] >= 0.05) {
      signif <- c(signif, factor)
      cont_drop <- c(cont_drop, factor)
    }
    line <- line + 1
  }
}

df_cont <- df_cont[df_cont$name != 0,]
df_cont <- df_cont[-1]

print(kable(df_cont), label = description)

# categorical vars
df_cat <- data.frame("init" = c(1:(ncol(df_init)-1)), "name" = 0, "pval" = 0, "n" = 0, "percent" = 0)
df_cat <- df_cat[-1]
line <- 1

cat_drop <- c()

for (factor in 2:ncol(df_init)) {
  if (length(table(df_init[,factor])) == 1) {} # drop factors wth no variation
  else if (!is.continuous(df = df_init, f = factor)) {
    df_cat$name[line] <- colnames(df_init)[factor]

    one <- df_init[df_init[,factor] == 1,]$egfr_ckd_epi_v3y0
    not_one <- df_init[df_init[,factor] != 1,]$egfr_ckd_epi_v3y0

    df_cat$pval[line] <- wilcox.test(one, not_one)$p.value
    cat_summary <- as.data.frame(table(df_init[,factor], useNA = "always"))
    df_cat$n[line] <- cat_summary[cat_summary$Var1==1,]$Freq[1]
    df_cat$percent[line] <- df_cat$n[line]/sum(cat_summary$Freq) * 100

    if (df_cat$pval[line] < 0.05) {
      signif <- c(signif, factor)
      cat_drop <- c(cat_drop, factor)
    }
    line <- line + 1
  }
}

df_cat <- df_cat[df_cat$name != 0,]

kable(df_cat)

print(paste0(length(cont_drop), " continuous features had a p value of less than 0.05 predicting baseline egfr with the linear model."))
print(paste0(length(cat_drop), " categorical features had a p value of less than 0.05 predicting baseline egfr with the wilcox test."))









