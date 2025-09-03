#' @title Exploration of the Data Used for Modeling
#'
#' @description
#' Function `explore` provides an initial exploration of a dataset. It calculate summary statistics for all variables in the provided formula or dataset,
#' visualizes the distribution of the response variable as a histogram density,and boxplot for Coefficient of Variation (CV) / Relative Standard Error (RSE).
#'
#' @param formula Optional formula to specify a response variable (e.g., y ~ x1 + x2).
#' @param CV Coefficient of Variation (CV) or Relative Standard Error (RSE) of the response variable
#' @param data The dataframe to be explored
#' @param normality Logical; if \code{TRUE}, the function will additionally check
#'   the normality of the response variable and display the result. Defaults to \code{FALSE}.
#'
#' @return Prints a data frame of summary statistics for the selected
#'   variables, including minimum, 1st quartile, median, mean, 3rd quartile, maximum,
#'   and number of missing values (NA). Plots are drawn to the current graphics device.
#'
#' @examples
#' dataBeta$CV <- sqrt(dataBeta$vardir)/dataBeta$y
#' explore(y~X1+X2, CV = "CV", data = dataBeta)
#'
#' @importFrom grDevices adjustcolor
#' @importFrom graphics boxplot hist lines polygon
#' @importFrom stats density shapiro.test
#'
#' @export
explore <- function(formula, CV = NULL, data, normality = FALSE) {

  # Prepare data
  formuladata <- model.frame(formula, data, na.action = NULL)

  # Summary statistics
  summ_list <- lapply(formuladata, function(x) summary(x)[1:6])
  sum_stat <- do.call(cbind, summ_list)
  sum_stat <- as.data.frame(sum_stat)
  na_counts <- colSums(is.na(formuladata))
  sum_stat <- rbind(sum_stat, "NA" = na_counts)
  print(sum_stat)

  if (!is.null(formula)) {
    response <- all.vars(formula)[1]

    # Optional normality test
    if (normality && response %in% colnames(data)) {
      cat("\nNormality test for", response, ":\n")
      shapiro_res <- shapiro.test(data[[response]])
      alpha <- 0.05
      if (shapiro_res$p.value >= alpha) {
        cat("Decision: Data follow normal distribution, with p.value =",
            round(shapiro_res$p.value, 4), ">=", alpha, "\n\n")
      } else {
        cat("Decision: Data do NOT follow normal distribution, with p.value =",
            round(shapiro_res$p.value, 4), "<", alpha, "\n\n")
      }
    }

    #Histogram Density
    dens <- density(data[[response]], na.rm = TRUE)
    if (response %in% colnames(data)) {
      hist(data[[response]],
           main = paste("Histogram of", response),
           xlab = response,
           col = "grey90",
           border = "grey47",
           freq = FALSE,
           cex.lab = 1,
           cex.axis = 0.9)
      lines(dens, col = "royalblue", lwd = 3)
      polygon(dens, col = adjustcolor("royalblue", alpha.f = 0.3), border = NA, lwd = 3)
    }
  }

  # Boxplot of CV / RSE
  if (!is.null(CV)) {
    formuladata$CV <- data[[CV]]
    max_CV <- max(formuladata$CV, na.rm = TRUE)
    boxplot(
      formuladata$CV,
      col = adjustcolor("skyblue2", alpha.f = 0.6),
      main = "Boxplot of CV",
      ylab = "CV (%)",
      xlab = all.vars(formula)[1],
      ylim = c(0, max_CV * 1.1),
      cex.lab = 1,
      cex.axis = 0.9
    )
  }

  invisible(sum_stat)
}
