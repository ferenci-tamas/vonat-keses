library(data.table)

cutlabs <- c("-0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-45", "46-60", "61-")

kesesstat <- function(x, metric) {
  if (all(is.na(x)) || length(x) == 0) return(NULL)
  
  x <- x[!is.na(x)]
  
  stats_list <- list()
  value1_list <- list()
  value2_list <- list()
  
  if ("N" %in% metric) {
    stats_list[["N"]] <- "Megállások száma"
    value1_list[["N"]] <- length(x)
    value2_list[["N"]] <- NA
  }
  
  if ("Megoszlás" %in% metric) {
    tab <- table(cut(x, c(-Inf, 0, 5, 10, 15, 20, 30, 45, 60, Inf)))
    stats_list[["Megoszlás"]] <- cutlabs
    value1_list[["Megoszlás"]] <- as.numeric(prop.table(tab)) * 100
    value2_list[["Megoszlás"]] <- as.numeric(tab)
  }
  
  if (any(c(
    "Átlag", "Medián", "75. percentilis", "90. percentilis",
    "99. percentilis", "Maximum") %in% metric)) {
    x_pmax0 <- pmax(0, x)
  }
  
  if (">5" %in% metric) {
    x_gt_5 <- x > 5
    stats_list[[">5"]] <- ">5"
    value1_list[[">5"]] <- mean(x_gt_5) * 100
    value2_list[[">5"]] <- sum(x_gt_5)
  }
  
  if (">20" %in% metric) {
    x_gt_20 <- x > 20
    stats_list[[">20"]] <- ">20"
    value1_list[[">20"]] <- mean(x_gt_20) * 100
    value2_list[[">20"]] <- sum(x_gt_20)
  }
  
  if ("Átlag" %in% metric) {
    stats_list[["Átlag"]] <- "Átlag"
    value1_list[["Átlag"]] <- mean(x_pmax0)
    value2_list[["Átlag"]] <- NA_real_
  }
  
  if ("Medián" %in% metric) {
    stats_list[["Medián"]] <- "Medián"
    value1_list[["Medián"]] <- median(x_pmax0)
    value2_list[["Medián"]] <- NA_real_
  }
  
  quantiles_needed <- c("75. percentilis", "90. percentilis",
                        "99. percentilis") %in% metric
  if (any(quantiles_needed)) {
    calculated_quantiles <- setNames(quantile(
      x, probs = c(0.75, 0.90, 0.99)[quantiles_needed]),
      c("75. percentilis", "90. percentilis",
        "99. percentilis")[quantiles_needed])
    
    if ("75. percentilis" %in% metric) {
      stats_list[["75. percentilis"]] <- "75. percentilis"
      value1_list[["75. percentilis"]] <- calculated_quantiles["75. percentilis"]
      value2_list[["75. percentilis"]] <- NA_real_
    }
    if ("90. percentilis" %in% metric) {
      stats_list[["90. percentilis"]] <- "90. percentilis"
      value1_list[["90. percentilis"]] <- calculated_quantiles["90. percentilis"]
      value2_list[["90. percentilis"]] <- NA_real_
    }
    if ("99. percentilis" %in% metric) {
      stats_list[["99. percentilis"]] <- "99. percentilis"
      value1_list[["99. percentilis"]] <- calculated_quantiles["99. percentilis"]
      value2_list[["99. percentilis"]] <- NA_real_
    }
  }
  
  if ("Maximum" %in% metric) {
    stats_list[["Maximum"]] <- "Maximum"
    value1_list[["Maximum"]] <- max(x_pmax0)
    value2_list[["Maximum"]] <- NA_real_
  }
  
  res <- data.table(
    stat = unlist(stats_list),
    value1 = unlist(value1_list),
    value2 = unlist(value2_list)
  )
  
  res[, formatted := fifelse(
    stat %in% c(cutlabs, ">5", ">20"),
    paste0(round(value1, 1), "% (", value2, ")"),
    fifelse(stat %in% c("Átlag", "Medián", "75. percentilis",
                        "90. percentilis", "99. percentilis",
                        "Maximum"),
            as.character(round(value1, 2)),
            fifelse(stat == "Megállások száma",
                    as.character(value1), NA_character_)))]
  
  return(res)
}
