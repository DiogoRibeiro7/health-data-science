prepare_lca_data <- function(df) {
  df$race3 <- factor(df$race3, levels = 1:3, labels = c("White", "Black", "Other"))
  df$urbandwell <- factor(df$urbandwell, levels = 1:3, labels = c("Metro", "Urban", "Rural"))
  df$hispanic3 <- factor(df$hispanic3, levels = 1:3, labels = c("Non-Hispanic", "Hispanic", "Unknown"))
  df$insurancetype <- factor(
    df$insurancetype,
    levels = 1:6,
    labels = c("Not Insured", "Private", "Medicaid", "Medicare", "Other Govt", "Unknown")
  )
  df$age4 <- factor(df$age4, levels = 1:4, labels = c("18-49", "50-64", "65-74", "75+"))
  df$SES <- factor(df$SES, levels = 1:3, labels = c("Low SES", "Med SES", "High SES"))
  df$facility <- factor(
    df$FACILITY_TYPE_CD,
    levels = c(1:4, 9),
    labels = c("Community Cancer", "Comprehensive Cancer", "Academic/Research", "Integrated Network", "Other")
  )
  df
}

run_lca <- function(data_path, output_path) {
  if (!is.character(data_path) || length(data_path) != 1) {
    stop("`data_path` must be a single character string", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1) {
    stop("`output_path` must be a single character string", call. = FALSE)
  }

  message("Reading data from: ", data_path)
  message("Saving results to: ", output_path)
  ensure_dir(output_path)
  require_data_file(data_path)

  early.puf <- tryCatch(
    read.csv(data_path),
    error = function(e) {
      stop("Failed to read input data '", data_path, "': ", e$message,
           "\nEnsure the file is a valid CSV and accessible.",
           call. = FALSE)
    }
  )

  early.puf <- prepare_lca_data(early.puf)

  lca.earlydata <- early.puf %>% dplyr::select(
    PUF_CASE_ID, optcare, SEX, facility, DX_RX_STARTED_DAYS, CROWFLY,
    CDCC_TOTAL_BEST, race3, hispanic3, urbandwell, age4, SES, insurancetype
  )
  eff <- with(
    lca.earlydata,
    cbind(race3, hispanic3, urbandwell, age4, SES, insurancetype) ~ 1
  )
  lc7 <- poLCA(eff, early.puf, nclass = 7, maxiter = 5000,
               tol = 1e-5, na.rm = TRUE, nrep = 20, verbose = TRUE, calc.se = TRUE)

  prob7 <- lc7$posterior
  class7 <- lc7$predclass
  idx <- seq_along(class7)
  memclass.prob <- prob7[cbind(idx, class7)]
  lcaprob <- data.frame(class7, memclass.prob)
  mem.lcaplot <- ggplot(lcaprob, aes(x = factor(class7), y = memclass.prob)) +
    geom_boxplot() + ggtitle("LCA Membership Probability")
  print(mem.lcaplot)

  complete.earlypuflca <- na.omit(lca.earlydata[, c(1, 8:13)])
  lcacomplete <- cbind(complete.earlypuflca, class7)
  lca.pufdata <- merge(early.puf, lcacomplete, by = "PUF_CASE_ID")

  save(lc7, lca.pufdata, file = output_path)
}
