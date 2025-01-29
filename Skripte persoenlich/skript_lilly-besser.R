### last try to work with the real data set

extract_and_analyze <- function(column_name, decoded_values = NULL) {
  data <- data2019  # Use NSDUH_2019 as the default dataset
  
  # Check if the column exists
  if (!column_name %in% colnames(data)) {
    stop("Column not found in the dataset")
  }
  # Extract the column
  col_data <- data[[column_name]]
  # Identify data type of the column
  col_class <- class(col_data)
  cat("\nColumn:", column_name, "\n")
  cat("Data Type:", col_class, "\n\n")
  # If decoded values are provided, decode the column
  if (!is.null(decoded_values)) {
    decoded_col_data <- factor(col_data, levels = names(decoded_values), labels = decoded_values)
    col_data <- decoded_col_data
    cat("Data Decoded Using Provided Values\n\n")
  }
  # Analyze based on data type
  if (is.numeric(col_data) || is.integer(col_data)) {
    # For numeric or integer columns
    summary_stats <- list(
      Min = min(col_data),
      Max = max(col_data),
      Mean = mean(col_data),
      Median = median(col_data),
      Std_Dev = sd(col_data),
      Quantiles = quantile(col_data)
    )
    print("Summary Statistics:")
    print(summary_stats)
    # Histogram of the column
    hist(col_data, 
         main = paste("Histogram of", column_name),
         xlab = column_name,
         col = "skyblue",
         border = "white")
  } else if (is.factor(col_data) || is.character(col_data)) {
    # For categorical columns
    cat("Unique Values (Top 10):\n")
    print(head(table(col_data), 10))
    # Barplot of top values with relative frequency
    col_freq <- table(col_data) / length(col_data)  # Calculate relative frequencies
    barplot(col_freq,
            main = paste("Relative Frequency of Categories in", column_name),
            xlab = column_name,
            ylab = "Relative Frequency",
            col = "lightgreen",
            las = 2)
  } else {
    cat("Data type not supported for analysis.")
  }
}

### MENTAL HEALTH ---------------------------------------------------------------------

decoded_values <- c(
  "1" = "All of the time",
  "2" = "Most of the time",
  "3" = "Some of the time",
  "4" = "A little of the time",
  "5" = "None of the time",
  "94" = "Don't Know",
  "97" = "Refused"
)

columns_of_interest <- c(
  "DSTNRV30", "DSTHOP30", "DSTRST30", "DSTCHR30", 
  "DSTEFF30", "DSTNGD30", "DSTWORST", "DSTNRV12"
)

for (column in columns_of_interest) {
  cat("\nAnalyzing column:", column, "\n")
  extract_and_analyze(column, decoded_values)
}

extract_and_analyze("DSTCHR30", decoded_values)

### DEPRESSION -----------------------------------------------------------------------
# During times when you felt sad, empty, 
# or depressed most of the day, did you ever feel discouraged about how things 
# were going in your life?

decoded_values <- c(
  "1" = "Yes",
  "2" = "No",
  "85" = "Bad Data",
  "94" = "Don't know",
  "97" = "Refused",
  "98" = "No Answer",
  "99" = "Skip"
)

extract_and_analyze("ADDPDISC", decoded_values)

