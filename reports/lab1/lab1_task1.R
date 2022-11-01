e1 <- read.csv('../data/lab1_e1.csv')
e1
sapply(e1, typeof)

fix_data <- function(df) {
  for (i in seq(ncol(df))) {
    col_num <- suppressWarnings(sapply(gsub(" ", "", df[[i]]), as.numeric))
    is_not_num = sapply(col_num, is.na)
    if (length(is_not_num[is_not_num==T]) == 0) {
      df[[i]] <- col_num
    }
  }
  return (df)
}

e1 <- fix_data(e1)
e1
sapply(e1, typeof)