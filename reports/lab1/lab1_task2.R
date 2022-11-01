load(file='../../data/lab1_e2.Rdata')
str(all_data)

get_id <- function(data) {
  for (i in seq(length(data))) {
    names(data[[i]])[names(data[[i]])=="temp"] <- paste("temp_day_", i, sep = "")
  }
  merged <- Reduce(function(df1, df2) merge(df1, df2,  by="id", all=F), data)
  without_passes <- data.frame("id"=merged$id, "mean_temp"=rowMeans(merged[-1]))
  return (without_passes)
}

data <- get_id(all_data)
data
