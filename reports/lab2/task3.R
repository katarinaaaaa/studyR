load(file='../../data/ExpImp.Rdata')
expimp_fd <- ExpImp[endsWith(ExpImp[["Регион"]], "федеральный округ"),]

exp <- expimp_fd[,endsWith(colnames(expimp_fd), "Экспорт")]
expimp_fd[["Суммарный Экспорт"]] <- rowSums(sapply(exp, function(x) 
  as.numeric((gsub("[^0-9.]", NA, x)))), na.rm=TRUE)

imp <- expimp_fd[,endsWith(colnames(expimp_fd), "Импорт")]
expimp_fd[["Суммарный Импорт"]] <- rowSums(sapply(imp, function(x) 
  as.numeric((gsub("[^0-9.]", NA, x)))), na.rm=TRUE)
expimp_fd

get_exp_more_than_imp <- function(all_data) {
  data = all_data[all_data["Суммарный Экспорт"] > all_data["Суммарный Импорт"],]
  return (data)
}

data <- get_exp_more_than_imp(expimp_fd)