library(plotly)
library(lubridate)

#if(!("bRema" %in% rownames(installed.packages())))
#{ 
#  library(devtools)
#  install_github('tinnaing347/bRema')
#}

library(devtools)
install_github('cunybpl/bRema')
library(bRema)

result_table <- function(result, model)
{ 
  final_test_result = result[[model]]$stat_test$main_test
  final_test_result = ifelse(is.null(final_test_result), NA, final_test_result)
  result_matrix = rbind(result[[model]]$params, result[[model]]$cp1, result[[model]]$cp2, final_test_result)
  row.names(result_matrix) = c(row.names(result[[model]]$params), 'CP1', 'CP2', 'Model Test')
  return(result_matrix)
}


