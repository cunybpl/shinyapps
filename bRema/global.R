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


plot_timeseries_2 <- function(util, energy, sqft_flag = TRUE)
{ 
  #require(plotly)
  options(digits=15)

  if(!is.POSIXlt(util$end_date) & !is.POSIXt(util$end_date) & !is.POSIXct(util$end_date))
  { 
    if (grepl('/', util$end_date[1]))
    {
      util$end_date = strptime(util$end_date, format = "%m/%d/%y")
    }else
    {
      util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
    }
  }

  util$end_date = as.factor(util$end_date) 

  util$estimated[util$estimated == 1] <- 'Est'
  util$estimated[util$estimated != 'Est'] <- 'Act'


  util_act = subset(util, util$estimated == 'Act')
  util_est = subset(util, util$estimated == 'Est')

  sqft_str = ifelse(sqft_flag, '/sqft/month)', '/month)')
  switch(as.character(energy), 
          'Elec' = 
          {
            title_n = paste('Usage(kWh', sqft_str)
            usage_color = 'rgba(51, 113, 213, 1)'
          },
          'Fuel' = 
          {
            title_n = paste('Usage(BTU', sqft_str)
            usage_color = 'rgba(240, 24, 28,1)'
          }
          )

  ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "OAT"
  )

    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter',
      			mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'),
      			name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter',
      			mode = 'lines', line = list(color = usage_color),
      			name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers',
      			marker = list(symbol = 'circle-open', color = usage_color, size = 9),
      			name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers',
      			marker = list(symbol = 'circle', color = usage_color, size = 9),
      			name = 'Act') %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title = title_n),
      				margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )


  return(p)
}