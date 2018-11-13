library(lubridate)
library(zoo)
library(plotly)
library(chron)
library(plyr)

prepare_data_func <- function(df)
{
	df$date = strptime(df$date, format = "%Y-%m-%d %H:%M")
	df$y_m_d = strftime(df$date, format = "%Y-%m-%d")
	df$year = year(df$date)
	df$week_number = strftime(df$y_m_d, format = "%U")
	df$date = as.POSIXct(df$date)
	df$hour = hour(df$date)
	df$hour_point = df$hour + minute(df$date)/60
	df$weekday = wday(df$date, label = TRUE, abbr = TRUE)
	df$weekday_flag = 0
	df$weekday_flag = 1
	df$weekday_flag[is.holiday(df$date) | is.weekend(df$date)] = 0
	return(df)
}

count_func <- function(df)
{
	date_count = length(unique(df$y_m_d))
	temp_hour_df = subset(df, !is.na(df$demand))
	hour_count = sum(temp_hour_df$hour_point)
	total_hour_count = sum(df$hour_point)
	approx_hour = total_hour_count - hour_count
	df = data.frame(date_count = date_count, hour_count = hour_count, approx_hour_count = approx_hour)
	return(df)
}

#use inter_df resulted from calc_usage_func
count_func <- function(df, interval)
{	
	date_count = length(unique(df$y_m_d))
	temp_hour_df = subset(df, !is.na(df$demand))
	length_temp = nrow(temp_hour_df)
	length_org = nrow(df)
	temp_hour_count = interval*length_temp
	total_hour_count = interval*length_org
	approx_hour = total_hour_count - temp_hour_count
	df = data.frame(date_count = date_count, hour_count = total_hour_count, approx_hour_count = approx_hour)
	return(df)
}

calc_usage_func <- function(df, interval)
{
	temp = df %>% mutate(approx = na.approx(demand))
	df$approx = temp$approx
	df$usage = df$demand * interval
	return(df)
}

plot_weekly <- function(year_df, week_i, interval = NULL)
{
	  temp_week_df = subset(year_df, year_df$week_number == week_i)
	  approx_week_df = subset(temp_week_df, is.na(temp_week_df$demand))
	  approx_week_df$usage = approx_week_df$approx*interval
	  week_name = paste('(Week ', week_i, ')', sep ='')
	  graph_title = paste(temp_week_df$y_m_d[1], '-', temp_week_df$y_m_d[nrow(temp_week_df)], week_name)
	  p1 = add_trace(p = plot_ly(), x = ~temp_week_df$hour_point, y = ~temp_week_df$usage, type ='scatter', mode = 'lines', color = ~temp_week_df$weekday) %>%
	        layout(title = graph_title, xaxis = list(title = 'Hours', zeroline = FALSE, showline = FALSE),yaxis = list(title= 'Demand (kW)', zeroline = FALSE, showline = FALSE))
	  p1 = add_trace( p = p1, x = ~approx_week_df$hour_point, y = ~approx_week_df$usage, type ='scatter', mode = 'lines', line = list(dash = 'dash'), color = ~approx_week_df$weekday)
}

week_match_func <- function(df, type_flag = 'approx')
{	
	agg_df = aggregate(df[,type_flag], list(df$year, df$week_number), mean)
	colnames(agg_df) = c('year', 'week_number', type_flag)
	date_vec = .POSIXct(character(1))
	for (i in c(1:nrow(agg_df)))
	{
		temp_df = subset(df, df$year == agg_df$year[i] & df$week_number == agg_df$week_number[i])
		date_vec = c(date_vec, as.POSIXct(temp_df$date[1]))
	}
	date_vec = date_vec[2:length(date_vec)]
	agg_df$date = date_vec
	return(agg_df[order(agg_df['date']),])
}


#subset by meter, calculate usage
#get time vec
get_time_vec_oat <- function(df)
{
  min_time = df$date[1]
  max_time = df$date[nrow(df)]
  time_vec = c(min_time, max_time)
  time_vec = strftime(time_vec, format = "%Y-%m-%d")
  time_vec = strptime(time_vec, format = "%Y-%m-%d")
  return(time_vec)
}

prepare_oat_func <- function(oat_df, time_vec)
{	
  oat_df$date = strptime(oat_df$date, format = "%Y-%m-%d %H")
  oat_df = subset(oat_df, oat_df$date >= time_vec[1] & oat_df$date <= time_vec[2])
  oat_df$date = as.POSIXct(oat_df$date)
  oat_df$year = year(oat_df$date)
  oat_df$y_m_d = strftime(oat_df$date, format = "%Y-%m-%d")
  oat_df$week_number = strftime(oat_df$y_m_d, format = "%U")
  temp = oat_df %>% mutate(approx = na.approx(OAT))
  oat_df$OAT = temp$approx
  return(oat_df)
}

calc_mean_sd_func <- function(df)
{
  temp <- ddply(df, c('weekday', 'hour'), summarise, length = mean(approx))
  colnames(temp) = c('weekday', 'hour', 'avg')
  temp$stat_dev = ddply(df, c('weekday', 'hour'), summarise, length = sd(approx))$length
  temp$length = ddply(df, c('weekday', 'hour'), summarise, length = length(approx))$length
  temp$error = qt(0.975, df=temp$length-1)*temp$stat_dev/sqrt(temp$length)
  temp$upper_lim = temp$avg + temp$error
  temp$lower_lim = temp$avg - temp$error
  temp$weekday_num = as.numeric(temp$weekday)
  temp$index_hour = temp$hour + (temp$weekday_num - 1)*24
  return(temp)
}

plot_ci_func <- function(mean_df, p_init, weekday_n)
{ 
  color_n = switch(as.character(weekday_n),
          'Sun' = 'rgba(77,0,86,1)',
          'Mon' = 'rgba(73,46,135,1)',
          'Tue' = 'rgba(25,104,145,1)',
          'Wed' = 'rgba(0,149,142,1)',
          'Thu' = 'rgba(0,191,119,1)',
          'Fri' = 'rgba(113,225,41,1)',
          'Sat' = 'rgba(255,236,0,1)'
          )

  color_t = switch(as.character(weekday_n),
          'Sun' = 'rgba(77,0,86,0.2)',
          'Mon' = 'rgba(73,46,135,0.2)',
          'Tue' = 'rgba(25,104,145,0.2)',
          'Wed' = 'rgba(0,149,142,0.2)',
          'Thu' = 'rgba(0,191,119,0.2)',
          'Fri' = 'rgba(113,225,41,0.2)',
          'Sat' = 'rgba(255,236,0,0.2)'
          )

  p_inter = p_init %>% add_trace(data = subset(mean_df, mean_df$weekday == weekday_n), x = ~index_hour, y = ~upper_lim, type = 'scatter', mode = 'lines', line = list(color = color_n ), showlegend = FALSE) %>%
    add_trace(data = subset(mean_df, mean_df$weekday == weekday_n), x = ~index_hour, y = ~lower_lim, type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor=color_t, line = list(color = color_n),
            showlegend = FALSE)
  return(p_inter)
}

main_plot_ci <- function(mean_df)
{
  p1 = add_trace(p = plot_ly(), x = ~mean_df$index_hour, y = ~mean_df$avg, type = 'scatter', mode = 'lines', color = ~mean_df$weekday) %>%
      layout(title = 'Typical Week load', xaxis = list(title = 'Hours', zeroline = FALSE, showline = FALSE), yaxis = list(title = 'Demand', zeroline = FALSE, showline = FALSE))
  for (i in c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'))
  {
    p1 = plot_ci_func(mean_df, p1, i)
  }
  return(p1)
}


plot_timeseries_agg_func <- function(oat_df, df, agg_oat, agg_df){
  ay <- list(tickfont = list(color = "red"), overlaying = "y", side = "right", title = "OAT")

  p1 = add_trace(p = plot_ly(), x = ~oat_df$date, y = ~oat_df$OAT, 
      type = "scatter", mode = "lines", line = list(color = "rgba(243, 154, 36, 0.9)", width = 0.7), 
      name = "OAT", yaxis = "y2") %>% add_trace(x = ~agg_oat$date, y = ~agg_oat$OAT, 
      type = "scatter", mode = "lines", line = list(color = "rgba(243, 154, 36, 1)", width = 5), 
      name = "Weekly Avg OAT", yaxis = "y2") %>% add_trace( x = ~df$date, 
      y = ~df$approx, type = "scatter", mode = "lines", line = list(color = "rgba(51, 113, 213, 0.9)", width = 0.7), 
      name = "Usage") %>% add_trace(x = ~agg_df$date, y = ~agg_df$approx, type = 'scatter', mode = 'lines', line = list(width = 5, color = 'rgba(51, 113, 213, 1)'), name = 'Weekly Average') %>%
      layout(title = "Time Series", 
      yaxis2 = ay, yaxis = list(title = "Demand (kW)"), margin = list(b = 100), 
      xaxis = list(type = "date", title = "Date"))
}

