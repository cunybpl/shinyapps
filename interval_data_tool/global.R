library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)

#get meter before fixing time, this will have to be inside renderUI
get_meter <- function(inter_df)
{
	m = colnames(inter_df)[2:(ncol(inter_df))]
	return(m)
}

#fix time for the main original input
fixing_time <- function(inter_df)
{
	inter_df$D2 = strptime(inter_df$Period, format = "%m/%d/%y %H:%M") #this convert factor from original data to time
	inter_df$D3 = format(inter_df$D2, format = "%a %m/%d/%Y") #get day's name from D2
	inter_df$times <- strftime(inter_df$D2, format="%H:%M:%S") #get hours and minutes
	inter_df$color = ifelse( 2 < wday(inter_df$D2)& wday(inter_df$D2) < 7, 'red', ifelse(wday(inter_df$D2) == 2,'blue', 'green')) #add color column
	inter_df$D2 = as.POSIXct(inter_df$D2) #POSIXlt at first, i think
	inter_df$D3 = as.factor(inter_df$D3) #convert time to factor to use in graph

	return(inter_df)
}

#after getting meter, filter according to meter, pretty much ready
interval_main <- function(inter_df, meter)
{
	inter_df = inter_df[, c('Period', meter, 'D2', 'D3', 'times', 'color')]
	colnames(inter_df) = c('Period', 'Usage', 'D2', 'D3', 'times', 'color')
	return(inter_df)
}

#get month index, according to the month user wants
get_month <- function(month_name)
{
  month_num = seq(1,12, by = 1)
  month_abr = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  month_df = data.frame(num =month_num, name = month_abr)
  month_n = subset(month_df$num, month_df$name == month_name)
  return(as.numeric(month_n))
}

#filter fixed time df by using month_index
df_month <- function(df, month_n)
{ 
  df_list = list()
  df = subset(df, month(df$D2) == month_n)
  num_days = days_in_month(df$D2[1])
  for (i in seq(1, num_days, by =1))
  {
    temp = subset(df, day(df$D2) == i)
    name =  paste("day", i, sep="")
    df_list[[name]] = temp
  }
  return(list(df_list = df_list, num_days = num_days))
}

interval_weekly <- function(df, month_n)
{
  #subset by month and year
  df = subset(df, month(df$D2) == month_n)
  num_days = days_in_month(df$D2[1])

  w1 = interval_figure(df, 1, 7)
  w2 = interval_figure(df, 8, 14)
  w3 = interval_figure(df, 15, 21)
  w4 = interval_figure(df, 22, num_days)

  fig_week = list(w1 = w1, w2 = w2, w3 = w3, w4 = w4)
  return(fig_week)
}

interval_monthly <- function(df, month_n)
{
  df = subset(df, month(df$D2) == month_n)
  num_days = days_in_month(df$D2[1])

  m_fig = interval_figure(df, 1, num_days)
  return(m_fig)
}

interval_figure <- function(df, day1, dayn)
{
  t0 = subset(df, day(df$D2) == day1)
  day2 = day1+1
  dayn_0 = dayn -1
  p1 = add_trace(p = plot_ly(width = 800, height = 500), data = t0, x = ~times, y = ~D3, z = ~Usage, type ='scatter3d', mode = 'lines', line = list(color = t0$color), inherit = FALSE)
  for (i in seq(day2, dayn_0, by = 1))
  {
    t1 = subset(df, day(df$D2) == i)
    p1 = add_trace(p = p1, data = t1, x = ~times, y = ~D3, z = ~Usage, type ='scatter3d', mode = 'lines', line = list(color = t1$color), inherit = FALSE)
  }
  t1 = t1 = subset(df, day(df$D2) == dayn)
  p2 = add_trace(p = p1 , data = t1, x = ~times, y = ~D3, z = ~Usage, type ='scatter3d', mode = 'lines', line = list(color = t1$color), inherit = FALSE) %>%
    layout(showlegend = FALSE, scene = list(
      xaxis = list(title = "Times"),
      yaxis = list(title = "Days"),
      zaxis = list(title = "Usage")))

  return(p2)
}

figure_average <- function(df)
{ 

  df$Month1 = month.abb[df$Month]
  p = plot_ly() %>%
    add_trace(data = df, x = ~Time, y =~Usage, type = 'scatter', mode = 'lines', color = df$Month1) %>%
    layout(title = 'Average Monthly Usage', xaxis = list(title = 'Time'), yaxis = list(title = 'Usage'), margin = list(b = 100))
  return(p)
}


get_monthly_time_averages <- function(df){
  df[is.na(df)] <- 0
  df$Period <- as.POSIXct(strptime(df$Period, "%m/%d/%y %H:%M"))
  df$Month <- month(df$Period)
  df$Day <- format(as.Date(df$Period, format="%Y/%d/%m %I:%M %p"),"%d")
  df$Date <- format(df$Period, format = "%m/%d/%y")
  df$Time <- strftime(df$Period, format="%H:%M:%S") #get times
  df <- filter(df, !is.na(Period))
  df_2 <- select(df, one_of("Total", "Time", "Date", "Month"))
  
  df_3 <- df_2 %>% 
    group_by(Month, Time) %>%
    summarise(Usage = mean(Total))
  
  return(df_3)
}

clean_data_heatmap <- function (df)
{
  df[is.na(df)] <- 0
  df$Period <- as.POSIXct(strptime(df$Period, "%m/%d/%y %H:%M"))
  df$Month <- month(df$Period)
  df$Day <- day(df$Period)  
  df$Date <- format(df$Period, format = "%m/%d/%y")
  df$Time <- strftime(df$Period, format="%H:%M:%S")
  df <- filter(df, !is.na(Period))
  df_2 <- select(df, one_of("Total", "Time", "Date"))
  df_2 <- spread(df_2, Time, Total)
  df_3 <- df_2[,-1]
  rownames(df_3) <- df_2[,1]
  return(df_3)
}

#Michele
heatmap <- function (df_3)
{
  plot_ly(z = data.matrix(df_3),
           x = colnames(df_3), 
           y = row.names(df_3), 
           type = "heatmap", colorscale = 'Greys') %>%
  layout(margin = list(l = 100, b = 100))
}
