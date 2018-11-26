library(plotly)
library(lubridate)

make_ind_df <- function(df, names, dt_names, i)
{
	start_pos = dt_names[i]
	if (start_pos == dt_names[length(dt_names)])
	{
		end_pos = length(names)
	}else
	{	
		end_pos = dt_names[i+1] - 1
	}

	df = df[,start_pos:end_pos]

	return(df)
}

main_handler <- function(df, y_cols_2 = NULL, title_n = '', y_name = '', y_name_2 = '', date_range = NULL, interval = 0, interval_type = 'Minute')
{
	names = colnames(df)
	dt_names = grep('datetime', names)

	min_max_val = min_max_col(df, names)

	if(is.null(date_range))
	{
		min_max_date = min_max_datetime(df, dt_names)
	}else
	{
		min_max_date = date_range
	}

	df_weekend = get_weekend(min_max_date[1], min_max_date[2])
	df_weekend = as.POSIXct(df_weekend)
	
	p1 = plot_ly(height = 550, width = 855)


	for (i in c(1:length(dt_names)))
	{	
		temp_df = make_ind_df(df, names, dt_names, i)
		p1 = plot_handler(temp_df, min_max_val, p1, y_cols_2, title_n, y_name, y_name_2, date_range, interval, interval_type)
	}
	if(length(df_weekend) != 0)
	{
		p1 = draw_weekend_vline(p1, df_weekend, min_max_val)
	}
	return(p1)
}

plot_handler <- function(df, min_max_val, p1 = plot_ly(), y_cols_2 = NULL, title_n = '', y_name = '', y_name_2 = '',date_range = NULL, interval = 0, interval_type = 'Minute')
{	
	col_name = colnames(df)
	x_name = col_name[1]
	y_cols = col_name[2:length(col_name)]

	second_flag = FALSE
	if(!is.null(y_cols_2))
	{	
		second_flag = TRUE
		y_cols = y_cols[!(y_cols %in% y_cols_2)]
	}


	format_n = date_time_format_finder(as.character(df[[x_name]][1]))
	df[[x_name]] = strptime(df[[x_name]], format = format_n)
	df[[x_name]] = as.POSIXct(df[[x_name]])

	if (!is.null(date_range))
	{
		df = subset(df, df[[x_name]] >= date_range[1] & df[[x_name]] <= date_range[2])
	}

	if (interval != 0)
	{
		df = interval_func(df, x_name, interval, interval_type)
	}

	#df_weekend = get_weekend(df, x_name)
	for (y_i in y_cols)
	{	
		p1 = add_trace(p = p1, x = df[[x_name]], y = df[[y_i]], type ='scatter', mode = 'lines', name = y_i) %>%
					layout(showlegend = TRUE, title = title_n, xaxis = list(type = "date", title="Date", tickformat = "%m/%d/%y %H:%M"),yaxis = list(title= y_name), margin = list(b = 200))
	}

	if (second_flag)
	{	
		  ay <- list(
		      tickfont = list(color = "red"),
		      overlaying = "y",
		      side = "right",
		      title = y_name_2
		 	)

		for (y_i_2 in y_cols_2)
		{	
			p1 = add_trace(p = p1, x = df[[x_name]], y = df[[y_i_2]], type ='scatter', mode = 'lines', name = y_i_2, yaxis = 'y2') %>%
					layout(showlegend = TRUE, title = title_n, xaxis = list(type = "date", title="Date", tickformat = "%m/%d/%y", tickangle = -45), yaxis2 = ay, margin = list(b = 200))
		}
	}

	return(p1)
}

get_weekend_old <- function(df, x_name)
{
	final_df = data.frame(weekend = wday(df[[x_name]]))
	final_df[[x_name]] = df[[x_name]]
	final_df = subset(final_df, final_df$weekend == 7 | final_df$weekend == 1)
	dummy = as.character(final_df[[x_name]])
	final_df$date_char = unlist(lapply( strsplit(dummy,split=" "), "[", 1))
	final_df = final_df[!duplicated(final_df$date_char), ]
	return(final_df)
}

datetime_col_handler <- function(df, start_time, end_time)
{ 
  df = as.character(df)
  df[1] = paste0(as.character(df[1]), " ", start_time)
  df[2] = paste0(as.character(df[2]), " ", end_time)
  df = strptime(df, format = "%Y-%m-%d %H:%M:%S")
  df = as.POSIXct(df)
  return(df)
}

vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x,
    opacity = 0.25, 
    line = list(dash = 'dash')
  )
}


min_max_col <- function(df, names)
{	
	y_name = grep('datetime', names, invert = TRUE)
	min_val = Inf
	max_val = -Inf
	for (i in y_name)
	{
		temp_min = colMin(df[i])
		temp_max = colMax(df[i])
		if (temp_min < min_val)
		{
			min_val = temp_min
		}
		if (temp_max > max_val)
		{
			max_val = temp_max
		}
	}

	return(as.numeric(c(min_val, max_val)))
}


min_max_datetime <- function(df, dt_names)
{	
	min_val = Inf
	max_val = -Inf
	for (i in dt_names)
	{	
		temp = df[,i]
		format_n = date_time_format_finder(as.character(temp[1]))
		temp = strptime(temp, format = format_n)
		temp = as.POSIXct(temp)
		temp = temp[!is.na(temp)]

		temp_min = min(temp)
		temp_max = max(temp)

		if (temp_min < min_val)
		{
			min_val = temp_min
		}
		if (temp_max > max_val)
		{
			max_val = temp_max
		}
	}

	return(c(min_val, max_val))
}

get_weekend <- function(min_date, max_date)
{
	dates = seq(min_date, max_date, "days")
	weekend = dates[weekdays(dates) %in% c("Saturday", "Sunday")]
	return(weekend)
}

draw_weekend_vline <- function(p1 = plot_ly(), df_weekend, min_max_val)
{	

	for(i in c(1:length(df_weekend)))
	{	
		p1 = add_segments(p = p1, x = df_weekend[i], xend = df_weekend[i], y = min_max_val[1], yend = min_max_val[2], line = list(dash = 'dash', color = 'black'), opacity = 0.25, showlegend = FALSE)
	}
	return(p1)
}

interval_func <- function(df, x_name, interval, interval_type)
{	
	if (interval_type == 'Minute')
	{
		df$interval = minute(df[[x_name]])
	}else
	{
		df$interval = second(df[[x_name]])
	}
	df$interval = df$interval%%interval
	df = subset(df, df$interval == 0)
	return(df)
}


date_time_format_finder <- function(x)
{
	x = sapply(strsplit(x, ":"), length)
	format_n = switch(as.character(x), 
		 "1" = "%m/%d/%y",
		 "2" = "%m/%d/%y %H:%M",
		 "3" = "%m/%d/%y %H:%M:%S"
		)
	return(format_n)
}


colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
