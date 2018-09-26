library(lubridate)
prepare_raw_data <- function(df)
{	#do not need to include this in for loop
	df[['actual_start_date']] = strptime(df[['actual_start_date']], format = "%Y-%m-%d")
	df[['actual_end_date']] = strptime(df[['actual_end_date']], format = "%Y-%m-%d")

	#df[['billing_period_start_date']] = strptime(df[['billing_period_start_date']], format = "%Y-%m-%d")
	#df[['billing_period_end_date']] = strptime(df[['billing_period_end_date']], format = "%Y-%m-%d")

	df$actual_start_date = as.POSIXct(df$actual_start_date)
	df$actual_end_date = as.POSIXct(df$actual_end_date)
	#df$billing_period_start_date = as.POSIXct(df$billing_period_start_date)
	#df$billing_period_end_date = as.POSIXct(df$billing_period_end_date)

	df$days_in_period = difftime(df$actual_end_date, df$actual_start_date, units = 'day')
	df$days_in_period = as.integer(df$days_in_period)

	df$new_usage = df$allocated_consumption/df$days_in_period

	return(df)
}

main_utility_handler <- function(df, oat_df)
{
	df = prepare_raw_data(df)
	df$epa_energy_type = ifelse(grepl('Elec', x = df$epa_energy_type), 'Elec', 'Fuel')
	final_df = data.frame()
	for (energy in unique(df$epa_energy_type))
	{
		temp = subset(df, df$epa_energy_type == energy)
		bdbid_df = data.frame()
		for (bdbid_n in unique(temp$bdbid))
		{
			temp_bdbid = inter_handler(temp, bdbid_n)
			bdbid_df = rbind(bdbid_df, temp_bdbid)
		}
		bdbid_df$energy_type = energy
		final_df = rbind(final_df, bdbid_df)
	}
	if (!is.null(oat_df))
	{
		final_df = add_oat_column(final_df, oat_df)
	}
	return(final_df)
}

inter_handler <- function(df, bdbid_n)
{
	df = subset(df, df$bdbid == bdbid_n)
	daily_index_df = construct_date_index_df(df)
	monthly_df = create_monthly_average_df(daily_index_df)
	final_df = get_act_est_info(monthly_df, daily_index_df)
	final_df$bdbid = bdbid_n
	final_df$prepost = 1
	return(final_df)
}

construct_date_index_df <- function(df)
{	#inside for loop
	daily_index_df = data.frame()
	for (i in c(1:nrow(df)))
	{
		temp = data.frame(date_index = seq(df$actual_start_date[i], df$actual_end_date[i], "days"))
		temp$estimated = df$reading_type[i]
		temp$usage = df$new_usage[i]
		daily_index_df = rbind(daily_index_df, temp)
	}
	return(daily_index_df)
}

create_monthly_average_df <- function(df)
{	
	#aggregating duplicates, after aggregating date_index and new_usage become Category and x
	df = aggregate(df$usage, by=list(Category=df$date_index), FUN=sum)
	df$temp_date = format(df$Category, format="%Y-%m")
	#aggregating by month and year;after aggregating temp_date becomes Category
	monthly_df = aggregate(df$x, by=list(Category=df$temp_date), FUN=sum)
	monthly_df$Category = strptime(paste(monthly_df$Category, "-01", sep =""), format = "%Y-%m-%d")
	monthly_df$Category = as.POSIXct(monthly_df$Category)
	day(monthly_df$Category) = days_in_month(monthly_df$Category)
	colnames(monthly_df) = c('end_date', 'usage')
	monthly_df$usage = monthly_df$usage/days_in_month(monthly_df$end_date)
	return(monthly_df)
}

get_act_est_info <- function(monthly_df, daily_index_df)
{	
	monthly_df$estimated = 0
	monthly_df$fiscal_year = NA
	for (i in c(1:nrow(monthly_df)))
	{
		end_date = monthly_df$end_date[i]
		start_date = end_date
		day(start_date) = 1
		est_vec = subset(daily_index_df$estimated,  daily_index_df$date_index >= start_date & daily_index_df$date_index <= end_date)
		if ('E' %in% est_vec)
		{
			monthly_df$estimated[i] = 1
		}
		if (month(end_date) < 7)
		{
			monthly_df$fiscal_year[i] = year(end_date)
		}else
		{
			monthly_df$fiscal_year[i] = year(end_date)+1
		}
	}
	return(monthly_df)
}

add_oat_column <- function(df, oat_df)
{	
	df$OAT = NA
	oat_df[['end_date']] = strptime(oat_df[['end_date']], format = '%Y-%m-%d')
	oat_df$end_date = as.POSIXct(oat_df$end_date)
	date_list = unique(df$end_date)
	for (i in c(1:length(date_list)))
	{	
		date_i = date_list[i]
		df$OAT[df$end_date == date_i] = subset(oat_df$temp_avg, oat_df$end_date == date_i)
	}
	df = df[,c(5,7,4,1,2,6,8,3)]
	return(df)
}

choose_fiscal_year <- function(df, fiscal_year_n, points = 24)
{	

	if (points == 24)
	{
		fiscal_year_vec = c(fiscal_year_n-1,fiscal_year_n)
	}else
	{
		fiscal_year_vec = c(fiscal_year_n)
	}
	df = subset(df, df$fiscal_year %in% fiscal_year_vec)
	return(df)
}

choose_date_range <- function(df, time_df)
{
	time_df = as.character(time_df)
	time_df = strptime(time_df, format = "%Y-%m-%d")
	time_df = as.POSIXct(time_df)

	df = subset(df, df$end_date >= time_df[1] & df$end_date <= time_df[2])
	return(df)
}
