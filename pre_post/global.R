library(plotly)
library(lubridate)

plot_baseload <- function(util, energy, model, B, cp1 = 0, cp2 =0, b_name) #x,y -> data, B -> bestvalue$params, cp1,2 -> bestvalue$cp1,2
{ 
  options(digits=15)
  #util$end_date = strptime(x = util$end_date, format = "%Y-%m-%d")

  util$pre = ifelse(util$prepost == 1, util$usage, NA)
  #util$period = ifelse(util$end_date <= temp_retrofit[1], util$usage, NA)
  util$post = ifelse(util$prepost == 3, util$usage, NA)

  x = util$OAT

  Ycp= B[1] #coeff, need this to get y intercept
  slope1 = B[2]
  yInter_1 = Ycp - slope1*cp1

  if (length(B) == 3) # 4P or 5p
  {
    slope2 = B[3]
    if (cp2 == 0) # 4P
    {
      yInter_2 = Ycp - slope2*cp1
    }else # 5P
    {
      yInter_2 = Ycp - slope2*cp2
    }
  }


  model = as.character(model)

  util = get_baseload_point(util, x, model, cp1, cp2, yInter_1, yInter_2, slope1, slope2, Ycp)
  point_fig = plot_baseload_point(util, energy)

  #figure = ggplot() + geom_line(data = util, aes(x= Start_Date, y = Elec),linetype = "dashed")+geom_line(data = util, aes(x= Start_Date, y= pre, color = 'red'), size = 0.75) + geom_line(data = util, aes(x= Start_Date, y= post, color = 'green'), size = 0.75) + geom_line(data = util, aes(x= Start_Date, y= post_adjust, color = 'orange'), size = 0.75)+ scale_color_discrete(labels = c("Post", "Adjusted", "Pre"))  + labs(x='Date',y='Usage')
  color_n = ifelse(energy == 'Elec','rgba(51, 113, 213, 1)','rgba(240, 24, 28,1)')

  p1 = add_lines(p = point_fig, x = ~util$end_date, y = ~util$usage, name = "Retrofit", line = list(color = 'black', width = 1, dash = 'dash'), inherit = FALSE)
  p2 = add_lines(p = p1, x = ~util$end_date, y = ~util$pre, name = 'Pre', line = list(color = color_n, width = 2))
  p3 = add_lines(p = p2, x = ~util$end_date, y = ~util$post, name = 'Post', line = list(color = 'rgba(109, 203, 15, 1)', width = 2))
  p4 = add_lines(p = p3, x = ~util$end_date, y = ~util$post_adjust, name = 'Adjusted', line = list(color = 'rgba(127, 63, 191,1)', width = 2)) %>%
        layout(title = b_name, margin = list(b = 100),
         xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date),
         yaxis = list (title = "Usage"))
  return (p4)
}


get_baseload_point <- function(util, x, model, cp1, cp2, yInter_1, yInter_2 = 0, slope1, slope2 =0, Ycp)
{
  if (model == '5P')
  {
    util$adjust = ifelse(x <= cp1, yInter_1 + slope1*x, ifelse(x >= cp2, yInter_2 + slope2*x,Ycp))
  }else if (model  == '4P')
  {
    util$adjust =ifelse(x <= cp1, yInter_1 + slope1*x, yInter_2 + slope2*x)
  }else if (model == '3PH')
  {
    util$adjust = ifelse(x <= cp1, yInter_1 + slope1*x, Ycp)
  }else if (model == '3PC')
  {
    util$adjust = ifelse(x > cp1, yInter_1 + slope1*x, Ycp)
  }else
  { 
    util$adjust = yInter_1 + slope1*x 
  }

  util$post_adjust = ifelse(util$prepost == 3, util$adjust, NA)
  return(util)
}

plot_baseload_point <- function(util, energy)
{ 
  util$re_point = ifelse(is.na(util$pre)& is.na(util$post) & is.na(util$post_adjust), util$usage, NA)
  re_est = subset(util$estimated, !is.na(util$re))

  util_act = subset(util, util$estimated == 0)
  util_est = subset(util, util$estimated == 1)

  color_n = ifelse(energy == 'Elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')

  p1 = add_trace(p = plot_ly(), x=~util$end_date, y = ~util$re_point, type ='scatter', mode ='markers', name= "Retrofit Usage", marker = list(symbol = ifelse(re_est == 0, 'circle', 'circle-open'), color = 'black', size = 9), inherit = FALSE)

  pre_fig_act = add_trace(p =p1, x = ~util_act$end_date, y = ~util_act$pre, type ='scatter', mode ='markers', name = 'Pre Act', marker = list(symbol = 'circle', color = color_n, size = 9), inherit = FALSE)
  pre_fig_est = add_trace(p = pre_fig_act, x = ~util_est$end_date, y = ~util_est$pre, type ='scatter', mode ='markers', name = 'Pre Est', marker = list(symbol = 'circle-open', color = color_n, size = 9), inherit = FALSE)

  post_fig_act = add_trace(p = pre_fig_est, x = ~util_act$end_date, y = ~util_act$post, type ='scatter', mode ='markers', name = 'Post Act', marker = list(symbol = 'circle', color = 'rgba(109, 203, 15, 1)', size = 9), inherit = FALSE)
  post_fig_est = add_trace(p = post_fig_act, x = ~util_est$end_date, y = ~util_est$post, type ='scatter', mode ='markers', name = 'Post Act', marker = list(symbol = 'circle-open', color = 'rgba(109, 203, 15, 1)', size = 9), inherit = FALSE)

  p4 = add_trace(p =post_fig_est, x = ~util$end_date, y = ~util$post_adjust, type ='scatter', mode ='markers', name = 'Adjusted Point', marker = list(symbol = 'circle-open', color = 'rgba(127, 63, 191,1)', size = 9), inherit = FALSE)
  return(p4)
}

flag_func <- function(df, bdbid_n, energy)
{
  if (bdbid_n %in% unique(df$bdbid) & energy %in% df$energy_type[df$bdbid == bdbid_n])
  {
    flag = TRUE
  }else
  {
    flag = FALSE
  }
  return(flag)
}

plot_model <- function(x, model, B, cp1, cp2, energy, pre_key, unit, p1 = plot_ly())
{ 
  options(digits=15)
  require(ggplot2)
  x = x[order(x)]
  x0 = x[1]
  xf = x[length(x)]
  Ycp= B[1] #coeff, need this to get y intercept
  slope1 = B[2]
  yInter_1 = Ycp - slope1*cp1

  if (length(B) == 3) # 4P or 5p
  {
    slope2 = B[3]
    if (cp2 == 0) # 4P
    {
      yInter_2 = Ycp - slope2*cp1
    }else # 5P
    {
      yInter_2 = Ycp - slope2*cp2
    }
  }

  if (model == '5P')
  { 
    x = c(x0, cp1, cp2, xf)
    estimated = ifelse(x <= cp1, yInter_1 + slope1*x, ifelse(x >= cp2, yInter_2 + slope2*x,Ycp))
  }else if (model  == '4P')
  { 
    x = c(x0, cp1, xf)
    estimated =ifelse(x <= cp1, yInter_1 + slope1*x, yInter_2 + slope2*x)
  }else if (model == '3PH')
  { 
    x = c(x0, cp1, xf)
    estimated = ifelse(x <= cp1, yInter_1 + slope1*x, Ycp)
  }else if (model == '3PC')
  { 
    x = c(x0, cp1, xf)
    estimated = ifelse(x > cp1, yInter_1 + slope1*x, Ycp)
  }else
  { 
    x = c(x0, xf)
    estimated = yInter_1 + slope1*x 
  }

  if (unit){
    df = data.frame(x = x, y = estimated*3412.14)
  }else
  {
    df = data.frame(x = x, y = estimated)
  }


  switch(as.character(pre_key),
    "1" = {
      pre_title = 'Pre'
      color_n = switch(as.character(energy), 'Elec' = 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
      },
    "3" ={
      pre_title = 'Post'
      color_n = 'rgba(109, 203, 15, 1)'
    }
    )

  name_n = paste(pre_title, energy, 'Model')


  model_fig = add_trace(p = p1,  x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = color_n), name = name_n, inherit = FALSE)
  
  return(model_fig)
}


plot_point <- function(df, energy, pre_key, model_fig = plot_ly(), b_name = '')
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  switch(as.character(pre_key),
    "1" = {
      pre_title = 'Pre'
      color_n = switch(as.character(energy), 'Elec' = 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
      },
    "3" ={
      pre_title = 'Post'
      color_n = 'rgba(109, 203, 15, 1)'
    }
    )

  name_n = paste(pre_title, energy)

  axis_title = switch(as.character(energy), 'Elec' = "Usage (kWh)", "Usage (BTU)")

  point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = color_n, size = 9), name = paste(name_n, 'Act'), inherit = FALSE)
  point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = color_n, size = 9), name = paste(name_n, 'Est'), inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = axis_title))

  return(point_fig)
}


params_list <- function(best_model, bdbid, energy, prepost)
{ 
  options(digits=15)
  model = best_model$model_type[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost]

  Ycp = best_model$ycp[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost]
  cp1 = best_model$xcp1[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost]
  cp2 = ifelse(model == '5P', best_model$xcp2[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost], 0)

  slope1 = ifelse(model == '3PC' | model == '2P', best_model$rs[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost], best_model$ls[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost])
  slope2 = ifelse(model == '4P' | model == '5P', best_model$rs[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost], 0)

  return(list(model = model, Ycp = Ycp, cp1 = cp1, cp2 = cp2, slope1 = slope1, slope2 = slope2))
}


params_matrix <- function(params_vec)
{ 
  options(digits=15)
  if (params_vec$slope2 == 0)
  {
    B = matrix(c(params_vec$Ycp, params_vec$slope1))
  }else
  {
    B = matrix(c(params_vec$Ycp, params_vec$slope1, params_vec$slope2))
  }
  return(B)
}

plot_timeseries <- function(util, energy)
{ 
  #plot timeseries. Unlike pplot_timeseries function, it also plots points and the shape of the points is now adjusted (Act or Est)
  options(digits=15)
  util$estimated = ifelse(util$estimated == 1, 'Est', ifelse(util$estimated == 2, 'Agg', 'Act'))

  util_act = subset(util, util$estimated == 'Act')
  util_est = subset(util, util$estimated == 'Est')

  ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "OAT"
  )

  switch(as.character(energy),
    'Elec' = {
      y_title = 'Usage(kWh/month)'
      usage_color = 'rgba(51, 113, 213, 1)'
      },
      {
        y_title = 'Usage(BTU/month)'
        usage_color = 'rgba(240, 24, 28,1)'
      }
    )

  p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = usage_color), name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = usage_color, size = 9), name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = usage_color, size = 9), name = 'Act') %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title= y_title), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )

  return(p)
}

savings_table <- function(uncertainty_df, savings_df, bdbid_n, energy){
  require(tools)
  if (bdbid_n %in% unique(savings_df$bdbid) & tolower(energy) %in% unique(savings_df$energy_type[savings_df$bdbid == bdbid_n]) ){
    savings_df = subset(savings_df, bdbid == bdbid_n & energy_type == tolower(energy))
    flag1 = TRUE
  }else
  {
    flag1 = FALSE
  }

  if (bdbid_n %in% unique(uncertainty_df$bdbid) & tolower(energy) %in% unique(uncertainty_df$energy_type[uncertainty_df$bdbid == bdbid_n]) )
  {
    uncertainty_df = subset(uncertainty_df, bdbid == bdbid_n & energy_type == tolower(energy))
    flag2 = TRUE
  }else
  {
    flag2 = FALSE
  }


  ps = ifelse(flag1, savings_df$savings[savings_df$calc_type == 'percent_savings']*100, NA)
  total_savings = ifelse(flag1, savings_df$savings[savings_df$calc_type == 'yearly_savings'], NA)

  moe = ifelse(flag2, uncertainty_df$savings_uncertainty[uncertainty_df$calc_type == 'moe_yr'], NA)
  psu = ifelse(flag2, uncertainty_df$savings_uncertainty[uncertainty_df$calc_type == 'psu']*100, NA)

  df = data.frame(ps = ps, total_savings = total_savings, moe = moe, psu = psu)
  colnames(df) = c('Percent Saving', 'Yearly Saving', 'Margin of Error', 'Percent Saving Uncertainty')
  return(df)
}


post_output <- function(post_df,  bdbid_n, energy)
{  
  options(digits=15)
  if (bdbid_n %in% unique(post_df$bdbid) & energy %in% unique(post_df$energy_type[post_df$bdbid == bdbid_n]))
  {
    post_df = subset(post_df, bdbid == bdbid_n & energy_type == energy)
    flag = TRUE
  }else
  {
    flag = FALSE
  }

  if (flag)
  {
    post_df = post_df[, c('model_type','prepost', 'heat_load', 'cool_load', 'total_consumption', 'percent_cooling', 'percent_heating', 'percent_baseload', 'cooling_change_point', 'heating_change_point')]
    if (length(post_df$prepost) == 2)
    {
      post_df$prepost = ifelse(post_df$prepost == 1, 'pre', 'post')
    }else
    {
      post_df$prepost = ifelse(post_df$prepost == 1, 'pre', 'post')
    }

    post_df = t(post_df)
    colnames(post_df) = as.vector(post_df['prepost',])
    post_df = post_df[!(rownames(post_df)%in%c('prepost')),]
  }else
  {
    post_df = data.frame(NULL)
  }

  return(post_df)
}


post_col <- function(post_df, n, energy)
{ 
  options(digits=15)
  if (length(post_df))
  {
    post_df = rbind(post_df, n)
    rownames(post_df) = c('Model Type','Estimated Heating', 'Estimated Cooling', 'Total Consumption', 'Percent Cooling', 'Percent Heating', 'Percent Baseload', 'Cooling Change-Point', 'Heating Change-Point', 'Total Points Observed')
    post_df = data.frame(post_df)
    if (energy == 'Elec')
    {
     post_df$Units = c('model_type', 'kWh', 'kWh', 'kWh', '%', '%', '%', 'F', 'F', 'Months')
    }else
    {
     post_df$Units = c('model_type', 'BTU', 'BTU', 'BTU', '%', '%', '%', 'F', 'F', 'Months')
    }
  }else
  {
    post_df = data.frame(NULL)
  }

  return(post_df)
}

stat_table <- function(best_model, bdbid_n, energy_n)
{ 
  options(digits=15)
  if (flag_func(best_model, bdbid_n, energy_n))
  { 
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'nac', 'r2', 'cv_rmse', 'heat_months', 'cool_months', 'n')]
  }else
  {
    df = data.frame(NULL)
  }
  return(df)
}


main_plot_model <- function(util, best_model, bdbid_n, energy, b_name = '')
{ 
  options(digits=15) 


  x1 = subset(util$OAT, util$prepost == 1)
  x3 = subset(util$OAT, util$prepost == 3)

  y1 = subset(util$usage, util$prepost == 1)
  y3 = subset(util$usage, util$prepost == 3)

  z1 = ifelse(subset(util$estimated, util$prepost == 1) == 1 , 'Est', 'Act')
  z3 = ifelse(subset(util$estimated, util$prepost == 3) == 1, 'Est', 'Act')

  df1 = data.frame(x = x1, y = y1, z = z1)
  df3 = data.frame(x = x3, y = y3, z = z3)


  if (flag_func(best_model, bdbid_n, energy))
  {   
      params_pre = params_list(best_model, bdbid_n, energy, 1)
      params_post = params_list(best_model, bdbid_n, energy, 3)

      B_pre = params_matrix(params_pre)
      B_post = params_matrix(params_post)

      pre_plot_model = plot_model(x1, params_pre$model, B_pre, params_pre$cp1, params_pre$cp2, energy,1, FALSE, plot_ly())
      post_plot_model = plot_model(x3, params_post$model, B_post, params_post$cp1, params_post$cp2, energy,3, FALSE, pre_plot_model)
      pre_point_fig = plot_point(df1, energy, 1,post_plot_model)
      final_figure = plot_point(df3, energy, 3, pre_point_fig, b_name)
  }else
  { 
    pre_point_fig = plot_point(df1, energy, 1, plot_ly())
    final_figure = plot_point(df3, energy, 3, pre_point_fig, b_name)
  }
  return(final_figure)
}


main_plot_baseload <- function(util, best_model, bdbid_n, energy, b_name = '')
{ 
  if (flag_func(best_model, bdbid_n, energy))
  {
      params_pre = params_list(best_model, bdbid_n, energy, 1)
      params_post = params_list(best_model, bdbid_n, energy, 3)

      B_pre = params_matrix(params_pre)
      B_post = params_matrix(params_post)

      adjust_fig = plot_baseload(util, energy, params_pre$model, B_pre, params_pre$cp1, params_pre$cp2, b_name)
  }else
  {
    adjust_fig = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy, 'for', b_name))
  }
  return(adjust_fig)
}


binfo_table <- function(binfo_df, bdbid_n)
{ 
  binfo_df = subset(binfo_df, bdbid == bdbid_n)
  location = paste(paste(binfo_df$city, binfo_df$state, sep = ', '), binfo_df$zip, sep =' ')
  b_id = paste("BDBID", binfo_df$bdbid, sep = ': ')
  binfo_df1 = t(data.frame(bdbid = b_id, name = binfo_df$building_name, address = binfo_df$address, location = location))
  binfo_df2 = t(data.frame(agency = binfo_df$oper_agency_acronym, primary = binfo_df$epapm_primary_function, area = binfo_df$total_bldg_gross_sq_ft))
  rownames(binfo_df2) = c('Agency', 'Primary Function', 'Gross Square Feet')
  return(list(binfo_df1 = binfo_df1, binfo_df2 = binfo_df2))
}

get_building_name <- function(binfo_df, bdbid_n)
{ 
  if (is.null(binfo_df))
  {
    plot_title = as.character(bdbid_n)
  }else
  {
    name = subset(binfo_df$building_name, binfo_df$bdbid == bdbid_n)
    plot_title = paste(bdbid_n, name, sep = ' - ')
  }
  return(plot_title)
}

stat_param_table <- function(best_model, bdbid_n, energy_n)
{ 
  if (flag_func(best_model, bdbid_n, energy_n))
  {
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'xcp1', 'xcp2', 'r2', 'cv_rmse', 'n')]
  }else
  {
    df = data.frame(NULL)
  }

  return(df)
}

params_table <- function(best_model, bdbid, energy)
{ 
  if(flag_func(best_model, bdbid, energy))
  {
    pl_pre = params_list(best_model, bdbid, energy, 1)
    pl_post = params_list(best_model, bdbid, energy, 3)
    df_pre = data.frame(prepost = '1', Model = as.character(pl_pre$model), Ycp = pl_pre$Ycp, cp1 = pl_pre$cp1, cp2 = pl_pre$cp2, slope1 = pl_pre$slope1, Slope2 = pl_pre$slope2)
    df_post = data.frame(prepost = '3', Model = as.character(pl_post$model), Ycp = pl_post$Ycp, cp1 = pl_post$cp1, cp2 = pl_post$cp2, slope1 = pl_post$slope1, Slope2 = pl_post$slope2)
    df = rbind(df_pre, df_post)
  }else
  {
    df = data.frame(NULL)
  }
  return(df)
}

help_table <- function()
{
  help_df = data.frame(c('Time Series', 'Parameter Model', 'Parameters', 'Stats', 'Post Modeller Table', 'Adjusted Baseline' ,'Adjusted Savings', 'Normalized Baseline', 'Normalized Savings'))
  help_df$Requirement = c('Utility CSV (a must)', 'Utility CSV and Best Model CSV', 'Utility CSV and Best Model CSV','Utility CSV and Best Model CSV', 'Utility CSV and Post Modeller Calcs CSV', 'Utility CSV and Best Model CSV','Adjusted Savings (and Utility)', 'Utility CSV', 'Normalized Savings (and Utility)')
  colnames(help_df) = c('Graph/Table', 'Requirement')
  return(help_df)
}

fixed_time <- function(util)
{ 
    if(!is.POSIXlt(util$end_date) & !is.POSIXt(util$end_date) & !is.POSIXct(util$end_date))
  { 
    if (grepl('/', util$end_date[1]))
    {
      util$end_date = strptime(util$end_date, format = "%m/%d/%Y")
    }else
    {
      util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
    }
  }

  util$end_date = as.factor(util$end_date)
  return(util)
}

missing_cols_handler <- function(cols, df)
{ 
  org_col = colnames(df)
  col_null = cols[!(cols %in% org_col)]
  df[,col_null] = NA
  return(df)
}

construct_saving_table <- function(saving_df, energy, bdbid_n, type_flag)
{ 
  if(is.null(saving_df))
  {
    return(NULL)
  }

  name = paste(type_flag, 'percent_savings', sep = '_')
  saving_df = subset(saving_df, saving_df$energy_type == energy & saving_df$bdbid == bdbid_n)
  unc_sav_col = paste(type_flag, 'percent_savings_uncertainty', sep = '_')
  total_sav_col = paste(type_flag, 'total_savings', sep = '_')

  unc_sav = .subset2(saving_df, unc_sav_col)[1]
  total_sav = .subset2(saving_df, total_sav_col)[1]
  per_sav = .subset2(saving_df, name)[1]

  if(is.null(per_sav))
  {
    per_sav = NA
  }

  moe = unc_sav*total_sav

  df = data.frame(per_sav*100, total_sav, moe, unc_sav)
  colnames(df) = c('Percent Savings','Total Savings', 'Margin of Error', 'Percent Saving Uncertainty')
  return(df)
}


plot_normalized_graph <- function(util, energy, b_name = '')
{ 
  util$end_date = strptime(util$end_date, format = "%Y-%m-%d")
  util$month = as.factor(month(util$end_date, label = TRUE, abbr = TRUE))

  util_pre = subset(util, util$prepost == 1)
  util_post = subset(util, util$prepost == 3)


  util_pre = util_pre[order(util_pre[,'month']),]
  util_post = util_post[order(util_post[,'month']),]

  pre_est = subset(util$estimated, !is.na(util$re))
  post_est = subset(util$estimated, !is.na(util$re))

  switch(as.character(energy),
    'Elec' = {
      y_title = 'Usage(kWh/sqft/month)'
      usage_color = 'rgba(51, 113, 213, 1)'
      },
      {
        y_title = 'Usage(BTU/sqft/month)'
        usage_color = 'rgba(240, 24, 28,1)'
      }
    )

  #pre_act = add_trace(p = plot_ly(), x = ~util_pre$month, y = ~util_pre$usage, type ='scatter', mode = 'lines', line = list(color = usage_color), name = "Pre Act Usage")
  #post_act = add_trace(p = pre_act, x = ~util_post$month, y = ~util_post$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(109, 203, 15, 1)'), name = "Post Act Usage")
  pre_plot = add_trace(p = plot_ly(), x = ~util_pre$month, y = ~util_pre$normalized_usage, type ='scatter', mode = 'lines', line = list(color = usage_color, dash = 'dash'), name = "Pre Norm Usage")
  post_plot = add_trace(p = pre_plot, x = ~util_post$month, y = ~util_post$normalized_usage, type ='scatter', mode = 'lines',
                        line = list(color = 'rgba(109, 203, 15, 1)', dash = 'dash'), name = "Post Normalized Usage") 
  pre_point = add_trace(p = post_plot, x = ~util_pre$month, y = ~util_pre$normalized_usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = usage_color, size = 9), name = 'Pre Norm point')
  post_point = add_trace(p = pre_point, x = ~util_post$month, y = ~util_post$normalized_usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(109, 203, 15, 1)', size = 9), name = 'Post Norm point') %>%
              layout(
                      title = b_name, yaxis = list(title= y_title), margin = list(b = 100),
                      xaxis = list(title="Date")
                    )

  return(post_point)
}
