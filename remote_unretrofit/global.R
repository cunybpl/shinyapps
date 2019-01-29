library(plotly)
library(rdrop2)

plot_model <- function(x, model, B, cp1, cp2, key, unit, bw, p1 = plot_ly())
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

  if (key == 'elec'){
    if (bw)
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Elec Model'), linetype = 2)
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)', dash = 'dash'), name = "Elec Model", inherit = FALSE)
    }else
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Elec Model'))
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)'), name = "Elec Model", inherit = FALSE)
    }
  }else
  { 
    if (bw)
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Fuel Model'), linetype = 2)
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)', dash = 'dash'), name = "Fuel Model", inherit = FALSE)
    }else
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Fuel Model'))
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)'), name = "Fuel Model", inherit = FALSE)
    }
  }
  return(model_fig)
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

plot_point <- function(x, y, z, key)
{ 
  options(digits=15)
  require(ggplot2)
  df = data.frame(x= x, y= y)
  if (key == 'elec'){
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'elec_act'))
  }else if (key == 'fuel')
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'fuel_act'))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'retofit_act'))
  }
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

params_table <- function(best_model, bdbid, energy)
{ 
  if(flag_func(best_model, bdbid, energy))
  {
    pl = params_list(best_model, bdbid, energy, 1)
    df = data.frame(Model = as.character(pl$model), Ycp = pl$Ycp, cp1 = pl$cp1, cp2 = pl$cp2, slope1 = pl$slope1, Slope2 = pl$slope2)
    df[,3:4] = as.character(round(df[,3:4], 2))
    if (df$Model == '3PC')
    {
      colnames(df)[5:6] = c('Cooling Sensitivity','Heating Sensitivity')
    }else
    {
      colnames(df)[5:6] = c('Heating Sensitivity','Cooling Sensitivity')
      if (energy == 'Elec')
      {
        colnames(df)[5:6] = c('Cooling Sensitivity','Heating Sensitivity')
      }
    }
    
    if(df$Model == '5P')
    {
      colnames(df)[3:4] = c('Heating Change-point', 'Cooling Change-point')
    }else if(df$Model == '2P')
    {
      df = df[,c(1,2,5,6)]
    }else
    {
      df = df[,c(1,2,3,5,6)]
      colnames(df)[3] = 'Change-point'
    }
  }else
  {
    df = data.frame(NULL)
  }
  return(df)
}


best_worst <- function(best_model, bdbid, energy, prepost)
{  
  if (is.null(best_model$best))
  {
    bw_bool = FALSE
  }else
  {
    bw = best_model$best[best_model$bdbid == bdbid & best_model$energy_type == energy & best_model$prepost == prepost]
    bw_bool = ifelse(as.character(bw) == 'rejected model', TRUE, FALSE)
  }
  return(bw_bool)
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

  options(digits=15)
  r1 = util$end_date[length(subset(util$end_date, util$prepost == 1))]
  ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "OAT"
  )
  if (energy == 'Elec')
  {
    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)'), name = "Usage") %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )
  }else
  {
    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)'), name = "Usage") %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )
  }

    return(p)
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
    post_df = post_df[, c('model_type', 'heat_load', 'cool_load', 'total_consumption', 'percent_cooling', 'percent_heating', 'percent_baseload', 'baseload', 'cooling_change_point', 'heating_change_point')]
    post_df = heat_cold_point(post_df, energy)
    post_df$baseload = post_df$total_consumption*post_df$percent_baseload
    post_df = t(post_df)
  }else
  {
    post_df = data.frame(NULL)
  }

  return(post_df)
}


heat_cold_point <- function(post_df, energy)
{ #adjust for heating and cooling point from post_df, used in post_output.
  model = as.character(post_df$model_type)
  if (model == '3PH')
  {
    post_df$cooling_change_point = 0
  }else if (model == '3PC')
  {
    post_df$heating_change_point = 0
  }else if (model == '4P')
  {
    if (energy == 'Elec')
    {
      post_df$heating_change_point = 0
    }else
    {
      post_df$cooling_change_point = 0
    }
  }
  return(post_df)
}


flag_fuel_oil <- function(breakdown_df, bdbid_n)
{
  if (!is.null(breakdown_df) & bdbid_n %in% breakdown_df$bdbid)
  { 
    flag = ifelse(subset(breakdown_df$site_fuel_no_2, breakdown_df$bdbid == bdbid_n) == 0, 1, 2)
  }else
  {
    flag = 0
  }
  return(flag)
}

post_col <- function(post_df, n, energy)
{  
  options(digits=15)
  if (length(post_df))
  {
    post_df = rbind(post_df, n)
    colnames(post_df) = c('Unretrofit')
    rownames(post_df) = c('Model Type','Estimated Heating', 'Estimated Cooling', 'Total Consumption', 'Percent Cooling', 'Percent Heating', 'Percent Baseload', 'Baseload','Cooling Change-point', 'Heating Change-point', 'Total Points Observed')
    post_df = data.frame(post_df)
    if (energy == 'Elec')
    {
      post_df$Units = c('model_type', 'kWh/sqft', 'kWh/sqft', 'kWh/sqft', '%', '%', '%', 'kWh/sqft','F', 'F', 'Months')
    }else
    {
      post_df$Units = c('model_type', 'BTU/sqft', 'BTU/sqft', 'BTU/sqft', '%', '%', '%', 'BTU/sqft','F', 'F', 'Months')
    }
  }else
  {
    post_df = data.frame(NULL)
  }
  
  return(post_df)
}

post_gross <- function(post_output_df, area, energy_n) # area = binfo_output_list()$binfo_df2['Gross Square Feet',]
{ 
  #mulitply estimated heating, estimated cooling and total consumption from post modeller with square feet from building info. The result is added as column to post_df modified by post_col
  post_output_df$Gross = ""
  post_output_df$Gross.Unit = ""
  post_output_df$Gross[2:4] = round(as.numeric(as.character(post_output_df$Unretrofit[2:4]))*as.numeric(area), 2)
  post_output_df$Gross[8] = round(as.numeric(as.character(post_output_df$Unretrofit[8]))*as.numeric(area), 2)
  if (energy_n == 'Elec')
  {
    post_output_df$Gross.Unit[2:4] = c('kWh','kWh','kWh')
    post_output_df$Gross.Unit[8] = c('kWh')
  }else
  {
    post_output_df$Gross.Unit[2:4] = c('BTU','BTU','BTU')
    post_output_df$Gross.Unit[8] = c('BTU')
  }
  post_output_df$Unretrofit = as.character(post_output_df$Unretrofit)
  post_output_df$Unretrofit[2:11] = as.character(round(as.numeric(post_output_df$Unretrofit[2:11]), 2))
  post_output_df$Unretrofit = prettyNum(post_output_df$Unretrofit, big.mark = ",", format = 'f')
  post_output_df$Gross[2:4] = prettyNum(post_output_df$Gross[2:4], big.mark = ",", format = 'f')
  post_output_df$Gross[8] = prettyNum(post_output_df$Gross[8], big.mark = ",", format = 'f')
  return(post_output_df)
}

stat_table <- function(best_model, bdbid_n, energy_n)
{ 
  if (flag_func(best_model, bdbid_n, energy_n))
  {
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'nac', 'r2', 'cv_rmse', 'heat_months', 'cool_months', 'n')]
    df[,6:7] = as.character(round(df[,6:7], 2))
  }else
  {
    df = data.frame(NULL)
  }

  return(df)
}


co2_numeric_rank <- function(co2eui_df, bdbid_n)
{ if (!is.null(co2eui_df) & bdbid_n %in% co2eui_df$bdbid)
  {
    site_numeric = subset(co2eui_df$site_eui_ft_numeric_rank, co2eui_df$bdbid == bdbid_n)
    source_numeric = subset(co2eui_df$source_eui_ft_numeric_rank, co2eui_df$bdbid == bdbid_n)
    co2_emissions = subset(co2eui_df$co2_emission_sq_ft_numeric_rank, co2eui_df$bdbid == bdbid_n)
    co2_numeric_table = data.frame(site = site_numeric, source = source_numeric, emissions = co2_emissions)
    colnames(co2_numeric_table) = c('Site EUI', 'Source EUI', 'CO2e kg/sqft')
    rownames(co2_numeric_table) = c('Numeric Ranking')
  }else
  {
    co2_numeric_table = data.frame()
  }
  return(co2_numeric_table)
}

co2_breakdown <- function(breakdown_df, bdbid_n)
{
  if (!is.null(breakdown_df) & bdbid_n %in% breakdown_df$bdbid)
  {
    breakdown = round(subset(breakdown_df$co2_emissions_metric_tons, breakdown_df$bdbid == bdbid_n),2)
    breakdown_table = data.frame(breakdown = breakdown)
    rownames(breakdown_table) = c('CO2e Emissions Metric Tons')
    breakdown_table[,1] = as.character(round(breakdown_table[,1], 2))
  }else
  {
    breakdown_table = data.frame()
  }

  return(breakdown_table)
}

percent_figure00 <- function(x)
{   
  #orginial
  x = x/100
  if (length(x) == 5)
  { bar_size = 20
    x1 <- c(0.25, 0.25, 0.25, 0.25, 0.25)
    x2 <- c(0.5, 0.5, 0.5, 0.5, 0.5)
    x3 <- c(0.25, 0.25, 0.25, 0.25,0.25)
    y = c('Baseload','Cooling Change-point', 'Heating Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
  }else
  { bar_size = 25
    x1 <- c(0.25, 0.25, 0.25, 0.25)
    x2 <-c(0.5, 0.5, 0.5, 0.5)
    x3 <- c(0.25, 0.25, 0.25, 0.25)
    y = c('Baseload','Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
  }
  q0 = add_trace(p = plot_ly(width = 700, height = 250), x = ~x, y = ~y, type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), name = 'Rank')
  q1 = add_trace(p = q0, x = ~x1, y = ~y, type = 'bar', marker = list(color = 'green', gradient = list(type = 'radial', color = 'orange')), name = 'Good')
  q2 = add_trace(p = q1, x = ~x2, y = ~y, type = 'bar', marker = list(color = 'orange', gradient = list(type = 'radial', color = 'red')), name = 'Average')
  q3 = add_trace(p = q2, x = ~x3, y = ~y, type = 'bar', marker = list(color = 'red',gradient = list(type = 'radial', color = 'red')), name = 'Bad') %>%
      layout(xaxis = list(title = "",showline = FALSE, showgrid = FALSE,showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "",showline = FALSE,showgrid = FALSE,showticklabels = FALSE,zeroline = FALSE),
          barmode = 'stack',legend = list(orientation = 'h'),margin = list(l = 120)) %>%
      add_annotations(xref = 'paper', yref = 'y', x = -0.007, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                            color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right')

  return(q3)
}

co2eui_figure <- function(x)
{ 
  x = x/100
  x1 <- c(0.25, 0.25, 0.25)
  x2 <-c(0.5, 0.5, 0.5)
  x3 <- c(0.25,0.25, 0.25)

  y = c('CO2e kg/sqft', 'Site EUI Rank', 'Source EUI Rank')
  q0 = add_trace(p = plot_ly(width = 700, height = 250), x = ~x, y = ~y, type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = 30, line = list(width = 3)), name = 'Rank')
  q1 = add_trace(p = q0, x = ~x1, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'green', gradient = list(type = 'veritcal')), width =0.8, name = 'Good')
  q2 = add_trace(p = q1, x = ~x2, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'orange'), width =0.8,name = 'Average')
  q3 = add_trace(p = q2, x = ~x3, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'red'), width =0.8,name = 'Bad') %>%
      layout(xaxis = list(title = "",showline = FALSE, showgrid = FALSE,showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "",showline = FALSE,showgrid = FALSE,showticklabels = FALSE,zeroline = FALSE),
          barmode = 'stack',legend = list(orientation = 'h'),margin = list(l = 120, b = 35, t = 55)) %>%
      add_annotations(xref = 'paper', yref = 'y', x = -0.007, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                            color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right')

  return(q3)
}

percent_get <- function(post_df, bdbid_n, energy)
{
  if (energy == 'Elec')
  {
      cp_per = post_df$cooling_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

  }else
  {
    cp_per = post_df$heating_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }
    
  heat_sen_per = post_df$heating_sensitivity_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  cool_sen_per = post_df$cooling_sensitivity_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  base_per = post_df$baseload_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

  if (post_df$model[post_df$bdbid == bdbid_n & post_df$energy_type == energy] == '5P')
  { 
    heat_cp_per = post_df$heating_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
    final_vec = c(base_per, cp_per, heat_cp_per, heat_sen_per, cool_sen_per)
  }else
  {
    final_vec = c(base_per, cp_per, heat_sen_per, cool_sen_per)
  }
  return(final_vec)
}

numeric_get <- function(post_df, bdbid_n, energy)
{ 
  if (energy == 'Elec')
    {
      cp_per = post_df$cooling_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }else
  {
    cp_per = post_df$heating_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }
    
  heat_sen_per = post_df$heating_sensitivity_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  cool_sen_per = post_df$cooling_sensitivity_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  base_per = post_df$baseload_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

  x = c(base_per, cp_per, heat_sen_per, cool_sen_per)
  return(numeric_post_table(x)) 
}

numeric_post_table <- function(x)
{ 
  df = data.frame(t(x))
  row.names(df) = c('Numeric Ranking')
  colnames(df) = c('Baseload', 'Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
  return(df)
}

per_num_func <- function(df, bdbid_n, energy)
{
  if (flag_func(df, bdbid_n, energy))
    {
      percent_vec = percent_get(df, bdbid_n, energy)
      percent_fig = percent_figure(percent_vec)

      numeric_df = numeric_get(df, bdbid_n, energy)
    }else
    {
      percent_fig = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No', energy, 'data points for Lean Anaylsis',sep = " "))
      numeric_df = data.frame(NULL)
    }

    return(list(numeric_df = numeric_df, percent_fig = percent_fig))
}


elec_fuel_graph_func <- function(temp_df, best_model, bdbid_n, b_name = "", height_ef = 455, width_ef = 850)
{
  util = subset(temp_df, temp_df$bdbid == bdbid_n)

  x1 = subset(util$OAT, util$energy_type == 'Elec')
  y1 = subset(util$usage, util$energy_type == 'Elec')
  temp_est_elec = subset(util$estimated, util$energy_type == 'Elec')
  if (length(temp_est_elec == 3))
  z1 = ifelse(temp_est_elec == 1, 'Est', ifelse(temp_est_elec == 2, 'Agg', 'Act'))

  #fuel
  x2 = subset(util$OAT, util$energy_type == 'Fuel')
  y2 = subset(util$usage, util$energy_type == 'Fuel')
  temp_est_fuel = subset(util$estimated, util$energy_type == 'Fuel')
  z2 = ifelse(temp_est_fuel == 1, 'Est', ifelse(temp_est_fuel == 2, 'Agg', 'Act'))

  df1 = data.frame(x = x1, y = y1, z = z1)
  df2 = data.frame(x = x2, y = y2, z = z2)

  params_elec = params_list(best_model, bdbid_n, 'Elec', 1)
  bw_elec = best_worst(best_model, bdbid_n, 'Elec', 1)
  B_elec = params_matrix(params_elec)

  params_fuel = params_list(best_model, bdbid_n, 'Fuel', 1)
  bw_fuel = best_worst(best_model, bdbid_n, 'Fuel', 1)
  B_fuel = params_matrix(params_fuel)

  df_btu = df1
  df_btu$y = df_btu$y *3412.14
  elec_model_btu_fig = plot_model(x1, params_elec$model, B_elec, params_elec$cp1, params_elec$cp2, 'elec', TRUE, bw = bw_elec, p1 = plot_ly(height = height_ef, width = width_ef))
  #elec_point_btu_fig = plot_point(x1, y1_btu, 'elec')
  elec_point_btu_fig = plot_point_3(df_btu, 'elec', elec_model_btu_fig, b_name)
  fuel_model_fig = plot_model(x2, params_fuel$model, B_fuel, params_fuel$cp1, params_fuel$cp2, 'fuel', FALSE, bw = bw_fuel, p1 = elec_point_btu_fig)
  #fuel_point_fig = plot_point(x2, y2,'fuel')
  fuel_point_fig = plot_point_3(df2,'fuel', fuel_model_fig, b_name)

  #color_fig_comb = scale_color_manual(values = c("Elec Model" = 'blue', 'Elec Consumption' = 'blue', 'Fuel Model' = 'red', 'Fuel Consumption' = 'red')) #create a new color scheme for combined energy
  #final_comb_figure = ggplot() + elec_model_btu_fig + elec_point_btu_fig + fuel_model_fig + fuel_point_fig + color_fig_comb + labs(x='Temperature',y='Usage (BTU)')
  final_comb_figure = fuel_point_fig
  return(final_comb_figure)
}

plot_point_2 <- function(df, key, model_fig = plot_ly(), b_name)
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (key == 'elec'){
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Elec Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Act', inherit = FALSE)
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (kWh)"))
  }else if (key == 'fuel')
  {
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Fuel Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Fuel Act', inherit = FALSE)
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Fuel Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (BTU)"))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'retofit_act'))
  }
  return(point_fig)
}


plot_point_3 <- function(df, key, model_fig = plot_ly(), b_name)
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (key == 'elec'){
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Elec Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Act', inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Elec Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (kWh/sqft/day)"))
  }else if (key == 'fuel')
  {
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Fuel Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Fuel Act', inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Fuel Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Fuel Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (BTU/sqft/day)"))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'retofit_act'))
  }
  return(point_fig)
}


plot_timeseries_2 <- function(util, energy)
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
  if (energy == 'Elec')
  {
    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)'), name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Act') %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage(kWh/sq/day)'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )
  }else
  {
    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'),  name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)'), name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Act') %>%
      layout(
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage(BTU/sqft/day)'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )
  }

  return(p)
}


co2_rank_get <- function(co2eui_df, bdbid_n)
{
  site_eui = subset(co2eui_df$site_eui_percent_rank, co2eui_df$bdbid == bdbid_n)
  source_eui = subset(co2eui_df$source_eui_percent_rank, co2eui_df$bdbid == bdbid_n)
  emission = subset(co2eui_df$co2_emissions_sq_ft_percent_rank, co2eui_df$bdbid == bdbid_n)
  return(c(emission, site_eui, source_eui))
}

fuel_oil_message <- function(flag)
{
  if (flag == 2)
  {
      "This facility has fuel oil."
  }else
  {
      ""
  }
}


main_plot <- function(temp_df, best_model, bdbid_n, energy, b_name = "")
{ 
  util_energy_type = subset(temp_df$energy_type, temp_df$bdbid == bdbid_n)
  if (energy %in% util_energy_type)
  { 
    #susbset by energy now
    util = subset(temp_df, bdbid == bdbid_n)
    x1 = subset(util$OAT, util$energy_type == energy)
    y1 = subset(util$usage, util$energy_type == energy)
    temp_est = subset(util$estimated, util$energy_type == energy)
    z1 = ifelse(temp_est== 1, 'Est', ifelse(temp_est == 2, 'Agg', 'Act'))

    df1 = data.frame(x = x1, y = y1, z = z1) #ready for point plot

    if (bdbid_n %in% unique(best_model$bdbid)) #bdbid is in best_model
    {
      best_energy_type = subset(best_model$energy_type, best_model$bdbid == bdbid_n)

      if(energy %in% best_energy_type) #energy and bdbid is in util and best
      {
        final_figure = main_line_point_plot(df1, best_model, bdbid_n, energy, b_name)
      }else #if FALSE, it means energy is not in best model so just point plot
      {
        final_figure = main_plot_point(df1, energy, b_name)
      }
    }else #if FALSE, it means bdbid does not make it to best_model so just point plot
    {
      final_figure = main_plot_point(df1, energy, b_name)
    }
  }else #if FALSE, it means energy is not in util frame so empty plot
  { 
    final_figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy, "for", b_name))
  }
  return(final_figure)
}

main_line_point_plot <- function(df1, best_model, bdbid_n, energy, b_name)
{
  params = params_list(best_model, bdbid_n, energy, 1)
    bw = best_worst(best_model, bdbid_n, energy, 1)
    B = params_matrix(params)
    x1 = df1$x

    if (energy == 'Elec')
    {   
        model_fig = plot_model(x1, params$model, B, params$cp1, params$cp2, 'elec', FALSE, bw = bw)
        #point_fig = plot_point(x1, y1, 'elec')
        #point_fig = plot_point_2(df1, 'elec')
        final_figure = plot_point_3(df1, 'elec', model_fig, b_name)
    }else
    {   
        model_fig = plot_model(x1, params$model, B, params$cp1, params$cp2, 'fuel', FALSE, bw = bw)
        #point_fig = plot_point(x1, y1, 'fuel')
        #point_fig = plot_point_2(df1, 'fuel')
        final_figure = plot_point_3(df1, 'fuel', model_fig, b_name)
    }
    return(final_figure)
}


main_plot_point <- function(df, energy, b_name)
{
  if(energy == 'Elec')
  {
    #point_fig = plot_point_2(df,'elec')
    #final_figure = ggplot() + point_fig + scale_color_manual(values = c('Elec Consumption' = 'blue'))+labs(x='Temperature',y='Usage (kWh)')
    final_figure = plot_point_3(df,'elec', b_name = b_name)
  }else
  {
    #point_fig = plot_point_2(df,'fuel')
    #final_figure = ggplot() + point_fig + scale_color_manual(values = c('Fuel Consumption' = 'red')) + labs(x='Temperature',y='Usage (BTU)')
    final_figure = plot_point_3(df,'fuel', b_name = b_name)
  }
  return(final_figure)
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

binfo_table <- function(binfo_df, bdbid_n)
{  
  if(!(bdbid_n %in% binfo_df$bdbid))
  {
    return(NULL)
  }
  binfo_df = subset(binfo_df, bdbid == bdbid_n)
  location = paste(paste(binfo_df$city, binfo_df$state, sep = ', '), binfo_df$zip, sep =' ')
  b_id = paste("BDBID", binfo_df$bdbid, sep = ': ')
  binfo_df1 = t(data.frame(bdbid = b_id, name = binfo_df$building_name, address = binfo_df$address, location = location))
  binfo_df2 = t(data.frame(agency = binfo_df$oper_agency_acronym, primary = binfo_df$epapm_primary_function, area = binfo_df$total_bldg_gross_sq_ft))
  rownames(binfo_df2) = c('Agency', 'Primary Function', 'Gross Square Feet')
  return(list(binfo_df1 = binfo_df1, binfo_df2 = binfo_df2))
}


help_table <- function()
{
  help_df = data.frame(c('Time Series', 'Building Information','Parameter Model', 'Parameters','Stats', 'Post Modeller Table', 'Elec Vs Fuel' ,'Lean Anaylsis Percent Ranking', 'Lean Anaylsis Numeric Ranking', 'CO2 EUI Percent Ranking', 'CO2e Emissions Metric Tons and Fuel Oil Notification Message', 'Multi Elec and Multi Fuel'))
  help_df$Requirement = c('Utility CSV (a must)','Utility CSV and Building Info CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Post Modeller CSV', 'Building with two different energy types ','Utility CSV and Post Modeller CSV','Utility CSV and Post Modeller CSV', 'CO2 EUI Ranking CSV', 'CO2e Breakdown CSV', 'Utility CSV, Best Model CSV/All Model CSV and Building Info CSV(optional)')
  colnames(help_df) = c('Graph/Table', 'Requirement')
  return(help_df)
}


aggregate_func <- function(df)
{ 
  for (i in 2:nrow(df))
  { 
    if (df$estimated[i] == 0 & df$estimated[i-1] == 1)
    {
      df$usage[i] = (df$usage[i] + df$usage[i-1])/2
      df$usage[i-1] = df$usage[i]
      df$estimated[i] = 2
      df$estimated[i-1] = 2
    }
  }
  return(df)
}

get_dropdown_info_2 <- function(dir_path = 'shiny/output', new_user_flag = FALSE)
{
  drop_auth(rdstoken = 'droptoken.rds')
  parent_path = drop_dir('shiny/output')$path_lower
  cat_df = data.frame(parent_path = parent_path)
  child_vec = c()
  for (i in parent_path)
  {
    child_folder = drop_dir(i)
    child_vec = c(child_vec, child_folder$name)
  }
  cat_df$category = child_vec
  cat_df$child_path = paste(cat_df$parent_path, cat_df$category, sep = '/')
  return(cat_df)
}

get_dd_period <- function(cat_df, category_n)
{
  path = subset(cat_df$child_path, cat_df$category == category_n)
  p_df = data.frame(p0 = drop_dir(path)$name)
  p_df = data.frame(p0 = sort(p_df$p0, decreasing = FALSE))

  p_vec = c()
  for (i in c(1:nrow(p_df)))
  { 
    temp_vec = strsplit(as.character(p_df$p0), "period")[[i]][2]
    p_vec = c(p_vec,temp_vec)
  }
  p_df$p1 = p_vec

  p_df$path = paste(path,p_df$p0,sep='/')
  return(p_df)
}


get_dd_year <- function(p_df, point)
{
  path_point = subset(p_df$path, p_df$p1 == point)
  fy_df = data.frame(fy0 = drop_dir(path_point)$name)
  fy_df$path = paste(path_point,fy_df$fy0,sep='/')
  return(fy_df)
}

get_dropdown_info <- function(dir_path, new_user_flag = FALSE)
{  
  drop_auth(rdstoken = 'droptoken.rds')

  file_list_df = data.frame(drop_dir(dir_path)) #list of files in shiny dropbox folder
  init_file = subset(file_list_df$name, grepl('utility', file_list_df$name)) #give you list of files that contain 'utility'
  dropdown_df = data.frame(strsplit(init_file, '_')) #spliting files name to information of category and stuff, and converting list to data frame
  colnames(dropdown_df) = NULL

  dropdown_df = data.frame(category = t(dropdown_df[2,]), year = t(dropdown_df[3,]), point = t(dropdown_df[4,]))
  colnames(dropdown_df) = c('category', 'year', 'point')
  dropdown_df$point_p = gsub('.csv', '', dropdown_df$point)
  dropdown_df$point_p = gsub('p', '', dropdown_df$point_p)
  dropdown_df$year_p = gsub('fy', '', dropdown_df$year)
  return(dropdown_df)
}


sqft_message <- function(sqft_col)
{ 
  sqft_vec = unique(sqft_col)
  if (length(sqft_vec) == 1)
  {
    message = switch(as.character(sqft_vec),
      "1" = "Energy usage is normalized by sqft.",
      "0" = "Energy usage is not normalized by sqft.",
      "No information avaliable for this facility regarding energy usage being normalized by sqft."
      )
  }else
  {
    message = "Houston, we have problem here. Man I am funny."
  }
  return(message)
}


missing_cols_handler <- function(cols, df)
{ 
  org_col = colnames(df)
  col_null = cols[!(cols %in% org_col)]
  df[,col_null] = NA
  return(df)
}


check_sqft_na <- function(df)
{
  if(!is.null(df))
  {
    if(is.na(df['Gross Square Feet',]))
    {
      flag = FALSE
    }else
    {
      flag = TRUE
    }
  }else
  {
    flag = FALSE
  }
  return(flag)
}

prepare_lean_rank <- function(x)
{ 
  df = data.frame(percent = x)
  df$color = NA
  df$color[df$percent <= 20] <- 'rgba(0,178,15,1)'
  df$color[df$percent > 20 & df$percent <= 40] <- 'rgba(201,204,0,1)'
  df$color[df$percent > 40 & df$percent <= 60] <- 'rgba(255,189,0,1)'
  df$color[df$percent > 60 & df$percent <= 80] <- 'rgba(255,115,0,1)'
  df$color[df$percent > 80] <- 'rgba(178,36,0,1)'
  df$color[is.na(df$percent)] <- 'rgba(119,121,124,1)'
  if (length(x) == 4)
  {
    df$x_vec = c('Baseload','Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    df$y_vec = c(1,1,1,1)
  }else if(length(x) == 5)
  {
    df$x_vec = c('Baseload','Cooling Change-point', 'Heating Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    df$y_vec = c(1,1,1,1,1)
  }else
  {
    df$x_vec = c('CO2e kg/sqft', 'Site EUI Rank', 'Source EUI Rank')
    df$y_vec = c(1,1,1)
  }
  df$percent_text = as.character(ceiling(df$percent))
  df$percent_text[is.na(df$percent_text)] <- 'NA'
  return(df)
}

percent_figure0 <- function(x)
{ 
  #second edition
  df = prepare_lean_rank(x)  
  r_n = 0
  domain_n = c(0,1)
  switch(as.character(nrow(df)),
      "3" = 
      {
        domain_n = c(0.15,1)
        width_n = 500
      },
      "4" =
      {
        width_n = 800
        r_n = 50
      },
      "5" = 
      {
        width_n = 850
      }
  )


  t <- list(
  family = "arial",
  size = 26,
  color = 'white')

  p = add_trace(p = plot_ly(width = width_n, height = 200), x = ~df$x_vec, y = ~df$y_vec,type = 'scatter', mode = 'markers', marker = list(color = df$color, size = 100)) %>%
      layout(showlegend = FALSE, xaxis = list(title = "", tickfont = list(size = 14),
          showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      zeroline = FALSE,
                      domain = domain_n),
              yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
              margin = list(l = 0, r = r_n, b= 35)
              )
  p1= add_text(p = p,  x = ~df$x_vec, y = ~df$y_vec, text = ~df$percent_text, textfont = t, textposition = "middle center")
  return(p1)
}

percent_figure <- function(x)
{
  x = x/100
  m1 = matrix(data = seq(0,1,length.out = 1000), nrow = 1, ncol = 1000)
  x1 = seq(0,1,length.out = 1000)

  len_key = length(x)
  height_n = 300

  if (len_key == 5)
  {
    y = c('Baseload','Cooling Change-point', 'Heating Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    bar_size = 20
    margin_l = 160
    width_n = 750
  }else if(len_key == 4)
  {
    y = c('Baseload','Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    margin_l = 135
    bar_size = 26
    width_n = 700
  }else
  { 
    width_n = 700
    bar_size = 27
    margin_l = 120
    height_n = 250
    y = c('CO2e kg/sqft', 'Site EUI Rank', 'Source EUI Rank')
  }

  p1 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[1], z = m1, colors = colorRamp(c("green", "yellow", "yellow", "red")), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p1 = add_trace(p = p1, x = ~x[1], y= ~y[1], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[1]) 

  p2 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[2], z = m1, colors = colorRamp(c("green", "yellow", "yellow", "red")), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p2 = add_trace(p = p2, x = ~x[2], y= ~y[2], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[2])

  p3 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[3], z = m1, colors = colorRamp(c("green", "yellow", "yellow", "red")), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p3 = add_trace(p = p3, x = ~x[3], y= ~y[3], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[3])

  if (len_key >= 4)
  { 
    p4 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[4], z = m1, colors = colorRamp(c("green", "yellow", "yellow", "red")), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
    p4 = add_trace(p = p4, x = ~x[4], y= ~y[4], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[4])

    if(len_key == 4)
    {
      p = subplot(p1,p2,p3,p4, nrows = 4, heights = c(0.2,0.22,0.22,0.2), shareX = TRUE, shareY = FALSE) %>% layout(xaxis = list(title ="", tickmode = 'array', tickvals = c(0.125,0.5,0.875), ticktext = c('Good','Average','Poor')))
    }else
    {
      p5 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[5], z = m1, colors = colorRamp(c("green", "yellow", "yellow", "red")), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
      p5 = add_trace(p = p5, x = ~x[5], y= ~y[5], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[5])

      p = subplot(p1,p2,p3,p4,p5, nrows = 5, heights = c(0.15,0.17,0.17,0.17,0.15), shareX = TRUE, shareY = FALSE) %>% layout(xaxis = list(title ="", tickmode = 'array', tickvals = c(0.125,0.5,0.875), ticktext = c('Good','Average','Poor')))
    }
  }else
  {
    p = subplot(p1,p2,p3, nrows = 3, heights = c(0.25,0.25,0.25), shareX = TRUE, shareY = FALSE) %>% layout(xaxis = list(title ="", tickmode = 'array', tickvals = c(0.125,0.5,0.875), ticktext = c('Good','Average','Poor')))
  }

  return(p)
}


construct_bdbid_name_func <- function(temp_df, binfo_df)
{
  bdbid_vec = unique(temp_df$bdbid)
  temp_index = match(bdbid_vec, binfo_df$bdbid)
  bdbid_na = bdbid_vec[is.na(temp_index)] ##bdbid that are not in building information
  temp_index = temp_index[!is.na(temp_index)] #remove missing bdbid

  b_name = binfo_df$building_name[temp_index]
  b_id = binfo_df$bdbid[temp_index]
  comb = paste(b_id, b_name, sep = ' - ')
  b_df = data.frame(bdbid = b_id, name = comb)

  if(length(bdbid_na))
  {
    b_na_df = data.frame(bdbid = bdbid_na, name = as.character(bdbid_na))
    b_df = rbind(b_df, b_na_df)
  }

  return(b_df)
}

building_comparison_graph <- function(df, binfo)
{ 
  b_name = subset(binfo$name, binfo$bdbid %in% df$bdbid)
  df =  subset(df, df$bdbid %in% binfo$bdbid)
  print(length(b_name))
  print(length(df$bdbid))
  p_init = plot_ly()
  p = add_trace(p = p_init, x = df$sq_ft, y = df$site_eui, type ='scatter', mode ='markers', text = b_name, hoverinfo = 'x+y+text',
    marker = list(symbol = 'circle', color = 'rgba(176,220,175,1)', size = 9, line = list(color = 'rgba(164,165,164,1)', width = 1)), name = 'blah', inherit = FALSE) %>%
  layout(title = 'Buliding Comparison',
    xaxis = list(title = "Property Size (sqft)",
                      showticklabels = TRUE,
                      zeroline = TRUE, rangemode = 'tozero'),
         yaxis = list(title = "EUI (kBTU/sqft)",
                      showticklabels = TRUE,
                      zeroline = TRUE, rangemode = 'tozero'))
  return(p)
}

