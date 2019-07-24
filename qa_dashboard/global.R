library(plotly)

#x_vec = c(min(OAT), max(OAT))
fitline_func <- function(x_vec, model_params){
  switch(as.character(model_params$model_type),
      "5P" = {
        x = c(x_vec, model_params$xcp1, model_params$xcp2)
        return(fit_5p_func(x, model_params))
      },
      "4P" = {
        x = c(x_vec, model_params$xcp1)
        return(fit_4p_func(x, model_params))
      },
      "3PH" = {
        x = c(x_vec, model_params$xcp1)
        return(fit_3ph_func(x, model_params))
      },
      "3PC" = {
        x = c(x_vec, model_params$xcp1)
        return(fit_3pc_func(x, model_params))
      },
      "2P" = {
        return(fit_2p_func(x_vec, model_params))
      }
  )
}

fit_5p_func <- function(x, model_params){
  yInter_1 = model_params$ycp - model_params$ls*model_params$xcp1
  yInter_2 = model_params$ycp - model_params$rs*model_params$xcp2
  estimated = ifelse(x <= model_params$xcp1, yInter_1 + model_params$ls*x, ifelse(x >= model_params$xcp2, yInter_2 + model_params$rs*x, model_params$ycp))
  estimated = estimated[order(x)]
  x = x[order(x)]
  return(list(x=x, y=estimated))
}

fit_4p_func <- function(x, model_params){
  yInter_1 = model_params$ycp - model_params$ls*model_params$xcp1
  yInter_2 = model_params$ycp - model_params$rs*model_params$xcp1
  estimated =ifelse(x <= model_params$xcp1, yInter_1 + model_params$ls*x, yInter_2 + model_params$rs*x)
  estimated = estimated[order(x)]
  x = x[order(x)]
  return(list(x=x, y=estimated))
}

fit_3ph_func <- function(x, model_params){
  yInter_1 = model_params$ycp - model_params$ls*model_params$xcp1
  estimated = ifelse(x <= model_params$xcp1, yInter_1 + model_params$ls*x, model_params$ycp)
  estimated = estimated[order(x)]
  x = x[order(x)]
  return(list(x=x, y=estimated))
}


fit_3pc_func <- function(x, model_params){
  yInter_1 = model_params$ycp - model_params$rs*model_params$xcp1
  estimated = ifelse(x > model_params$xcp1, yInter_1 + model_params$rs*x, model_params$ycp)
  estimated = estimated[order(x)]
  x = x[order(x)]
  return(list(x=x, y=estimated))
}

fit_2p_func <- function(x, model_params){
  yInter_1 = model_params$xcp2 - model_params$rs*model_params$xcp1
  estimated = model_params$ycp + model_params$rs*x
  return(list(x=x, y=estimated))
}

plot_model <- function(x, y, energy, pre_key, p1 = plot_ly(), retrofit = FALSE)
{
  switch(as.character(pre_key),
    "1" = {
      pre_title = ifelse(retrofit, 'Pre', '')
      color_n = switch(as.character(energy), 'Elec' = 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
      },
    "3" ={
      pre_title = 'Post'
      color_n = 'rgba(109, 203, 15, 1)'
      }
  )

  name_n = paste(pre_title, energy, 'Model')
  model_fig = add_trace(p = p1,  x = ~x, y = ~y, type ='scatter', mode = 'lines', line = list(color = color_n), name = name_n, inherit = FALSE)
  return(model_fig)
}

plot_point <- function(df, energy, pre_key, model_fig = plot_ly(), retrofit = FALSE, b_name = '')
{
  util_act = subset(df, df$estimated == 1)
  util_est = subset(df, df$estimated == 0)

  switch(as.character(pre_key),
    "1" = {
      pre_title = ifelse(retrofit, 'Pre', '')
      color_n = switch(as.character(energy), 'Elec' = 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
      },
    "3" ={
      pre_title = 'Post'
      color_n = 'rgba(109, 203, 15, 1)'
    }
    )

  name_n = paste(pre_title, energy)

  axis_title = switch(as.character(energy), 'Elec' = "Usage (kWh)", "Usage (BTU)")

  point_fig_act = add_trace(p = model_fig, x = ~util_act$oat, y = ~util_act$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = color_n, size = 9), name = paste(name_n, 'Act'), inherit = FALSE)
  point_fig = add_trace(p = point_fig_act, x = ~util_est$oat, y = ~util_est$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = color_n, size = 9), name = paste(name_n, 'Est'), inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100), plot_bgcolor='rgb(240, 242, 247)', paper_bgcolor='rgb(240, 242, 247)',
      xaxis = list(title = "Temperature"),
      yaxis = list(title = axis_title))

  return(point_fig)
}

#just pass necessary frames, handle both baseline and retrofit, unique values of energy and bdbid; handle no model, just pass empty frame
main_param_plot <- function(utility, best_model, b_name = ''){
  best_row = as.character(nrow(best_model))
  print('best_row')
  print(best_row)
  print(class(best_row))
  print(best_model)
  switch(best_row,
    "0" = { #only points
      energy = unique(utility$energy_type)
      prepost = unique(utility$prepost)

      if(length(prepost) == 1){ #baseline
        fig = plot_point(utility, energy, prepost, model_fig = plot_ly(), retrofit=FALSE, b_name = b_name)
      }else{ #retrofit
        pre_util = subset(utility, utility$prepost == 1)
        pre_fig = plot_point(pre_util, energy, 1, model_fig = plot_ly(), retrofit=TRUE, b_name = b_name)

        post_util = subset(utility, utility$prepost == 3)
        fig = plot_point(post_util, energy, 3, model_fig = pre_fig, retrofit=TRUE, b_name = b_name)
      }
    },
    "1" = { #baseline
      fig = general_plot_func(utility, best_model, p1 = plot_ly(), retrofit=FALSE, b_name = b_name)
    },
    "2" = { #retrofit
      pre_util = subset(utility, utility$prepost == 1)
      pre_best = subset(best_model, best_model$prepost == 1)
      pre_fig = general_plot_func(pre_util, pre_best, p1 = plot_ly(), retrofit=TRUE, b_name = b_name)

      post_util = subset(utility, utility$prepost == 3)
      post_best = subset(best_model, best_model$prepost == 3)
      fig = general_plot_func(post_util, post_best, p1 = pre_fig, retrofit=TRUE, b_name = b_name)
    }
  )
  return(fig)
}

#only take frames with one unique value of energy, bdbid, prepost
general_plot_func <- function(utility, best_model, p1 = plot_ly(), retrofit=FALSE, b_name =''){
  x_vec = c(min(utility$oat, na.rm=TRUE), max(utility$oat, na.rm=TRUE))
  fit_out = fitline_func(x_vec, best_model)
  model_fig = plot_model(fit_out$x, fit_out$y, best_model$energy_type, best_model$prepost, p1=p1, retrofit=retrofit)
  final_fig = plot_point(utility, best_model$energy_type, best_model$prepost, model_fig = model_fig, retrofit=retrofit, b_name = b_name)
  return(final_fig)
}

flag_func <- function(df, bdbid_n, energy)
{
  if(is.null(df))
  {
    return(FALSE)
  }

  if (bdbid_n %in% unique(df$bdbid) & energy %in% df$energy_type[df$bdbid == bdbid_n])
  {
    flag = TRUE
  }else
  {
    flag = FALSE
  }
  return(flag)
}

percent_heat_cool_func <- function(df)
{
  options(digits=15)
  df$percent_cooling = 100*df$cool_load/df$total_consumption
  df$percent_heating = 100*df$heat_load/df$total_consumption
  df$percent_baseload = 100 - df$percent_cooling - df$percent_heating
  df$baseload = df$total_consumption*df$percent_baseload
  return(df)
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

params_table <- function(best_model, bdbid, energy, pre_key)
{
  if(flag_func(best_model, bdbid, energy))
  {
    pl = params_list(best_model, bdbid, energy, pre_key)
    df = data.frame(Model = as.character(pl$model), Prepost = pre_key, Ycp = pl$Ycp, cp1 = pl$cp1, cp2 = pl$cp2, slope1 = pl$slope1, Slope2 = pl$slope2)
  }else
  {
    df = data.frame(NULL)
  }
  return(df)
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

post_output_retrofit <- function(post_df,  bdbid_n, energy)
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
    if('site_fuel_no_2' %in% colnames(breakdown_df))
    {
      flag = ifelse(subset(breakdown_df$site_fuel_no_2, breakdown_df$bdbid == bdbid_n) == 0, 1, 2)
    }else
    {
      flag = 0
    }
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
      post_df$Units = c('model_type', 'kWh/sqft/day', 'kWh/sqft/day', 'kWh/sqft', '%', '%', '%', 'kWh/sqft','F', 'F', 'Months')
    }else
    {
      post_df$Units = c('model_type', 'BTU/sqft/day', 'BTU/sqft/day', 'BTU/sqft', '%', '%', '%', 'BTU/sqft','F', 'F', 'Months')
    }
  }else
  {
    post_df = data.frame(NULL)
  }

  return(post_df)
}

post_col_retrofit <- function(post_df, n, energy)
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

post_gross <- function(post_output_df, area, energy_n) # area = binfo_output_list()$binfo_df2['Gross Square Feet',]
{
  #mulitply estimated heating, estimated cooling and total consumption from post modeller with square feet from building info. The result is added as column to post_df modified by post_col
  post_output_df$Gross = ""
  post_output_df$Gross.Unit = ""
  post_output_df$Gross[2:4] = round(as.numeric(as.character(post_output_df$Unretrofit[2:4]))*as.numeric(area), 2)
  post_output_df$Gross[8] = round(as.numeric(as.character(post_output_df$Unretrofit[8]))*as.numeric(area), 2)
  if (energy_n == 'Elec')
  {
    post_output_df$Gross.Unit[2:4] = c('kWh/day','kWh/day','kWh')
    post_output_df$Gross.Unit[8] = c('kWh/day')
  }else
  {
    post_output_df$Gross.Unit[2:4] = c('BTU/day','BTU/day','BTU')
    post_output_df$Gross.Unit[8] = c('BTU/day')
  }
  post_output_df$Unretrofit = as.character(post_output_df$Unretrofit)
  post_output_df$Unretrofit[2:11] = as.character(round(as.numeric(post_output_df$Unretrofit[2:11]), 2))
  post_output_df$Unretrofit = format(post_output_df$Unretrofit, na.encode = TRUE, justify = 'none')
  post_output_df$Unretrofit = prettyNum(post_output_df$Unretrofit, big.mark = ",", format = 'f')
  post_output_df$Gross[2:4] = prettyNum(post_output_df$Gross[2:4], big.mark = ",", format = 'f')
  if(!is.na(post_output_df$Gross[8]))
  {
    post_output_df$Gross[8] = prettyNum(post_output_df$Gross[8], big.mark = ",", format = 'f')
  }
  return(post_output_df)
}

stat_table <- function(best_model, bdbid_n, energy_n)
{
  if (flag_func(best_model, bdbid_n, energy_n))
  {
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'nac', 'r2', 'cv_rmse', 'heat_months', 'cool_months', 'period')]
  }else
  {
    df = data.frame(NULL)
  }

  return(df)
}

co2_numeric_rank <- function(co2eui_df, bdbid_n)
{ if (!is.null(co2eui_df) & bdbid_n %in% co2eui_df$bdbid)
  {
    site_numeric = subset(co2eui_df$site_eui_numeric_rank, co2eui_df$bdbid == bdbid_n)
    source_numeric = subset(co2eui_df$source_eui_numeric_rank, co2eui_df$bdbid == bdbid_n)
    site_emission = subset(co2eui_df$co2emissions_kg_sqft_site_numeric_rank, co2eui_df$bdbid == bdbid_n)
    source_emission = subset(co2eui_df$co2emissions_kg_sqft_source_numeric_rank, co2eui_df$bdbid == bdbid_n)

    co2_numeric_table = data.frame(x = c(site_numeric, site_emission, source_numeric, site_emission))
    rownames(co2_numeric_table) = c('Site EUI', 'Source EUI', 'CO2e kg/sqft')
    rownames(co2_numeric_table) = c('Numeric Ranking')
  }else
  {
    co2_numeric_table = data.frame()
  }
  return(co2_numeric_table)
}

co2_breakdown <- function(breakdown_df, bdbid_n)
{
  name = 'CO2e Emissions Metric Tons'
  if (!is.null(breakdown_df) & bdbid_n %in% breakdown_df$bdbid)
  {
    if('co2_emissions_metric_tons' %in% colnames(breakdown_df))
    {
      breakdown = round(subset(breakdown_df$co2_emissions_metric_tons, breakdown_df$bdbid == bdbid_n),2)
    }else
    {
      breakdown = round(subset(breakdown_df$co2emissions_kg_sqft_site, breakdown_df$bdbid == bdbid_n),2)
      name = 'Site CO2e Emissions(kg/sqft)'
    }
    breakdown_table = data.frame(breakdown = breakdown)
    rownames(breakdown_table) = c(name)
  }else
  {
    breakdown_table = data.frame()
  }

  return(breakdown_table)
}

co2_value_get <- function(breakdown_df, bdbid_n)
{

  if(is.null(breakdown_df))
  {
    return(data.frame())
  }

  if (bdbid_n %in% breakdown_df$bdbid)
  {
     site_eui = round(subset(breakdown_df$site_eui, breakdown_df$bdbid == bdbid_n),2)
     source_eui = round(subset(breakdown_df$source_eui, breakdown_df$bdbid == bdbid_n),2)

     site_emission = round(subset(breakdown_df$co2emissions_kg_sqft_site, breakdown_df$bdbid == bdbid_n), 2)
     source_emission = round(subset(breakdown_df$co2emissions_kg_sqft_source, breakdown_df$bdbid == bdbid_n), 2)
     df = data.frame(site_eui, site_emission, source_eui, source_emission)
     colnames(df) = c('Site EUI', 'Site CO2e kg/sqft', 'Source EUI', 'Source CO2e kg/sqft')
     return(df)
  }else
  {
    return(data.frame())
  }
}

percent_figure0 <- function(x)
{
  x = x*100
  x1 <- c(25, 25, 25, 25)
  x2 <-c(50, 50, 50, 50)
  x3 <- c(25, 25, 25, 25)

  y = c('Baseload','Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
  q <- plot_ly() %>%
  add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = 25, line = list(width = 3)), name = 'Rank')%>%
  add_trace(x = ~x1, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'green', gradient = list(type = 'veritcal')), name = 'Good')%>%
  add_trace(x = ~x2, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'orange'), name = 'Average') %>%
  add_trace(x = ~x3, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'red'), name = 'Bad') %>%
    layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         margin = list(l = 120, r = 10, t = 140, b = 80),
         legend = list(orientation = 'h')) %>%
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                            color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right')

  return(q)
}

percent_get <- function(post_df, bdbid_n, energy, model)
{

  if (energy == 'Elec' & model != '5P')
  {
      cp_per = post_df$cooling_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }else if (energy == 'Fuel' & model != '5P')
  {
    cp_per = post_df$heating_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }

  heat_sen_per = post_df$heating_sensitivity_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  cool_sen_per = post_df$cooling_sensitivity_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  base_per = post_df$baseload_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

  if (model == '5P')
  {
    heat_cp_per = post_df$heating_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
    cool_cp_per = post_df$cooling_change_point_percent_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
    return(c(base_per, cool_cp_per, heat_cp_per, heat_sen_per,cool_sen_per))
  }

  return(c(base_per, cp_per, heat_sen_per, cool_sen_per))
}

numeric_get <- function(post_df, bdbid_n, energy, model)
{

  if (energy == 'Elec' & model != '5P')
    {
      cp_per = post_df$cooling_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }else if (energy == 'Fuel' & model != '5P')
  {
    cp_per = post_df$heating_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  }

  heat_sen_per = post_df$heating_sensitivity_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  cool_sen_per = post_df$cooling_sensitivity_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
  base_per = post_df$baseload_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

  if (model == '5P')
  {
    cool_cp_per = post_df$cooling_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]
    heat_cp_per = post_df$heating_change_point_numeric_rank[post_df$bdbid == bdbid_n & post_df$energy_type == energy]

    x = c(base_per, cool_cp_per, heat_cp_per, heat_sen_per,cool_sen_per)
    return(numeric_post_table(x))
  }

  x = c(base_per, cp_per, heat_sen_per, cool_sen_per)
  return(numeric_post_table(x))
}

numeric_post_table <- function(x)
{
  df = data.frame(t(x))
  row.names(df) = c('Numeric Ranking')
  if(length(x) == 4)
  {
    colnames(df) = c('Baseload', 'Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
  }else
  {
    colnames(df) = c('Baseload', 'Cooling Change-point', 'Heating Change-point','Heating Sensitivity', 'Cooling Sensitivity')
  }
  return(df)
}

per_num_func <- function(df, bdbid_n, energy)
{
  if (flag_func(df, bdbid_n, energy))
    {
      df = subset(df, df$bdbid == bdbid_n & df$energy_type == energy)
      model = df$model_type

      percent_vec = percent_get(df, bdbid_n, energy, model)
      percent_fig = percent_figure(percent_vec)

      numeric_df = numeric_get(df, bdbid_n, energy, model)
    }else
    {
      percent_fig = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy, sep = " "), paper_bgcolor='rgb(240, 242, 247)', plot_bgcolor='rgb(240, 242, 247)')
      numeric_df = data.frame(NULL)
    }

    return(list(numeric_df = numeric_df, percent_fig = percent_fig))
}


elec_fuel_graph_func <- function(temp_df, best_model, bdbid_n, b_name = "", height_ef = 455, width_ef = 850)
{
    util = subset(temp_df, temp_df$bdbid == bdbid_n)

    p = plot_ly()
    for(energy in c('Elec', 'Fuel')){
      x1 = subset(util$oat, util$energy_type == energy)
      y1 = subset(util$usage, util$energy_type == energy)
      z1 = ifelse(subset(util$estimated, util$energy_type == energy) == 1, 'Est', 'Act')
      df1 = data.frame(x = x1, y = y1, z = z1)

      unit = switch(energy, 'Elec' = TRUE, 'Fuel' = FALSE)


      p = main_line_point_plot(df1, best_model, bdbid_n, energy, b_name, unit, p)
    }
    return(p)
}


plot_timeseries <- function(util, energy)
{
  #plot timeseries. Unlike pplot_timeseries function, it also plots points and the shape of the points is now adjusted (Act or Est)
  options(digits=15)
  util$estimated = ifelse(util$estimated == 1, 'Est', 'Act')

  unit_usage = switch(as.character(.subset2(util, 'using_sqft')[1]), "1" = "/month)", "/sqft/month)")

  util_act = subset(util, util$estimated == 'Act')
  util_est = subset(util, util$estimated == 'Est')

  switch(energy, 'Elec' = {
    color_n = 'rgba(51, 113, 213, 1)'
    y_title = paste('Usage(kWh',unit_usage, sep ='')
    #y_title = 'Usage(kWh/sq/month)'
    },
    'Fuel' = {
      color_n = 'rgba(240, 24, 28,1)'
      y_title = paste('Usage(BTU',unit_usage, sep ='')
      #y_title = 'Usage(BTU/sqft/month)'
      })

  ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "OAT"
  )

  p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$oat, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = color_n), name = "Usage") %>%
      add_trace(x = ~util_est$end_date, y = ~util_est$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = color_n, size = 9), name = 'Est') %>%
      add_trace(x = ~util_act$end_date, y = ~util_act$usage, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = color_n, size = 9), name = 'Act') %>%
      layout(
      title = "Time Series", paper_bgcolor='rgb(240, 242, 247)', plot_bgcolor='rgb(240, 242, 247)', yaxis2 = ay, yaxis = list(title= y_title), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date))

  return(p)
}


co2_rank_get <- function(co2eui_df, bdbid_n)
{
  site_eui = subset(co2eui_df$site_eui_percent_rank, co2eui_df$bdbid == bdbid_n)
  source_eui = subset(co2eui_df$source_eui_percent_rank, co2eui_df$bdbid == bdbid_n)
  site_emission = subset(co2eui_df$co2emissions_kg_sqft_site_percent_rank, co2eui_df$bdbid == bdbid_n)
  source_emission = subset(co2eui_df$co2emissions_kg_sqft_source_percent_rank, co2eui_df$bdbid == bdbid_n)
  return(c(site_eui, site_emission, source_eui, source_emission))
}

co2eui_figure <- function(x)
{
  x = x/100
  x1 <- c(0.25, 0.25, 0.25)
  x2 <-c(0.5, 0.5, 0.5)
  x3 <- c(0.25,0.25, 0.25)

  y = c('CO2e kg/sqft', 'Site EUI Rank', 'Source EUI Rank')
  q <- plot_ly() %>%
  add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = 33, line = list(width = 3)), name = 'Rank')%>%
  add_trace(x = ~x1, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'green', gradient = list(type = 'veritcal')), name = 'Good')%>%
  add_trace(x = ~x2, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'orange'), name = 'Average') %>%
  add_trace(x = ~x3, y = ~y, type = 'bar', orientation = 'h', marker = list(color = 'red'), name = 'Bad') %>%
    layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         margin = list(l = 120, r = 10, t = 140, b = 80),
         legend = list(orientation = 'h')) %>%
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                            color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right')

  return(q)
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

stat_param_table <- function(best_model, bdbid_n, energy_n)
{
  if (flag_func(best_model, bdbid_n, energy_n))
  {
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'xcp1', 'xcp2', 'ls', 'rs', 'r2', 'cv_rmse', 'period')]
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
  help_df = data.frame(c('Time Series', 'Building Information','Parameter Model', 'Parameters','Stats', 'Post Modeller Table', 'Elec Vs Fuel' ,'Lean Anaylsis Percent Ranking', 'Lean Anaylsis Numeric Ranking', 'CO2 EUI Percent Ranking', 'CO2e Emissions Metric Tons','Energy Breakdown Table and Donut Chart',  'Multi Elec and Multi Fuel'))
  help_df$Requirement = c('Utility CSV (a must)','Utility CSV and Building Info CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Best Model CSV/All Model CSV', 'Utility CSV and Post Modeller CSV', 'Building with two different energy types ','Utility CSV and Post Modeller CSV','Utility CSV and Post Modeller CSV', 'CO2 EUI Ranking CSV', 'CO2e Breakdown CSV', 'Energy Breakdown CSV','Utility CSV, Best Model CSV/All Model CSV and Building Info CSV(optional)')
  colnames(help_df) = c('Graph/Table', 'Requirement')
  return(help_df)
}

missing_cols_handler <- function(cols, df)
{
  org_col = colnames(df)
  col_null = cols[!(cols %in% org_col)]
  df[,col_null] = NA
  return(df)
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
    message = "ERROR. Please notify the maintainer."
  }
  return(message)
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

percent_figure <- function(x, co2_flag = FALSE)
{
  x = x*100
  m1 = matrix(data = seq(0,100,length.out = 10000), nrow = 1, ncol = 10000)
  x1 = seq(0,100,length.out = 10000)

  len_key = length(x)
  height_n = 300

  if (len_key == 5)
  {
    y = c('Baseload','Cooling Change-point', 'Heating Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    bar_size = 20
    margin_l = 160
    width_n = 750
  }else
  {
    bar_size = 26
    width_n = 700
    if (co2_flag)
    {
      #x = x*100
      y = c('Site EUI Rank', 'Site CO2e kg/sqft', 'Source EUI Rank', 'Source CO2e kg/sqft')
      margin_l = 150
    }else
    {
      margin_l = 135
      y = c('Baseload','Change-point', 'Heating Sensitivity', 'Cooling Sensitivity')
    }
  }

  color_vec = c('#29CD3F', 'yellow', 'yellow', '#F73434')

  p1 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[1], z = m1, colors = colorRamp(color_vec), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p1 = add_trace(p = p1, x = ~x[1], y= ~y[1], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[1])

  p2 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[2], z = m1, colors = colorRamp(color_vec), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p2 = add_trace(p = p2, x = ~x[2], y= ~y[2], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[2])

  p3 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[3], z = m1, colors = colorRamp(color_vec), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p3 = add_trace(p = p3, x = ~x[3], y= ~y[3], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[3])

  p4 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[4], z = m1, colors = colorRamp(color_vec), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
  p4 = add_trace(p = p4, x = ~x[4], y= ~y[4], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[4])

  if(len_key == 4)
  {
      p = subplot(p1,p2,p3,p4, nrows = 4, heights = c(0.2,0.22,0.22,0.2), shareX = TRUE, shareY = FALSE) %>% layout(xaxis = list(title ="", tickmode = 'array', tickvals = c(12.5,50,87.5), ticktext = c('Good','Average','Poor')))
      return(p)
  }else
  {
      p5 = add_trace(p = plot_ly(width = width_n, height = height_n), x = x1, y= y[5], z = m1, colors = colorRamp(color_vec), type = "heatmap", showscale = FALSE) %>% layout(yaxis = list(tickcolor = 'white', showline = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l = margin_l))
      p5 = add_trace(p = p5, x = ~x[5], y= ~y[5], type = 'scatter', mode = 'markers', marker = list(symbol = "line-ns-open", color = 'black', size = bar_size, line = list(width = 3)), showlegend = FALSE, name = y[5])

      p = subplot(p1,p2,p3,p4,p5, nrows = 5, heights = c(0.15,0.17,0.17,0.17,0.15), shareX = TRUE, shareY = FALSE) %>% layout(xaxis = list(title ="", tickmode = 'array', tickvals = c(12.5,50,87.5), ticktext = c('Good','Average','Poor')))
      return(p)
  }

}

energy_break_pie_chart <- function(df)
{

  df$color = NA

  df$color[df$epa_energy_type == 'District Steam'] <- 'rgba(244,226,66,1)'
  df$color[df$epa_energy_type == 'Electricity - Grid Purchase'] <- 'rgba(116,173,209,1)'
  df$color[df$epa_energy_type %in% c('Fuel Oil', "Fuel Oil (No. 2)","Fuel Oil (No. 4)","Fuel Oil (No. 5 and No. 6)")] <- 'rgba(223,89,82,1)'
  df$color[df$epa_energy_type == 'Natural Gas'] <- 'rgba(244,109,67,1)'
  df$color[df$epa_energy_type == 'Average Influent Flow'] <- 'rgba(84, 211,78,1)'
  df$color[df$epa_energy_type == "Electricity - Car Charging"] <- 'rgba(149, 203, 237, 1)'
  df$color[df$epa_energy_type == "Electricity - Generated On Site"] <- 'rgb(186, 227, 252)'
  df$color[df$epa_energy_type == "Diesel"] <- 'rgba(232, 119, 113,1)'
  df$color[df$epa_energy_type == "Liquid Propane"] <- 'rgba(247, 162, 158, 1)'

  site_p <- add_pie(p = plot_ly(), data = df, labels = ~epa_energy_type, values = ~site_energy_kbtu, type = 'pie', textposition = 'outside', textinfo = 'percent', hoverinfo = 'text',
        marker = list(colors = df$color, line = list(color = '#FFFFFF', width = 1)), hole = 0.6,
        insidetextfont = list(color = '#FFFFFF')) %>%
    layout(title = '', showlegend = TRUE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  source_p <- add_pie(p = plot_ly(), data = df, labels = ~epa_energy_type, values = ~source_energy_kbtu, type = 'pie', textposition = 'outside', textinfo = 'percent', hoverinfo = 'text',
        marker = list(colors = df$color, line = list(color = '#FFFFFF', width = 1)), hole = 0.6,
        insidetextfont = list(color = '#FFFFFF')) %>%
    layout(title = '', showlegend = TRUE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(list(site_p = site_p, source_p = source_p))
}

make_break_table <- function(df)
{
  df = df[, c('epa_energy_type','reported_consumption_unit', 'reported_site_energy', 'reported_source_energy')]
  colnames(df) = c('EPA Energy Type', 'Unit','Reported Site Energy', 'Reported Source Energy')
  return(df)
}

post_output_df_server <- function(post_df, bdbid_n, energy_n, area_info, n)
{
    post_output_df = post_output(post_df, bdbid_n, energy_n)
    post_output_df = post_col(post_output_df, n, energy_n)

    if(area_info$flag_area)
    {
      post_output_df = post_gross(post_output_df, area_info$area, energy_n)
    }else
    {
      post_output_df$Unretrofit = as.character(post_output_df$Unretrofit)
      post_output_df$Unretrofit[2:11] = as.character(round(as.numeric(post_output_df$Unretrofit[2:11]), 2))
      post_output_df$Unretrofit = format(post_output_df$Unretrofit, na.encode = TRUE, justify = 'none')
      post_output_df$Unretrofit = prettyNum(post_output_df$Unretrofit, big.mark = ",", format = 'f')
    }
    return(post_output_df)
}


building_comparison_graph <- function(df, binfo, primary_fun)
{
  if (is.null(df)){
    return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data')))
  }
  aes_df = subset(binfo, binfo$bdbid %in% df$bdbid)
  aes_df = aes_df[order(aes_df$bdbid),]
  df = df[order(df$bdbid),]
  p_init = plot_ly(source = 'B')

  if(primary_fun == 'None'){
    pal = colorRamps::matlab.like(length(unique(aes_df$primary_fun)))
    pal = setNames(pal, unique(aes_df$primary_fun))
    p = add_trace(p = p_init, x = df$total_bldg_gross_sq_ft, y = df$site_eui, type ='scatter',
              mode ='markers', text = aes_df$name, hoverinfo = 'x+y+text', color = aes_df$primary_fun, colors = pal,
              marker = list(symbol = 'circle', size = 9, opacity = 0.6,
                        line = list(color = 'rgba(164,165,164,1)', width = 1)), inherit = FALSE)
  }else
  {
    p = add_trace(p = p_init, x = df$total_bldg_gross_sq_ft, y = df$site_eui, type ='scatter', mode ='markers', text = aes_df$name, hoverinfo = 'x+y+text',
    marker = list(symbol = 'circle', color = 'rgba(176,220,175,1)', size = 9, line = list(color = 'rgba(164,165,164,1)', width = 1)), name = 'blah', inherit = FALSE)
  }
  p = p %>% layout(title = 'Buliding Comparison', xaxis = list(title = "Property Size (sqft)",
                      showticklabels = TRUE, zeroline = FALSE, showline = FALSE),
                  yaxis = list(title = "EUI (kBTU/sqft)", showticklabels = TRUE, showline = FALSE,
                      zeroline = FALSE))
  return(p)
}

lean_table_handler <- function(lean_df, energy, b_df){
  if(is.null(b_df$bdbid) | is.null(lean_df))
  {
    return(NULL)
  }

  #df = subset(lean_df, lean_df$energy_type == energy & lean_df$bdbid %in% b_df$bdbid)
  df = subset(lean_df, lean_df$energy_type == energy)
  total_num = nrow(df)
  df = subset(df, df$bdbid %in% b_df$bdbid)
  df = df[,c('bdbid', "cooling_change_point_numeric_rank", "cooling_sensitivity_numeric_rank", "heating_change_point_numeric_rank", "heating_sensitivity_numeric_rank", "baseload_numeric_rank")]
  colnames(df) = c('bdbid', 'Cooling Change Point Numeric Rank', 'Cooling Sens Numeric Rank', 'Heating Change Point Numeric Rank', 'Heating Sens Numeric Rank', 'Baseload Numeric Rank')
  df = df[order(df$bdbid),]
  df$bdbid = subset(b_df$name, b_df$bdbid %in% df$bdbid)
  return(list(df = df, total_num = total_num))
}

point_lookup <- function(breakdown_df, x){
  bdbid = subset(breakdown_df$bdbid, breakdown_df$total_bldg_gross_sq_ft == x)
  return(bdbid)
}

make_model_info_table <- function(best_model, b_df){
  if (is.null(best_model) | is.null(b_df)){
    return(NULL)
  }
  df = data.frame(bdbid = b_df$bdbid, name = b_df$name, elec_model = NA, fuel_model = NA)
  df = df[order(df$bdbid),]
  for (bdbid_n in df$bdbid){
    best_df = subset(best_model, best_model$bdbid == bdbid_n)
    model_vec = get_model_information(best_df)
    df$elec_model[df$bdbid == bdbid_n] = model_vec[1]
    df$fuel_model[df$bdbid == bdbid_n] = model_vec[2]
  }
  df = df[,c('name', 'elec_model', 'fuel_model')]
  colnames(df) = c('bdbid', 'Elec Model', 'Fuel Model')

  return(df)
}

get_model_information <- function(best_df){
  if(nrow(best_df) == 0){
    return(c(NA,NA))
  }

  final_vec = c()
  for (energy in c('Elec','Fuel')){
    model = best_df$model[best_df$energy_type == energy]
    if (length(model))
    {
      final_vec = c(final_vec, model)
    }else
    {
      final_vec = c(final_vec, NA)
    }
  }
  return(final_vec)
}



chunky_paginator <- function(url = '', bdbid_vec = c(), query_params = list(), chunky_vec = NULL){
  if(is.null(chunky_vec)){
    query_params$bdbid = paste0(bdbid_vec, collapse = ',')
    final_df = paginator_fetch_request(url, query_params=query_params)$result
    return(final_df)
  }

  iter_len = length(chunky_vec) - 1
  final_df = data.frame()
  for (i in c(1:iter_len)){
    start_i = chunky_vec[i]
    end_i = ifelse(i == iter_len, chunky_vec[i+1], chunky_vec[i+1]-1)
    bdbid_str = paste0(bdbid_vec[start_i:end_i], collapse = ',')
    query_params$bdbid = bdbid_str
    df = paginator_fetch_request(url, query_params=query_params)$result
    final_df = rbind(final_df, df)
  }
  return(final_df)
}

get_batch_result <- function(url, query){
  delay_n = 0.1
  task_id = post_request(url, payload = query)$task_id
  for (i in c(1:20)){
    res = fetch_request(paste('/bema/results/', task_id, sep =''))
    if (res$status == 'SUCCESS'){
      break
    }
    delay_n = delay_n +(i/200)
    Sys.sleep(delay_n)
  }
  return(res)
}

char_to_vec <- function(x){
  if (is.vector(x)){
    return(x)
  }else{
    return(c(x))
  }
}
