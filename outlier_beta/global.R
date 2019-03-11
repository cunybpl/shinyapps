library(plotly)
library(dplyr)
library(lubridate)

if(length(new.packages))
{ 
  library(devtools)
  #install_github('tinnaing347/bRema')
  library(bRema)
}


plot_model <- function(x, model, B, cp1, cp2, key, unit, bw, p1 = plot_ly(), source_n)
{  
  options(digits=15)
  require(ggplot2)
  x = x[order(x)]
  x0 = x[1]
  xf = x[length(x)]
  Ycp= B[1] #coeff, need this to get y intercept
  slope1 = B[2]
  yInter_1 = Ycp - slope1*cp1

  if (source_n != "")
  { 
    p1 = plot_ly(source = source_n)
  }

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
    estimated = Ycp + slope1*x 
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


stat_table <- function(best_model, bdbid_n, energy_n)
{ 
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


plot_point_2 <- function(df, key, model_fig = plot_ly(), b_name, source_n, line_flag)
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (source_n != "" & line_flag == FALSE){
    model_fig = plot_ly(source = source_n)
  }

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

plot_outlier_point <- function(outlier_df, figure = plot_ly(), graph_title, energy_n)
{ 
  color_n = ifelse(energy_n == 'Elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
  p = add_trace(p = figure, x = ~outlier_df$OAT, y = ~outlier_df$usage, type ='scatter', mode ='markers', marker = list(symbol = ifelse(outlier_df$estimated == 1, 'star-triangle-up-open', 'star-triangle-up'), color = color_n, size = 9), name = graph_title, inherit = FALSE)
  return(p)
}

plot_fail_outlier_point <- function(new_util, outlier_df, graph_title, energy_n)
{ 
  color_n = ifelse(energy_n == 'Elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
  p_new = plot_point_2(new_util, tolower(energy_n), model_fig = plot_ly(), b_name ='', source_n ="", FALSE)
  p = add_trace(p = p_new, x = ~outlier_df$OAT, y = ~outlier_df$usage, type ='scatter', mode ='markers', marker = list(symbol = ifelse(outlier_df$estimated == 1, 'star-triangle-up-open', 'star-triangle-up'), color = color_n, size = 9), name = graph_title, inherit = FALSE)
  return(p)
}

main_plot <- function(temp_df, best_model, bdbid_n, energy, b_name = "", source_n = "")
{ 
  util_energy_type = subset(temp_df$energy_type, temp_df$bdbid == bdbid_n)
  if (energy %in% util_energy_type)
  { 
    #susbset by energy now
    util = subset(temp_df, bdbid == bdbid_n)
    x1 = subset(util$OAT, util$energy_type == energy)
    y1 = subset(util$usage, util$energy_type == energy)
    z1 = ifelse(subset(util$estimated, util$energy_type == energy) == 1, 'Est', 'Act')

    df1 = data.frame(x = x1, y = y1, z = z1) #ready for point plot

    if (bdbid_n %in% unique(best_model$bdbid)) #bdbid is in best_model
    {
      best_energy_type = subset(best_model$energy_type, best_model$bdbid == bdbid_n)

      if(energy %in% best_energy_type) #energy and bdbid is in util and best
      { 
        final_figure = main_line_point_plot(df1, best_model, bdbid_n, energy, b_name, source_n)
      }else #if FALSE, it means energy is not in best model so just point plot
      { 
        final_figure = main_plot_point(df1, energy, b_name, source_n)
      }
    }else #if FALSE, it means bdbid does not make it to best_model so just point plot
    { 
      final_figure = main_plot_point(df1, energy, b_name, source_n)
    }
  }else #if FALSE, it means energy is not in util frame so empty plot
  { 
    final_figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy, "for", b_name))
  }
  return(final_figure)
}

main_line_point_plot <- function(df1, best_model, bdbid_n, energy, b_name, source_n)#both point and line
{
  params = params_list(best_model, bdbid_n, energy, 1)
    bw = best_worst(best_model, bdbid_n, energy, 1)
    B = params_matrix(params)
    x1 = df1$x

    if (energy == 'Elec')
    {   
        model_fig = plot_model(x1, params$model, B, params$cp1, params$cp2, 'elec', FALSE, bw = bw, source_n = source_n)
        #point_fig = plot_point(x1, y1, 'elec')
        #point_fig = plot_point_2(df1, 'elec')
        final_figure = plot_point_2(df1, 'elec', model_fig = model_fig, b_name, source_n = source_n, line_flag = TRUE)
    }else
    {   
        model_fig = plot_model(x1, params$model, B, params$cp1, params$cp2, 'fuel', FALSE, bw = bw, source_n = source_n)
        #point_fig = plot_point(x1, y1, 'fuel')
        #point_fig = plot_point_2(df1, 'fuel')
        final_figure = plot_point_2(df1, 'fuel', model_fig = model_fig, b_name, source_n = source_n, line_flag = TRUE)
    }
    return(final_figure)
}


main_plot_point <- function(df, energy, b_name, source_n)#just points, call when energy_type does not make it to best_model
{
  if(energy == 'Elec')
  {
    #point_fig = plot_point_2(df,'elec')
    #final_figure = ggplot() + point_fig + scale_color_manual(values = c('Elec Consumption' = 'blue'))+labs(x='Temperature',y='Usage (kWh)')
    final_figure = plot_point_2(df,'elec', b_name = b_name, source_n = source_n, line_flag = FALSE)
  }else
  {
    #point_fig = plot_point_2(df,'fuel')
    #final_figure = ggplot() + point_fig + scale_color_manual(values = c('Fuel Consumption' = 'red')) + labs(x='Temperature',y='Usage (BTU)')
    final_figure = plot_point_2(df,'fuel', b_name = b_name, source_n = source_n, line_flag = FALSE)
  }
  return(final_figure)
}



stat_param_table <- function(best_model, bdbid_n, energy_n)
{ 
  if (flag_func(best_model, bdbid_n, energy_n))
  {
    df = subset(best_model, bdbid == bdbid_n & energy_type == energy_n)
    df = df[ ,c('prepost', 'model_type', 'xcp1', 'xcp2', 'ls', 'rs', 'r2', 'cv_rmse', 'n')]
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

bind_values <- function()
{ a<-data.frame()
    function(x)
    {   
        if(length(x))
        {
          a<<- rbind(a,x)
          a
        }else
        {
          a<<- data.frame(NULL)
          a
        }
    }
}

run_values <- function()
{ b<-0
    function(x)
    {
        b<<- x
        b
    }
}

run_outlier_model <- function(util)
{ 
  options(digits=15)
  final_best = list()
  model_best = list()

  x = util$OAT
  y = util$usage
  z = ifelse(util$estimated == 1, 'Est', 'Act')
  z = z[sort.list(x)]

  model_list = c('2P', '3PC', '3PH', '4P', '5P')
  temp = sort_matrix(x,y)
  x = temp$x
  y = temp$y
  n = length(x)

  df  = data.frame(x = x, y = y, z = z)
  for (model in model_list)
  {
    bestvalue = create_model(x,y,model)
    bestvalue[['stat_test']]= model_test(x, bestvalue, model) 

    model_best[[model]] = bestvalue
    final_best = c(final_best, model_best)
  }
  
  return (final_best)
}

outlier_free_user <- function(util, outlier_df)
{
  new_util = subset(util, !(round(util$OAT,6) %in% round(outlier_df$OAT,6)))
  #new_util = subset(util, !mapply(function(x, y) {isTRUE(all.equal(x, y))}, util$OAT, outlier_df$OAT) & !mapply(function(x, y) {isTRUE(all.equal(x, y))}, util$usage, outlier_df$usage))
  return(new_util)
}

outlier_reverse_user <- function(util, outlier_df)
{ 
  out_util = subset(util, round(util$OAT,6) %in% round(outlier_df$OAT,6))
  #out_util = subset(util, mapply(function(x, y) {isTRUE(all.equal(x, y))}, util$OAT, outlier_df$OAT) & mapply(function(x, y) {isTRUE(all.equal(x, y))}, util$usage, outlier_df$usage))
  return(out_util)
}

outlier_free <- function(util, outlier_df)
{
  new_util = subset(util, !(util$OAT %in% outlier_df$OAT) & !(util$usage %in% outlier_df$usage))
  #new_util = subset(util, !mapply(function(x, y) {isTRUE(all.equal(x, y))}, util$OAT, outlier_df$OAT))
  return(new_util)
}


outlier_reverse <- function(util, outlier_df)
{
  out_util = subset(util, util$OAT %in% outlier_df$OAT & util$usage %in% outlier_df$usage)
  return(out_util)
}


inter_outlier_func <- function(temp_df, outlier_df, bdbid_n, energy_n, flag_user = TRUE, out_type)
{
  utility = subset(temp_df, temp_df$bdbid == bdbid_n & temp_df$energy_type == energy_n)
  if (flag_user)
  {
    new_util = outlier_free_user(utility, outlier_df) #use this for plotting
    out_util = outlier_reverse_user(utility, outlier_df)
  }else
  { 
    if(out_type == 'M')
    {
      out_util = subset(outlier_df, outlier_df$bdbid == bdbid_n & outlier_df$energy_type == energy_n & outlier_df$mahal_dist == 1)
    }else{
      out_util = subset(outlier_df, outlier_df$bdbid == bdbid_n & outlier_df$energy_type == energy_n & outlier_df$confidence_ellipse == 1)
    }
    new_util = outlier_free(utility, out_util)
  }
  inter_result = run_outlier_model(new_util)
  final_result = best_model_main_func(inter_result, energy_n)
  return(list(final_result = final_result, new_util = new_util, out_util = out_util))
}


model_pass_func <- function(inter_result, energy_n)
{ 
  best_result = list()
  if (energy_n == 'Elec')
  { 
    model_list = c('5P', '4P', '3PC', '3PH', '2P')
    for (model in model_list)
    {
      if (inter_result[[model]]$stat_test$final_test_result == 'Pass' & inter_result[[model]]$stats[3,1] <= 25 & inter_result[[model]]$stats[2,1] >= 0.75)
      { 
        best_result[[model]] = inter_result[[model]]
      }
    }
  }else
  {
    model_list = c('2P', '3PH', '3PC', '4P', '5P')
    best_result = list()
    for (model in model_list)
    {
      if (inter_result[[model]]$stat_test$final_test_result == 'Pass' & inter_result[[model]]$stats[3,1] <= 50 & inter_result[[model]]$stats[2,1] >= 0.75)
      { 
        best_result[[model]] = inter_result[[model]]
      }
    }
  }
  return(best_result)
}


metric_weight <- function(best_result, energy_n)
{ 
  weight_df = data.frame()
  for (i in 1:length(best_result))
  { 
    if (energy_n == 'Elec')
    {
      weight_df = elec_weight_df_func(best_result, i, weight_df)
    }else
    {
      weight_df = fuel_weight_df_func(best_result, i, weight_df)
    }
  }
  return(weight_df)
}


finessing_the_best <- function(best_result, weight_df)
{
  model_type_best = rownames(subset(weight_df, max(weight_df$weight)==weight_df$weight))
  best_result = best_result[[model_type_best]]
}

best_model_main_func <- function(inter_result, energy_n)
{
  best_result = model_pass_func(inter_result, energy_n)
  if (length(best_result))
  {
    weight_df = metric_weight(best_result, energy_n)
    best_model = finessing_the_best(best_result, weight_df)
  }else
  {
    best_model = list()
  }

  return(best_model)
}


elec_weight_df_func <- function(best_result, i, df)
{
  if (best_result[[i]][['model']] == '2P')
  {
    df['2P','weight'] = 1
  }else if (best_result[[i]][['model']] == '3PC')
  {
    df['3PC','weight'] = 2
  }else if (best_result[[i]][['model']] == '3PH')
  {
    df['3PH','weight'] = 3
  }else if (best_result[[i]][['model']] == '4P')
  {
    df['4P','weight'] = 4
  }else if (best_result[[i]][['model']] == '5P')
  {
    df['5P','weight'] = 5
  }
  return(df)
}

fuel_weight_df_func <- function(best_result, i, df)
{
  if (best_result[[i]][['model']] == '2P')
  {
    df['2P','weight'] = 1
  }else if (best_result[[i]][['model']] == '3PC')
  {
    df['3PC','weight'] = 5
  }else if (best_result[[i]][['model']] == '3PH')
  {
    df['3PH','weight'] = 4
  }else if (best_result[[i]][['model']] == '4P')
  {
    df['4P','weight'] = 3
  }else if (best_result[[i]][['model']] == '5P')
  {
    df['5P','weight'] = 2
  }
  return(df)
}


out_main_plot <- function(res, util, energy_n)
{
    x1 = util$OAT
    y1 = util$usage
    z1 = ifelse(util$estimated == 1, 'Est', ifelse(util$estimated == 0, 'Act', ifelse(util$estimated == 3, 'Prev', 'Agg')))

    df1 = data.frame(x = x1, y = y1, z = z1)
    x = df1$x

    if(length(res))
    { 
      if (energy_n == 'Elec')
      {
        model_fig = plot_model(x, res$model, res$params, res$cp1, res$cp2, 'elec', FALSE, bw = FALSE, source_n = "")
        final_figure = plot_point_2(df1, 'elec', model_fig = model_fig, b_name = "", source_n = "", line_flag = TRUE)
      }else
      {
        model_fig = plot_model(x, res$model, res$params, res$cp1, res$cp2, 'fuel', FALSE, bw = FALSE, source_n = "")
        final_figure = plot_point_2(df1, 'fuel', model_fig = model_fig, b_name = "", source_n = "", line_flag = TRUE)
      }
    }else
    {
      if (energy_n == 'Elec')
      {
        final_figure = plot_point_2(df1,'elec', b_name = "", source_n = "", line_flag = FALSE)
      }else
      {
        final_figure = plot_point_2(df1,'fuel', b_name = "", source_n = "", line_flag = FALSE)
      }
    }
    return(final_figure)
}


stat_table_out <- function(stat, pop, model, n)
{
  return(data.frame(model_type = model, r2 = stat[2,1], cv_rmse = stat[3,1], heat_months = pop[2,1], cool_months = pop[3,1], n = n))
}

params_table_out <- function(model, B, cp1, cp2)
{
  if (nrow(B) == 2)
  {
    return(data.frame(Model = model, Ycp = B[1,1], cp1 = cp1, cp2 = cp2, slope1 = B[2,1], slope2 = 0))
  }else
  {
    return(data.frame(Model = model, Ycp = B[1,1], cp1 = cp1, cp2 = cp2, slope1 = B[2,1], slope2 = B[3,1]))
  }
}

outlier_func <- function(utility, bdbid_n, energy_n)
{ 
  utility = subset(utility, bdbid == bdbid_n & energy_type == energy_n)
  utility = utility[,c('OAT', 'usage')]
  colnames(utility) = c('x','y')

  #p = ggplot(utility, aes(x, y)) + geom_point() + stat_ellipse(type = "t", segments = 1000)

  ellipse_list = get_ellipse_points(utility, energy_n)
  el = ellipse_list$el
  center = ellipse_list$ctr

  points_df = get_points_center(el, center)
  slopes = get_slopes(points_df)

  angle = get_angle(slopes[2])
  radii = points_df$dist2center[1:2]
  final_df = ellipse_equation(utility, center, radii, angle)

  return(list(final_df = final_df))
  #return(list(final_df = final_df, center = center, radii = radii, angle = angle, slopes = slopes, points_df = points_df, figure = p))
}


get_ellipse_points <- function(utility, energy_n)
{ 
  seg = ifelse(energy_n == 'Fuel', 1000, 100000)
  p = ggplot(utility, aes(x, y)) + geom_point() + stat_ellipse(type = "t", segments = seg) #for elec use 100000
  pb = ggplot_build(p)
  el = pb$data[[2]][c("x","y")] #get point
  ctr = MASS::cov.trob(el)$center #get center
  dist2center <- sqrt(rowSums((t(t(el)-ctr))^2))
  el = cbind(el, dist2center)
  return(list(el = el, ctr = ctr))
}

get_points_center <-function(df, center) #excute this before get_slopes
{
  points_max = subset(df, df$dist2center == max(df$dist2center))
  points_min = subset(df, df$dist2center == min(df$dist2center))
  center = data.frame(t(center))
  center$dist2center = 0
  points_df = rbind(points_max, points_min, center)
  rownames(points_df) = c('max','min','center')
  return(points_df)
}

get_slopes <- function(df) #points_df
{ 
  m_max = (df['max','y']-df['center', 'y'])/(df['max','x'] - df['center','x'])#the slope of the biggest distance
  m_min = -1/m_max #the slope of the smallest distance
  slopes = c(m_max, m_min)
  return(slopes)
}

get_angle <- function(slope)
{
  angle= atan(slope)
  return(angle)
}

ellipse_equation <- function(df, center, radii, angle)
{
  df$x_point = (df$x-center[1])*cos(angle) + (df$y-center[2])*sin(angle)
  df$y_point = (df$x-center[1])*sin(angle) - (df$y-center[2])*cos(angle)
  df$combine = (df$x_point)**2/min(radii)**2 + (df$y_point)**2/max(radii)**2
  df$outside = ifelse(df$combine <= 1, 0, 1)
  return(df)
}


mahala_ellipse_func <- function(utility, energy){
  utility_energy <- filter(utility, energy_type == energy) 
  bdbids <- unique(utility_energy$bdbid)
  mahal_dist <- c()
  local_outlier_factor <- c()
  confidence_ellipse <- c()
  set.seed(1)
  
  for (id in bdbids){
    utility_one <- filter(utility_energy, bdbid == id)
    x <- utility_one$usage
    y <- utility_one$OAT
    data <- cbind(x, y)
    
    # Mahalanobis Distance
    m_dist <- mahalanobis(data, colMeans(data[,1:2]), cov(data[,1:2]))
    m_dist[m_dist < 6] <- 0
    m_dist[m_dist > 6] <- 1
    mahal_dist <- c(mahal_dist, m_dist)
    
    # Confidence Ellipses
    CE <- outlier_func(utility_one, id, energy) 
    outside <- data.frame(CE$final_df$outside)
    confidence_ellipse <- unlist(c(confidence_ellipse, outside))
  }
  
  new_utility <- cbind(utility_energy, mahal_dist, confidence_ellipse)
  return(new_utility)
}

outlier_output_handler <- function(final_result, new_util, out_df, energy_n, outlier_type)
{
  figure = out_main_plot(final_result, new_util, energy_n)
  figure = plot_outlier_point(out_df, figure, outlier_type, energy_n)
  stat = stat_table_out(final_result$stats, final_result$stat_test$pop_test, final_result$model, nrow(new_util))
  params = params_table_out(final_result$model, final_result$params, final_result$cp1, final_result$cp2)
  return(list(figure = figure, stat = stat, params = params))
}

outlier_output_error <- function(new_util, out_util, energy_n, outlier_type)
{ 
  if (length(new_util) | length(out_util))
  {
    new_util_df = data.frame(x = new_util$OAT, y=new_util$usage, z=new_util$estimated)
    new_util_df$z = ifelse(new_util_df$z == 1, 'Est', 'Act')
    figure = plot_fail_outlier_point(new_util_df, out_util, outlier_type, energy_n)
  }else
  {
    figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy_n))
  }
  stat = data.frame()
  params = data.frame()
  return(list(figure = figure, stat = stat, params = params))
}


param_stat_combined <- function(bema, user, mahala, ellipse, prev, agg)
{
  b_df = get_stat_col(bema, 'BEMA')
  u_df = get_stat_col(user, 'User')
  m_df = get_stat_col(mahala, 'Mahalanobis')
  e_df = get_stat_col(ellipse, 'Ellipse')
  p_df = get_stat_col(prev, 'Prev')
  a_df = get_stat_col(agg, 'Agg')
  new_df = rbind(b_df, u_df, m_df, e_df, p_df, a_df)
  return(new_df)
}

get_stat_col <- function(df, type_n)
{
  if(length(df))
  {
    df = df[,c('model_type', 'r2', 'cv_rmse', 'n')]
    df$type = type_n
  }else
  {
    df = data.frame()
  }
  return(df)
}

pre_agg_point_plot <- function(util,energy_n, figure = plot_ly(), graph_title)
{ 
  num_key = ifelse(graph_title == 'Prev', 3, 2)
  color_n = ifelse(energy_n == 'Elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
  df = subset(util, util$estimated == num_key)
  p = add_trace(p = figure, x = ~df$OAT, y = ~df$usage, type ='scatter', mode ='markers', marker = list(symbol = 'star-triangle-up-open', color = color_n, size = 9), name = graph_title, inherit = FALSE)
}

plot_fail_prev_agg_point <- function(util, energy_n, pre_agg_key)
{
  color_n = ifelse(energy_n == 'Elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')
  p_new = out_main_plot(res = data.frame(), util, energy_n)
  p = pre_agg_point_plot(util, energy_n, figure = p_new, pre_agg_key)
  return(p)
}

prev_agg_output_handler <- function(util, final_result, energy_n, pre_agg_key)
{ 
  figure = out_main_plot(final_result, util, energy_n)
  stat = stat_table_out(final_result$stats, final_result$stat_test$pop_test, final_result$model, nrow(util))
  params = params_table_out(final_result$model, final_result$params, final_result$cp1, final_result$cp2)
  figure = pre_agg_point_plot(util, energy_n, figure, pre_agg_key)
  list(stat = stat, params = params, figure = figure)
}

prev_agg_output_error <- function(util, energy_n, pre_agg_key)
{ 
  if (length(util))
  {
      figure = plot_fail_prev_agg_point(util, energy_n, pre_agg_key)
  }else
  {
    figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy_n))
  }
  stat = data.frame()
  params = data.frame()
  return(list(figure = figure, stat = stat, params = params))
}


clean_utility_data <- function(utility, utility_previous, energy){
  #get energy values
  utility_energy = filter(utility, energy_type == energy)
  utility_previous_energy = filter(utility_previous, energy_type == energy)
  #clean utility data
  utility_energy = filter(utility_energy, !is.na(usage))
  utility_previous_energy = filter(utility_previous_energy, !is.na(usage))
  #add months
  utility_energy$Month = month(as.POSIXct(strptime(utility_energy$end_date, "%F")))
  utility_previous_energy$Month = month(as.POSIXct(strptime(utility_previous_energy$end_date, "%F")))
  return(list(utility_energy = utility_energy, utility_previous_energy = utility_previous_energy))
}

previous_year <- function(utility, utility_previous, energy){
  #clean data
  utility_clean = clean_utility_data(utility, utility_previous, energy)
  utility_energy = utility_clean$utility_energy
  utility_previous_energy = utility_clean$utility_previous_energy
  #get bdbid
  bdbids = unique(utility_energy$bdbid) 
  
  for (id in bdbids){
    #get estimated and actual data 
    data_2015 = utility_previous_energy %>% filter(bdbid == id & fiscal_year == 2015 & estimated == 0)
    data_2016_est = utility_energy %>% filter(bdbid == id & fiscal_year == 2016 & estimated == 1)
    data_2016_act = utility_energy %>% filter(bdbid == id & fiscal_year == 2016 & estimated == 0)
    data_2017 = utility_energy %>% filter(bdbid == id & fiscal_year == 2017 & estimated == 1)
    #only run if there are estimated datapoints
    length_2017 = length(data_2017$bdbid)
    if (length_2017 > 0){
      utility = replace_usage(utility, data_2016_act, data_2017)
    }
    #do the same for 2016
    length_2016 = length(data_2016_est$bdbid)
    if (length_2016 > 0){
      utility <- replace_usage(utility, data_2015, data_2016_est)
    }
  }
  return(utility)
}

replace_usage <- function(utility, data_previous, data_current){
  #get months
  months_previous = data_previous$Month
  months_current = data_current$Month
  for (m in months_current){
    #only run if estimated has actual data from previous year
    if (m %in% months_previous) {
      #get usage and replace for each month
      data_previous_m = data_previous %>% filter(Month == m)
      data_current_m = data_current %>% filter(Month == m)
      new_usage = data_previous_m$usage
      old_usage = data_current_m$usage
      utility$estimated[utility$usage == old_usage] <- 3
      utility$usage[utility$usage == old_usage] <- new_usage
    }
  }
  return(utility)
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

graph_df_error_handler <- function(bdbid_n, energy_n)
{
  figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy_n, ' for ', bdbid_n))
  stat = data.frame()
  params = data.frame()
  list(figure = figure, stat = stat, params = params)
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
