library(plotly)
library(devtools)
install_github('tinnaing347/bRema')
library(bRema)

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

outlier_free <- function(util, outlier_df)
{
  new_util = subset(util, !(util$OAT %in% outlier_df$OAT))
  return(new_util)
}


choose_best <- function(inter_result, energy_n)
{ 
  best_result = list()
  if (energy_n == 'Elec')
  { 
    model_list = c('5P', '4P', '3PC', '3PH', '2P')
    for (model in model_list)
    {
      if (inter_result[[model]]$stat_test$final_test_result == 'Pass' & inter_result[[model]]$stats[3,1] <= 25 & inter_result[[model]]$stats[2,1] >= 0.75)
      { 
        best_result = inter_result[[model]]
        break
      }
    }
  }else
  {
    model_list = c('2P', '3PH', '3PC', '4P', '5P')
    best_result = list()
    for (model in model_list)
    {
      if (inter_result[[model]]$stat_test$final_test_result == 'Pass' & inter_result[[model]]$stats[3,1] <= 25 & inter_result[[model]]$stats[2,1] >= 0.75)
      { 
        best_result = inter_result[[model]]
        break
      }
    }
  }
  return(best_result)
}

out_main_plot <- function(res, util, energy_n)
{
  x1 = util$OAT
    y1 = util$usage
    z1 = ifelse(util$estimated == 1, 'Est', 'Act')

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

