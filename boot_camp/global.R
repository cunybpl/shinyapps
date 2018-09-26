library(devtools)
library(plotly)
install_github('tinnaing347/bRema')
library(bRema)


#this is pretty equivalent to params table out
result_table <- function(result)
{ 
  result_matrix = rbind(result$params, result$cp1, result$cp2)
  row.names(result_matrix) = c(row.names(result$params), 'CP1', 'CP2')
  return(as.data.frame(t(result_matrix)))
}

stat_table_out <- function(stat, pop)
{
  return(data.frame(r2 = stat[2,1], cv_rmse = stat[3,1], heat_months = pop[2,1], cool_months = pop[3,1]))
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

tests_table_out <- function(stat_test)
{
  return(data.frame(pop_test = stat_test$pop_test[1], shape_test = stat_test$shape_test[1], tTest = stat_test$tTest[1], final_test_result = stat_test$final_test_result))
}


all_table_handler <- function(result)
{ 
  params = result_table(result)
  stats = stat_table_out(result$stats, result$stat_test$pop_test)
  stat_test = tests_table_out(result$stat_test)
  return(list(params = params, stats = stats, stat_test = stat_test))
}

run_ind_model <- function(util, model, cp1 = 0, cp2 =0)
{ 
  options(digits=15)
  final_best = list()
  model_best = list()

  x = util$OAT
  y = util$usage
  z = ifelse(util$estimated == 1, 'Est', 'Act')
  z = z[sort.list(x)]

  temp = sort_matrix(x,y)
  x = temp$x
  y = temp$y

  if (model == '2P')
  {
    bestvalue = test_model(model, x,y)
  }else if (model == '5P')
  {
    bestvalue = test_model(model, x, y, cp1, cp2)
  }else
  {
    bestvalue = test_model(model, x, y, cp1, cp2)
  }

  bestvalue[['stat_test']]= model_test(x, bestvalue, model)

  return (bestvalue)
}


main_plot_handler <- function(util, bestvalue, energy, source_n = "")
{ 
  x1 = util$OAT
  y1 = util$usage
  temp_est = util$estimated
  z1 = ifelse(temp_est== 1, 'Est', ifelse(temp_est == 2, 'Agg', 'Act'))
  df1 = data.frame(x = x1, y = y1, z = z1)

  if (is.null(bestvalue))
  {
    final_figure = main_plot_point(df1, energy, b_name = '', source_n)
  }else
  {
    final_figure = main_line_point_plot(df1, bestvalue, energy, b_name = '', source_n)
  }
  return(final_figure)
}


main_line_point_plot <- function(df1, best_model, energy, b_name, source_n)
{
    B = best_model$params #not params, just baseload, and slope(s)
    x1 = df1$x

    if (energy == 'Elec')
    {
        model_fig = plot_model(x = x1, model = best_model$model, B = B, cp1 = best_model$cp1, cp2 = best_model$cp2, key = 'elec', unit = FALSE, p1 = plot_ly(), bw = FALSE, source_n)
        final_figure = plot_point_3(df1, 'elec', model_fig, b_name, source_n, TRUE)
    }else
    {   
        model_fig = plot_model(x = x1, model = best_model$model, B = B, cp1 = best_model$cp1, cp2 = best_model$cp2, key = 'fuel', unit = FALSE,p1 = plot_ly(), bw = FALSE, source_n)
        final_figure = plot_point_3(df1, 'fuel', model_fig, b_name, source_n, TRUE)
    }
    return(final_figure)
}

main_plot_point <- function(df, energy, b_name, source_n)
{
  if(energy == 'Elec')
  {
    final_figure = plot_point_3(df,'elec', model_fig = plot_ly(), b_name = b_name, source_n, FALSE)
  }else
  {
    final_figure = plot_point_3(df,'fuel', model_fig = plot_ly(), b_name = b_name, source_n, FALSE)
  }
  return(final_figure)
}

plot_point_3 <- function(df, key, model_fig = plot_ly(), b_name, source_n, line_flag)
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (source_n != "" & line_flag == FALSE){
    model_fig = plot_ly(source = source_n)
  }


  if (key == 'elec'){
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Act', inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Elec Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(51, 113, 213, 1)', size = 9), name = 'Elec Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (kWh)"))
  }else if (key == 'fuel')
  {
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(240, 24, 28,1)', size = 9), name = 'Fuel Act', inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Fuel Agg', inherit = FALSE)
    }
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

  if (source_n != "")
  { 
    p1 = plot_ly(source = source_n)
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
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)', dash = 'dash'), name = "Elec Model", inherit = FALSE)
    }else
    {
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(51, 113, 213, 1)'), name = "Elec Model", inherit = FALSE)
    }
  }else
  { 
    if (bw)
    {
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)', dash = 'dash'), name = "Fuel Model", inherit = FALSE)
    }else
    {
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)'), name = "Fuel Model", inherit = FALSE)
    }
  }
  return(model_fig)
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


