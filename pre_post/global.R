library(plotly)

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

plot_model <- function(x, model, B, cp1, cp2, key, pre_key, unit, bw, p1 = plot_ly())
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
    color_n = ifelse(pre_key == 1, 'rgba(51, 113, 213, 1)', 'rgba(109, 203, 15, 1)')
    name_n = ifelse(pre_key == 1, 'Pre Elec Model', 'Post Elec Model')
    if (bw)
    { 
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Elec Model'), linetype = 2)
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = color_n, dash = 'dash'), name = name_n, inherit = FALSE)
    }else
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Elec Model'))
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = color_n), name = name_n, inherit = FALSE)
    }
  }else
  { 
    color_n = ifelse(pre_key == 1, 'rgba(240, 24, 28,1)', 'rgba(109, 203, 15, 1)')
    name_n = ifelse(pre_key == 1, 'Pre Fuel Model', 'Post Fuel Model')
    if (bw)
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Fuel Model'), linetype = 2)
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = color_n, dash = 'dash'), name = name_n, inherit = FALSE)
    }else
    {
      #model_fig = geom_line(data = df, aes(x = x, y = y, color = 'Fuel Model'))
      model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = color_n), name = name_n, inherit = FALSE)
    }
  }
  return(model_fig)
}


plot_point_3 <- function(df, key, pre_key, model_fig = plot_ly(), b_name = '')
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (key == 'elec'){
    color_n = ifelse(pre_key == 1, 'rgba(51, 113, 213, 1)', 'rgba(109, 203, 15, 1)')
    name_n = ifelse(pre_key == 1, 'Pre Elec', 'Post Elec')
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Elec Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = color_n, size = 9), name = paste(name_n, 'Act', sep = ' '), inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Elec Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = color_n, size = 9), name = paste(name_n, 'Est', sep = ' '), inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (kWh)"))
  }else if (key == 'fuel')
  { 
    color_n = ifelse(pre_key == 1, 'rgba(240, 24, 28,1)', 'rgba(109, 203, 15, 1)')
    name_n = ifelse(pre_key == 1, 'Pre Fuel', 'Post Fuel')
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Fuel Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = color_n, size = 9), name = paste(name_n, 'Act', sep = ' '), inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Fuel Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = color_n, size = 9), name = paste(name_n, 'Est', sep = ' '), inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (BTU)"))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'retofit_act'))
  }
  return(point_fig)
}

plot_point <- function(x, y, key)
{ 
  require(ggplot2)
  df = data.frame(x= x, y= y)
  if (key == 'pre'){
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'pre_act'))
  }else if (key == 'post')
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'post_act'))
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

plot_timeseries <- function(util, energy)
{ 
  options(digits=15)
  r1 = util$end_date[length(subset(util$end_date, util$prepost == 1))]
  r2 = util$end_date[length(subset(util$end_date, util$prepost == 1 | util$prepost == 2))+1]
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
      layout(shapes=list(list(type='line', x0= ~r1, x1= ~r1, y0=min(util$usage), y1=max(util$usage), line=list(dash='dot', width=1)),
      list(type='line', x0= ~r2, x1= ~r2, y0=min(util$usage), y1=max(util$usage), line=list(dash='dot', width=1))),
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date))
  }else{
    p <- plot_ly() %>%
      add_trace(x = ~util$end_date, y = ~util$usage, type ='scatter', mode = 'lines', line = list(color = 'rgba(240, 24, 28,1)'), name = "Usage") %>%
      add_trace(x = ~util$end_date, y = ~util$OAT, type ='scatter', mode = 'lines', line = list(color = 'rgba(243, 154, 36, 1)'), name = "OAT", yaxis = "y2") %>%
      layout(shapes=list(list(type='line', x0= ~r1, x1= ~r1, y0=min(util$usage), y1=max(util$usage), line=list(dash='dot', width=1)),
      list(type='line', x0= ~r2, x1= ~r2, y0=min(util$usage), y1=max(util$usage), line=list(dash='dot', width=1))),
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date))
  }

  return(p)
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
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage(kWh/sq/month)'), margin = list(b = 100),
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
      title = "Time Series", yaxis2 = ay, yaxis = list(title='Usage(BTU/sqft/month)'), margin = list(b = 100),
      xaxis = list(type = "date", title="Date", tickformat = '%b-%y', tickvals = util$end_date)
    )
  }

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


plot_point_2 <- function(df, key)
{ 
  require(ggplot2)
  if (key == 'pre'){
    point_fig = geom_point(data = df, aes(x = x, y= y, shape = factor(z), color = 'Pre Consumption'))
  }else if (key == 'post')
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, shape = factor(z), color = 'Post Consumption'))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, shape = factor(z), color = 'retofit_act'))
  }
  return(point_fig)
}

main_plot_model <- function(util, best_model, bdbid_n, energy, b_name = '')
{ 
  options(digits=15) 
  if (flag_func(best_model, bdbid_n, energy))
  {   
      params_pre = params_list(best_model, bdbid_n, energy, 1)
      params_post = params_list(best_model, bdbid_n, energy, 3)

      bw_pre = best_worst(best_model, bdbid_n, energy, 1)
      bw_post = best_worst(best_model, bdbid_n, energy, 3)

      B_pre = params_matrix(params_pre)
      B_post = params_matrix(params_post)

      x1 = subset(util$OAT, util$prepost == 1)
      x2 = subset(util$OAT, util$prepost == 2)
      x3 = subset(util$OAT, util$prepost == 3)

      y1 = subset(util$usage, util$prepost == 1)
      y2 = subset(util$usage, util$prepost == 2)
      y3 = subset(util$usage, util$prepost == 3)

      z1 = ifelse(subset(util$estimated, util$prepost == 1) == 1 , 'Est', 'Act')
      #z2 = ifelse(subset(util$estimated, util$prepost == 2) == 1, 'Est', 'Act')
      z3 = ifelse(subset(util$estimated, util$prepost == 3) == 1, 'Est', 'Act')

      df1 = data.frame(x = x1, y = y1, z = z1)
      df3 = data.frame(x = x3, y = y3, z = z3)

      energy_key = ifelse(energy == 'Elec', 'elec', 'fuel')
      pre_plot_model = plot_model(x1, params_pre$model, B_pre, params_pre$cp1, params_pre$cp2, energy_key,1, FALSE, bw_pre, plot_ly())
      post_plot_model = plot_model(x3, params_post$model, B_post, params_post$cp1, params_post$cp2, energy_key,3, FALSE, bw_post, pre_plot_model)
      pre_point_fig = plot_point_3(df1, energy_key, 1,post_plot_model)
      final_figure = plot_point_3(df3, energy_key, 3, pre_point_fig, b_name)
  }else
  { 
    final_figure = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('No data points for', energy, 'for', b_name))
  }
  return(final_figure)
}


main_plot_baseload <- function(util, best_model, bdbid_n, energy, b_name = '')
{ 
  if (flag_func(best_model, bdbid_n, energy))
  {
      params_pre = params_list(best_model, bdbid_n, energy, 1)
      params_post = params_list(best_model, bdbid_n, energy, 3)

      bw_pre = best_worst(best_model, bdbid_n, energy, 1)
      bw_post = best_worst(best_model, bdbid_n, energy, 3)

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


get_dropdown_info_2 <- function(dir_path = 'shiny/retrofit_output', new_user_flag = FALSE)
{
  drop_auth(rdstoken = 'droptoken.rds')
  parent_path = drop_dir(dir_path)$path_lower
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


get_dd_date <- function(cat_df, category_n)
{
  path = subset(cat_df$child_path, cat_df$category == category_n)
  p_df = data.frame(p0 = drop_dir(path)$name)
  #p_df = data.frame(p0 = sort(p_df$p0, decreasing = FALSE))
  p_df$path = paste(path,p_df$p0,sep='/')
  return(p_df)
}

get_dd_year <- function(cat_df, point)
{
  path_point = subset(cat_df$path, cat_df$p1 == point)
  fy_df = data.frame(fy0 = drop_dir(path_point)$name)
  fy_df$path = paste(path_point,fy_df$fy0,sep='/')
  return(fy_df)
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
  help_df = data.frame(c('Time Series', 'Parameter Model', 'Parameters', 'Stats', 'Post Modeller Table', 'Adjusted Baseline' ,'Gross Savings'))
  help_df$Requirement = c('Utility CSV (a must)', 'Utility CSV and Best Model CSV', 'Utility CSV and Best Model CSV','Utility CSV and Best Model CSV', 'Utility CSV and Post Modeller Calcs CSV', 'Utility CSV and Best Model CSV','Total Savings CSV and Savings Uncertainty CSV')
  colnames(help_df) = c('Graph/Table', 'Requirement')
  return(help_df)
}
