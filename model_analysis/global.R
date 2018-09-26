library(plotly)
library(rdrop2)
library(shiny)

plot_model <- function(x, model, B, cp1, cp2, key, unit, bw, flag_line_type = 1, p1 = plot_ly())
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

  line_name = ifelse(key == 'elec', paste(model, 'Elec', sep = ' '), paste(model, 'Fuel', sep = ' '))
  line_type = ifelse(flag_line_type == 1, 'solid', 'dash')
  model_color = ifelse(model == '4P', 'rgba(51, 113, 213, 1)', 'rgba(51, 213, 56, 1)') #ignore this for other.
  #energy_color = ifelse(key == 'elec', 'rgba(51, 113, 213, 1)', 'rgba(240, 24, 28,1)')

	model_fig = add_trace(p = p1, x = ~df$x, y = ~df$y, type ='scatter', mode = 'lines', line = list(color = model_color, dash = line_type), name = line_name, inherit = FALSE)
  
  return(model_fig)
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



params_list_2 <- function(modeller, bdbid, energy, model, prepost)
{ 
  options(digits=15)

  Ycp = modeller$ycp[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model]
  cp1 = modeller$xcp1[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model]
  cp2 = ifelse(model == '5P', modeller$xcp2[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model], 0)

  slope1 = ifelse(model == '3PC' | model == '2P', modeller$rs[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model], modeller$ls[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model])
  slope2 = ifelse(model == '4P' | model == '5P', modeller$rs[modeller$bdbid == bdbid & modeller$energy_type == energy & modeller$prepost == prepost & modeller$model_type == model], 0)

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


plot_point_3 <- function(df, key, model_fig = plot_ly(), b_name)
{ 
  util_act = subset(df, df$z == 'Act')
  util_est = subset(df, df$z == 'Est')

  if (key == 'elec'){
    #point_fig = geom_point(data = df, aes(x = x, y= y, shape= factor(z), color = 'Elec Consumption'))
    point_fig_act = add_trace(p = model_fig, x = ~util_act$x, y = ~util_act$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(148, 150, 153, 1)', size = 9), name = 'Elec Act', inherit = FALSE)
    if (length(unique(df$z)) == 3)
    {
      util_agg = subset(df, df$z == 'Agg')
      point_fig_act = add_trace(p = point_fig_act, x = ~util_agg$x, y = ~util_agg$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle', color = 'rgba(38, 114, 38, 1)', size = 9), name = 'Elec Agg', inherit = FALSE)
    }
    point_fig = add_trace(p = point_fig_act, x = ~util_est$x, y = ~util_est$y, type ='scatter', mode ='markers', marker = list(symbol = 'circle-open', color = 'rgba(148, 150, 153, 1)', size = 9), name = 'Elec Est', inherit = FALSE)%>%
    layout(title = b_name, showlegend = TRUE, margin = list(b = 100),
      xaxis = list(title = "Temperature"),
      yaxis = list(title = "Usage (kWh)"))
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
      yaxis = list(title = "Usage (BTU)"))
  }else
  {
    point_fig = geom_point(data = df, aes(x = x, y= y, color = 'retofit_act'))
  }
  return(point_fig)
}


main_plot <- function(temp_df, modeller, best_model = NULL, bdbid_n, energy, b_name = "")
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

    if (bdbid_n %in% unique(modeller$bdbid)) #bdbid is in best_model
    {
      energy_type_vec = subset(modeller$energy_type, modeller$bdbid == bdbid_n)

      if(energy %in% energy_type_vec) #energy and bdbid is in util and best
      {
        final_figure = main_line_point_plot(df1, modeller, best_model, bdbid_n, energy, b_name)
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

main_line_point_plot <- function(df1, modeller, best_model, bdbid_n, energy, b_name)
{	
	model_vec = modeller$model_type[modeller$bdbid == bdbid_n & modeller$energy_type == energy & modeller$prepost == 1]
	x1 = df1$x
	model_fig = plot_ly()
	key = tolower(energy)
	for (model in model_vec)
	{
		params = params_list_2(modeller, bdbid_n, energy, model, 1)
    	B = params_matrix(params)
    	#bw = best_worst(best_model, bdbid_n, energy, 1) still in progress
    	flag_line_type = solid_dash_dot_flag_func(best_model, bdbid_n, energy, model)
    	model_fig = plot_model(x1, params$model, B, params$cp1, params$cp2, key, FALSE, bw = FALSE, flag_line_type, p1 = model_fig)
	}
	final_figure = plot_point_3(df1, key, model_fig, b_name)
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

solid_dash_dot_flag_func <- function(best_model, bdbid_n, energy, model)
{	
	if (is.null(best_model))
	{
		return(1)
	}
	model_from_best = subset(best_model$model_type, best_model$bdbid == bdbid_n & best_model$energy_type == 'Elec')
	flag_line_type = ifelse(model_from_best == model, 1, 0)#1 best model;0 not best
	return(flag_line_type)
}

get_dropdown_info_2 <- function(dir_path = 'shiny/model_analysis', new_user_flag = FALSE)
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

create_response_id <- function(bdbid)
{ 
  res_vec <- c('user_name')
  for (bdbid_n in bdbid)
  {
    temp = paste('res_in', bdbid_n, sep = '')
    res_vec = c(res_vec,temp)
  }
  res_vec = c(res_vec, 'misc_note')
  return(res_vec)
}

saveData <- function(data, bdbid) {
  #outputDir = 'shiny/responses'
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  colnames(data) = c('Name', bdbid, 'Note')
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = 'shiny/responses')
}


saveData2 <- function(data, bdbid) {
  #outputDir = 'shiny/responses'
  # Create a unique file name

  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  colnames(data) = c('Name', bdbid, 'Note')
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = 'shiny/responses')
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir('shiny/responses')
  filePaths <- filesInfo$path_lower
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  lapply(filePaths, drop_delete)
  return(data)
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