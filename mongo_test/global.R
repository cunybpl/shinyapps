get_key <- function(){
	usr = Sys.getenv("mongodb_username")
	pwd = Sys.getenv("mongodb_pwd")
	return(list(usr = usr, pwd = pwd))
}