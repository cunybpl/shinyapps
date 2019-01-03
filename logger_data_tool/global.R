library(mongolite)

construct_mongo_url <- function(username, password){
  str1 = 'mongodb+srv://'
  str2 = '@cluster0-mrfjh.mongodb.net/test?retryWrites=true'
  final_str = paste(str1, as.character(username), ':', as.character(password), str2, sep = '')
  return(final_str)
}
