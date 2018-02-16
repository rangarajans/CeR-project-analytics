library(RMySQL)

sqlQuery <- function(query) {
  mydb = dbConnect(MySQL(), user='dbuser', password='dbuser123', dbname='projectdb', host='localhost')
  on.exit(dbDisconnect(mydb))
  ret <- dbSendQuery(mydb, query)
  result <- fetch (ret, -1)
  return(result)
}

quoteString <- function(string) {
  # text <- paste(string, collapse = '\',\'')
  text <- paste('\'', string, '\'', sep = '')
  return(text)
}

colName <- function(string, data) {
  colNum <- unique(which(data == string, arr.ind = TRUE)[,2])
  return(colnames(data)[colNum])
}
