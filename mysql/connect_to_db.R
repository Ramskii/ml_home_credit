library(DBI)

con <- dbConnect(RMySQL::MySQL(), host = "localhost", port = 3306,
                 user = "remote_user", password = "admin", dbname = "ml_projet")

dbListTables(con)

res <- dbSendQuery(con, "SELECT * FROM application_test limit 5")
dbFetch(res)
dbClearResult(res)