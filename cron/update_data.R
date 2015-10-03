library("sejmRP")
library(RPostgreSQL)

#only arguments after --args
args <- commandArgs(TRUE)

#connecting to database
drv <- dbDriver("PostgreSQL")
database_diet <- dbConnect(drv,dbname = dbname,user = user,password = password,host = host)

#updating deputies table
tryCatch(deputies_update_table(dbname,user,password,host),
  error = function(err){ 
    suppressWarnings(dbDisconnect(database_diet))
    stop("Error during updating deputies table")
  })

#updating votings table
tryCatch(votings_update_table(dbname,user,password,host, verbose=TRUE),
  error = function(err){ 
    suppressWarnings(dbDisconnect(database_diet))
    #removing a flag file if error occured
    file.remove("sejmRP_votings_flag")
    stop("Error during updating votings table")
  })

#updating votes table
tryCatch(votes_update_table(dbname,user,password,host, verbose=TRUE),
  error = function(err){ 
    suppressWarnings(dbDisconnect(database_diet))
    stop("Error during updating votes table")
  })

#updating statements table
tryCatch(statements_update_table(dbname,user,password,host),
  error = function(err){ 
    suppressWarnings(dbDisconnect(database_diet))
    stop("Error during updating statements table")
  })

#disconnecting to database
suppressWarnings(dbDisconnect(database_diet))