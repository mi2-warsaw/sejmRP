#' Retrieve filtered statements from a database
#'
#' Function \code{get_filtered_statements} reads filtered statements from a database.
#'
#' @details
#' Function \code{get_filtered_statements} reads filtered statements from a database.
#' The result of this function is an invisible data frame with statements' data.
#'
#' Possible filters:
#' \enumerate{
#' \item terms_of_office - range of terms of office's numbers. This filter is a integer
#' vector with two elements, where the first describes a left boundary of range
#' and the second a right boundary. It is possible to choose only one term of office,
#' just try the same number as first and second element of vector.
#' \item deputies - full names of deputies. This filter is a character vector with full
#' names of deputies in format: 'surname first_name second_name'. If you are not sure
#' if the deputy you were thinking about has second name, try 'surname first_name' or
#' just 'surname'. There is high probability that proper deputy will be chosen.
#' It is possible to choose more than one deputy.
#' \item dates - period of time. This filter is a character vector with two elements
#' in date format 'YYYY-MM-DD', where the first describes left boundary of period and
#' the second right boundary. It is possible to choose only one day, just try the same
#' date as first and second element of vector.
#' \item topics - text patterns. This filter is a character vector with text patterns of
#' topics in order points. Note that the order points are written like
#' sentences, so remember about case inflection of nouns and adjectives and use stems of
#' words as patterns. For example if you want to find order points about education 
#' (in Polish: szkolnictwo) try 'szkolnictw'. It is possible to choose more than one pattern.
#' \item content - text patterns. This filter is a character vector with text patterns
#' in statements. Note that strings with statements are sentences, so remember about case
#' inflection of nouns and adjectives and use stems of words as patterns. 
#' For example if you want to find order points about education (in Polish:
#' szkolnictwo) try 'szkolnictw'. It is possible to choose more than one pattern.}
#'
#' If you did not choose any filter, the whole database will be downloaded.
#' Note that, due to data size (<= ~150 MB) it may take few seconds / minutes
#' to download all statements.
#'
#' Because of encoding issue on Windows operation system, you also need to select
#' if you use Windows.
#'
#' @usage get_filtered_statements(dbname = 'sejmrp', user = 'reader',
#'   password = 'qux94874', host = 'services.mini.pw.edu.pl',
#'   windows = .Platform$OS.type == 'windows', terms_of_office = integer(0), 
#'   deputies = character(0), dates = character(0), topics = character(0))
#'   content = integer(0)
#'
#' @param dbname name of database; default: 'sejmrp'
#' @param user name of user; default: 'reader'
#' @param password password of database; default: 'qux94874'
#' @param host name of host; default: 'services.mini.pw.edu.pl'
#' @param windows information of used operation system; default: .Platform$OS.type == 'windows'
#' @param terms_of_office range of terms of office's numbers that will be taken to filter data
#' from database; default: integer(0)
#' @param terms_of_office range of terms of office's numbers that will be taken to filter data
#' from database; default: integer(0)
#' @param deputies full names of deputies that will be taken to filter data from database;
#' default: character(0)
#' @param dates period of time that will be taken to filter data from database;
#' default: character(0)
#' @param topics text patterns that will be taken to filter data from database;
#' default: character(0)
#' @param content text patterns that will be taken to filter data from database;
#' default: character(0)
#' @param max_rows maximum number of rows to download
#'
#' @return data frame with NULL
#'
#' @examples
#' \dontrun{
#' filtered_statements <- get_filtered_statements()
#' dim(filtered_statements)
#' # [1] 2568       6
#' names(filtered_statements)
#' [1] 'id_statement' 'nr_term_of_office' 'surname_name' 'date_statement'
#' [5] 'titles_order_points' 'statement'
#' object.size(filtered_statements)
#' # 6488552 bytes}
#'
#' @note
#' Default parameters use privilages of 'reader'. It can only SELECT data from database.
#'
#' All information is stored in PostgreSQL database.
#'
#' @author Tomasz Mikolajczyk, Piotr Smuda
#'
#' @export
#'
#' @importFrom dplyr src_postgres
#' @importFrom dplyr tbl
#' @importFrom dplyr sql
#' @importFrom dplyr filter
#' @importFrom dplyr between
#' @importFrom dplyr mutate
#' @importFrom dplyr collect
#'

get_filtered_statements <- function(dbname = "sejmrp", user = "reader", password = "qux94874", 
                                    host = "services.mini.pw.edu.pl", windows = .Platform$OS.type == "windows", 
                                    terms_of_office = integer(0), deputies = character(0), 
                                    dates = character(0), topics = character(0), content = character(0), 
                                    max_rows=Inf) {
  stopifnot(is.numeric(max_rows), is.character(dbname), is.character(user), is.character(password), 
            is.character(host), is.logical(windows), is.numeric(terms_of_office), is.character(deputies), 
            is.character(dates), is.character(topics), is.character(content), 
            all(c(terms_of_office)%%1 == 0))
  
  length_terms_of_office <- length(terms_of_office)
  length_deputies <- length(deputies)
  length_dates <- length(dates)
  length_topics <- length(topics)
  length_content <- length(content)
  
  stopifnot(length_terms_of_office == 0 | length_terms_of_office == 2, length_deputies >= 0, length_dates == 0 | length_dates == 2, 
            length_topics >= 0, length_content >= 0)
  
  # connecting to database with dplyr to get statements
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
  
  # add information about new SELECT to the counter table
  dbSendQuery(database_diet, paste0("INSERT INTO counter (what, date) VALUES ('filt_statements','", Sys.Date(), "')"))
  
  suppressWarnings(dbDisconnect(database_diet))
  
  # fake variables in order to pass CRAN CHECK
  nr_term_of_office <- NULL
  surname_name <- NULL
  date_statement <- NULL
  titles_order_points <- NULL
  statement <- NULL
  `%SIMILAR TO%` <- NULL
  
  # connecting to database with dplyr to get statements
  database_diet <- src_postgres(dbname = dbname, user = user, password = password, host = host)
  
  # read data dodac potem
  statements <- tbl(database_diet, sql("SELECT * FROM statements"))
  
  # terms_of_office filter
  if (length_terms_of_office == 2) {
    statements <- filter(statements, between(nr_term_of_office, terms_of_office[1], terms_of_office[2]))
  } 
  
  # deputies filter
  if (length_deputies > 0) {
    # changing polish characters for any character
    deputies <- stri_replace_all_regex(deputies, "[^a-zA-Z %]", "_")
    deputies <- paste0("(%", deputies, "%)")
    deputies <- paste0(deputies, collapse = "|")
    statements <- filter(statements, surname_name %SIMILAR TO% deputies)
  }
  
  # dates filter
  if (length_dates == 2) {
    statements <- filter(statements, between(date_statement, dates[1], dates[2]))
  }
  
  # topics filter
  if (length_topics > 0) {
    # changing polish characters for any character
    topics <- stri_replace_all_regex(topics, "[^a-zA-Z %]", "_")
    topics <- paste0("(%", topics, "%)")
    topics <- paste0(topics, collapse = "|")
    statements <- filter(statements, titles_order_points %SIMILAR TO% topics)
  }
  
  # content filter
  if (length_content > 0) {
    # changing polish characters for any character
    content <- stri_replace_all_regex(content, "[^a-zA-Z %]", "_")
    content <- paste0("(%", content, "%)")
    content <- paste0(content, collapse = "|")
    statements <- filter(statements, statement %SIMILAR TO% content)
  }
  
  # reading data
  statements <- as.data.frame(collect(statements, stringsAsFactors = FALSE, n = max_rows))
  
  # if empty result of query
  if (nrow(statements) == 0) {
    suppressWarnings(dbDisconnect(database_diet$con))
    return(statements)
  }
  
  # encoding for windows
  if (windows) {
    statements[, 3] <- iconv(statements[, 3], from = "UTF-8", to = "Windows-1250")
    statements[, 5] <- iconv(statements[, 5], from = "UTF-8", to = "Windows-1250")
    statements[, 6] <- iconv(statements[, 6], from = "UTF-8", to = "Windows-1250")
  }
  
  suppressWarnings(dbDisconnect(database_diet$con))
  return(invisible(statements))
  
}
