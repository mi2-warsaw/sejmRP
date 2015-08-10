#' Getting data about deputies
#'
#' Function \code{srp_deputies_data} gets data about deputies.
#'
#' @details
#' // to do
#'
#' @usage srp_deputies_data(type)
#'
#' @param type type of deputies which be add to table with deputies: active, inactive
#'
#' @return data frame with two columns: id_deputy, surname_name
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

srp_deputies_data <- function(type){
  
  #choosing proper page of deputies
  if(type=="active"){
    page <- "http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=A"
  } else if(type=="inactive"){
    page <- "http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=B"
  }
  
  #getting data from page with deputies
  page <- html(page)
  deputies_data <- html_nodes(page, "#contentBody li")
  
  #getting ids of deputies
  links <- html_nodes(deputies_data, "a")
  links <- html_attrs(links)
  links <- sapply(links,function(element){
    element[2]
  })
  id_deputies <- unlist(stri_extract_all_regex(links,"(?<=id=)[0-9]+"))
  
  #getting names and surnnames of deputies
  deputies <- html_nodes(deputies_data, ".deputyName")
  deputies <- html_text(deputies)
  
  #creating data frame with deputies data
  deputies_df <- data.frame(id_deputy=id_deputies,surname_name=deputies,stringsAsFactors=FALSE)
  
  return(deputies_df)
}