#' tbd


statements_get_statements_data <- function(statements_links){
  stopifnot(is.list(statements_links),length(statements_links)>0)
  
  #Get speakers' names
  names <- html_text(statements_links)
  
  #Check whether the speaker is a deputy 
  if_deputy <- stri_detect_regex(names,"Poseł |Minister |[p|P]rezes Rady Ministrów ")
  names <- names[if_deputy]
  
  #Get links to statements
  statements_links <- html_attr(statements_links,"href")
  statements_links <- statements_links[if_deputy]
  statements_links <- paste0(home_page,statements_links)
  
  #Get statements' ids
  ids <- unlist(stri_extract_all_regex(statements_links,"[0-9]+$"))
  
  #Create dataframe with deputies' names and statements and statements' ids
  statements_data <- data.frame(names=names,statements_links=statements_links,ids=ids,stringsAsFactors=FALSE)
  
  return(statements_data)
}