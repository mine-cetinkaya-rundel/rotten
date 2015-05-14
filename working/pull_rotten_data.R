#' Pull Rotten Tomatoes data from the API corresponding to movie ids from IMDB
#' 
#' @param ids vectors containing movie id from IMDB for which Rotten Tomatoes data is wanted
#' @param key Rotten Tomatoes API key as a string
#' 
#' @return a data frame of values from Rotten Tomatoes, one row for each movie
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' 
#' @examples
#' pull_rotten_data(all$id[1:5], "[key]")
pull_rotten_data <- function(ids, key, ...) {
    data.frame(id = ids) %>%
    mutate(url = paste0("http://api.rottentomatoes.com/api/public/v1.0/movie_alias.json?id=", 
                        id, 
                        "&type=imdb&apikey=",
                        key)) %>%
    rowwise() %>%
    do(json = readLines(.$url, skipNul = TRUE)[nchar(readLines(.$url, skipNul = TRUE)) > 0], sleep = Sys.sleep(1)) %>% select(-sleep) -> res
    
    json <- "["
    json <- paste0(json, do.call(paste, list(res$json, collapse=',')))
    json <- paste0(json, "]")
    
    return(fromJSON(json))
}

#' Pull boxoffice data from IMDB corresponding to movies data from IMDB
#' 
#' @param match_data data frame containing movie id from IMDB
#' @param batch_id vector of row ids within \code{match_data} to pull from Rotten Tomatoes
#' @examples
#' pull_boxoffice_data(all$id[1:5])
pull_boxoffice_data <- function(ids) {
    data.frame(id = ids) %>%
      mutate(url = paste0("http://www.imdb.com/title/tt", id,"/business?ref_=tt_dt_bus")) %>%
      mutate(text = html(url) %>% html_nodes("#tn15content") %>% html_nodes("text")) %>%
      mutate(budget = str_split(text, "Budget")[2],
             gross = str_split(text, "Budget")[2])
      
}