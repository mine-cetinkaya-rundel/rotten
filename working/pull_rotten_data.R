library(rjson)
library(stringr)
library(dplyr)
library(rvest)
library(tidyr)

# data to match downloaded from imdb
all = read.csv("all.csv", stringsAsFactors = FALSE)
all$id = str_replace(all$const,"^tt","")


#' Pull Rotten Tomatoes data from the API corresponding to movie ids from IMDB
#' 
#' @param ids vectors containing movie id from IMDB for which Rotten Tomatoes data is wanted
#' @param key Rotten Tomatoes API key as a string
#' @param ... further parameters passed to \code{download.file}, see 
#' @seealso \code{\link{download.file}} for further options
#' @examples
#' pull_rotten_data(all$id[1:5], "[key]", method = "internal", mode = "a")
pull_rotten_data <- function(ids, key, ...) {
    data.frame(id = ids) %>%
    mutate(url = paste0("http://api.rottentomatoes.com/api/public/v1.0/movie_alias.json?id=", 
                        id, 
                        "&type=imdb&apikey=",
                        key)) %>%
    mutate(i = batch_id) %>%
    rowwise() %>%
    do(success = download.file(url = .$url, destfile = "json/tmp.json", ...) %>% Sys.sleep(1))
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