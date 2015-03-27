library(rjson)
library(stringr)
library(dplyr)

# data to match downloaded from imdb
all = read.csv("working/all.csv", stringsAsFactors = FALSE)
all$id = str_replace(all$const,"^tt","")

pull_rotten_data <- function(key, match_data, batch_id) {
  match_data[batch_id, ] %>%
    mutate(url = paste0("http://api.rottentomatoes.com/api/public/v1.0/movie_alias.json?id=", 
                        id, 
                        "&type=imdb&apikey=",
                        key)) %>%
    mutate(i = batch_id) %>%
    mutate(filename = paste0("json/", i,"_", id, ".json")) %>%
    rowwise() %>%
    do(success = download.file(url = .$url, destfile = .$filename) >%% Sys.sleep(1))
}