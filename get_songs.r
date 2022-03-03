library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)





download <- function(from, to){
  base <- "https://music.abcradio.net.au/api/v1/plays/search.json?limit=100&offset=0&page=0&station=triplej&from="
  from_char <- format(from, "%Y-%m-%dT%H:%M:%S.000Z")
  to_char <- format(to, "%Y-%m-%dT%H:%M:%S.000Z")
  url <- paste0(base,
                from_char,
                "&to=",
                to_char)
  res <- GET(url)
  if (res$status == 200) {
    data <- fromJSON(rawToChar(res$content))
    list <- data$items
    tb <- tibble(
      song_title = list$recording$title,
      artist = map_chr(list$recording$artists, ~.$name[1]),
      played_time = ymd_hms(list$played_time)
    ) %>% 
      arrange(played_time)
    return(tb)
  }
}

get_songs <- function(start, end, progress = FALSE) {
  from <- ymd_hms(start)
  to <- from + dhours(5)
  end <- ymd_hms(end)
  
  songs <- NULL
  
  while (to < end) {
    if (progress) {
      print(from)
      print(to)
    }
    tb <- download(from, to)
    songs <- bind_rows(songs, tb)
    from <- to + dseconds(1)
    to <- to + dhours(5)
  }
  tb <- download(from, end)
  songs <- bind_rows(songs, tb)
  return(songs)
}



songs <- get_songs(ymd_hms("2020-12-31 13:00:01 UTC"),
                   ymd_hms("2021-12-31 12:59:59 UTC"))


