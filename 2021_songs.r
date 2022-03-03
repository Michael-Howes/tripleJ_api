source("get_songs.r")
songs <- get_songs(ymd_hms("2020-12-31 13:00:01 UTC"),
                   ymd_hms("2021-12-31 12:59:59 UTC"),
                   progress = TRUE)

write_csv(songs, "2021_songs.csv")