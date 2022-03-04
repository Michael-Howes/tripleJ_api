library(tidyverse)


songs_2021 <- read_csv("2021_songs.csv")

GO <- songs_2021 %>% 
  filter(artist == "Genesis Owusu") 

GO_top_songs <- GO %>% 
  mutate(song_title = imap_chr(map_if(GO$song_title, 
                                      function(x) {return(x == "Waitin On Ya")},
                                      function(x) {return("Waitin' On Ya")}), ~.x[1])) %>% 
  group_by(song_title) %>% 
  mutate(i = 1,
         plays_over_time = cumsum(i),
         total_plays = sum(i)) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(desc(total_plays))) %>% 
  select(!i) %>% 
  filter(rank <= 6) %>% 
  bind_rows() %>% 
  mutate(song_title = fct_reorder2(song_title, 
                                   played_time,
                                   plays_over_time))
start_end <- GO_top_songs %>% 
  mutate(max = max(played_time),
         min = min(played_time)) %>% 
  group_by(song_title) %>% 
  summarise(max = mean(max),
            min = mean(min), 
            rank = mean(rank),
            total_plays = mean(total_plays),
            artist = "Genesis Owusu") %>% 
  pivot_longer(c(max,min), names_to = "plays_over_time", values_to = "played_time") %>% 
  mutate(plays_over_time = total_plays*(plays_over_time == "max"))




p <- ggplot(bind_rows(GO_top_songs, start_end)) +
  geom_line(aes(x = played_time, 
                y = plays_over_time,
                color = song_title)) +
  labs(title = "Genesis Owusu",
       color = "Song", 
       x = "Date",
       y = "Cummulative plays")

