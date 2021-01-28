
# REQUIRED LIBRARIES

library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)

# READING JSON STREAMING HISTORY

streamHistory <- fromJSON("StreamingHistory0.json", flatten = TRUE)

# ADDING DATE AND TIMING 

mySpotify <- streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% as_date, seconds = msPlayed / 1000, minutes = seconds / 60)

# PLAYBACK ACTIVITY PER WEEK AND HOURS

streamingHours <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date) %>% 
  group_by(date = floor_date(date, "week")) %>%
  summarize(hours = sum(minutes) / 60) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = hours)) + 
  geom_col(aes(fill = hours)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Date", y= "Hours of music playback") + 
  ggtitle("On what dates I've listened to more or less music on Spotify?", "Playback activity per week")
streamingHours
ggplotly()

# PLAYBACK ACTIVITY PER SPECIFIC ARTIST

hoursArtist <- mySpotify %>% 
  group_by(artistName, date = floor_date(date, "month")) %>% 
  summarize(hours = sum(minutes) / 60) %>% 
  ggplot(aes(x = date, y = hours, group = artistName)) + 
  labs(x= "Date", y= "Hours of music playback") + 
  ggtitle("On what dates I've listened to more or less music by a specific artist?", "E.g: Alton Ellis and Jarabe de Palo") +
  geom_line() + 
  gghighlight(artistName == "Alton Ellis" || artistName == "Jarabe De Palo") 
hoursArtist
ggplotly()

# MOST LISTENED ARTISTS (MORE THAN 3 HOURS)

minutesMostListened <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(artistName) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  filter(minutesListened >= 180) %>%
  ggplot(aes(x = artistName, y = minutesListened)) + 
  geom_col(aes(fill = minutesListened)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Artist", y= "Minutes of music playback") + 
  ggtitle("What were the most listened artists on my Spotify?", "> 3 hours listened") +
  theme(axis.text.x = element_text(angle = 90))
minutesMostListened
ggplotly()

# PLAYBACK ACTIVITY BY DATE AND TIME OF DAY

timeDay <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date, hour = hour(endTime)) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = date, fill = minutesListened)) + 
  geom_tile() + 
  labs(x= "Time of the day", y= "Date") + 
  ggtitle("When has there been more playback activity on my Spotify?", "Activity by date and time of day") +
  scale_fill_gradient(low = "yellow", high = "red")
timeDay
ggplotly()

# PLAYBACK ACTIVITY BY TIME OF THE DAY

hoursDay <- mySpotify %>% 
  filter(date >= "2019-01-01") %>% 
  group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
  summarize(minutesListened = sum(minutes))

hoursDay %>% 
  ggplot(aes(x = hour, y = minutesListened, group = date)) + 
  geom_col(fill = "#ff6600") +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What time of day I've listened to the most music on Spotify?", "Activity from 0 to 24 hours") 
ggplotly()  

# PLAYBACK ACTIVITY BY TIME OF THE DAY AND WEEKDAY

hoursDay %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, weekday, fill = minutes)) + 
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x= "Time of the day", y= "Weekday") + 
  ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Weekly activity from 0 to 24 hours") 
ggplotly()

# PLAYBACK ACTIVITY BY TIME OF THE DAY AND WEEKDAY - LINE CHART

weekDay <- hoursDay %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = weekday)) + 
  geom_line() +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Line chart - Weekly activity from 0 to 24 hours") 
weekDay
ggplotly()  

# PLAYBACK ACTIVITY BY DAY TYPE

dayType <- hoursDay %>% 
  mutate(day_type = if_else(weekday %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
  group_by(day_type, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = day_type)) + 
  geom_line() +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What day type I've listened to the most music on Spotify?", "Weekday and weekend activity from 0 to 24 hours") 
dayType
ggplotly()  

# ESTABLISH CONNECTION SPOTIFY API

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_CLIENT_SECRET')

get_spotify_authorization_code()

# GET SPECIFIC PLAYLIST FEATURES

playlist_username <- 'cosmoduende'
playlist_uris <- c('0PV31w7ireeI6d0oSYJ2H0')
playlistFavsEnglish <- get_playlist_audio_features(playlist_username, playlist_uris)

# PLOT LESS POPULARITY TRACKS ON SPECIFIC PLAYLIST 

playlistFavsEnglish %>% 
  group_by(track.popularity) %>% 
  filter(track.popularity <= "35" && track.popularity != "0" ) %>%
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly()

# GET FAVORITE TRACKS

myFavTracks <- ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% 
  reduce(rbind) %>%
  write_rds('raw_myFavTracks.rds')

# GET FIRST ADDED LIKED TRACK

myFavTracks %>%
  mutate(added_at = ymd_hms(added_at)) %>%
  arrange(added_at) %>%
  head(1, wt = added_at) %>%
  select(track.name, track.album.name, added_at)  %>%
  kable()

# GET TOP ARTISTS BASED ON LIKED TRACKS 

favTracksArtist <- myFavTracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)

trackNumArtist <- favTracksArtist %>%
  count(id, sort = TRUE) %>%
  left_join(favTracksArtist, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(10, n)

# PLOT TOP 10 ARTISTS BASED ON LIKED TRACKS

plotMyFavs <- trackNumArtist %>%
  mutate(freq = case_when(n > 100 ~ '> 100 tracks',
      between(n, 50, 99) ~ '50-99 tracks',
      between(n, 20, 49) ~ '20-49 tracks',
      TRUE ~ '< 20 tracks')) %>%
  mutate(freq = factor(freq, levels = c('> 100 tracks', '50-99 tracks', '20-49 tracks', '< 20 tracks'))) %>%
  ggplot(mapping = aes(x = reorder(name, -n), y = n, fill = freq)) +
  geom_col() +
  scale_fill_brewer(palette="Dark2") +
  labs(x= "Artist name", y= "Number of tracks", fill = NULL) +
  ggtitle("What are my Top 10 favorite artists?", "Based on my ♥ tracks") +
  theme(axis.text.x = element_text(angle = 90))
plotMyFavs
ggplotly()

# GET FEATURES TOP FOUR FAVORITE ARTISTS

favArtist1 <- get_artist_audio_features(artist= "Bunbury")
favArtist2 <- get_artist_audio_features(artist= "Kevin Johansen")
favArtist3 <- get_artist_audio_features(artist= "Vicentico")
favArtist4 <- get_artist_audio_features(artist= "Los Auténticos Decadentes")

# MAKE A SINGLE DATA SET

topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)

# PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS

emotionalQuadrant <- ggplot(data = topFourArtists, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry / Turbulent") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful / Happy") +
  annotate('text', 1.75 / 2, 0.05, label = "Peace / Chill") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing / Sad") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional quadrant Top four artists", "Based on energy y valence")  
emotionalQuadrant
ggplotly()


