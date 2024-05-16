# Alex Abbas
# 211008
# access my playlists on Spotify

library(tidyverse)
library(spotifyr)
library(httpuv)

clientid = "1e3e367c14764614b74fd97888444a27"
clientsecret = "a8ea7e8f974349efaea5fdda89603a9e"

Sys.setenv(SPOTIFY_CLIENT_ID = clientid)
Sys.setenv(SPOTIFY_CLIENT_SECRET = clientsecret)

access_token <- get_spotify_access_token()
#access_token <- get_spotify_authorization_code(scope = spotifyr::scopes)#scope = 'playlist-read-private')

my_id = "alexanderabbas"

all_plists <- get_user_playlists(my_id, limit = 50)#,authorization = access_token)
my_plists = all_plists %>% filter(owner.id == "alexanderabbas")

tracks <- apply(my_plists, 1, function(plist) {
  get_playlist_tracks(plist["id"],fields = c("track.name","track.id","track.album.name","name")) %>% mutate(playlist = plist$name)
})

tracksDat <- bind_rows(tracks) %>% filter(!is.na(track.id))

idChunks = split(tracksDat$track.id, ceiling(seq_along(tracksDat$track.id)/50))
getArtistChunk = function(someIds) {
  res = get_tracks(someIds)$artists
  sapply(res,function(x) { x[1,"name"] } )
}
artistChunks = sapply(idChunks,getArtistChunk)
artistVec = unlist(artistChunks)

tracksDat = tracksDat %>% mutate(artist = artistVec)


trackColClasses <- lapply(tracksDat,class)
trackLists <- names(trackColClasses[trackColClasses=="list"])
tracksDat <- tracksDat %>% select(-!!trackLists)

tracksDat = tracksDat[colSums(!is.na(tracksDat)) > 0]

tracksTable = tracksDat %>% select(track.name) %>% unlist() %>% table()
duplicateTracks = sort(tracksTable[tracksTable>1])

write.csv(tracksDat,"/Volumes/GoogleDrive-116919193630903670642/My Drive/etc/music/spotify/spotifyPlaylists220316.csv")

likedPlaylists <- read.delim("~/Google Drive (alexabbas@gmail.com)/etc/music/spotify/playlistsLiked.txt",header = F)$V1
likedPlTracksDf <- tracksDat %>% filter(playlist %in% likedPlaylists)

duplicateLikedTracks = table(likedPlTracksDf$track.name)

tracksDat %>% filter(playlist == "Cool slice") %>% left_join(
  data.frame(
    track.name = names(duplicateLikedTracks),
    trackCount = duplicateLikedTracks
  )
)
