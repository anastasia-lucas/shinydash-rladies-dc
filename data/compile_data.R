rm(list = ls())

library(geniusr)
library(dplyr)
# also requires tidytext

geniusr::genius_token()

################################################################################
#                                   functions                                  #
################################################################################

#' Create a data.frame of lyrics for specified albums
#' @param albums list of albums
#' @param artist artist name if consistent across albums or list of artist names
#' @return data.frame with unnested lyrics
create_lyrics_df <- function(albums, artist){
  
  if(length(artist) != 1 & length(artist) != length(albums)){
    stop("Albums and artists have inconsisent lengths")
  }
  
  if(length(artist) == 1 & length(albums) > 1){ artist <- rep(artist, length(albums)) }
  
  album.tracklists <- mapply(function (x,y) get_album_tracklist_search(x, y), 
                             artist, albums, SIMPLIFY = FALSE)
  
  # song.lyrics <- lapply(album.tracklists, function(x) {
  #   lapply(x$song_lyrics_url, get_lyrics_url)
  #   })
  
  # very non-elegant solution but the above was resulting in a weird 
  # inconsistent bug that seems like the API call was getting overloaded
  album.lyrics <- list()
  
  for(j in 1:length(album.tracklists)){
    
    x <- album.tracklists[[j]]
    
    all.lyrics <- list()
    
    for(i in 1:length(x$song_lyrics_url)){
      
      print(paste("Fetching lyrics for...", x$song_title[[i]]))
      
      all.lyrics[[i]] <- tryCatch(
        { get_lyrics_url(x$song_lyrics_url[i]) },
        error = function(e) { 
          print("Trying again...")
          Sys.sleep(30)
          tryCatch( 
            { get_lyrics_url(x$song_lyrics_url[i]) },
            error = function(e){ NULL }
           )
 
          }
        )
                                  
      Sys.sleep(12)
    }
    
    lyrics.combined <- do.call("rbind", all.lyrics)
    
    album.lyrics[[j]] <- lyrics.combined
    
    if(j < length(album.tracklists)){
      print("Giving the API a rest...")
      Sys.sleep(60)
    }
    
  }
  
  names(album.lyrics) <- albums
  
  tidy.lyrics <- bind_rows(album.lyrics, .id = "id") %>%
    tidytext::unnest_tokens(word, line) %>%
    rename(album = id)

  return(tidy.lyrics)

}


#' Remove stop words and format for exercises
#' @param data.frame as returned from get_lyrics()
#' @return data.frame with track_title, word, album, n (occurrences)
clean_lyrics <- function(lyrics){
   lyrics.clean <- lyrics %>% 
     anti_join(tidytext::stop_words, 
               by = "word") %>%
     group_by(song_name, word, album) %>%
     tally()
   
   return(lyrics.clean)
}

################################################################################
#                                   variables                                  #
################################################################################

albums <- list()

albums$lady.gaga <- c("The Fame",
                      "Born This Way",
                      "ARTPOP",
                      "Joanne",
                      "Chromatica")
albums$megan.thee.stallion <- c("Fever",
                                "Suga",
                                "Good News",
                                "Something for Thee Hotties",
                                "Traumazine")
albums$stevie.nicks <- c("Fleetwood Mac",
                         "Rumours", 
                         "Bella Donna",
                         "The Wild Heart",
                         "Rock a Little")
albums$oldies <- c("Where Did Our Love Go",
                   "Chapel of Love",
                   "Heat Wave",
                   "Leader of the Pack",
                   "Tapestry")
albums$santigold <- c("Santigold",
                      "Master of My Make Believe",
                      "99",
                      "I Don t Want The Goldfire Sessions",
                      "Spirituals")
albums$taylor.swift <- c("Lover",
                         "reputation",
                         "Red",
                         "folklore",
                         "Midnights")


################################################################################
#                                    main                                      #
################################################################################

lyrics <- list()

lyrics$lady.gaga <- create_lyrics_df(albums = albums$lady.gaga, 
                                     artist = "Lady Gaga")

lyrics$megan.thee.stallion <- create_lyrics_df(albums = albums$megan.thee.stallion,
                                               artist = "Megan Thee Stallion")

lyrics$stevie.nicks <- create_lyrics_df(albums = albums$stevie.nicks,
                                        artist =  c(rep("Fleetwood Mac", 2),
                                                    rep("Stevie Nicks", 3)))

lyrics$oldies <- create_lyrics_df(albums = albums$oldies,
                                  artist = c("The Supremes", 
                                             "The Dixie Cups",
                                             "Martha Reeves & The Vandellas",
                                             "The Shangri Las",
                                             "Carole King"))

lyrics$santigold <- create_lyrics_df(albums = albums$santigold,
                                     artist = "Santigold")

lyrics$taylor.swift <- create_lyrics_df(albums = albums$taylor.swift,
                                        artist = "Taylor Swift")

# save(lyrics, file = "data/lyrics")

################################################################################
#                                    clean up                                  #
################################################################################

lyrics.clean <- lapply(lyrics, clean_lyrics)

################################################################################
#                                    export                                    #
################################################################################

for(artist.name in names(lyrics.clean)){
  
  write.table(lyrics.clean[[artist.name]],
              file = file.path("data", paste0(artist.name, "-lyrics.txt")),
              sep = "\t", quote = FALSE, row.names = FALSE)
  
}





