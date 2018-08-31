library(xml2)
library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(stringr)
library(radiant.data)
library(radiant)

# ------------------------Part 1: Web Scraping from Billboard ----------------------
url_base <- "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_%d" # base url

billboards <- map_df(1968:2017, function(i) {
  cat(".")  # effective progress indicator
  
  url <- read_html(sprintf(url_base, i)) # read url
  
  data = url %>% # scrape webpage
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
    html_table(fill = TRUE)
  
  data_df <- data %>% .[[length(data)]]
  
  colnames(data_df) <- c("Ranking", "Title", "Artist(s)")
  
  first_col = "Ranking"   # Convert column type if needed
  if (class(data_df[[first_col]]) == "character") {
    # Get the index of "Tie"
    index = which(data_df[[first_col]] == "Tie")
    data_df[index, first_col] <- data_df[index - 1, first_col]
    data_df[[first_col]] <- as.integer(data_df[[first_col]])
  }
  yearTable <- data_df %>% mutate(year = i)
})

# --------------------------Part 2: Web Scraping for genre --------------------------
## rename columns
colnames(billboards) <- c("rank", "song_name", "singer", "year")

## create a working copy for billboards dataset (to keep the original work for part 1)
billboards_wrk <- billboards

## remove \" for the variable song_name
billboards_wrk$song_name <- unlist(lapply(billboards_wrk$song_name, function(x) gsub("\"", "", x, fixed = TRUE)))

## add dash within song name for url search
billboards_wrk$song_name_dash <- str_replace_all(billboards_wrk$song_name, ",", "") %>% 
  str_replace_all(., "&", "") %>% 
  str_replace_all(., "  ", " ") %>% 
  str_replace_all(., " ", "-")  
                                          
## scraping the genre_raw
for (i in 1968:2017){
  
  if (i %% 5 == 0){   # rest per 5 year(avoid being blocked)
    Sys.sleep(runif(1,min = 20,max = 30))
  }
  
  ## create a dataset for year i
  dat <- billboards_wrk %>% 
    filter(year == i)
  
  for (j in 1:nrow(dat)){
    
    ## get a nap between loop(avoid being blocked by Google)
    Sys.sleep(runif(1))
 
    ## get song_name & singer
    song_name <- dat[j,"song_name_dash"]
    singer <- dat[j,"singer_spl"]
    
    ## get URL
    URL <- read_html(paste0("https://www.google.com/search?q=", tolower(song_name), "+",
                            tolower(singer), "+genre"))
    
    ## get genre_raw    
    dat[j, "genre_raw"] <- URL %>% 
      html_nodes("div") %>% 
      html_text() %>% 
      unlist(.) %>% 
      as.data.frame(.) %>% .[30,]
  } # 2nd loop

  ## rbind
  billboards_genre <- rbind(billboards_genre, dat)
  
}
## rename columns
colnames(billboards) <- c("rank", "song_name", "singer", "year")

## clean the genre
temp_dat <-
  map_df(data.frame(billboards_genre$genre_raw,
                    billboards_genre$song_name_dash),
         ~str_replace_all(.,"\\(.*\\)", "") %>%  # ingnore text in brackets
           # str_replace_all(.,"\\s+"," ") %>% # add dashes
           str_replace_all(.,"\\&|'[nN]'","and") %>% # match "and"
           str_replace_all(.,"[tT]he","") %>% # ignore "the/The"
           str_replace_all(.,"in'|ing","in") %>%  # match "ing"
           str_replace_all(.,"[^[:alnum:]]+","-") )%>% # remove all symbols
  as.data.frame(., colnames("genres","song_name")) 

billboards_genre$genre <- 
  map2(temp_dat$billboards_genre.genre_raw,
       temp_dat$billboards_genre.song_name_dash ,
       function(song_genre,song_name){
         
         ## conditional extraction
         if (grepl("Genre", song_genre) & grepl("Length", song_genre)){
           
           str_replace_all(song_genre,'^.*Genre|Length.*$', '') # get the text between 2 words
           
         } else{
           if (grepl("Genre", song_genre)) {
             
             if ( grepl(x = song_genre,
                        pattern = paste0("-*",song_name,".*-*,*-*Genre[s]*-*"),
                        ignore.case = TRUE) ) {
               
               gsub(x = song_genre,
                    pattern = paste0("-*",song_name,".*-*,*-*Genre[s]*-*"),
                    replacement = '',
                    ignore.case = TRUE) 
               
             } else {
               "Extraction failed"
             } # else 1
           } else{
             "No genre detected"
           } # else 2
         } # else 3
       }) %>% unlist()# loop 


## check the error in genre extracting
fail_list <- billboards_genre %>% 
  filter(genre %in% c("Extraction failed","No genre detected") ) %>%  #,"No genre detected"
  select(singer_spl,song_name,genre_raw,genre) 

fail_list %>% 
  group_by(genre) %>% 
  count() #


## define a function to deal with errors/ warnings
readUrl <- function(url) {
  out <- tryCatch(
    {
      #message("This is the 'try' part")
      readLines(con=url, warn=FALSE) 
      
      wiki_url <- read_html(url) %>% 
        html_nodes("tr") %>% 
        #as.character(.) %>% 
        html_text()
      
      return(wiki_url)
      
    },
    error=function(cond) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      #message(cond)
      return("Error!")
    },
    warning=function(cond) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message:")
      #message(cond)
      return("Warning!")
    },
    finally={
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
    }
  )    
  return(out)
}

## for loop: get genre for 'wiki' terms
for (r in 1:nrow(billboards_genre)){

  if (grepl("wiki", billboards_genre[r, "genre_raw"]) & billboards_genre[r, "genre"] == "No genre detected"){
    
    url <- gsub('^.*Wikipedia*|*Cached.*$', '', billboards_genre[r, "genre_raw"])
    
    wiki_url <- readUrl(url)
    
    if (wiki_url[1] != "Error!" & wiki_url[1] != "Warning!") {
      
      ## get genre
      temp <- data.frame(col1 = wiki_url %>%  
        unlist(.))
      
      genre_i <- which(grepl("Genre", temp[, 1]))
      
      if (length(genre_i) == 0){
        genre_i <- which(grepl("Genres", temp[, 1]))
      }
      
      if (length(genre_i) != 0) {
        billboards_genre[r, "genre"] <- as.character(temp[genre_i,]) %>% 
          str_replace_all(., "\n", "") %>% 
          gsub("^.*Genres*", "", .) %>% 
          paste(., collapse=" ")
      } 
      
    } else { # else wiki_url == "Error!" OR wiki_url == "Warnings!"
      
      billboards_genre[r, "genre"] <- "Wiki page 404"
      
    }
    
  } # end outer if 
  
} # end for loop

billboards_genre_updated <- billboards_genre

## output dataset
my_path <- "data/billboards_genre_updated.rds"
readr::write_rds(billboards_genre_updated, path = my_path)

## --- spread genres --- 

#billboard_join <- readRDS("C:/Users/Xiaoyi/Desktop/git/MGTA495Lyrics/data/billboard_join.rds")

## create a working copy 
billboard_join_wrk <- billboard_join

## spread genres 1 if TRUE, 0 otherwise
billboard_join_wrk <- billboard_join_wrk %>%
  mutate(pop = ifelse(grepl("pop", str_replace_all(genre, "Pop", "pop")), 1, 0)) %>% 
  mutate(rock = ifelse(grepl("rock", str_replace_all(genre, "Rock", "rock")), 1, 0)) %>% 
  mutate(RandB = ifelse(grepl("rand", str_replace_all(genre, "Rhythm-and-blues|RandB|R&B", "randb")), 1, 0)) %>% 
  mutate(soul = ifelse(grepl("soul", str_replace_all(genre, "Soul", "soul")), 1, 0)) %>% 
  mutate(hiphop = ifelse(grepl("hip-hop", str_replace_all(genre, "Hip-hop|Hip hop|hip hop", "hip-hop")), 1, 0)) %>% 
  mutate(rap = ifelse(grepl("rap", str_replace_all(genre, "Rap", "rap")), 1, 0)) %>% 
  mutate(country = ifelse(grepl("country", str_replace_all(genre, "Country", "country")), 1, 0)) %>% 
  mutate(dance = ifelse(grepl("dance", str_replace_all(genre, "Dance", "dance")), 1, 0)) %>% 
  mutate(alternative_indie = ifelse(grepl("Alternative-indie|Alternative-Indie", genre), 1, 0)) %>% 
  mutate(blues = ifelse(grepl("blues", str_replace_all(genre, "Blues", "blues")), 1, 0)) %>% 
  mutate(disco = ifelse(grepl("disco", str_replace_all(genre, "Disco", "disco")), 1, 0)) %>% 
  mutate(funk = ifelse(grepl("funk", str_replace_all(genre, "Funk", "funk")), 1, 0)) %>% 
  mutate(jazz = ifelse(grepl("jazz", str_replace_all(genre, "Jazz", "jazz")), 1, 0)) %>% 
  mutate(folk = ifelse(grepl("folk", str_replace_all(genre, "Folk", "folk")), 1, 0)) %>% 
  mutate(easy_listenin = ifelse(grepl("Easy-listenin", genre), 1, 0)) %>% 
  mutate(singer_songwriter = ifelse(grepl("songwriter", genre), 1, 0)) %>% 
  mutate(electric = ifelse(grepl("electric", str_replace_all(genre, "Electric", "electric")), 1, 0)) %>% 
  mutate(adult_contemporary = ifelse(grepl("Adult-contemporary", genre), 1, 0)) %>% 
  mutate(punk = ifelse(grepl("punk", str_replace_all(genre, "Punk", "punk")), 1, 0)) %>% 
  mutate(metal = ifelse(grepl("metal", str_replace_all(genre, "Metal", "metal")), 1, 0))
  
## define a function to sum specific cols, and create a new column "others" if the song does not fall into any categories
sum_cols <- function(x, colspec) {
  select(x, colspec) %>% 
    mutate(others = ifelse(rowSums(.) > 0, 0, 1)) %>% 
    select(others) %>% 
    cbind(x, .)
}

from <- which(colnames(billboard_join_wrk) == "pop")
to <- which(colnames(billboard_join_wrk) == "metal")

billboard_join_wrk <- sum_cols(billboard_join_wrk, seq(from, to, 1))

## to see how many "others"
temp <- billboard_join_wrk %>% 
  filter(others == 1)

## output dataset
my_path <- "data/billboard_join_final.rds"
readr::write_rds(billboard_join_wrk, path = my_path)

## add an index column to the output data set -- updated on 05/03/2018
billboard_join_final$lyricsID <- seq.int(nrow(billboard_join_final))

my_path <- "data/billboard_join_final.rds"
readr::write_rds(billboard_join_final, path = my_path)

# ------------------------Part 3: Web Scraping for lyrics -------------------------
lyrics_data <- data.frame()

lyricsURL <- 'https://www.azlyrics.com/lyrics'

billboards_genre <- readRDS("~/Desktop/git/MGTA495Lyrics/data/billboards_genre.rds")

billboards_lyrics <- readRDS("~/Desktop/git/MGTA495Lyrics/data/billboards_lyrics.rds")

hot100_sub <- billboards_genre %>% 
  slice(1:5001)

readUrl <- function(song_name_raw, singer_raw, lyrics_data) {
  song <- str_replace_all(song_name_raw, "Scarborough Fair", "Scarborough FairCanticle") %>% 
    str_replace_all(., "(Sweet Sweet Baby) Since You've Been Gone", "Since You've Been Gone") %>% 
    str_replace_all(., "Jumpin' Jack Flash", "Jumping Jack Flash") %>% 
    str_replace_all(., "Hurdy Gurdy Man", "The Hurdy Gurdy Man") %>% 
    str_replace_all(., "Do You Know the Way to San Jose", "Do You Know the Way to San Jos") %>% 
    str_replace_all(., "Sly and the Family Stone", "Sly&the Family Stone") %>% 
    str_replace_all(., "In the Year 2525", "In The Year 2525 (Exordium And Terminus)") %>% 
    str_replace_all(., "Sweet Cream Ladies", "Sweet Cream Ladies Forward March") %>% 
    str_replace_all(., "Neither One of Us (Wants to Be the First to Say Goodbye)", "Neither One of Us") %>% 
    str_replace_all(., "Gypsys, Tramps & Thieves", "Gypsys Tramps and Thieves") %>% 
    str_replace_all(., "(It's Just)", "") %>% 
    str_replace_all(., "Case of the Ex", "Case of the Ex What'cha Gonna Do") %>% 
    str_replace_all(., "Diary", "diary53795") %>% 
    str_replace_all(., "Live Like You Were Dying", "Live Like You Were Dyin") %>% 
    str_replace_all(., "Mr. Brightside", "Mr. Brightside39068") %>% 
    str_replace_all(., "How We Do", "This Is How We Do Fresh 83") %>% 
    str_replace_all(., "Snap Yo Fingers", "Snap Ya Fingers") %>% 
    str_replace_all(., "(When You Gonna) Give It Up to Me", "Give It Up to Me") %>% 
    str_replace_all(., "Black Horse and the Cherry Tree", "Black Horse & the Cherry Tree") %>% 
    str_replace_all(., "Déjà Vu", "djvu") %>% 
    str_replace_all(., "I Think They Like Me", "oh I Think They Like Me") %>% 
    str_replace_all(., "Crank That (Soulja Boy)", "Crank dat (Soulja Boy)") %>% 
    str_replace_all(., "Pop, Lock & Drop It", "Pop, Lock and Drop It") %>% 
    str_replace_all(., "Summer Love", "Summer Love Set The Mood Prelude") %>% 
    str_replace_all(., "I'm a Flirt", "I'm A Flirt (Remix)") %>% 
    str_replace_all(., "Big Shit Poppin' (Do It)", "Big Things Poppin' (Do It)") %>% 
    str_replace_all(., "Blame It", "Blame It (On The Alcohol)") %>% 
    str_replace_all(., "Yoü and I", "You and I") %>% 
    str_replace_all(., "Stronger (What Doesn't Kill You)", "What Doesn't Kill You Stronger") %>% 
    str_replace_all(., "Turn Me On", "Turning Me On") %>% 
    str_replace_all(., "I Don't Want This Night to End", "Don't Want This Night to End") %>% 
    str_replace_all(., "Habits (Stay High)", "Habits") %>% 
    str_replace_all(., "Hey Brother", "Brothersister") %>% 
    str_replace_all(., "Like I'm Gonna Lose You", "Like I'm going to Lose You") %>% 
    str_replace_all(., "Can't Feel My Face", "I Can't Feel My Face")
  
  song <- as.character(tolower(str_replace_all(song, "[^[:alnum:]]", "")))
  
  singer <- str_replace_all(singer_raw, "Paul Mauriat", "Johnny Mathis") %>% 
    str_replace_all(., "Paul Mauriat", "Johnny Mathis") %>% 
    str_replace_all(., "O. C. Smith", "Tony Bennett") %>% 
    str_replace_all(., "Hugh Masekela", "Raven Symone") %>% 
    str_replace_all(., "Merrilee Rush", "Juice Newton") %>% 
    str_replace_all(., "Hopkin", "Dolly Parton") %>% 
    str_replace_all(., "The Crazy World of Arthur Brown", "Emerson Lake Palmer") %>% 
    str_replace_all(., "Smokey Robinson and the Miracles", "Smokey Robinson&the Miracles") %>% 
    str_replace_all(., "Kenny Rogers and The First Edition", "Kenny Rogers") %>% 
    str_replace_all(., "Kenny Rogers & The First Edition", "Kenny Rogers") %>% 
    str_replace_all(., "Kenny Rogers & Kim Carnes", "Kenny Rogers") %>% 
    str_replace_all(., "Dottie West & Kenny Rogers", "Kenny Rogers") %>% 
    str_replace_all(., "Kenny Rogers and Sheena Easton", "Kenny Rogers") %>% 
    str_replace_all(., "Kenny Rogers and Dolly Parton", "Kenny Rogers") %>% 
    str_replace_all(., "Richard Harris", "Andy Williams") %>% 
    str_replace_all(., "José Feliciano", "Jose Feliciano") %>% 
    str_replace_all(., "Marvin Gaye & Tammi Terrell", "Marvin Gaye") %>% 
    str_replace_all(., "Diana Ross & Marvin Gaye", "Marvin Gaye") %>% 
    str_replace_all(., "Sérgio Mendes", "Sergio Mendes") %>% 
    str_replace_all(., "Lil Wayne, Wiz Khalifa and Imagine Dragons with Logic", "Lil Wayne") %>% 
    str_replace_all(., "Jay and the Americans", "Jay & the Americans") %>% 
    str_replace_all(., "Bob Seger System", "Bob Seger") %>% 
    str_replace_all(., "The Supremes & The Temptations", "The Supremes") %>% 
    str_replace_all(., "Gladys Knight & the Pips", "Gladys Knight") %>% 
    str_replace_all(., "Dionne and Friends (Dionne Warwick, Gladys Knight, Elton John and Stevie Wonder)", "Gladys Knight") %>% 
    str_replace_all(., "Paul Stookey", "Peter Paul And Mary") %>% 
    str_replace_all(., "Sinéad O'Connor", "sineadoconnor") %>% 
    str_replace_all(., "Pras Michel featuring Ol' Dirty Bastard and Mýa", "Pras") %>% 
    str_replace_all(., "Missy Elliott", "Missy 'Misdemeanor' Elliott") %>% 
    str_replace_all(., "Beyoncé", "Beyonce Knowles") %>% 
    str_replace_all(., "The Game featuring 50 Cent", "Game") %>% 
    str_replace_all(., "Lil Jon featuring Usher and Ludacris", "Lil Jon & The East Side Boyz") %>% 
    str_replace_all(., "The Game featuring Lil Wayne", "Game") %>% 
    str_replace_all(., "Soulja Boy Tell 'Em", "Soulja Boy") %>% 
    str_replace_all(., "Eminem, Dr. Dre and 50 Cent", "Eminem") %>% 
    str_replace_all(., "Kesha", "Keha") %>% 
    str_replace_all(., "Kanye West, Big Sean, Pusha T and 2 Chainz", "Kanye West") %>% 
    str_replace_all(., "Snoop Dogg and Wiz Khalifa featuring Bruno Mars", "Snoop Dogg") %>% 
    str_replace_all(., "The Script featuring will.i.am", "Script") %>% 
    str_replace_all(., "Emeli Sandé", "Emeli Sand") %>% 
    str_replace_all(., "DJ Snake and Lil Jon", "DJ Snake") %>% 
    str_replace_all(., "A Great Big World and Christina Aguilera", "A Great Big World") %>% 
    str_replace_all(., "Jessie J, Ariana Grande, and Nicki Minaj", "Jessie J") %>% 
    str_replace_all(., "Miranda Lambert and Carrie Underwood", "Miranda Lambert") %>% 
    str_replace_all(., "Drake and Future", "Drake") %>% 
    str_replace_all(., "Zara Larsson and MNEK", "Zara Larsson") %>% 
    str_replace_all(., "The Weeknd featuring Daft Punk", "The Weeknd") %>% 
    str_replace_all(., "Lil Wayne, Wiz Khalifa and Imagine Dragons with Logic and Ty Dolla $ign featuring X Ambassadors", "Lil Wayne") %>% 
    str_replace_all(., "Major Lazer and DJ Snake featuring MØ", "Major Lazer") %>% 
    str_replace_all(., "Shawn Mendes and Camila Cabello", "Shawn Mendes") %>% 
    str_replace_all(., "Seals and Crofts", "Seals Crofts") %>% 
    str_replace_all(., "A Boogie wit da Hoodie featuring Kodak Black", "Boogie wit da Hoodie") %>% 
    str_replace_all(., "Hailee Steinfeld and Grey featuring Zedd", "Hailee Steinfeld")
    
  if (grepl(" Featuring ", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\ Featuring .*", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl(" featuring ", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\ featuring .*", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl(" with ", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\ with .*", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl("The", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\The ", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl("&", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\&", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl("-", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\-", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl(" x ", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\ x .*", "", singer), "[^[:alnum:]]", "")))
    
  } else if (grepl(" + ", singer)){
    
    singer <- as.character(tolower(str_replace_all(gsub("\\ + .*", "", singer), "[^[:alnum:]]", "")))
    
  } else {
    
    singer <- as.character(tolower(singer))
    
  }
  
  singer <- gsub(" ", "", singer, fixed = TRUE) %>% # remove space
    gsub(".", "", ., fixed = TRUE) %>% # remove .
    gsub(",", "", ., fixed = TRUE) %>% # remove ,
    gsub("!", "", ., fixed = TRUE) %>% # remove !
    gsub("+", "", ., fixed = TRUE) %>% # remove +
    gsub("'", "", ., fixed = TRUE)     # remove '
  
  ## get complete URL
  url <- paste0(lyricsURL, "/", singer, "/", song, ".html")
  out <- tryCatch(
    {message("Yao loves u")
      
      URL <- read_html(url) 
      
      div_list <- URL %>% 
        html_nodes("div") 
      
      text <- div_list[[22]] %>% 
        html_text() %>% 
        str_replace_all(., "\n", " ") %>% 
        str_replace_all(., "\r", "") %>% 
        str_replace_all(., "\"", "") %>% 
        trimws(.) %>% 
        as.character(.)
      
      ## create a data frame
      return(data.frame(song_name = song_name_raw, singer = singer_raw, 
                        lyricsURL = url, lyrics = text))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    },
    finally={
      message(paste("Processed URL:", url))
      message(paste(song_name_raw, singer_raw))
    }
  )    
  return(out)
}

for (i in 1:5001) {
  ## ifelse(i %% 50 == 0, Sys.sleep(runif(1, 20, 30)), Sys.sleep(runif(1,1,3)))
  Sys.sleep(round(runif(1,1,5)))
  ## get song names
  song_name_raw <- hot100_sub[i, "song_name"]
  
  ## get singers
  singer_raw <- hot100_sub[i, "singer"]
  
  temp <- readUrl(song_name_raw, singer_raw, lyrics_data)
  if (!is.null(temp) & !is.na(temp)) {
    lyrics_data <- rbind(lyrics_data, temp)
  }
}

## c` onver t factor to character
lyrics_data$song_name <- as.character(lyrics_data$song_name)
lyrics_data$singer <- as.character(lyrics_data$singer)

## join hot100 and lyrics data sets
billboard_lyrics_data <- hot100_sub %>% 
  inner_join(lyrics_data, by = c("song_name", "singer"))

billboard_lyrics_data <- rbind(billboards_lyrics, billboard_lyrics_data)

## output dataset
my_path <- "data/billboards_lyrics.rds"
readr::write_rds(billboard_lyrics_data, path = my_path)

# ----------------------second round--------------------

billboards_genre <- readRDS("~/Desktop/git/MGTA495Lyrics/data/billboards_genre.rds")

billboards_lyrics <- readRDS("~/Desktop/git/MGTA495Lyrics/data/billboards_lyrics.rds")

lyrics <- billboards_lyrics %>% 
  select(rank, song_name, singer, year, lyrics)

lyrics <- unique(lyrics)

origin <- billboards_genre %>% 
  select(rank, song_name, singer, year)

lack <- origin %>% 
  anti_join(lyrics, by = c("rank", "song_name", "singer", "year"))

#--------------------------------------------------------
for (i in 1:1213) {
  ## ifelse(i %% 50 == 0, Sys.sleep(runif(1, 20, 30)), Sys.sleep(runif(1,1,3)))
  Sys.sleep(round(runif(1,1,5)))
  ## get song names
  song_name_raw <- lack[i, "song_name"]
  
  ## get singers
  singer_raw <- lack[i, "singer"]
  
  temp <- readUrl(song_name_raw, singer_raw, lyrics_data)
  if (!is.null(temp) & !is.na(temp)) {
    lyrics_data <- rbind(lyrics_data, temp)
  }
}

lyrics_data$song_name <- as.character(lyrics_data$song_name)
lyrics_data$singer <- as.character(lyrics_data$singer)

billboard_lyrics_data <- origin %>% 
  inner_join(lyrics_data, by = c("song_name", "singer")) %>% 
  select(rank, song_name, singer, year, lyrics)

billboard_lyrics_data <- rbind(lyrics, billboard_lyrics_data)

my_path <- "data/billboards_lyrics.rds"
readr::write_rds(billboard_lyrics_data, path = my_path)

#-------------------------------------------------------
# The below is the web scraping process on http://www.metrolyrics.com, 
# plz refer to the result in data folder as shown below.

# The updated data with lyrics scraped (4191+)
billboards_lyrics <- readRDS("data/billboards_lyrics_new.rds")

# The leftovers without lyrics (800+)
leftover_no_lyrics <- readRDS("data/leftover_no_lyrics.rds")


# ------------ PART 4: combine the genre and lyrics dataset -----------------
billboard_join <- billboards_lyrics_new %>% 
  left_join(billboards_genre_updated, by = c("rank","year","song_name","singer")) %>% 
  select(rank,year,song_name,singer,lyrics,genre_raw,genre) %>% 
  filter (genre != "Wiki page 404") %>% 
  mutate(genre = ifelse(genre == "Extraction failed", genre_raw,genre))

# save data
readr::write_rds(billboard_join, path = "data/billboard_join.rds")
 
