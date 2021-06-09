ayudantia 6
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cluster)

library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
load("/Users/gabrielengel/Downloads/beats.RData")
beats1<- beats

beats[beats == ""] <- NA

beats %>% summarise_all(funs(sum(is.na(.))))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ##   artist_name artist_id album_id album_type album_release_date
    ## 1           0         0        0          0                  0
    ##   album_release_year album_release_date_precision danceability energy key
    ## 1                447                            0            0      0   0
    ##   loudness mode speechiness acousticness instrumentalness liveness valence
    ## 1        0    0           0            0                0        0       0
    ##   tempo track_id analysis_url time_signature disc_number duration_ms explicit
    ## 1     0        0            0              0           0           0        0
    ##   track_href is_local track_name track_preview_url track_number type track_uri
    ## 1          0        0          0            174714            0    0         0
    ##   external_urls.spotify album_name key_name mode_name key_mode
    ## 1                     0          0        0         0        0

``` r
beats <- beats[!duplicated(beats$track_id),]

beats$track_number <- as.numeric(as.character(beats$track_number))
```

``` r
beats <- beats %>% 
  filter(!(is.na(track_number)))


beats <- beats[!grepl("<U",beats$track_name),]
beats <- beats[!grepl("<U",beats$artist_name),]

beats %>% count(duplicated(beats$track_name))
```

    ##   duplicated(beats$track_name)      n
    ## 1                        FALSE 181310
    ## 2                         TRUE 263787

``` r
beats$duplicate <- duplicated(beats[,c("track_name", "artist_name")])

data_dupli <- beats %>% 
  filter(beats$duplicate == TRUE) %>% 
  arrange("track_name", "track_number", desc(track_number))

data_dupli <- data_dupli %>% 
  distinct(track_name, artist_name, .keep_all = TRUE)

beats <- beats[!(beats$duplicate == TRUE),]

beats <- rbind(beats, data_dupli)
beats$duplicate <- NULL
```

``` r
beats$track_id <- as.character(beats$track_id)
beats$track_name <- as.character(beats$track_name)
beats$track_artist <- as.character(beats$artist_name)
beats$track_album_id <- as.character(beats$album_id)
beats$track_album_name <-  as.character(beats$album_name)
beats$danceability <- as.double(as.character(beats$danceability))
beats$energy <- as.double(as.character(beats$energy))
beats$key <- as.double(as.character(beats$key))
beats$loudness <- as.double(as.character(beats$loudness))
beats$mode <- as.double(as.character(beats$mode))
beats$speechiness <- as.double(as.character(beats$speechiness)) 
beats$acousticness <- as.double(as.character(beats$acousticness))
beats$instrumentalness <- as.double(as.character(beats$instrumentalness))
beats$liveness <- as.double(as.character(beats$liveness))
beats$valence <- as.double(as.character(beats$valence))
beats$tempo <- as.double(as.character(beats$tempo))
beats$duration_ms <- as.double(as.character(beats$duration_ms))


data_char <- c("track_id", "track_name", "artist_name", "album_id", "album_name")
data_dou <- c("track_popularity","danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")


beats <- beats %>% 
  filter(!(is.na(key)|is.na(danceability)))
summary(beats)
```

    ##  artist_name         artist_id           album_id          album_type       
    ##  Length:242039      Length:242039      Length:242039      Length:242039     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  album_release_date album_release_year album_release_date_precision
    ##  Length:242039      Min.   :   0       Length:242039               
    ##  Class :character   1st Qu.:2005       Class :character            
    ##  Mode  :character   Median :2015       Mode  :character            
    ##                     Mean   :2010                                   
    ##                     3rd Qu.:2019                                   
    ##                     Max.   :2021                                   
    ##                     NA's   :406                                    
    ##   danceability        energy            key            loudness      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   : 0.000   Min.   :-60.000  
    ##  1st Qu.:0.2900   1st Qu.:0.0983   1st Qu.: 2.000   1st Qu.:-22.247  
    ##  Median :0.4150   Median :0.2710   Median : 5.000   Median :-15.671  
    ##  Mean   :0.4333   Mean   :0.3771   Mean   : 5.119   Mean   :-15.952  
    ##  3rd Qu.:0.5710   3rd Qu.:0.6590   3rd Qu.: 8.000   3rd Qu.: -8.181  
    ##  Max.   :0.9860   Max.   :1.0000   Max.   :11.000   Max.   :  0.496  
    ##                                                                      
    ##       mode         speechiness       acousticness    instrumentalness   
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000000  
    ##  1st Qu.:0.0000   1st Qu.:0.03680   1st Qu.:0.1810   1st Qu.:0.0000752  
    ##  Median :1.0000   Median :0.04350   Median :0.8510   Median :0.1760000  
    ##  Mean   :0.6707   Mean   :0.07194   Mean   :0.6288   Mean   :0.3983078  
    ##  3rd Qu.:1.0000   3rd Qu.:0.05800   3rd Qu.:0.9790   3rd Qu.:0.8730000  
    ##  Max.   :1.0000   Max.   :0.97100   Max.   :0.9960   Max.   :1.0000000  
    ##                                                                         
    ##     liveness         valence           tempo          track_id        
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :  0.00   Length:242039     
    ##  1st Qu.:0.0957   1st Qu.:0.1400   1st Qu.: 87.02   Class :character  
    ##  Median :0.1220   Median :0.3300   Median :110.11   Mode  :character  
    ##  Mean   :0.2135   Mean   :0.3766   Mean   :112.09                     
    ##  3rd Qu.:0.2350   3rd Qu.:0.5770   3rd Qu.:132.97                     
    ##  Max.   :1.0000   Max.   :0.9960   Max.   :244.95                     
    ##                                                                       
    ##  analysis_url       time_signature   disc_number      duration_ms     
    ##  Length:242039      Min.   :0.000   Min.   : 1.000   Min.   :   1066  
    ##  Class :character   1st Qu.:4.000   1st Qu.: 1.000   1st Qu.: 153106  
    ##  Mode  :character   Median :4.000   Median : 1.000   Median : 221145  
    ##                     Mean   :3.775   Mean   : 1.288   Mean   : 255324  
    ##                     3rd Qu.:4.000   3rd Qu.: 1.000   3rd Qu.: 302200  
    ##                     Max.   :5.000   Max.   :26.000   Max.   :4796395  
    ##                                                                       
    ##   explicit        track_href         is_local        track_name       
    ##  Mode :logical   Length:242039      Mode :logical   Length:242039     
    ##  FALSE:237256    Class :character   FALSE:242039    Class :character  
    ##  TRUE :4783      Mode  :character                   Mode  :character  
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##  track_preview_url   track_number        type            track_uri        
    ##  Length:242039      Min.   :  1.00   Length:242039      Length:242039     
    ##  Class :character   1st Qu.:  5.00   Class :character   Class :character  
    ##  Mode  :character   Median :  9.00   Mode  :character   Mode  :character  
    ##                     Mean   : 20.54                                        
    ##                     3rd Qu.: 16.00                                        
    ##                     Max.   :545.00                                        
    ##                                                                           
    ##  external_urls.spotify  album_name          key_name          mode_name        
    ##  Length:242039         Length:242039      Length:242039      Length:242039     
    ##  Class :character      Class :character   Class :character   Class :character  
    ##  Mode  :character      Mode  :character   Mode  :character   Mode  :character  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##    key_mode         track_artist       track_album_id     track_album_name  
    ##  Length:242039      Length:242039      Length:242039      Length:242039     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ## 

``` r
str(beats)
```

    ## 'data.frame':    242039 obs. of  39 variables:
    ##  $ artist_name                 : chr  "2Pac" "2Pac" "2Pac" "2Pac" ...
    ##  $ artist_id                   : chr  "1ZwdS5xdxEREPySFridCfh" "1ZwdS5xdxEREPySFridCfh" "1ZwdS5xdxEREPySFridCfh" "1ZwdS5xdxEREPySFridCfh" ...
    ##  $ album_id                    : chr  "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" ...
    ##  $ album_type                  : chr  "album" "album" "album" "album" ...
    ##  $ album_release_date          : chr  "2019-08-01" "2019-08-01" "2019-08-01" "2019-08-01" ...
    ##  $ album_release_year          : num  2019 2019 2019 2019 2019 ...
    ##  $ album_release_date_precision: chr  "day" "day" "day" "day" ...
    ##  $ danceability                : num  0.656 0.81 0.548 0.839 0.854 0.697 0.77 0.805 0.818 0.912 ...
    ##  $ energy                      : num  0.882 0.642 0.59 0.657 0.694 0.598 0.613 0.864 0.627 0.465 ...
    ##  $ key                         : num  0 8 4 5 0 2 1 11 11 7 ...
    ##  $ loudness                    : num  -3.01 -8.65 -9.3 -4.96 -4.26 ...
    ##  $ mode                        : num  1 1 0 0 0 1 0 0 1 1 ...
    ##  $ speechiness                 : num  0.0941 0.244 0.475 0.222 0.123 0.136 0.0585 0.183 0.184 0.36 ...
    ##  $ acousticness                : num  0.033 0.048 0.113 0.0526 0.00944 0.00522 0.00653 0.271 0.264 0.0585 ...
    ##  $ instrumentalness            : num  0.00 0.00 7.22e-04 1.06e-04 7.19e-02 0.00 2.83e-04 0.00 0.00 1.71e-05 ...
    ##  $ liveness                    : num  0.67 0.264 0.229 0.391 0.0767 0.172 0.276 0.389 0.132 0.0534 ...
    ##  $ valence                     : num  0.782 0.694 0.267 0.615 0.776 0.387 0.897 0.663 0.637 0.478 ...
    ##  $ tempo                       : num  91.7 91 87.8 85.1 104.4 ...
    ##  $ track_id                    : chr  "6ayeqYtOtwVhqVB6k6MKoh" "1UDsnzBp8gUCFsrzUDlZI9" "3bKs15o7F9VP6GBExCbb6H" "4L0iAst3yLonw8aGxTRCvb" ...
    ##  $ analysis_url                : chr  "https://api.spotify.com/v1/audio-analysis/6ayeqYtOtwVhqVB6k6MKoh" "https://api.spotify.com/v1/audio-analysis/1UDsnzBp8gUCFsrzUDlZI9" "https://api.spotify.com/v1/audio-analysis/3bKs15o7F9VP6GBExCbb6H" "https://api.spotify.com/v1/audio-analysis/4L0iAst3yLonw8aGxTRCvb" ...
    ##  $ time_signature              : int  4 4 4 4 4 4 4 4 4 4 ...
    ##  $ disc_number                 : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration_ms                 : num  347973 241026 240013 295026 241000 ...
    ##  $ explicit                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ track_href                  : chr  "https://api.spotify.com/v1/tracks/6ayeqYtOtwVhqVB6k6MKoh" "https://api.spotify.com/v1/tracks/1UDsnzBp8gUCFsrzUDlZI9" "https://api.spotify.com/v1/tracks/3bKs15o7F9VP6GBExCbb6H" "https://api.spotify.com/v1/tracks/4L0iAst3yLonw8aGxTRCvb" ...
    ##  $ is_local                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ track_name                  : chr  "California Love" "Slippin' Into Darkness" "Ride or Die" "I Ain't Mad At Cha" ...
    ##  $ track_preview_url           : chr  "https://p.scdn.co/mp3-preview/93e456ef0b73f23f50eeadaeaad852d79d4f4610?cid=ac26d97eca664234ab133e5208ea5737" "https://p.scdn.co/mp3-preview/440595604d3f49464bcf28efc867f7df31d62e53?cid=ac26d97eca664234ab133e5208ea5737" "https://p.scdn.co/mp3-preview/cc18dc90d609d37591e5993615a0cea1fa25f428?cid=ac26d97eca664234ab133e5208ea5737" "https://p.scdn.co/mp3-preview/d138f0170423cd9a14f31006d4add57c07f705c4?cid=ac26d97eca664234ab133e5208ea5737" ...
    ##  $ track_number                : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ type                        : chr  "track" "track" "track" "track" ...
    ##  $ track_uri                   : chr  "spotify:track:6ayeqYtOtwVhqVB6k6MKoh" "spotify:track:1UDsnzBp8gUCFsrzUDlZI9" "spotify:track:3bKs15o7F9VP6GBExCbb6H" "spotify:track:4L0iAst3yLonw8aGxTRCvb" ...
    ##  $ external_urls.spotify       : chr  "https://open.spotify.com/track/6ayeqYtOtwVhqVB6k6MKoh" "https://open.spotify.com/track/1UDsnzBp8gUCFsrzUDlZI9" "https://open.spotify.com/track/3bKs15o7F9VP6GBExCbb6H" "https://open.spotify.com/track/4L0iAst3yLonw8aGxTRCvb" ...
    ##  $ album_name                  : chr  "California Love" "California Love" "California Love" "California Love" ...
    ##  $ key_name                    : chr  "C" "G#" "E" "F" ...
    ##  $ mode_name                   : chr  "major" "major" "minor" "minor" ...
    ##  $ key_mode                    : chr  "C major" "G# major" "E minor" "F minor" ...
    ##  $ track_artist                : chr  "2Pac" "2Pac" "2Pac" "2Pac" ...
    ##  $ track_album_id              : chr  "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" "1nGbXgS6toEOcFCDwEl5R3" ...
    ##  $ track_album_name            : chr  "California Love" "California Love" "California Love" "California Love" ...
