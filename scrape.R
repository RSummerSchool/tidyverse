library(tidyverse)
library(rvest)

pokewiki_url <- "https://www.pokewiki.de/Pok%C3%A9mon-Liste"

overview <- read_html(pokewiki_url)

# Get path of pokemon
pokemon <- overview %>%
  html_nodes("td:nth-child(3) a") %>% 
  {tibble(german = html_attr(., "title"), path = html_attr(., "href"))} %>% 
  mutate(
    path = xml2::url_absolute(path, pokewiki_url),
    rn = row_number()
    )

# Scrape Pokemon details

scrape_poke_attr <- function(x){
  # What rows to scrape
  poke_attr <- c("Englisch$", "Typ$|Typen$", "Geschlecht$", "Gewicht$","Größe$", "EP Lv. 100$", "Farbe$")
  # How to name them. The order must match the above one!!!
  col_names <- c("english", "types", "gender", "weight","size", "ep_to_100", "color")
  # Extract the table
  dat <- x %>% html_node(".innerround") %>% html_table
  # Which rows contain the data?
  poke_attr_ind <- map(poke_attr, ~stringr::str_detect(dat[["X1"]], .)) %>% map_int(which)
  dat %>% 
    slice(poke_attr_ind) %>% 
    # rename the labels - while in long format
    mutate(X1 = col_names) %>%
    # transpose the data
    spread(X1, X2)
}

scrape_poke_stats <- function(x){
  # scrape the table
  x %>% 
    html_node(".lastisroundtable") %>% 
    html_table(fill = TRUE, header = TRUE) %>% 
    # set unique names - the tables is not propperly formatted => This is needed
    set_tidy_names(quiet = TRUE) %>% 
    # select only the base data of the Pokemon
    select(Statuswerte, Basiswerte) %>% 
    slice(2:7) %>% 
    # transpose the data
    spread(Statuswerte, Basiswerte) %>% 
    # rename the data - while in wide format
    rename(hp = "KP",
           attack = "Angriff",
           defense = "Vert.",
           speed = "Init.",
           attack_special = `Spez.-Angr.`,
           defense_special = `Spez.-Vert.`) %>% 
    # convert all columns to integer
    mutate_all(parse_integer)
}

scrape_poke_generation <- function(x){
  x %>% 
    html_node("h3+ .zentriert.c") %>% 
    html_node("td") %>% 
    html_text(trim = TRUE)
}

# Get pokemon details-page
pokemon <- pokemon %>% 
  # Only take the 3 starter Pokemon and their evolutions
  head(9) %>% 
  # Read in the website (= download and parse the website)
  mutate(doc = map(path, read_html)) %>% 
  # Scrape the attributes per row/pokemon
  mutate(poke_attr = map(doc, scrape_poke_attr)) %>% 
  # Scrape the statistics (strength etc) per row/pokemon
  mutate(poke_stats = map_chr(doc, scrape_poke_stats)) %>% 
  # Scrape the generation that the Pokemon was introduced
  mutate(generation = map(doc, scrape_poke_generation)) %>% 
  # drop the raw html and unnest the data
  select(-doc) %>% 
  unnest %>% 
  # seperate gender and type
  separate(gender, c("male", "female"), extra = "drop", sep = "[^[:alnum:],]+") %>%
  separate(types, c("type_1", "type_2"), extra = "drop", fill = "right") %>%
  # convert to the appropriate types
  type_convert(locale = locale(decimal_mark = ",", grouping_mark = "."),
               col_types = cols(
                 english = col_character(),
                 ep_to_100 = col_number(),
                 color = col_character(),
                 male = col_number(),
                 female = col_number(),
                 weight = col_number(),
                 size = col_number(),
                 type_1 = col_character(),
                 type_2 = col_character()
               )) %>% 
  # bring the data in the propper order
  select(rn, german, english, color, type_1, type_2, size, weight, 
         attack, speed, hp, defense, attack_special, defense_special, 
         ep_to_100, male, female, path, everything())


# pokemon <- pokemon %>% 
#   # # Only take the 3 starter Pokemon and their evolutions
#   # head(9) %>%
#   # Read in the website (= download and parse the website)
#   mutate(doc = map(paste0("./pokemon/", german), read_html)) %>% 
#   # Scrape the attributes per row/pokemon
#   mutate(poke_attr = map(doc, scrape_poke_attr)) %>% 
#   # Scrape the statistics (strength etc) per row/pokemon
#   mutate(poke_stats = map(doc, scrape_poke_stats)) %>% 
#   mutate(generation = map(doc, scrape_poke_generation)) %>% 
#   # drop the raw html and unnest the data
#   select(-doc) %>% 
#   unnest
