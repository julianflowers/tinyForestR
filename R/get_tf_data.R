
#' Retrieves and formats data from the Tiny Forest Earthwatch website
#'
#' This function scrapes the Tiny Forest Earthwatch website to obtain data about each Tiny Forest site including GPS coordinates, planted date, area, and planted species.
#' @import tidyverse
#' @import data.table
#' @import rvest
#' @import ggthemes
#' @import ggmap
#' @import mapview
#' @import vegan
#' @import needs
#' @import devtools
#' @import myScrapers
#'
#' @return A tidy data frame of Tiny Forest site information including GPS coordinates, planted date, area, and planted species for each site
#' @export
#'
#' @examples
#' get_tf_data()


get_tf_data <- function(){

  # Load necessary packages
  library(needs)
  needs(rvest, tidyverse, tidyfast, data.table, devtools, sf, vegan, mapview, ggthemes, ggmap)

  # Install package from GitHub
  devtools::install_github("julianflowers/myScrapers")

  # Load package from GitHub
  library(myScrapers)


  # Define URL to scrape
  url <- 'https://tinyforest.earthwatch.org.uk/tiny-forest-sites/'


  ## scrape web addresses for each tiny forest
  links <- get_page_links(url) %>%
    .[grepl("8-tiny-forest", .)] |>
    unique() |>
    enframe()

  # Create a new data frame to store scraped information
  tf_df <- links |>
    # Add the URL to the data frame
    mutate(url = paste0("https://tinyforest.earthwatch.org.uk/", value),
           # Add a 'stub' column which removes some unnecessary text from the web address
           stub = str_remove(value, "/tiny-forest-sites/8-tiny-forest/"),
           # Add a tf_id column to identifyeach tiny forest
           tf_id = parse_number(stub)) |>
    # Remove any NA values from the data frame
    drop_na()

  tf_df |>
    filter(tf_id == 343) |>
    select(url)


  # Define a function to create a table of information for each tiny forest
  create_tf_table <- function(df, i){

    read_html(tf_df$url[i]) |>
      html_nodes("body") |>
      html_text2() |>
      str_split("\n") |>
      enframe() |>
      unnest("value") |>
      filter(str_detect(value, "m2") |
               str_detect(value, "GPS") |
               str_detect(value, "^\\d{2}\\D.*20\\d{2}")|
               str_detect(value, "Species")) |>
      mutate(tf_id = df$tf_id[i])

  }

  # Use the 'map_dfr' function to apply the 'create_tf_table' function
  # to each tiny forest in the data frame and combine the results into a single data frame
  tf_table <- map_dfr(1:nrow(links), ~(create_tf_table(tf_df, .x)))

  create_tf_table(tf_df, 183)


  # Subset the data frame to only include information
  # for tiny forests that have at least 4 pieces of information
  tf_table_planted <- tf_table |>
    slice(-c(36, 894)) |>
    group_by(tf_id) |>
    left_join(tf_df, by = "tf_id") |>
    mutate(n = n(),
           id = row_number()) |>
    #count(tf_id) |>
    filter(n > 3) |>
    #DT::datatable(filter = "top")
    mutate(metric = case_when(id == 1 ~ "plant_date",
                              id == 2 ~ "area",
                              id == 3 & n == 5 ~ "class area",
                              id == 3 & n == 4 ~ "trees",
                              id == 4 & n == 5 ~ "trees",
                              TRUE ~ "gps"))


  # tf_table_planted |>
  #   filter(tf_id == 92)
  #   reactable::reactable(filterable = TRUE, selection = "multiple", pageSizeOptions = 100)

  # Reformat the data frame to be in a tidy format
  tf_table_tidy <- tf_table_planted |>
    # Remove unnecessary columns
    select(-c(n, name.x, id)) |>
    # Reshape the data frame so each piece of information is in its own column
    pivot_wider(names_from = "metric", values_from = "value.x", values_fn = list ) |>
    unnest("gps") |>
    #unnest("class area") |>
    unnest("area") |>
    unnest("plant_date") |>
    unnest("trees") |>
    select(-contains("class")) |>
    mutate(date = dmy(plant_date),
           area = parse_number(area),
           trees = str_remove(trees, "Species Planted in the Forest:"),
           gps = str_remove(gps, "GPS:")) |>
    # Split the GPS column into separate latitude and longitude columns
    separate(gps, c("lat", 'lon'), sep = ",\\s?") |>
    # Split the trees column into separate values based on the "|" delimiter
    mutate(trees = str_split(trees, "\\|"),
           lat = as.numeric(lat),
           lon = as.numeric(lon))

  output <- list(ids = tf_df, tidy_rf = tf_table_tidy)


}



