# Get current data online
#' Get the current the stats from the fantasy API
#'
#' @return data frame with all players and variables
#' @export
#'
#' @examples
#' # Get the stats from online
#' weekly_stats <- get_premier_stats()
#' head(weekly_stats) # print stats
#'
get_premier_stats <- function(){

  # API Page
  url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'

  response <- httr::GET(url, httr::add_headers("user-agent" = "Mozilla/5.0"))

  data <- jsonlite::fromJSON(httr::content(response, as = "text",encoding = "UTF-8"))

  #dataset <- data$elements
  # return all elements
  return(data)
}

# Filter data elements from web
#' Filter player elements from the web
#'
#' @param data list from api
#' @param get_photos optional boolean: If TRUE, will gather photos from FPL website
#' @param suppress option boolean: If FALSE, will print out print messages and
#' player photos not found.
#' @return returns data frame with additional columns, player photo paths, and team abbreviations
#' @export
#'
#' @examples \dontrun{
#' data_list <- get_premier_stats()
#' players <- filter_players(data_list)
#' }
filter_players <- function(data, get_photos = F, suppress = T){

  # Potential Key Stats to obtain
  players <- data$elements
  element_types <- data$element_types
  teams <- data$teams

  # Column names
  colnames <- names(players)
  # Attempt to convert columns into numeric values, if you can
  players[] <- suppressWarnings(lapply(players, function(x) {
    # Check if the column can be coerced to numeric
    if (all(!is.na(as.numeric(as.character(x))))) {
      # If all values can be converted to numeric, do the conversion
      as.numeric(as.character(x))
    } else {
      # Otherwise, leave the column as is
      x
    }
  }))
  # Create a mapping of element_type IDs to position names
  team_mapping <- setNames(teams$short_name, teams$code)
  position_mapping <- setNames(element_types$plural_name_short, element_types$id)

  players$position <- position_mapping[as.character(players$element_type)]
  players$team <- team_mapping[as.character(players$team_code)]
  players$cost <- players$now_cost/10 # millions cost
  if("web_name" %in% colnames(players)){
    players$name <- players$web_name
  }

  # Base URL for player photos
  base_url <- "https://resources.premierleague.com/premierleague/photos/players/110x140/p"

  # Adjust players photo - remove the last 4 characters ".jpg"
  players$photo_id <- substr(players$photo,1, nchar(players$photo) -4)
  # Construct the full photo URLs
  players$photo_url <- paste0(base_url, players$photo_id, ".png")

  # Specify a directory to save the photos
  photo_dir <- file.path(system.file("extdata", package = "fpl"), "PlayerPhotos")

  # Create new direction if it doesn't exist
  dir.create(photo_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

  # Download the available photos
  players$photo_paths <- download_photos(players, photo_dir, get_photos = get_photos, suppress = suppress)

  # Reorganize some of the columns
  select_columns <- c("team", "web_name", "form", "points_per_game", "cost", "position",  "goals_scored")

  # Get other columns that are not selected
  other_columns <- setdiff(names(players), select_columns)

  # Combine the selected columns with the other columns
  new_order <- c(select_columns, other_columns)

  # Reorder the data.frame using base R column indexing
  players_reordered <- players[, new_order]
  return(players_reordered)
}

# Download photos helper function
download_photos <- function(players, photo_dir, header = "", get_photos = F, suppress = T){
  players_photos <- sapply(1:nrow(players), function(x){
    file_name <- paste0(photo_dir, "/", players$first_name[x],"_",players$second_name[x], ".png")
    if(file.exists(file_name)){
      if(!suppress){
        cat("Player", players$web_name[x], "is downloaded.\n")
      }
      return(file_name)
    }
    if(get_photos){
      tryCatch({
        download.file(players$photo_url[x], file_name, mode = "wb")
        return(file_name)
      }, error = function(e) {
        if(!supress){
          cat("Failed to download:", players$web_name[x], "\n")
        }
        return(NA)
      })
    }else{
      return(NA)
    }
  })
}

# Define the URL you want to access
# url <- "https://fantasy.premierleague.com/api/bootstrap-static/"
#
# # Define the cookies as a single string


# Make the request with the cookies
# # Make the request with the cookies
# response <- GET(
#   url,
#   user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36"),
#   add_headers(Cookie = cookie_header)
# )
#
# # Check the response
# if (status_code(response) == 200) {
#   data <- content(response, "text")
#   cat("Request successful!")
# } else {
#   cat("Request failed. Status code:", status_code(response))
# }
#' Get the Premier League fixture data
#'
#' @return Fixture list
#' @export
#'
#' @examples fixture_list <- get_fixture_data()
#'
get_fixture_data <- function(){
  # API Page
  url <- "https://fantasy.premierleague.com/api/fixtures/"

  response <- httr::GET(url, httr::add_headers("user-agent" = "Mozilla/5.0"))

  data <- jsonlite::fromJSON(httr::content(response, as = "text",encoding = "UTF-8"))

  return(data)
}
