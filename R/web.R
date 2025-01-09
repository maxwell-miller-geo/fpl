# Get current data online
#' Get the current the stats from the fantasy API
#'
#' @return data frame with all players and variables
#' @export
#'
#' @examples
#' # Get the stats from online
#' weekly_stats <- get_current_stats()
#' head(weekly_stats) # print stats
#'
get_current_stats <- function(){
  # API Page
  url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'

  response <- httr::GET(url, httr::add_headers("user-agent" = "Mozilla/5.0"))

  data <- jsonlite::fromJSON(httr::content(response, as = "text",encoding = "UTF-8"))

  #dataset <- data$elements
  # return all elements
  return(data)
}

# Filter data elements from web
filter_players <- function(data){

  # Potential Key Stats to obtain
  players <- data$elements
  element_types <- data$element_types
  teams <- data$teams
  # Create a mapping of element_type IDs to position names
  team_mapping <- setNames(teams$short_name, teams$code)
  position_mapping <- setNames(element_types$plural_name_short, element_types$id)

  players$position <- position_mapping[as.character(players$element_type)]
  players$team <- team_mapping[as.character(players$team_code)]
  players$cost <- players$now_cost/10 # millions cost
  # Get the photos

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
  players$photo_paths <- download_photos(players, photo_dir)

  return(players)
}

# Download photos helper function
download_photos <- function(players, photo_dir, header = ""){
  players_photos <- sapply(1:nrow(players), function(x){
    file_name <- paste0(photo_dir, "/", players$first_name[x],"_",players$second_name[x], ".png")
    if(file.exists(file_name)){
      cat("Player", players$web_name[x], "is downloaded.\n")
      return(file_name)
    }
    tryCatch({
      download.file(players$photo_url[x], file_name, mode = "wb")
      return(file_name)
    }, error = function(e) {
      cat("Failed to download:", players$web_name[x], "\n")
      return(NA)
    })
  })
}

# Define the URL you want to access
# url <- "https://fantasy.premierleague.com/api/bootstrap-static/"
#
# # Define the cookies as a single string
cookie_header <- "OptanonAlertBoxClosed=2024-12-24T19:39:27.910Z; datadome=T~N1ZqK90KQ4_jVYn70j2CKDKk35~yDAvdK18swDL6OYgyO2efjTH34x21Gd9_~oWARdNRkBrdrl8NpY2fW7AvmJC34RpvPSO~Eq1uaIxorJq_q5cIHz7lq4mjsh3Vyc; pl_profile=eyJzIjogIld6SXNPRGMxTXpNMk5qaGQ6MXRWaVBWOkNicjVuSmJNVjV5UndNOWdxZURZRUhpZ1lidUNhRWZpYnl3Y2YzM0poU2ciLCAidSI6IHsiaWQiOiA4NzUzMzY2OCwgImZuIjogIm1heCIsICJsbiI6ICJtaWxsIiwgImZjIjogOH19; OptanonConsent=isGpcEnabled=0&datestamp=Wed+Jan+08+2025+19%3A42%3A10+GMT-0700+(Mountain+Standard+Time)&version=202302.1.0&isIABGlobal=false&hosts=&consentId=5abe3b8a-d1d3-469a-a600-b79c0b109db5&interactionCount=1&landingPath=NotLandingPage&groups=C0001%3A1%2CC0002%3A1%2CC0003%3A1%2CC0004%3A1&geolocation=US%3BAZ&AwaitingReconsent=false"

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

