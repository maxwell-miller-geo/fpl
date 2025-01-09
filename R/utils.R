stringMatch <- function(dataset, guessName = "Discharge", string = T, multi = F){
  matchLocations <- grep(guessName, colnames(dataset), ignore.case = T) # index of guesses
  if(length(matchLocations) == 1){
    stringName <- colnames(dataset)[matchLocations]
  }else if(length(matchLocations > 1)){
    if(multi){
      stringName <- colnames(dataset)[matchLocations]
      # Find the index of the shortest string
      shortest_index <- which.min(nchar(stringName))
      # Get the shortest string
      shortest_string <- stringName[shortest_index]
      return(shortest_string)
    }
    print("Found multiple matches with data sheet, using first match")
    stringName <- colnames(dataset)[matchLocations][1]
  }else{
    print("Could not find matches for input string: Please check input string")
    stringName <- matchLocations <- NA
  }
  if(string){
    return(stringName) # index of matching column
  }
  return(matchLocations[[1]])
}

beautify <- function(string){
  remove <- stringr::str_replace_all(string, "_", " ")
  capitalize <- capwords(remove)
  ## and the better, more sophisticated version:
  return(capitalize)
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Plot icons
icons <- function(table, icon_path = r"(C:\Users\themi\OneDrive\Documents\PremierLeagueIcons)"){
  #install.packages("ggimage")
  #require(ggimage)
  if(!file.exists(icon_path)){
    print("Could not find icons")
    return(table)
  }
  table$image <- paste0(icon_path, "\\", table$team, ".png")
  return(table)
}

premier_league_colors <- list(
  ARS = "#EF0107",  # Arsenal
  AVL = "#670E36",  # Aston Villa
  BOU = "#DA291C",  # Bournemouth
  BRE = "#E30613",  # Brentford
  BHA = "#0057B8",  # Brighton & Hove Albion
  BUR = "#6C1D45",  # Burnley
  CHE = "#034694",  # Chelsea
  CRY = "#1B458F",  # Crystal Palace
  EVE = "#003399",  # Everton
  FUL = "#000000",  # Fulham
  IPS = "#3A64A3",  # Ipswich Town
  LEE = "#FFFFFF",  # Leeds United
  LEI = "#003090",  # Leicester City
  LIV = "#C8102E",  # Liverpool
  MCI = "#6CABDD",  # Manchester City
  MUN = "#DA291C",  # Manchester United
  NEW = "#241F20",  # Newcastle United
  NFO = "#E53233",  # Nottingham Forest
  SOU = "#D71920",  # Southampton
  TOT = "#132257",  # Tottenham Hotspur
  WHU = "#7A263A",  # West Ham United
  WOL = "#FDB913"   # Wolverhampton Wanderers
)

