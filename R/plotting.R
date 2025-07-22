# Plot two variables with the linear square regression line
#' Plot two variables with a linear square regression
#'
#' Data or spreadsheet can be obtained from the premier league API. Using
#' get_player_stats() followed by filter_players() will obtain the current player
#' records.
#'
#' @param spreadsheet input data frame, spreadsheet, or path to data
#' @param x default: "cost", name of column in spreadsheet variable. Input is flexible
#' and will return the shortest partial match. E.g., with columns: "cost", "starting_cost",
#' the "cost" column will be selected because it is shortest.
#' @param y default: "points_per_game", name of column in spreadsheet variable,
#' similar to x variable. Will by plotted on the y-axis
#' @param group default: "names". Group or column to plot categories by.
#' Options: \itemize{
#' \item"names" - plots points as players display names
#' \item"teams" - plots points as team icon
#' \item"photos" - plots points as player images
#' }

#' @param pos default: NULL. Option to filter players by position.
#' Options:
#' \itemize{
#' \item"GKP" - Goalies,
#' \item"FWD" - Forwards,
#' \item"MID" - Midfielders,
#' \item"DEF" - Defenders
#' }
#' @param top_percent default: NULL. Option to filter players by total points
#' percentage. Expects values from 0 - 1.
#' @param remove_player default: NULL. Input vector of player(s) to remove players from
#' plotting or regression. E.g. the input c("Haaland") will remove him from the table.
#' @return Plot of input parameters with linear square regression and regression
#' coefficients. Additionally, prints out filtered table.
#' @import data.table
#' @export
#'
#' @examples \dontrun{
#' stats <- get_premier_stats()
#' spreadsheet <- filter_players(stats)
#' fpl_plot(spreadsheet, x = "cost", y = "total_points", group = "names", pos = "FWD")
#' }
fpl_plot <- function(spreadsheet, x = "cost", y = "points_per_game", group = "names", pos = NULL, top_percent = NULL, remove_player = NULL){

  #requireNamespace("data.table")
  # Remove some variables
  position <- points_per_million <- image <- name <- team <- NULL
  if(is.character(spreadsheet)){
    players <- data.table::fread(spreadsheet)
  }else{
    players <- data.table::data.table(spreadsheet)
  }
  # Make sure it stays a data.table
  # Select by position
  if(!is.null(pos)){
    players <- players[position == pos]
    if(!is.null(top_percent)){
      # Determine range of points
      top_players <- stats::quantile(players$total_points, top_percent)
      players <- players[players$total_points >= top_players]
    }
  }
  # players with more than 200 min
  players <- players[players$minutes > 200,]
  # players <- players[players$total_points > 50]
  #players <- players[players$cost < 8.5]
  # Remove players from data.table
  if(!is.null(remove_player)){
    players <- players[players$name != remove_player]
  }


  # Calculate Points Per Million dollars
  players$points_per_million <- as.numeric(players$total_points/players$cost)
  # Calculate color schemes based on abbrev
  players$color_hex <- as.character(premier_league_colors[players$team])
  # Sort players by points per million
  players <- players[order(-players$points_per_million),]
  # Find the x column
  col1 <- stringMatch(players, x, multi = T)
  col2 <- stringMatch(players, y, multi = T)
  # Correlate the two variables
  x1 <- as.numeric(players[[col1]])
  y1 <- as.numeric(players[[col2]])
  # Determine the range of x
  xlimit <- range(x1)[2] *.85
  ylimit <- range(y1)[1] * 1.1
  ymax <- range(y1)[2]*.9
  if(ylimit < 0){
    ylimit <- range(y1)[1]*.9
  }

  # Regression
  if(!is.character(players[1,..x1][[1]]) | !is.character(players[1,..y1][[1]])){
    coeff <- stats::lm(y1~x1+0) # linear model coefficients with intercept set at 0
    rsquared <- round(summary(coeff)$r.squared,3)
    slope <- coeff$coefficients[[1]]
  }else{
    rsquared <- 0
    slope <- 0
  }

  # Remove NA values
  players <- players[!is.na(players[,x1])]
  players <- players[!is.na(players[,y1])]
  # If no factors
  if(is.null(pos)){
    factors <- "position"
  }else if(group == "teams"){
    factors <- "images"
    players <- icons(players)
  } else if(group == "names"){
    factors <- "name"
  } else if(group == "photos"){
    factors <- "photos"
  }
  # Create a custom arrow with a color (e.g., red)
  #custom_arrow <- arrow(type = "closed", length = unit(0.15, "inches"))
  if(col1 == "cost"){
    xrange <- c(3.9, max(players[[col1]]+0.1))
  }else{
    xrange <- NULL
  }
  # Caption variations
  caption <- paste0("Correlation coefficient (R\u00B2): ", rsquared)
  caption2 <- paste(beautify(col2)," =", round(slope,2), "*",beautify(col1))
  # Separate Plot paths for images and symbols
  if(factors == "images"){
    p <- ggplot2::ggplot(players, mapping = ggplot2::aes(x = get(col1), y = get(col2))) +
      ggplot2::geom_abline(intercept = coeff$coefficients[[1]], slope = slope, col = "black") +
      ggimage::geom_image(ggplot2::aes(image = image), size = 0.05)+
      ggplot2::labs(x = beautify(col1), y = beautify(col2), title = paste0("FPL Players by Team: ", pos,"s")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), panel.background = ggplot2::element_rect(fill = "grey", color = "grey80")) +
      ggplot2::annotate("text", x = range(x1)[2]*.88, y = ylimit*1, label = caption, size = 3) +
      ggplot2::annotate("text", x = range(x1)[2]*.88, y = ylimit*1.26, label = caption2, size = 3)+
      ggplot2::scale_color_viridis_d(option = "turbo")
    #theme_update(panel.background = element_rect(fill = "grey", colour = "grey50"))
    #p + ggplot2::geom_smooth(method = "lm", se = F)
    if(!is.null(xrange)){
      p <- p + ggplot2::scale_x_continuous(limits = xrange)
    }
    players
    return(p)
  }
  if(factors == "photos"){
    p <- ggplot2::ggplot(players, mapping = ggplot2::aes(x = get(col1), y = get(col2))) +
      ggplot2::geom_abline(intercept = coeff$coefficients[[1]], slope = slope, col = "black") +
      ggimage::geom_image(ggplot2::aes(image = photo_paths), size = 0.05)+
      ggplot2::labs(x = beautify(col1), y = beautify(col2), title = paste0("FPL Players: ", pos,"s")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), panel.background = ggplot2::element_rect(fill = "grey", color = "grey80")) +
      ggplot2::annotate("text", x = range(x1)[2]*.88, y = ylimit*1, label = caption, size = 3) +
      ggplot2::annotate("text", x = range(x1)[2]*.88, y = ylimit*1.26, label = caption2, size = 3)+
      ggplot2::scale_color_viridis_d(option = "turbo")
    #theme_update(panel.background = element_rect(fill = "grey", colour = "grey50"))
    #p + ggplot2::geom_smooth(method = "lm", se = F)
    if(!is.null(xrange)){
      p <- p + ggplot2::scale_x_continuous(limits = xrange)
    }
    players
    return(p)
  }
  if(factors == "name"){
    #require(ggrepel)
    title <- paste0(beautify(col1)," versus ", beautify(col2), ": ", pos)
    p <- ggplot2::ggplot(players, mapping = ggplot2::aes(x = get(col1), y = get(col2))) +
      ggplot2::geom_abline(intercept = coeff$coefficients[[1]], slope = slope, col = "black") +
      # ggplot2::geom_text(aes(label = name)) +
      # geom_label_repel(aes(label = team, fill = color),  # Map label and fill to columns
      #                  color = "white",  # Text color
      #                  size = 5,        # Label size
      #                  fontface = "bold")
      ggrepel::geom_label_repel(ggplot2::aes(label = name, fill = team), segment.color = "black", color = "grey90", box.padding = 0.1, point.padding = 0.1, size = 3, max.overlaps = 20) +
      #scale_fill_identity() +
      ggplot2::scale_fill_manual(
        values = stats::setNames(players$color_hex, players$team)  # Map team abbreviations to colors
      )+  # Custom labels+
      ggplot2::labs(x = beautify(col1), y = beautify(col2), title = title) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),panel.background = ggplot2::element_rect(fill = "grey80")) +
      ggplot2::annotate("text", x = xlimit+.5, y = ylimit+range(y1)[1]*.25, label = caption) +
      ggplot2::annotate("text", x = xlimit+.5, y = ylimit+0.25, label = caption2)
    #p + ggplot2::geom_smooth(method = "lm", se = F)http://127.0.0.1:38731/graphics/253bf0dd-0e9b-4a17-b13c-fc245f742e3d.png
    if(!is.null(xrange)){
      p <- p + ggplot2::scale_x_continuous(limits = xrange)
    }
    players
  }
  return(p)
}
