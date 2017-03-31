#' Output a vector containing the continent of the country
#'
#' @param x A data frame with two columns: country names and percentages.
#' @export
#' @return A data frame with two columns: continent and average percentage.
#' @examples
#' data(cousin)
#' byContinent(cousin)

byContinent <- function(x, y){
  df <- data.frame(x, y)
  x$continents <- countrycode(x, origin = "country.name", destination = "continent")
  x %>% group_by(continents) %>% summarize(y)
}
