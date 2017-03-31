#' Output a vector containing the continent of the country
#'
#' @param x A vector of country names and percentages.
#' @param y A vector of percentages.
#' @export
#'
#' @importFrom utils getFromNamespace
#' @importFrom countrycode countrycode
#'
#' @return A data frame with two columns: continent and average percentage.
#' @examples
#' data(cousin)
#' byContinent(cousin$Country, cousin$Percent)

byContinent <- function(x, y){
  df <- data.frame(x, y)
  df <- df %>% mutate(continent =  getFromNamespace("countrycode", asNamespace("countrycode"))(x, origin = "country.name", destination = "continent")) %>%
    group_by(continent) %>% summarize(summary = mean(y))
  return(df)
}
