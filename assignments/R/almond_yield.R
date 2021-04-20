#' Almond Yield
#'
#' Determine the almond yield anomaly for each given year
#' Using climate date containg daily tempature maximums and minimums
#' As well as daily precipitation amounts.
#'
#' @param  climate date frame containing the daily climate variables (precipitation, min temp, max temp)
#' @param  t.coeff1 default = -0.015
#' @param  t.coeff2 default = -0.0046
#' @param  p.coeff1 default = -0.07
#' @param  p.coeff2 default = 0.0043
#' @return A data frame containg almond yield per year and the two variable need for the anomoly equation
#' @author Ruben Sanchez Ramirez, Gabe De La Rosa, Alex Ehrens
#' @references Lobell et al. 2006

almond_yield = function(climate, t.coeff1 = -0.015, t.coeff2 = -0.0046, p.coeff1 = -0.07, p.coeff2 = 0.0043, intercep = 0.28) {
  # Find the average minimum temptarure for the month of Febuary for each year
  temp <- climate %>%
    group_by(month, year) %>%
    dplyr::summarize(tmin_c = mean(tmin_c)) %>%
    filter(month == 2)
  # Find the total precipitation for the month of January for each year
  precip <- climate %>%
    group_by(month, year) %>%
    dplyr::summarize(precip = sum(precip)) %>%
    filter(month == 1)
  # Create a data frame containing the two previously filtered varables and year
  data <- merge(temp, precip, by = "year") %>%
    select(year, tmin_c, precip)
  # Create a vector for the average minimum tempatures in Febuary
  t.n.2 <- temp$tmin_c
  # Create a vector for the total precipitation in January
  p.1 <- precip$precip
  # Create a new column in the same data frame containing the calculated yeild
  # Based on the given coefficients and the created vectors for min temp and precip
  data$yield <- t.coeff1*t.n.2+t.coeff2*t.n.2^2+p.coeff1*p.1+p.coeff2*p.1^2+intercep
  # Return a data frame containing:
    # year, avearge min temp for Feburary, total precip for January, and Yeild
  return(data)

}
