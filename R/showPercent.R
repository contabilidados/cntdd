showPercent <- function(value, absolute = T, digits = 2){
  if(absolute){
    scales::percent_format(accuracy = 1/(10^floor(digits)))(abs(value))
  } else {
    scales::percent_format(1/(10^floor(digits)))(value)
  }
}




