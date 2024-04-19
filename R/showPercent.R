showPercent <- function(value, absolute = F, digits = 2){
  if(absolute){
    scales::percent_format(accuracy = 1/(10^floor(digits)),
                           big.mark = ".", decimal.mark = ",")(abs(value))
  } else {
    scales::percent_format(1/(10^floor(digits)),
                           big.mark = ".", decimal.mark = ",")(value)
  }
}
