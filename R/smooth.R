smoothDensity <- function(method, x, y, n)
{
  if (method == "linear")
    return(stats::filter(y, rep(1, n)/n))
  else if (method == "spline")
    return(stats::smooth.spline(y, spar = n)$y)
  else if (method == "loess")
    return(stats::loess(y~x, span = n)$fitted)
  else
   stop("Wrong smoothing method")
}
