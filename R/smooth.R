smoothDensity <- function(method, x, y, n)
{
  if (method == "linear")
    return(stats::filter(y, rep(1, n)/n))
  else if (method == "median")
    return(stats::runmed(y, n))
  else if (method == "savistsky")
    return(stats::filter(y, savistsky_golay_kernel(n)))
  else if (method == "spline")
    return(stats::smooth.spline(y, spar = n)$y)
  else if (method == "loess")
    return(stats::loess(y~x, span = n)$fitted)
  else if (method == "fft")
    return(fft.filter(y, n))
  else
   stop("Wrong smoothing method")
}

savistsky_golay_kernel <- function(width)
{
  x <- 1:width - width/2
  y <- max(x^2)-x^2
  sg <- y/sum(y)
  return(sg)
}

fft.filter = function(x, threshold, plot = FALSE)
{
  x.fft = stats::fft(x)

  x.fft.filter = x.fft
  x.fft.filter[threshold:length(x.fft)] = 0 + 0i

  x.ifft = stats::fft(x.fft.filter, inverse = TRUE)/length(x.fft.filter)

  if(plot)
  {
    graphics::layout(matrix(1:3, 3,1))
    graphics::plot(x, type = "l", main = "Original  Data")
    graphics::plot(0:(length(x.fft)-1), Mod(x.fft), t = "h", lwd = 2, main="Fourrier transform", xlab="Frequency (Hz)", ylab="Strength", xlim = c(0,round(0.1*length(x.fft))))
    graphics::plot(Re(x.ifft), type = "l", main = "Fourier  Transform  Filtering")
  }

  return(Re(x.ifft))
}

