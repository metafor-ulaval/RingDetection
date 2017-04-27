#' Algorithm for ring detection
#'
#' Automatic ring detection and its graphic user interface (GUI)
#'
#' @param radius numeric. Position from the pith
#' @param density numeric. Density for each position
#' @param smooth_method character. Name of a method for smoothing. Can be 'linear' or 'spline'
#' @param filter_width scalar. Smoothing parameter for the density profile
#' @param low_limit scalar.
#' @param up_limit scalar.
#' @param threshold scalar.
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' data(oakprofile)
#'
#' result = ring_detection(oakprofile$rad_pos, oakprofile$density)
#'
#' \dontrun{
#' ring_detection_UI(oakprofile$rad_pos, oakprofile$density)
#' }
ring_detection <- function(radius, density, smooth_method, filter_width, low_limit, up_limit, threshold)
{
  if (missing(smooth_method)) smooth_method = 'linear'
  if (missing(filter_width))  filter_width   = 5
  if (missing(low_limit))     low_limit     = 400
  if (missing(up_limit))      up_limit      = 800
  if (missing(threshold))     threshold     = 200

  scaled_density   = scaleDensity(density)
  smoothed_density = smoothDensity(smooth_method, radius, scaled_density, filter_width)

  ix = core_detection(radius, smoothed_density, low_limit, up_limit, threshold)

  year = numeric(length(radius))
  year[ix] = 1
  year = cumsum(year)
  year = year - year[length(year)]

  data = data.frame(radius)
  data$density   = density
  data$year      = year
  data$smDensity = smoothDensity(smooth_method, radius, density, n = filter_width)
  data$woodtype  = identifyWoodtype(smoothed_density, year)

  attr(data, "fw") = filter_width
  attr(data, "ul") = up_limit
  attr(data, "ll") = low_limit
  attr(data, "th") = threshold

  return(data)
}

scaleDensity <- function(density)
{
  density_min <- min(density)
  density_max <- max(density)

  offset <- density_min
  scale  <- 1000.0 / (density_max - density_min)
  scaled <- (density - offset) * scale

  return(scaled)
}

core_detection <- function(radius, density, low_limit, up_limit, threshold)
{
  k = -derivative(radius, density)
  k[density >= up_limit | density <= low_limit | k <= 0 | is.na(k)] = 0

  i = 1:(length(density)-1)
  l = k[i] > threshold & k[i+1] == 0

  return(which(l))
}

derivative = function(x,y)
{
  y. = diff(y)/diff(x)
  y. = c(y., NA)
  return(y.)
}

identifyWoodtype <- function(smoothed_density, year)
{
  wt <- rep( 'U', length(smoothed_density))

  for (i in unique(year))
  {
    ix <- which( year == i )
    dd <- c(-9999.0, diff(smoothed_density[ix]))

    mx <- which(dd == max(dd, na.rm=TRUE))

    if(mx+1 < length(ix))
    {
      wt[min(ix):ix[mx]] = 'E'
      wt[ix[mx+1]:max(ix)] = 'L'
    }
  }

  return(wt)
}
