#' Algorithm for ring detection
#'
#' Automatic ring detection and its graphic user interface (GUI)
#'
#' @param radius numeric. Position from the pith
#' @param density numeric. Density for each position
#' @param filter_with scalar. Smoothing parameter for the density profile
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
ring_detection <- function(radius, density, filter_with, low_limit, up_limit, threshold)
{
  if (missing(filter_with))  filter_with = 5
  if (missing(low_limit))    low_limit   = 400
  if (missing(up_limit))     up_limit    = 800
  if (missing(threshold))    threshold   = 200

  scDensity = scaleDensity(density)
  smDensity = smoothDensity(scDensity, filter_with)

  ix = core_detection(radius, smDensity, low_limit, up_limit, threshold)

  year = numeric(length(radius))
  year[ix] = 1
  year = cumsum(year)
  year = year - year[length(year)]

  data = data.frame(radius)
  data$density   = density
  data$year      = year
  data$smDensity = smoothDensity(density, n = filter_with)
  data$woodtype  = identifyWoodtype( smDensity, year )

  attr(data, "fw") = filter_with
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

smoothDensity <- function(density, n)
{
  maf <- rep(1, n)/n
  smoothed <- stats::filter(density, maf)
  return(smoothed)
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

identifyWoodtype <- function(density, year)
{
  wt <- rep( 'U', length(density))

  for (i in unique(year))
  {
    ix <- which( year == i )
    dd <- c(-9999.0, diff(density[ix]))

    mx <- which(dd == max(dd, na.rm=TRUE))

    wt[min(ix):ix[mx]] = 'E'
    wt[ix[mx+1]:max(ix)] = 'L'
  }

  return(wt)
}
