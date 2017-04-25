#' @importFrom  graphics par
plotDensity <- function(data, smooth = TRUE, rings = TRUE, earlywood = TRUE, derivative = FALSE, limits = TRUE, ...)
{
  x  = data$radius
  y  = data$density
  wt = data$woodtype
  rd = data$radius

  end_year = max(data$year)

  ix = data$year[-1] - data$year[-length(data$year)]
  ix = c(0, ix)
  ix = which(ix == 1)

  v_pos = rd[ix]
  lab   = seq(end_year - length(ix), end_year)

  x_pos_lab = c(min(rd), rd[ix], max(rd))
  x_pos_lab= diff(x_pos_lab)/2 + x_pos_lab[1:length(x_pos_lab)-1]

  par(mar = c(5, 4, 4, 5) + 0.1)
  graphics::plot(x, y, type="l", xlab = "Radius (mm)", ylab = "Wood density (kg/m\u00B3)", main= "Density profile \n", ...)

  if(limits)  graphics::abline(h = c(attr(data, "ul"), attr(data, "ll")))
  if (smooth) graphics::lines(x, data$smDensity, col="red")
  if (rings)  graphics::abline(v = v_pos, col = "lightgrey", lwd = 2, lty = "dotted")

  if(earlywood)
  {
    for (i in seq(2, length(x-1)))
    {
      if (wt[i] == 'L' & wt[i-1] == 'E')
        graphics::abline(v = rd[i], col = 'forestgreen', lty = "dashed", lwd = 2)
    }

    graphics::text(x_pos_lab, max(y), lab, srt = 90, col= "forestgreen", cex = 0.8)
  }

  if(derivative)
  {
    graphics::par(new = TRUE)
    graphics::plot(x, derivative(data$radius, data$smDensity), type = "l", col="blue", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    graphics::axis(4)
    graphics::mtext("Wood density derivative", side = 4, line = 3, cex = par("cex.lab"))
  }
}
