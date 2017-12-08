#' @export
#' @rdname ring_detection
ring_detection_UI = function(radius, density)
{
  year = as.numeric(format(Sys.time(), "%Y"))
  start = min(radius)
  end = max(radius)

  ui = shiny::shinyUI(shiny::fluidPage(
    title = "Ring detection",

    shiny::plotOutput("plot", height = 550, width = "100%"),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(3,
        shiny::h4("Smooth density"),

        shiny::selectInput("smooth", "Method", c("Moving filter (average)" = "linear",
                                                 "Moving filter (median)" = "median",
                                                 "Moving filter (Savistsky-Gaulay)" = "savistsky",
                                                 "FFT filter" = "fft",
                                                 "Spline" = "spline",
                                                 "Loess" = "loess")),

        shiny::conditionalPanel(
          condition = "input.smooth == 'linear' | input.smooth == 'median' | input.smooth == 'savistsky'",
          shiny::sliderInput("fw_linear", "Filter with:", min = 1,   max = 20,   value = 5, step = 1)
        ),
        shiny::conditionalPanel(
          condition = "input.smooth == 'spline'",
          shiny::sliderInput("fw_spline", "Soothing degree:", min = 0, max = 0.5, value = 0.06, step = 0.0025)
        ),
        shiny::conditionalPanel(
          condition = "input.smooth == 'loess'",
          shiny::sliderInput("fw_loess", "Soothing degree:", min = 0.01, max = 0.05, value = 0.02, step = 0.001)
        ),
        shiny::conditionalPanel(
          condition = "input.smooth == 'fft'",
          shiny::sliderInput("fw_fft", "Number of harmonics:", min = 0, max = length(radius), value = length(radius), step = 1)
        )
      ),
      shiny::column(3,
        shiny::h4("Rings Detection"),
        shiny::sliderInput("limits", "Lower limit",    min = 300, max = 1000, value = c(600, 800), step = 1),
        shiny::sliderInput("threshold", "Threshold:",     min = 0,   max = 1000, value = 200, step = 1)
      ),
      shiny::column(3,
        shiny::h4("Plot options"),
        shiny::checkboxInput("smdensity", "Smoothed density", TRUE),
        shiny::checkboxInput("ringlimit", "Ring limits", TRUE),
        shiny::checkboxInput("earlywood", "Early wood limits", TRUE),
        shiny::checkboxInput("disp_limits", "Up and low limits", FALSE),
        shiny::checkboxInput("derivative", "Density derivative", FALSE),
        shiny::sliderInput("zoom", label = "Zoom", min = start, max = end, value = c(start, end)),
        shiny::hr()
      ),
      shiny::column(3,
        shiny::h4("Export results"),
        shiny::actionButton("quit", "Export results")
      )
    )
  ))

  server = shiny::shinyServer(function(input, output) {

    output$plot <- shiny::renderPlot({
      sth = input$smooth
      fft = input$fw_fft
      fwl = input$fw_linear
      fws = input$fw_spline
      fwo = input$fw_loess
      ll  = input$limits[1]
      ul  = input$limits[2]
      thd = input$threshold
      smd = input$smdensity
      rgl = input$ringlimit
      ew  = input$earlywood
      lim = input$disp_limits
      der = input$derivative
      zoo = input$zoom

      if(sth == "linear" | sth == "median" | sth == "savistsky")
        fw = fwl
      else if (sth == "spline")
        fw = fws
      else if (sth == "loess")
        fw = fwo
      else if (sth == "fft")
        fw = fft

      data = ring_detection(radius, density, sth, fw, ll, ul, thd)
      plotDensity(data, smd, rgl, ew, der, lim, xlim = zoo)
    })

    shiny::observe({
      if(input$quit > 0)
      {
        sth = input$smooth
        fft = input$fw_fft
        fwl = input$fw_linear
        fws = input$fw_spline
        fwo = input$fw_loess
        ll  = input$limits[1]
        ul  = input$limits[2]
        thd = input$threshold
        smd = input$smdensity
        rgl = input$ringlimit
        ew  = input$earlywood
        lim = input$disp_limits
        der = input$derivative
        zoo = input$zoom

        if(sth == "linear" | sth == "median" | sth == "savistsky")
          fw = fwl
        else if (sth == "spline")
          fw = fws
        else if (sth == "loess")
          fw = fwo
        else if (sth == "fft")
          fw = fft

        data = ring_detection(radius, density, sth, fw, ll, ul, thd)
        shiny::stopApp(data)
      }
    })
  })

  return(shiny::runApp(list(ui = ui, server = server)))
}
