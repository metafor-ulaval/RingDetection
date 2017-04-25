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
        shiny::sliderInput("filter_with", "Filter with:", min = 1,   max = 20,   value = 5,   step = 1),
        shiny::sliderInput("low_limit", "Lower limit",    min = 300, max = 1000, value = 600, step = 1),
        shiny::sliderInput("up_limit", "Upper limit:",    min = 300, max = 1000, value = 800, step = 1),
        shiny::sliderInput("threshold", "Threshold:",     min = 0,   max = 1000, value = 200, step = 1)
      ),
      shiny::column(4,
        shiny::h4("Plot options"),
        shiny::checkboxInput("smdensity", "Smoothed density", TRUE),
        shiny::checkboxInput("ringlimit", "Ring limits", TRUE),
        shiny::checkboxInput("earlywood", "Early wood limits", TRUE),
        shiny::checkboxInput("limits", "Up and low limits", FALSE),
        shiny::checkboxInput("derivative", "Density derivative", FALSE),
        shiny::sliderInput("zoom", label = "Zoom", min = start, max = end, value = c(start, end)),
        shiny::hr(),
        shiny::h4("Export results"),
        shiny::actionButton("quit", "Export results")
      )
    )
  ))

  server = shiny::shinyServer(function(input, output) {

    output$plot <- shiny::renderPlot({
      fw = input$filter_with
      ll = input$low_limit
      ul = input$up_limit
      th = input$threshold
      sm = input$smdensity
      rl = input$ringlimit
      ew = input$earlywood
      li = input$limits
      de = input$derivative
      xlim = input$zoom

      data = ring_detection(radius, density, fw, ll, ul, th)
      plotDensity(data, sm, rl, ew, de, li, xlim = xlim)
    })

    shiny::observe({
      if(input$quit > 0)
      {
        fw = input$filter_with
        ll = input$low_limit
        ul = input$up_limit
        th = input$threshold
        data = ring_detection(radius, density, fw, ll, ul, th)
        shiny::stopApp(data)
      }
    })
  })

  return(shiny::runApp(list(ui = ui, server = server)))
}
