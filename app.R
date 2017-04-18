library("shiny")
library("ggplot2")
library("reshape2")

data <- read.csv('data.csv')
data1 <- data[!data$Region=="", ]
data2 <- melt(data1, id.vars=c('Country.Name', 'Indicator.Name', 'Region'))
data2$variable <- as.numeric(substring(data2$variable, 2))
names(data2) <- c('Country', 'Indicator', 'Region', 'Year', 'Value')
data3 <- dcast(data2, Country + Region + Year ~ Indicator, value.var='Value')
names(data3) <- c('Country', 'Region', 'Year', 'Fertility_Rate', 'Life_Expectancy', 'Population')

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    checkboxGroupInput('region', 'Region Filter',
                        choices = sort(unique(factor(data3$Region))),
                        selected = sort(unique(factor(data3$Region)))),
    sliderInput('population', 'Population Scale', value = 5,
                min = 0, max = 10, ticks=FALSE),
    sliderInput('year', 'Year', value=1960, min=1960, max=2014, sep = "",
                animate=animationOptions(interval=2000), ticks=FALSE)
  ),
  mainPanel(
    # this is an extra div used ONLY to create positioned ancestor for tooltip
    # we don't change its position
    div(
      style = "position:relative",
      plotOutput("plot1", 
                 hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      uiOutput("hover_info")
    ),
    width = 7
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    data3[data3$Year == input$year, ]
  })
  
  output$plot1 <- renderPlot({
    theme <- theme(
      plot.title = element_text(size = 24, hjust = 0.5),
      panel.background = element_rect(fill = 'white', colour = 'gray'),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(colour='gray'),
      panel.grid.major.y = element_line(colour='gray'),
      panel.border = element_blank()
    )
    p <- ggplot(data=selectedData(), aes(x=Life_Expectancy, y=Fertility_Rate,
                                         size=Population**input$population))
    p <- p + geom_point(aes(fill=Region), alpha=0.3,
                        colour='black', shape = 21, stroke=0.2)
    # below is terribly inefficient, but... it works, so...
    p <- p + geom_point(data=selectedData()[selectedData()$Region %in% input$region, ],
                        aes(fill=Region), colour='black', shape = 21, stroke=0.2)
    # something with scale_size_continuous to make sizes bigger, but not working :(
    p <- p + scale_size(guide = "none") + theme
    p <- p + ggtitle("Life Expectancy vs Fertility Rate")
    p <- p + scale_x_continuous('Life Expectancy', breaks=c(20, 40, 60, 80), limits=c(20,80))
    p <- p + scale_y_continuous('Fertility Rate', breaks=seq(1:9), limits=c(1,9))
    p <- p + labs(fill='Region')
    p
  })
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(selectedData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Region: </b>", point$Region, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>",
                    "<b> Life Expectancy: </b>", point$Life_Expectancy, "<br/>",
                    "<b> Fertility Rate: </b>", point$Fertility_Rate)))
    )
  })
}

shinyApp(ui = ui, server = server)