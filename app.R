#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(TTR)
library(leaflet)
library(stringr)
library(httr)
library(jsonlite)
library(lubridate)

load("data.RData")
load("gpxs.RData")

css <- '.nav-tabs>li>a {
  font-family: "Lucida Sans", sans-serif;
  color: black;
}'

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  # Application title
  titlePanel("Colorado hike recommender and safety suggestions"),
  setBackgroundImage("mountain.jpg", shinydashboard = TRUE),
  tags$h5("By Yannik Roell (yannik.roell@mg.thedataincubator.com)"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "preset",
        "Not sure where to start, check out these default hikes:",
        c(
          "Starting to be active" = "easy",
          "Active but not in elevation" = "medium",
          "Active and can handle elevation" = "hard",
          "Want a random hike" = "random",
          "Want to set inputs manually below" = "manual"
        )
      ),
      
      numericInput("distance",
                   label = "Choose a distance to hike",
                   value = 1),
      
      numericInput("elevation",
                   label = "Choose an elevation to hike",
                   value = 1),
      
      radioButtons(
        "units",
        "Units:",
        c(
          "Metric (meters, kilometers, celcius)" = "metric",
          "Imperial (feet, miles, fahrenheit)" = "imperial"
        )
      ),
      
      actionButton("go", "Submit")
      
    ),
    
    # Set up tabs
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Hike Details and Suggestions",
          tableOutput("hike"),
          tableOutput("weather"),
          uiOutput("safety")
        ),
        tabPanel("Interactive Map", leafletOutput("mymap")),
        tabPanel("Elevation Profile", plotOutput("ele_profile")),
        tabPanel("General Information", uiOutput("general"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  preset_value <- eventReactive(input$go, {
    if (input$preset == "easy") {
      o = c(1, 1)
    } else if (input$preset == "medium") {
      o = c(5, 100)
    } else if (input$preset == "hard") {
      o = c(10, 100)
    } else if (input$preset == "random") {
      o = c(sample(1:40, 1), sample(1:2000, 1))
    } else {
      o = c(input$distance, input$elevation)
    }
  })
  
  unit_metric <- eventReactive(input$go, {
    if (input$units == "metric") {
      x = TRUE
    } else {
      x = FALSE
    }
  })
  
  dataset <- eventReactive(input$go, {
    if (unit_metric() == TRUE) {
      closest = as.matrix(dist(rbind(c((preset_value()[1] - 1.1) / (37.2 - 1.1),
                                       (preset_value()[2] - 15) / (2363 - 15)
      ), clust$centers)))[-1, 1]
      clust_num = which(closest %in% min(closest))
      
      similar = df_clust[clust$cluster == clust_num, ]
      likely = as.matrix(dist(rbind(c((preset_value()[1] - 1.1) / (37.2 - 1.1),
                                      (preset_value()[2] - 15) / (2363 - 15)
      ), similar)))[-1, 1]
      names(likely) = row.names(similar)
      
      y = head(df[as.numeric(names(sort(likely))), ], 5)[sample(1:3, 1), ]
      
    } else {
      closest = as.matrix(dist(rbind(c((preset_value()[1] * 0.621371 - 1.1 * 0.621371) / (37.2 * 0.621371 - 1.1 * 0.621371),
                                       (preset_value()[2] * 3.28084 - 15 * 3.28084) / (2363 * 3.28084 - 15 * 3.28084)
      ),
      clust$centers)))[-1, 1]
      clust_num = which(closest %in% min(closest))
      
      similar = df_clust[clust$cluster == clust_num, ]
      likely = as.matrix(dist(rbind(c((preset_value()[1] * 0.621371 - 1.1 * 0.621371) /
                                        (37.2 * 0.621371 - 1.1 * 0.621371),
                                      (preset_value()[2] * 3.28084 - 15 * 3.28084) / (2363 * 3.28084 - 15 * 3.28084)
      ), similar)))[-1, 1]
      names(likely) = row.names(similar)
      
      y = head(df[as.numeric(names(sort(likely))), ], 5)[sample(1:3, 1), ]
      
    }
    
  })
  
  forecast <- eventReactive(input$go, {
    x = paste0(dataset()[1, 1], ".csv")
    lng = gpx_list[[x]]$Longitude
    lat = gpx_list[[x]]$Latitude
    res = GET(
      paste0(
        "http://forecast.weather.gov/MapClick.php?lat=",
        lat[1],
        "&lon=",
        lng[1],
        "&FcstType=json"
      )
    )
    data = fromJSON(rawToChar(res$content))
    day = data$time$startPeriodName[1:4]
    temperature = as.numeric(data$data$temperature[1:4])
    weather = data$data$weather[1:4]
    dates = data$time$startValidTime[c(1, 4)]
    list(day, temperature, weather, dates)
  })
  
  output$hike <- renderTable({
    if (unit_metric() == TRUE) {
      z = dataset()
      dis_split = str_split(z$distance, " ")[[1]][1]
      dis_split = str_replace(dis_split, ",", "")
      z$distance = paste(as.numeric(dis_split), "km")
      ele_split = str_split(z$gain, " ")[[1]][1]
      ele_split = str_replace(ele_split, ",", "")
      z$gain = paste(as.numeric(ele_split), "m")
      colnames(z) = c(
        "Trail Name",
        "Route",
        "Difficulty",
        "Distance",
        "Elevation Gain",
        "Rating",
        "Rated"
      )
      z[, 1:5]
    } else {
      z = dataset()
      dis_split = str_split(z$distance, " ")[[1]][1]
      dis_split = str_replace(dis_split, ",", "")
      z$distance = paste(round(as.numeric(dis_split) * 0.621371, 1), "mi")
      ele_split = str_split(z$gain, " ")[[1]][1]
      ele_split = str_replace(ele_split, ",", "")
      z$gain = paste(round(as.numeric(ele_split) * 3.28084, 0), "ft")
      colnames(z) = c(
        "Trail Name",
        "Route Type",
        "Difficulty",
        "Distance",
        "Elevation Gain",
        "Rating",
        "Rated"
      )
      z[, 1:5]
    }
  })
  
  output$weather <- renderTable({
    wet = forecast()
    if (unit_metric() == TRUE) {
      data.frame(
        "Day" = wet[[1]],
        "Weather" = wet[[3]],
        "Temperature" = paste(round((wet[[2]] - 32) * (5 / 9), 1), "\u00B0C"),
        row.names = "Day"
      )
    } else {
      data.frame(
        "Day" = wet[[1]],
        "Weather" = wet[[3]],
        "Temperature" = paste(wet[[2]], "\u00B0F"),
        row.names = "Day"
      )
    }
  }, rownames = TRUE)
  
  output$safety <- renderUI({
    wet = forecast()
    x = paste0(dataset()[1, 1], ".csv")
    sun_today = str_detect(str_to_lower(wet[[3]][1]), "sun")
    sun_tom = str_detect(str_to_lower(wet[[3]][3]), "sun")
    snow_today = str_detect(str_to_lower(wet[[3]][1]), "snow")
    snow_tom = str_detect(str_to_lower(wet[[3]][3]), "snow")
    rain_today = str_detect(str_to_lower(wet[[3]][1]), "rain")
    rain_tom = str_detect(str_to_lower(wet[[3]][3]), "rain")
    if (unit_metric() == TRUE) {
      z = dataset()
      dis_split = str_split(z$distance, " ")[[1]][1]
      dis_split = str_replace(dis_split, ",", "")
      ele_split = str_split(z$gain, " ")[[1]][1]
      ele_split = str_replace(ele_split, ",", "")
      distance = paste(as.numeric(dis_split), "km")
      elevation = paste(as.numeric(ele_split), "m")
      time = as.numeric(dis_split) * (1 / 5) + as.numeric(ele_split) * (1 / 600)
      cold = paste(round((45 - 32) * (5 / 9), 1), "\u00B0C")
      if ((wet[[2]][1] - 32) * (5 / 9) < 7.2) {
        cold_today = TRUE
      } else {
        cold_today = FALSE
      }
      if ((wet[[2]][3] - 32) * (5 / 9) < 7.2) {
        cold_tom = TRUE
      } else{
        cold_tom = FALSE
      }
      rule = "305 m"
      num_rule = 1000 / 3.28084
      liter = round(time / 2 + as.numeric(ele_split) / (1000 / 3.28084), 1)
    } else {
      z = dataset()
      dis_split = str_split(z$distance, " ")[[1]][1]
      dis_split = str_replace(dis_split, ",", "")
      ele_split = str_split(z$gain, " ")[[1]][1]
      ele_split = str_replace(ele_split, ",", "")
      distance = paste(round(as.numeric(dis_split) * 0.621371, 1), "mi")
      elevation = paste(round(as.numeric(ele_split) * 3.28084, 0), "ft")
      time = as.numeric(dis_split) * (1 / 5) + as.numeric(ele_split) * (1 / 600)
      cold = paste(45, "\u00B0F")
      if (wet[[2]][1] < 45) {
        cold_today = TRUE
      } else {
        cold_today = FALSE
      }
      if (wet[[2]][3] < 45) {
        cold_tom = TRUE
      } else{
        cold_tom = FALSE
      }
      rule = "1000 ft"
      num_rule = 1000
      liter = round(time / 2 + as.numeric(ele_split) / (1000 / 3.28084), 1)
    }
    tagList(
      tags$b(paste0("SHERIFF NUMBER for ", gpx_list[[x]]$county[1], " County: ", gpx_list[[x]]$phone[1], " (call in case of an emergency).")),
      tags$br(),
      tags$br(),
      tags$b(
        paste0(
          "If hiking ",
          dataset()[1, 1],
          " today or tomorrow, here are some safety considerations:"
        )
      ),
      tags$ol(
        tags$li("Duration of hike"),
        tags$ul(tags$li(
          paste(
            "This hike should take",
            round(time, 2),
            "hours on average."
          )
        ),
        if (time > 2) {
          tags$li("Since you will be hiking longer than 2 hours, packing snacks would be wise")
        }),
        tags$li("Water requirements"),
        tags$ul(tags$li(
          paste(
            "You should drink a minimum of",
            liter,
            "liters of water on this trail."
          )
        ),
        if (liter > 3) {
          tags$li(
            "Carrying more than 3 liters of water is difficult and you should only go on the hike if a water source is available for refilling."
          )
        },
        if (any(gpx_list[[x]]$river) == TRUE & liter > 3) {
          tags$li(
            "A water source (river) exists somewhere on this trail so bring a filter to refill your water."
          )
        } else if (any(gpx_list[[x]]$river) == FALSE &
                   liter > 3) {
          tags$li(
            "No water source is available on this trail which means you should not hike this trail unless you are an experienced hiker."
          )
        }),
        tags$li("Weather"),
        tags$ul(
          if (cold_today == TRUE & cold_tom == TRUE) {
            tags$li(
              paste0(
                "The temperature is cold today and tomorrow (below ",
                cold,
                "), make sure to dress in layers and pack a warm hat and gloves."
              )
            )
          } else if (cold_today == TRUE & cold_tom == FALSE) {
            tags$li(
              paste0(
                "The temperature is cold today (below ",
                cold,
                "), make sure to dress in layers and pack a warm hat and gloves, but tomorrow should be nice (above ",
                cold,
                "), dress accordingly."
              )
            )
          } else if (cold_today == FALSE & cold_tom == TRUE) {
            tags$li(
              paste0(
                "The temperature is nice today (above ",
                cold,
                "), dress accordingly, but tomorrow should be cold (below ",
                cold,
                "), make sure to dress in layers and pack a warm hat and gloves."
              )
            )
          } else {
            tags$li(
              paste0(
                "The temperature is nice today and tomorrow (above ",
                cold,
                "), dress accordingly."
              )
            )
          },
          if (sun_today == TRUE & sun_tom == TRUE) {
            tags$li(
              "The sun should be shining today and tomorrow, make sure to pack sunscreen, a hat, and sunglasses."
            )
          } else if (sun_today == TRUE) {
            tags$li("The sun should be shining today, make sure to pack sunscreen, a hat, and sunglasses.")
          } else if (sun_tom == TRUE) {
            tags$li(
              "The sun should be shining tomorrow, make sure to pack sunscreen, a hat, and sunglasses."
            )
          },
          if (snow_today == TRUE & snow_tom == TRUE) {
            tags$li(
              "Snow is expected today and tomorrow, make sure to pack warm clothes including a warm hat and gloves."
            )
          } else if (snow_today == TRUE) {
            tags$li(
              "Snow is expected today, make sure to pack warm clothes including a warm hat and gloves."
            )
          } else if (snow_tom == TRUE) {
            tags$li(
              "Snow is expected tomorrow, make sure to pack warm clothes including a warm hat and gloves."
            )
          },
          if (rain_today == TRUE & rain_tom == TRUE) {
            tags$li(
              "Rain is expected today and tomorrow, make sure to pack a rain jacket and rain pants as well as an extra pair of socks in case your feet get wet."
            )
          } else if (rain_today == TRUE) {
            tags$li(
              "Rain is expected today, make sure to pack a rain jacket and rain pants as well as an extra pair of socks in case your feet get wet."
            )
          } else if (rain_tom == TRUE) {
            tags$li(
              "Rain is expected tomorrow, make sure to pack a rain jacket and rain pants as well as an extra pair of socks in case your feet get wet."
            )
          },
          if (mean(month(ymd_hms(wet[[4]]))) > 4 &
              mean(month(ymd_hms(wet[[4]]))) < 9) {
            tags$li(
              "Afternoon lightning and hailstorms are possible near mountain peaks between May and August."
            )
            tags$li(
              "If on the trail past noon, drop below tree-line due to possible lightning strikes."
            )
          } else if (mean(month(ymd_hms(wet[[4]]))) <= 4 |
                     mean(month(ymd_hms(wet[[4]]))) >= 9) {
            tags$li(
              "Snow can persist on trails between September and April so bring crampons just in case."
            )
          }
        ),
        tags$li("Other"),
        tags$ul(if (any(gpx_list[[x]]$river) == TRUE) {
          tags$li(
            "A river exists somewhere near this trail and you might need to wade through. Do not attempt if the water is too high."
          )
        },
        if (any(gpx_list[[x]]$color == "orange")) {
          tags$li(
            "Part of this trail does not have cell phone coverage so bring a satellite phone for emergencies."
          )
        }),
        tags$br(),
        tags$br()
      )
    )
  })
  
  output$mymap <- renderLeaflet({
    x = paste0(dataset()[1, 1], ".csv")
    if (unit_metric() == TRUE) {
      leaflet() %>%
        addTiles() %>%
        addCircles(
          lng = gpx_list[[x]]$Longitude,
          lat = gpx_list[[x]]$Latitude,
          popup = dataset()[1, 1],
          color = gpx_list[[x]]$color,
          radius = gpx_list[[x]]$rad,
          label = as.character(paste(gpx_list[[x]]$Elevation, "m"))
        ) %>%
        addLegend("bottomleft", colors = c("blue", "orange"), 
                  labels = c("Cell Coverage", "No Cell Coverage"),
                  title = "Emergency Cell Coverage",
                  opacity = 1)
      
    } else {
      leaflet() %>%
        addTiles() %>%
        addCircles(
          lng = gpx_list[[x]]$Longitude,
          lat = gpx_list[[x]]$Latitude,
          popup = dataset()[1, 1],
          color = gpx_list[[x]]$color,
          radius = gpx_list[[x]]$rad,
          label = as.character(paste(
            round(gpx_list[[x]]$Elevation * 3.28084, 0), "ft"))
        ) %>%
        addLegend("bottomleft", colors = c("blue", "orange"), 
                  labels = c("Cell Coverage", "No Cell Coverage"),
                  title = "Emergency Cell Coverage",
                  opacity = 1)
    }
    
  })
  
  output$ele_profile <- renderPlot({
    x = paste0(dataset()[1, 1], ".csv")
    ele = gpx_list[[x]]
    slope = ele$slope
    slopevalue = .colMeans(slope, 100, length(slope) / 100)
    slopecolor = "green"
    d = c()
    for (i in 1:as.integer(length(ele$distance_total) / 100)) {
      d[i] = ele$distance_total[i * 100]
    }
    test = data.frame(slopevalue, d)
    test$slopecolor = "#ffeda0"
    test$slopecolor[test$slopevalue > 10] = "#feb24c"
    test$slopecolor[test$slopevalue > 20] = "#f03b20"
    if (unit_metric() == TRUE) {
      y = c(0, max(test$d[1:length(test$d) - 1]))
      y = data.frame(y, 1)
      par(fig = c(0, 1, 0.1, 1))
      plot(
        ele$distance_total,
        ele$Elevation,
        type = "l",
        xaxt = "n",
        xlab = "",
        ylab = "Elevation (m)",
        axes = FALSE
      )
      axis(2, at = seq(min(ele$Elevation), max(ele$Elevation)+100, by = 100))
      par(xpd=TRUE)
      legend("bottom", title = "Slope", legend = c("Not steep", "Moderately steep", "Very steep"), fill = c("#ffeda0", "#feb24c", "#f03b20"), ncol = 3, bty = "n")
      par(fig = c(0, 1, 0, 0.43), new = TRUE)
      plot(
        y,
        type = "l",
        yaxt = "n",
        ylab = "",
        xlab = "Distance (km)",
        axes = FALSE
      )
      seg = seq.int(0, round(max(y), 0))
      if (length(seg) <= 2) {
        axis(1, at = c(seg), label = c(seg))
      } else {
        axis(1, at = c(0, seg[3:length(seg) - 1], round(max(y), 0)), label = c(seg))
      }
      axis(1, at = c(0, max(y)), labels = c("", ""))
      segments(
        x0 = c(0, test$d[1:length(test$d) - 1]),
        y0 = 1,
        x1 = test$d,
        y1 = 1,
        col = test$slopecolor,
        lwd = 20
      )
      rect(max(y),
           0.7,
           max(y) + 1.8,
           1.3,
           col = "white",
           lwd = 0)
      rect(-0.5, 0.3, 0, 1.3, col = "white", lwd = 0)
      } else {
       y = c(0, max(test$d[1:length(test$d) - 1] * 0.621371))
       y = data.frame(y, 1)
       par(fig = c(0, 1, 0.1, 1))
       plot(
         ele$distance_total,
         round(ele$Elevation * 3.28084, 0),
         type = "l",
         xaxt = "n",
         xlab = "",
         ylab = "Elevation (ft)",
         axes = FALSE
       )
       axis(2, at = seq(round(min(ele$Elevation) * 3.28084, 0), round(max(ele$Elevation) * 3.28084 + 300, 0), by = 300))
       par(xpd=TRUE)
       legend("bottom", title = "Slope", legend = c("Not steep", "Moderately steep", "Very steep"), fill = c("#ffeda0", "#feb24c", "#f03b20"), ncol = 3, bty = "n")
       par(fig = c(0, 1, 0, 0.43), new = TRUE)
       plot(
         y,
         type = "l",
         yaxt = "n",
         ylab = "",
         xlab = "Distance (mi)",
         axes = FALSE
       )
       seg = seq.int(0, round(max(y), 0))
       if (length(seg) <= 2) {
         axis(1, at = c(seg), label = c(seg))
       } else {
         axis(1, at = c(0, seg[3:length(seg) - 1], max(y)), label = c(seg[1:length(seg)-1], ""))
       }
       axis(1, at = c(0, max(y)), labels = c("", ""))
       segments(
         x0 = c(0, test$d[1:length(test$d) - 1]) * 0.621371,
         y0 = 1,
         x1 = test$d * 0.621371,
         y1 = 1,
         col = test$slopecolor,
         lwd = 20
       )
       rect(max(y),
            0.7,
            max(y) + 1.8,
            1.3,
            col = "white",
            lwd = 0)
       rect(-0.5, 0.3, 0, 1.3, col = "white", lwd = 0)
     }
  })
  
  output$general <- renderUI({
    tagList(
      tags$b("General information that you should know before hiking:"),
      tags$br(),
      tags$ul(
        tags$li("The duration of the hike is calculated using ", a("Naismith's rule.", href="https://en.wikipedia.org/wiki/Naismith%27s_rule#:~:text=The%20original%20Naismith's%20rule,his%20report%20from%20a%20trip.&text=The%20basic%20rule%20assumes%20hikers,terrain%2C%20and%20under%20normal%20conditions")),
        tags$li("The amount of water needed is calculated is based on a rule of thumb that you should drink 0.5 liters per hour with an additional liter for every 1000 ft or 305 m in elevation gain."),
        tags$li("As a beginner hiker, it can be tough to know how much food and water you need, A good general recommendation for how much to eat is 200–300 calories per hour. For water intake, about a half liter per hour of moderate activity in moderate temperatures is a good starting place. These amounts depend heavily on several factors, such as the intensity of your hike, the weather, your age, your sweat rate and your body type. As you gain more experience, you’ll get a better sense for just how much you need. It’s always a good idea to carry a little extra food and water in case your trip takes longer than anticipated."),
        tags$li("Many hikers carry all the water they will need for a day hike. However, if you anticipate needing more than about 3 liters, that can be quite heavy. By filtering and treating water from backcountry streams and lakes, you refill your water bottles or hydration reservoir and reduce your load."),
        tags$li("When choosing what to wear during a hike, choose clothing made of quick-drying, moisture-wicking fabrics, such as wool or polyester. Avoid cotton, which takes a long time to dry when wet."),
        tags$li("For footwear, the terrain you’ll be walking on will affect your decision. Lightweight, low-cut hiking shoes may be fine on well-maintained trails without a lot of obstacles, whereas sturdy boots may serve you better on a rugged trail with rocks, roots and streams."),
        tags$li("A hiking backpack for short treks on trails that are close to home and on days with pleasant weather, a daypack with a capacity of about 15–20 liters provides enough space for water, a few snacks and a lightweight clothing layer. When you venture farther into the wilderness, you’ll need to carry more gear, clothing, water and food. A pack with a capacity of about 30 liters is a good choice for these journeys."),
        tags$li("While most of us don’t intend to harm our natural surroundings, we may not know how to preserve them, or we’re simply overlooking a few important behaviors. Leave No Trace provides principles that provide guidance for enjoying the outdoors in a sustainable way that avoids human impacts. The principles are: 1) Plan ahead and prepare, 2) Travel and camp on durable surfaces, 3) Dispose of waste properly, 4) Leave what you find, 5) Minimize campfire impacts, 6) Respect wildlife, and 7) Be considerate of other visitors"),
        tags$li("You don’t need to be a medical pro to take a hike, but it’s wise to know some basic first aid. Always carry a first-aid kit and know how to use it. The farther afield you go, the more important it can be to have medical training."),
        tags$li("A good source of information for beginner hikers can be found on ", a("REI's website.", href="https://www.rei.com/learn/expert-advice/hiking-for-beginners.html"))
      ),
      tags$br(),
      tags$br()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
