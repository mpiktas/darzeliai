library(ggplot2)
library(shinydashboard)
library(leaflet)
library("grid")
library(rgdal)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')
df_GISCOUNTY = readRDS("cleandata/gis_seniuniju_rybos.rds")
df_MAIN = readRDS("cleandata/darzeliai_seniunijos_demand_suply_coords.rds")
df_MAIN$year = 2015

df_MAIN_tmp = df_MAIN
for (yr in 2016:2018) {
  eps = rnorm(nrow(df_MAIN_tmp), 1.05, 0.1)
  df_MAIN_tmp = subset(df_MAIN, year == (yr-1))
  df_MAIN_tmp$year = yr
  df_MAIN_tmp$Demand = round(df_MAIN_tmp$Demand * eps)
  df_MAIN = rbind(df_MAIN, df_MAIN_tmp)
}

df_COUNTY_ = df_MAIN %>%
  dplyr::group_by(name) %>% 
  dplyr::summarise(
    sum_demand = sum(Demand, na.rm = T),
    sum_supply = sum(supply, na.rm = T),
    count = n()
  ) %>% 
  dplyr::mutate(
    score = sum_demand / sum_supply
  )
df_GISCOUNTY_ = df_GISCOUNTY
df_GISCOUNTY_@data = df_GISCOUNTY@data %>% 
  dplyr::left_join(df_COUNTY_, by="name")


df_data_gis = df_GISCOUNTY_@data %>% dplyr::select(full_id, osm_id, osm_type, admin_leve, name, name_lt, sum_demand, sum_supply, score)
df_data_demand = df_MAIN %>% dplyr::filter(!is.na(Demand))
df_data_supply = df_MAIN %>% dplyr::filter(!is.na(supply))




theme_orange = theme(
    axis.text = element_text(size = 12),
    legend.background = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border =    element_blank(),
    plot.margin = unit(c(0,0,0,0),"mm"),
    
    # axis.line=element_blank(),
    axis.ticks = element_line(colour = 'white'),
    axis.title.x = element_text(colour = 'white'),
    axis.title.y = element_text(colour = 'white'),
    axis.text.x = element_text(colour = 'white'),
    axis.text.y = element_text(colour = 'white'),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = 'white', size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = 'white'),
    panel.grid.major.y = element_line(colour = 'white', size = 0.1),
    line = element_line(colour = 'white')
    
)
cbPalette <-
    c(
        "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
    )
scale_fill_manual(values = cbPalette)
scale_colour_manual(values = cbPalette)



ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Kindergarten.INSIGHT"),
    dashboardSidebar(
        sliderInput(
            "yearForecast", "Year", sep = "",
            min = 2015, max = 2018, value = 2015, step = 1
        ),
        checkboxGroupInput(
            "layer", "Inspect:",
            selected = "county",
            c(
                "Counties" = "county",
                "Demand" = "demand",
                "Supply" = "supply",
                "Show Private" = "private"
            )
        ),
        
        sidebarMenu(
            menuItem(
                "Vilnius Kindergartens", tabName = "dashboard", icon = icon("globe"),
                selected = TRUE
            ),
            menuItem(
                "Data Table", icon = icon("table"),
                menuItem("Demand", tabName = "tab_data_demand", icon = icon("table")),
                menuItem("Supply", tabName = "tab_data_supply", icon = icon("table")),
                menuItem("GIS", tabName = "tab_data_gis", icon = icon("table"))
                
            ),
            menuItem("Get Data", tabName = "tab_get_data", icon = icon("download")),
            menuItem("Data Sources", tabName = "tab_data_sources", icon = icon("info"))
        )
    ),
    dashboardBody(tabItems(
        tabItem(
            "dashboard",
            fluidRow(
                valueBoxOutput("laukia"),
                valueBoxOutput("lanko"),
                box(
                    width = 4, class = "slim_chart", background = 'orange',
                    tags$head(tags$style(
                        HTML(
                            " div.box { border-radius: 0px !important; border-top: 0px !important; box-shadow:none !important;}"
                        )
                    )),
                    imageOutput("chart01", height = 82)
                )
                
                
            ),
            fluidRow(
                box(
                    tags$head(tags$style(
                        HTML(" #mapbox > div { height:65vh !important;}")
                    )),
                    id = "mapbox", width = 12, height = "100%",
                    leafletOutput("map", width = "100%")
                    
                )
            )
        ),
        tabItem(
          "tab_data_demand",
          dataTableOutput("data_demand")
        ),
        tabItem(
          "tab_data_supply",
          dataTableOutput("data_supply")
        ),
        tabItem(
          "tab_data_gis",
          dataTableOutput("data_gis")
        ),
        tabItem(
          "tab_get_data",
          fluidRow(
            box(width = 12,
              p("Aggregated data will be available soon :)")    
            )
          )
        ),
        tabItem(
          "tab_data_sources",
          fluidRow(
            box(width = 12,
                p("Data source details:"),
                p("Kindergarten demand information was available by default on open data fest Git hub. We analize this data to evaluate end predict demand forecast statistics. Povilas Poderskis gladly provided 2 additional data sets. First one with information about kindergarten address and county which we use to display position on map. And second file with groups size available in kinder-gardens. This was used to evaluate capacity we present together with demand as circles on the map of radius depend on capacity value."),
                p("Data with county areas was extracted from online sources and available as county boundaries on our map and group factor for kindergarten. Online Google API was used to transform address information to GIS location for kindergartens where it's missing. More realable and full information about whole education institution was introduced by 'Musu darzelis' institution reffer Kęsturis Vaškevičius. For first stage of darzelis portal implementation we used newest capacity information from that source only.")    
            )
          )
        )
    ))
    
)




server <- function(input, output) {
    output$lanko <- renderValueBox({
        df_MAIN = subset(df_MAIN, year == input$yearForecast)
        valueBox(
            value = sum(df_MAIN$supply, na.rm = T),
            subtitle = "Happy children",
            icon = icon("area-chart"),
            color = "green"
        )
    })
    
    
    output$laukia <- renderValueBox({
        df_MAIN = subset(df_MAIN, year == input$yearForecast)
        valueBox(
            value = sum(df_MAIN$Demand, na.rm = T),
            subtitle = "Awaiting applications",
            icon = icon("area-chart"),
            color = "red"
        )
    })
    
    output$map <- renderLeaflet({

        df_MAIN = subset(df_MAIN, year == input$yearForecast)
        df_COUNTY = df_MAIN %>%
            dplyr::group_by(name) %>% 
            dplyr::summarise(
                sum_demand = sum(Demand, na.rm = T),
                sum_supply = sum(supply, na.rm = T),
                count = n()
            ) %>% 
            dplyr::mutate(
                score = sum_demand / sum_supply
            )
        
        df_GISCOUNTY@data = df_GISCOUNTY@data %>% 
            dplyr::left_join(df_COUNTY, by="name")
      
        if (!("private" %in% input$layer)) {
            df_MAIN = df_MAIN %>% dplyr::filter(valstybinis)
        }
        
        rbPal = colorRampPalette(c("grey", "blue"))
        colo = rbPal(32)[as.numeric(cut(df_GISCOUNTY@data$score, breaks = 32))]
        
        m = leaflet(df_GISCOUNTY) %>% addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v4/anatolijne.nabccho6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYW5hdG9saWpuZSIsImEiOiIwNl9KajZzIn0.U1zCMm5fMIvZb78820P_yA")
        m = m %>% addPolygons(
            fill = FALSE,
            weight = 2,
            stroke = FALSE,
            fillColor = 'white',
            color = "white",
            fillOpacity = 0.0
        )
        
        if ("county" %in% input$layer) {
            m = m %>% addPolygons(
                fill = TRUE,
                weight = 2,
                stroke = TRUE,
                fillColor = colo,
                color = "white",
                fillOpacity = 0.6,
                popup = paste(
                  "<strong>", df_GISCOUNTY@data$name, "</strong><br />", 
                  "Summary Demand:", df_GISCOUNTY@data$sum_demand, "<br />",
                  "Summary Supply:", df_GISCOUNTY@data$sum_supply, "<br />",
                  "Kindergarten count:", df_GISCOUNTY@data$count, "<br />",
                  "Score:", round(df_GISCOUNTY@data$score, 2), "<br />"
                )
            )
        }
        
        if ("demand" %in% input$layer) {
            m = m %>% addCircles(
                lng = df_MAIN$lng,
                lat = df_MAIN$lat,
                weight = 2,
                color = "white",
                fillColor = "orange",
                fill = TRUE,
                stroke = TRUE,
                fillOpacity = 0.6,
                
                radius = df_MAIN$Demand + df_MAIN$supply,
                popup = paste(
                  "<strong>", df_MAIN$dabartinisInstitucijosPavadinimasLt, "</strong><br/>",
                    df_MAIN$institucijosAdresasTekstu, "<br/>",
                    "Demand: ", df_MAIN$Demand, "<br/>",
                    "Supply: ", df_MAIN$supply, "<br/>"
                )
            )
        }
        
        if ("supply" %in% input$layer) {
            m = m %>% addCircles(
                lng = df_MAIN$lng,
                lat = df_MAIN$lat,
                weight = 1,
                color = "white",
                fillColor = "green",
                fillOpacity = 0.6,
                fill = TRUE,
                stroke = TRUE,
                radius = df_MAIN$supply,
                popup = paste(
                    "<strong>", df_MAIN$dabartinisInstitucijosPavadinimasLt, "</strong><br/>",
                    df_MAIN$institucijosAdresasTekstu, "<br/>",
                    "Demand: ", df_MAIN$Demand, "<br/>",
                    "Supply: ", df_MAIN$supply, "<br/>"
                )
            )
        }
        
        print(m)
        
    })
    
    
    
    output$data_demand <- renderDataTable(df_data_demand)
    output$data_supply <- renderDataTable(df_data_supply)
    output$data_gis <- renderDataTable(df_data_gis)
    
  
    
    
    output$chart01 <- renderPlot({
        df = data.frame(
            year = as.character(2011:2018), demand = c(4520, 5250, 6328, 5723, 4720, 5673, 6421, 7268)
        )
        
        ggplot(data = df, aes(x = year, y = demand)) +
            geom_bar(
                stat = "identity", colour = 'white', fill = 'white'
            ) +
            theme_orange + theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = unit(c(2,0,-2,0),"mm"),
                axis.text.x = element_text(
                    angle = -90, vjust = 0.5, size = 11
                ),
                axis.text.y = element_text(vjust = 0.5, size = 11)
            )
    }, bg = "transparent")
}

shinyApp(ui, server)