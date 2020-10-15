# Load packages and prepare data
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(shinydashboard)

library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)

crimes <- read_excel("Data_Tables_Criminal_Incidents_Visualisation_Year_Ending_December_2019.xlsx", sheet = "Table 01")
victims <- read_excel("Data_Tables_Victim_Reports_Visualisation_Year_Ending_December_2019.xlsx", sheet = "Table 04")
lga <- read_excel("Data_Tables_LGA_Recorded_Offences_Year_Ending_December_2019.xlsx", sheet = "Table 01")

crimes <- crimes %>% 
    rename(
        "Incidents_Recorded" = "Incidents Recorded",
        "Offence_Division" = "Offence Division",
        "Offence_Subdivision" = "Offence Subdivision"
    )

lga <- lga %>% rename("Local_Government_Area" = "Local Government Area")

victims$Year <- as.factor(victims$Year)
victims$Sex <- as.factor(victims$Sex)
victims$`Age Group` <- as.factor(victims$`Age Group`)
lga$Local_Government_Area <- as.factor(lga$Local_Government_Area)
lga$Local_Government_Area <- toupper(lga$Local_Government_Area)

grouped <- crimes %>% group_by(Year) %>% summarise(sum = sum(Incidents_Recorded), .groups='drop')


vic.lga.shp <-  rgdal::readOGR("vmlite_lga_cm/vmlite_lga_cm.shp")
merge.lga.data<-sp::merge(vic.lga.shp, lga, by.x='lga_name', by.y='Local_Government_Area', duplicateGeoms = TRUE)



# Assign ui function

ui <- dashboardPage(
    dashboardHeader(title = "Crime Statistics of Victoria", titleWidth = 450),
    dashboardSidebar(disable = FALSE, width = 300,
                     helpText("Data Source: Crime Statistics Agency, Victoria. (2020). Year Ending 31 December 2019. https://www.crimestatistics.vic.gov.au/crime-statistics/latest-crime-data/download-data"),
                     helpText("Hover over the bars to explore the Crime Statistics by SubDivisions and Victim Reports by Age Group.")),
    dashboardBody(
        fluidRow(
            box(plotlyOutput("overAll", height = 275)),
            box(leafletOutput("map", height = 275))
        ),
        fluidRow(
            box(plotlyOutput("crimeDivisions", height = 275)),
            box(plotlyOutput("victims", height = 275))
        )
    ))



# Assign server function

server <- function(input, output, session) {
    
    output$overAll <- renderPlotly({
        p4 <- plot_ly(data = grouped, x = ~Year, y =~sum, type = "bar") %>%
            layout(yaxis = list(title = "Total number of crime incidents"),
                   xaxis = list(title = "Years"),
                   title = "Crime Trend over the years 2010-2019")
    })
    
    output$crimeDivisions <- renderPlotly({
        p5 <- plot_ly(data = crimes, x = ~Year, y =~Incidents_Recorded, type = "bar", 
                      color = ~Offence_Division, colors = "Blues", 
                      text = paste("Subdivision = ", crimes$Offence_Subdivision)) %>%
            layout(yaxis = list(title = "Crime Incidents Recorded"),
                   xaxis = list(title = "Years"),
                   title = "Divisions of Crime Incidents Recorded",
                   legend = list(title = list(text = "Crime Divisions")))
    })
    
    output$victims <- renderPlotly({
        p5 <- plot_ly(data = victims, x = ~Year, y =~`Victim Reports`, type = "bar", color = ~Sex, 
                      colors = "Blues", text = paste("Age group = ",victims$`Age Group`)) %>%
            layout(yaxis = list(title = "Victim Reports"),
                   xaxis = list(title = "Year"),
                   title = "Demographics of Victim Reports",
                   legend = list(title = list(text = "Gender")))
    })
    
    output$map <- renderLeaflet({
        
        bins <- quantile(
            lga$`Offence Count`,
            probs = seq(0,1,.2), names = FALSE, na.rm = TRUE)
        
        pal <- colorBin(
            "Blues",
            domain = lga$`Offence Count`, 
            bins = bins
        )
        p3 <- leaflet(merge.lga.data) %>% 
            setView(lng = 147, lat = -36.5, zoom = 6)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g Offence Count",
            merge.lga.data$lga_name, 
            merge.lga.data$`Offence Count`
        ) %>% lapply(htmltools::HTML)
        
        p3 %>% addPolygons(
            fillColor = ~pal(`Offence Count`),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
            addLegend(pal = pal, 
                      values = ~lga_name, 
                      opacity = 0.7, title = "Offence Count",
                      position = "bottomright") %>% 
            addControl("Total Offence Count in each Local Government Area from 2010-2019",
                       position = "topright") 
    })
    
}



# Deploy app

shinyApp(ui = ui, server = server)