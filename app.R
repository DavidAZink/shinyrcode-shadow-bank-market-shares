library(rgdal)
library(rsconnect)
library(dvmisc)
library(maps)
library(ggplot2)
library(tigris)
library(scales)
library(Hmisc)
library(rgdal)
data1=readRDS('census-app/data/data1.rds')
cities=readRDS('census-app/data/cities.rds')
states=as.character(levels(as.factor(data1$region)))



# User interface ----
ui <- fluidPage(
  titlePanel("Shadow Bank Market Shares"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Plots the percent of home mortgage originations in each county by shadow mortgage lenders"),
      
      selectInput("state", 
                  label = "Choose a state to display",
                  choices = c("USA", capitalize(states)),
                  selected = "USA"),
      
      selectInput("year", 
                  label = "Choose a year",
                  choices=2000:2019, 
                  selected=2019)
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({if(input$state!='USA'){
    lat_range=max(filter(data1, Year==input$year & region==tolower(input$state))$lat)-
              min(filter(data1, Year==input$year & region==tolower(input$state))$lat)
    ggplot() +
      geom_polygon(data=filter(data1, Year==input$year & region==tolower(input$state)), 
                   aes(long, lat, group=group, fill=filter(data1, Year==input$year & region==tolower(input$state))$shadow_share),
                   size=0.0001, colour = alpha("black", 0.001))+
      geom_text(data=filter(cities, tolower(state_name)==tolower(input$state))[1, ], mapping=aes(lng, lat-lat_range*0.05, label=city), 
                size=5) +
      geom_point(data=filter(cities, tolower(state_name)==tolower(input$state))[1, ], mapping=aes(lng, lat), size=4) +
      
      scale_fill_gradient2(low=muted('red'), high=muted('green'), mid=0.5, 
                           midpoint=.5, limits=c(0, 1), 
                           space='Lab', guide='colourbar', breaks=seq(0, 1, 0.1), labels=scales::percent) + 
      coord_fixed(ratio=1.3) +
      labs(fill='', title=paste('Shadow Bank Market Share ', input$year, ': ', capitalize(input$state)))+
      theme(legend.position="bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(), 
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),  
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), 
            plot.title = element_text(hjust = 0.5, size=12, face='bold'), legend.key.width = unit(3, "cm"))}
    else {
      lat_range=max(filter(data1)$lat)-min(filter(data1)$lat)
      
      ggplot() +
        geom_polygon(data=filter(data1, Year==input$year), 
                     aes(long, lat, group=group, fill=filter(data1, Year==input$year)$shadow_share), size=0.0001, 
                     colour = alpha("black", 0.001))+
        geom_polygon(data=map_data('state'), aes(long, lat, group=group), fill=NA, colour='white') +
        geom_text(data=cities[1:5, ], mapping=aes(lng, lat-lat_range*0.05, label=city), 
                  size=6, col='black') +
        geom_point(data=cities[1:5, ], mapping=aes(lng, lat), size=4) +
        
        scale_fill_gradient2(low=muted('red'), high=muted('green'), mid='white', midpoint=0.5,
                             space='Lab', guide='colourbar', breaks=seq(0, 1, 0.10), labels=scales::percent) + 
        coord_fixed(ratio=1.3) +
        labs(fill='', title=paste('Shadow Bank Market Share ', input$year))+
        theme(legend.position="bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(), 
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),  
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(), 
              plot.title = element_text(hjust = 0.5, size=12, face='bold'), legend.key.width = unit(3, "cm"))
    }
  })
}
# Run app ----
shinyApp(ui, server)
#rsconnect::deployApp(getwd())

