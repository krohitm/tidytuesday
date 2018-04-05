#import the required libraries
library(shiny)
library(tidyverse)
library(stringr)
library(maps)

#importing the data
file_name = './us_avg_tuition.xlsx'
us_avg_tuition <- readxl::read_excel(file_name)

#transform the data to turn years into rows and states into columns, 
#so that they are easy to plot
us_avg_tuition_t <- us_avg_tuition %>%
  gather(key=session, value=tuition, '2004-05':'2015-16') %>%
  spread(key=State, value = tuition)

#fixing names of columns like 'New York' to remove spaces
names(us_avg_tuition_t) <- names(us_avg_tuition_t) %>%
  str_replace_all(c(" "="_"))


#Change sessions to particular years for better presentation
us_avg_tuition_t <- us_avg_tuition_t %>%
  separate(session, into = 'year', remove = FALSE, convert = TRUE, extra = "drop")

#Plotting tuitions on USA map

## Importing map data for USA states, and joining with our data
USA <- map_data("state")
USA$region<- USA$region %>%
  str_to_title

map_avg_tuition <- USA %>%
  left_join(us_avg_tuition, by=c('region'='State')) %>%
  gather(key=session, value=tuition, '2004-05':'2015-16') 

#using a common theme for ggplots
common_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Define UI for application that plots for trend
ui <- fluidPage(
  # Application title
  titlePanel('US average tuition for different states'),
  
  #creating a side bar layout for plotting the map for each session
  sidebarLayout(
    sidebarPanel(
      selectInput("session", label="Session", choices=map_avg_tuition$session %>%
                    unique,
                  selected = '2004-05')
    ),
    
    # Show a plot of the map
    mainPanel(
      plotOutput("mapByYear")
    )
  ),
  
  #creating another side bar layout for plotting the trend of fees for each state
  sidebarLayout(
    sidebarPanel(
      selectInput("state", label="State", 
                  choices=names(us_avg_tuition_t) %>%
                    setdiff(c('session', 'year')),
                  selected = 'Alabama')
    ),
    
    # Show a plot of the trend
    mainPanel(
      plotOutput("trend")
    )
  )
)

# Define server logic required to plot the trend
server <- function(input, output) {
  
  min_avg_tuition <- us_avg_tuition %>%
    select(which(sapply(., is.numeric))) %>%
    min
  
  max_avg_tuition <- us_avg_tuition %>%
    select(which(sapply(., is.numeric))) %>%
    max
  
  #filter output by the session selected for the map plot
  output$mapByYear <- renderPlot({
    tuition_mapByYear <- map_avg_tuition %>%
      filter(session==input$session)
    
    #plot the tuition for selected session on the US map
    USA %>%
      ggplot(aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) +
      common_axes +
      theme(panel.background = NULL)+
      geom_polygon(data = tuition_mapByYear, aes(fill = tuition), color = "white") +
      scale_fill_gradientn(limits=c(min_avg_tuition, max_avg_tuition), colours=c('#7FFFD4','#FF8C00'))+
      ggtitle("Average tuition fees in different states") +
      guides(fill=
               guide_legend(
                 title = 'Avg. Tuition ($)'
               ))
  })
  
  #plot the fees trend for the selected state
  output$trend <- renderPlot({
    us_avg_tuition_t %>%
      ggplot() +
      geom_line(aes(x=year, y=us_avg_tuition_t[input$state]), color = 'blue', size=1.5)  +
      theme(panel.background = NULL) +
      labs(x='Session', y='Average Tuition Fees ($)') +
      ggtitle(paste("Average tuition fees over different years in", input$state)) +
      ylim(min_avg_tuition, max_avg_tuition)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)