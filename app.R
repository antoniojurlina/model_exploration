library(tidyverse)
library(ggthemes)
library(shinythemes)
library(shinyWidgets)
library(shiny)

load("app data.RData")

choices <- c("catalog_size", "starting_members", "join_rate", "prosociality")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ABM"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 2,
        	pickerInput(
        		inputId = "filter_catalog_size",
        		label = "Filter catalog size:",
        		choices = unique(abm_data_generic_collapsed$catalog_size),
        		multiple = TRUE,
        		options = list(
        			style = "btn-primary",
        		  title = "Pick value(s).")
        	),
        	pickerInput(
        		inputId = "filter_starting_members",
        		label = "Filter starting members:",
        		choices = unique(abm_data_generic_collapsed$starting_members),
        		multiple = TRUE,
        		options = list(
        			style = "btn-primary",
        			title = "Pick value(s).")
        	),
        	pickerInput(
        		inputId = "filter_prosociality",
        		label = "Filter prosociality:",
        		choices = unique(abm_data_generic_collapsed$prosociality),
        		multiple = TRUE,
        		options = list(
        			style = "btn-primary",
        			title = "Pick value(s).")
        	),
        	pickerInput(
        		inputId = "filter_join_rate",
        		label = "Filter join rate:",
        		choices = unique(abm_data_generic_collapsed$join_rate),
        		multiple = TRUE,
        		options = list(
        			style = "btn-primary",
        			title = "Pick value(s).")
        	),
        	pickerInput(
        		inputId = "facet1",
        		label = "Pick row facet:",
        		choices = choices,
        		options = list(
        			style = "btn-primary",
        			title = "Pick row facet.")
        	),
        	pickerInput(
        		inputId = "facet2",
        		label = "Pick column facet:",
        		choices = choices,
        		options = list(
        			style = "btn-primary",
        			title = "Pick column facet.")
        	),
        	pickerInput(
        		inputId = "color",
        		label = "Pick a color:",
        		choices = choices,
        		options = list(
        			style = "btn-primary",
        		  title = "Pick a color.")
        	),
        	actionBttn(
        		inputId = "generate",
        		label = "Generate",
        		style = "float",
        		color = "primary"
        	)
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
           plotOutput("dataviz", width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

	  plot <- eventReactive(input$generate, {
	  	data <- abm_data_generic_collapsed %>%
	  		filter(catalog_size     %in% input$filter_catalog_size,
	  					 starting_members %in% input$filter_starting_members,
	  					 join_rate        %in% input$filter_join_rate,
	  					 prosociality     %in% input$filter_prosociality)

	  	data %>%
	  		ggplot(aes(x = step, y = members, color = get(isolate(input$color)))) +
	  		geom_point(size = 1) +
	  		facet_grid(isolate(get(input$facet1))~isolate(get(input$facet2))) +
	  		theme_linedraw() +
	  		scale_color_gradient2_tableau() +
	  		theme(legend.title = element_blank(),
	  					legend.position = "bottom")
	  })

    output$dataviz <- renderPlot({
    	plot()
    }, height = 650)
}

# Run the application
shinyApp(ui = ui, server = server)
