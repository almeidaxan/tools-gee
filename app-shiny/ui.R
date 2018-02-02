shinyUI(pageWithSidebar(
	
	headerPanel("Landsat Pixel Downloader"),
	
	sidebarPanel(
		fileInput('datafile', 'Choose CSV File',
				  accept=c('.csv')),
		br(),
		helpText("The input data must be a .csv file, with comma sep. The columns must have the names lat (for latitude) and long (for longitude)."),
		actionButton("botaoDownload", "Download", icon = icon("download", lib="font-awesome"))
	),
	
	mainPanel(
		plotOutput("distPlot")
	)
	
))