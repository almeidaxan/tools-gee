shinyUI(navbarPage(

	title = "Landsat Downloader",
	tabPanel(
		title = "Pixel",

		icon = icon(name = "th", lib = "font-awesome", class = "fa-lg"),

		sidebarPanel(
			# Set up shinyjs
			useShinyjs(),

			fileInput(inputId = "datafile",
						 label = "Choose CSV File",
						 accept = c(".csv")),
			helpText("The input data must be a .csv file, with comma sep. There must be three columns: plot (id), lat (latitude) and long (longitude)."),
			br(),
			textInput(inputId = "filename",
						 label = "Downloaded data file name",
						 value = "downloaded-data"),
			selectInput(inputId = "versionLS",
							label = "Landsat SR Version",
							choices = list("Collection 1" = "new",
												"Pre-Collection" = "old")),
			bsButton(
				inputId = "botaoDownload",
				label = "Download",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "40%"
			)
			# verbatimTextOutput("teste", placeholder = FALSE)
		),

		mainPanel(
			plotOutput("distPlot")
		)
	)
))