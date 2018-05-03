shinyUI(navbarPage(

	title = div(em(strong("Landsat Downloader")), HTML("<small>by Menini & Almeida</small>")),
	windowTitle = "Landsat Downloader",
	tabPanel(
		title = "Pixel",

		icon = icon(name = "th", lib = "font-awesome", class = "fa-lg"),

		sidebarPanel(
			# Set up shinyjs
			useShinyjs(),

			fileInput(inputId = "pixel_datafile",
						 label = "Choose CSV File",
						 accept = c(".csv")),
			helpText("The input data must be a .csv file, with comma sep. There must be three columns: plot (id), lat (latitude) and long (longitude)."),
			br(),
			checkboxInput("pixel_showMap", "Show points on the map?", FALSE),
			textInput(inputId = "pixel_filename",
						 label = "Downloaded data file name",
						 value = "downloaded-data"),
			selectInput(inputId = "pixel_versionLS",
							label = "Landsat SR Version",
							choices = list("Collection 1" = "new",
												"Pre-Collection" = "old")),
			bsButton(
				inputId = "pixel_botaoDownload",
				label = "Download",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			)
			# verbatimTextOutput("teste", placeholder = FALSE)
		),

		mainPanel(
			leafletOutput(
				outputId = "pixel_leaf"
			)
		)
	),

	tabPanel(
		title = "Raster",

		icon = icon(name = "square-o", lib = "font-awesome", class = "fa-lg"),

		sidebarPanel(
			# Set up shinyjs
			useShinyjs(),

			fileInput(inputId = "raster_datafile",
						 label = "Choose shapefile",
						 accept = c(".zip")),
			helpText(
				"The shape must be compressed into a zip with, at least, the .shp, .shx, .dbf, and .prj files. The zip file must have the same name as its contents."
			),
			br(),
			checkboxInput("raster_showMap", "Show shapefile on the map?", FALSE),
			selectInput(inputId = "raster_versionLS",
							label = "Landsat SR Version",
							choices = list("Collection 1" = "SR_new",
												"Pre-Collection" = "SR_old",
												"TOA" = "TOA")),
			selectInput(inputId = "raster_satellite",
							label = "Landsat Number",
							choices = list(4, 5, 7, 8)),
			textInput(inputId = "raster_periodStart",
						 label = "Period start",
						 value = "2000-01-01"),
			textInput(inputId = "raster_periodEnd",
						 label = "Period end",
						 value = "2000-12-31"),
			bsButton(
				inputId = "raster_botaoDownload",
				label = "Download",
				style = "primary",
				icon = icon("download", lib = "font-awesome"),
				width = "50%"
			),
			textOutput("msg")
		),

		mainPanel(
			leafletOutput(
				outputId = "raster_leaf"
			)
		)
	)

))