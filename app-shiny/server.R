shinyServer(function(input, output) {
		
	filedata <- reactive({
		
		infile <- input$datafile
		
		if (is.null(infile)) {
			return(NULL)
		}
		
		read.csv(infile$datapath, header = TRUE, sep = ",")
	})
	
	output$distPlot <- renderPlot({
		input$botaoDownload

		df <- filedata()

		withProgress(message = 'Getting time series', value = 0, {

			n <- nrow(df)
			lat <- NULL
			long <- NULL

			for (i in 1:n) {
				# Each time through the loop, add another row of data. This is
				# a stand-in for a long-running computation.
				lat[i] <- df$lat[i]
				long[i] <- df$long[i]

				# Increment the progress bar, and update the detail text.
				incProgress(1/n, detail = paste("Doing part", i))

				# Pause for 0.1 seconds to simulate a long computation.
				Sys.sleep(0.01)
			}
		})

		plot(long, lat)

	})
})