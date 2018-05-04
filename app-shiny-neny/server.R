source("global.R")

#TODO exibir mensagem de sucesso quando os dados forem baixados
#TODO verificar se, quando nao existem observacoes, o valor NULL/NA eh retornado
#TODO incluir ID na lista
#TODO mudar nome da primeira coluna do csv para id

shinyServer(function(input, output) {

	# output$teste <- renderText({  })

	pixel_filedata <- reactive({
		infile <- input$pixel_datafile

		if (is.null(infile)) {
			return(NULL)
		} else {
			print(infile$datapath)
			return(read.csv(infile$datapath, header = TRUE, sep = ","))
		}
	})

	raster_filedata <- reactive({

		infile <- input$raster_datafile
		infolder <- substr(infile$name, 1, nchar(infile$name) - 4)

		if (is.null(infile)) {
			return(NULL)
		} else {
			unzip(infile$name, exdir = infolder)
			shp <- shapefile(file.path(getwd(), infolder, paste0(infolder, ".shp")))
			return(list(infolder, shp))
		}
	})

	# habilita/desabilita o botao de download conforme disponibilidade de df
	# se a condicao for satisfeita, eh habilitado
	observeEvent(input$pixel_datafile, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "pixel_botaoDownload",
			condition = !is.null(input$pixel_datafile)
		)
		shinyjs::toggleState(
			id = "pixel_showMap",
			condition = !is.null(input$pixel_datafile)
		)
	})

	observeEvent(input$raster_datafile, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "raster_botaoDownload",
			condition = !is.null(input$raster_datafile)
		)

		shinyjs::toggleState(
			id = "raster_showMap",
			condition = !is.null(input$raster_datafile)
		)
	})

	observeEvent(input$pixel_botaoDownload, {

		isolate ({
			dfCoords <- pixel_filedata()
		})

		collection <- input$pixel_versionLS
		pathArquivo <- file.path(getwd(), paste0(input$pixel_filename, ".rds"))

		if(collection == "new"){
			sat <- c("LT04/C01/T1_SR", "LT05/C01/T1_SR", "LE07/C01/T1_SR", "LC08/C01/T1_SR")
		} else{
			sat <- c("LT4_SR", "LT5_SR","LE7_SR", "LC8_SR")
		}

		if(file.exists(pathArquivo)) {
			serieList <- readRDS(pathArquivo)
			startJ <- (serieList %>% length) + 1
		} else {
			serieList <- list()
			startJ <- 1
		}

		if(startJ <= nrow(dfCoords)) {
			withProgress(message = 'Downloading', value = 0, {
				for(j in startJ:nrow(dfCoords)) {
					setProgress(j / nrow(dfCoords), detail = paste0(j, "/", nrow(dfCoords)))
					# Ponto para ser baixado
					lat <- dfCoords$lat[j]
					lng <- dfCoords$long[j]
					python.assign("coords", c(lng, lat)) # <- deve estar na ordem (lng, lat)

					# Chama o script em Python para download das series
					df <- NULL
					for(i in 1:length(sat)) {
						# Define qual satelite vai ser baixado
						python.assign("satChoice", sat[i])

						# Executa o script do Python
						python.load(file.path(paste0("gee-px-ls-", collection,".py")))

						# Recebe o output do Python; se dados nao estiverem disponiveis, recebe NULL
						if (is.null(unlist(python.get("serie")))) {
							serie <- NULL
						} else {
							serie <- unlist(python.get("serie"))
						}

						if(serie %>% is.null %>% not) {
							# Transforma dados do Python em um df
							# Remove
							tmp <- matrix(serie,
											  ncol = python.get("numCol"),
											  byrow = T) %>% as.data.frame()
							isRowNA <- apply(tmp, MARGIN = 1, FUN = function(x) {
								(x == "NA") %>% sum
							})
							tmp <- tmp[isRowNA == 0, ]

							# Caso todas as linhas sejam NA, nao roda o resto do codigo
							if(nrow(tmp) > 0) {
								tmp[, 1] %<>% as.character
								tmp[, 2:python.get("numCol")] %<>% lapply(FUN = function(x) {
									x %>% as.character %>% as.numeric %>% round(4)
								})

								tmp %<>% as.data.frame()

								# Formatacao das classes dos dados e colunas do df
								tmp[, 1] <- as.Date(tmp[, 1], format = "%Y_%m_%d")
								tmp[, 2:ncol(tmp)] <- apply(tmp[, 2:ncol(tmp)],
																	 MARGIN = 2,
																	 as.numeric)
								colnames(tmp) <- python.get("colNames")

								# Exclui dados saturados, caso existam
								filterWhich <- which(rowSums(tmp[, 2:ncol(tmp)] == 2) > 0)
								if (length(filterWhich) > 0) {
									tmp <- tmp[-filterWhich, ]
								}

								# Se os dados existem, cria coluna com nome do satelite e cresce o df final
								if(tmp$date[1] %>% is.na %>% not) {
									tmp$sat <- sat[i]
									df <- rbind(df, tmp)
								}
							}
						}

						python.assign("aux", NULL)
						python.assign("serie", NULL)
						python.assign("values", NULL)
						python.assign("numCol", NULL)
						python.assign("colNames", NULL)

						# cat(paste(sat[i], "baixado\n"))
					}

					if(collection == "new" & df %>% is.null %>% not) {
						tmp <- intToBits(df$pixel_qa) %>% as.numeric %>% matrix(nrow = nrow(df), byrow = T)
						df$clearBit <- tmp[, 2]
						df$confBit <- tmp[, 7] + tmp[, 8] * 2
						setDT(df)
						df <- df[clearBit == 1, ]
						df <- df[, -c("pixel_qa", "clearBit")]
						df[sat == "LT04/C01/T1_SR", sat := "LSR4"]
						df[sat == "LT05/C01/T1_SR", sat := "LSR5"]
						df[sat == "LE07/C01/T1_SR", sat := "LSR7"]
						df[sat == "LC08/C01/T1_SR", sat := "LSR8"]
						setkey(df, "date")
					}

					if(collection == "old") {
						setDT(df)
						df[sat == "LT4_SR", sat := "LSR4"]
						df[sat == "LT5_SR", sat := "LSR5"]
						df[sat == "LT7_SR", sat := "LSR7"]
						df[sat == "LT8_SR", sat := "LSR8"]
					}

					serieList[[j]] <- df

					saveRDS(serieList, pathArquivo)

				}
			})
		}
	})

	observeEvent(input$raster_botaoDownload, {
		workpath <- getwd()
		isolate ({
			shape <- raster_filedata()[[1]]
		})

		python.assign("msg", NULL) # nome da pasta descomprimida
		python.assign("shape", shape) # nome da pasta descomprimida
		python.assign("satellite", input$raster_satellite) # numero do satelite
		python.assign("satprod", input$raster_versionLS) # versao do landsat
		python.assign("periodStart", input$raster_periodStart) # data para comecar a baixar
		python.assign("periodEnd", input$raster_periodEnd) # data que termina de baixar

		# Executa o script do Python
		pathR <- getwd()
		python.load(file.path("gee-ls-prepare.py"))

		# Pega o numero de imagens para serem baixadas
		nRaster <- python.get("imgColLen")

		if(nRaster > 0) {
			withProgress(message = 'Downloading', value = 0, {
				for(i in 0:(nRaster-1)) {
					setProgress((i+1) / nRaster, detail = paste0((i+1), "/", nRaster))
					python.assign("i", i) # atualizada o valor de i do loop
					python.load(file.path(pathR,"download-raster-ls.py"))
				}
			})
			if(input$download_SRTM) {
				python.load(file.path(pathR,"download-SRTM.py"))
			}
		}

		output$msg <- renderText({ python.get("msg") })

		setwd(workpath)

	})

	output$pixel_leaf <- renderLeaflet({

		m <- leaflet(options = list(attributionControl = F))
		m <- addTiles(map = m,
						  urlTemplate = "http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
						  attribution = "Imagery &copy;2016 TerraMetrics",
						  options = list(maxZoom = 20, noWrap = T, subdomains = c('mt0','mt1','mt2','mt3')))
		m <- setMaxBounds(m, -180, -90, 180, 90)

		if(input$pixel_showMap){

			dfCoords <- pixel_filedata()


			for(i in 1:nrow(dfCoords)){
				center <- SpatialPoints(coords = data.frame(dfCoords$long[i], dfCoords$lat[i]),
												proj4string = CRS(proj_ll))
				proj_utm_def <- proj_utm(center)
				center <- spTransform(center, CRS(proj_utm_def))

				xm <- extent(center)[1] # lon
				ym <- extent(center)[3] # lat

				size <- 30
				xypoly <- data.frame(x = c(xm-size/2, xm+size/2, xm+size/2, xm-size/2),
											y = c(ym-size/2, ym-size/2, ym+size/2, ym+size/2))

				outShape <- SpatialPolygons(list(Polygons(list(Polygon(xypoly)),1)),
													 proj4string = CRS(proj_utm_def))
				outShape <- spTransform(outShape, CRS(proj_ll))
				outShape <- SpatialPolygonsDataFrame(outShape, data.frame("ID"=0))

				m <- addPolygons(m, data = outShape, color = "red", opacity=1, fillOpacity=0)
			}
			m
		} else {
			m <- setView(m, lat = 0, lng = 0, zoom = 2)
			m
		}
	})

	output$raster_leaf <- renderLeaflet({
		shp <- raster_filedata()[[2]]

		m2 <- leaflet(options = list(attributionControl = F))
		m2 <- addTiles(map = m2,
						  urlTemplate = "http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
						  attribution = "Imagery &copy;2016 TerraMetrics",
						  options = list(maxZoom = 20, noWrap = T, subdomains = c('mt0','mt1','mt2','mt3')))

		m2 <- setMaxBounds(m2, -180, -90, 180, 90)

		if(input$raster_showMap){
			m2 <- addPolygons(m2, data = shp, color = "white", opacity=1, fillOpacity=0)
			m2
		} else {
			m2 <- setView(m2, lat = 0, lng = 0, zoom = 2)
			m2
		}
	})

})