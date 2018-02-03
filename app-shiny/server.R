source("global.R")

#TODO exibir mensagem de sucesso quando os dados forem baixados
#TODO verificar se, quando nao existem observacoes, o valor NULL/NA eh retornado
#TODO incluir ID na lista
#TODO mudar nome da primeira coluna do csv para id

shinyServer(function(input, output) {

	# output$teste <- renderText({ class(input$datafile) })



	filedata <- reactive({
		infile <- input$datafile

		if (is.null(infile)) {
			return(NULL)
		} else {
			return(read.csv(infile$datapath, header = TRUE, sep = ","))
		}
	})

	# habilita/desabilita o botao de download conforme disponibilidade de df
	# se a condicao for satisfeita, eh habilitado
	observeEvent(input$datafile, ignoreNULL = F, {
		shinyjs::toggleState(
			id = "botaoDownload",
			condition = !is.null(input$datafile)
		)
	})

	observeEvent(input$botaoDownload, {

		isolate ({
			dfCoords <- filedata()
		})

		collection <- input$versionLS
		pathArquivo <- file.path(getwd(), paste0(input$filename, ".rds"))

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

})