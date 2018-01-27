require(pacman)
p_load(data.table)
p_load(magrittr)
p_load(rPython)

# Satelites para baixar
sat <- c("LT04/C01/T1_SR", "LT05/C01/T1_SR", "LE07/C01/T1_SR", "LC08/C01/T1_SR")

# Ponto para ser baixado
lat <- 3.75
lng <- -62.25
python.assign("coords", c(lng, lat)) # deve estar na ordem (lng, lat)

# Call Python download script for each selected satellite
df <- NULL
for(i in 1:length(sat)) {
	# Define qual satelite vai ser baixado
	python.assign("satChoice", sat[i])

 	# Executa o script to Python
	python.load(file.path("gee-px-ls.py"))

	# Recebe o output do Python; se dados nao estiverem disponiveis, recebe NA
	if (is.null(unlist(python.get("serie")))) {
		serie <- NA
	} else {
		serie <- unlist(python.get("serie"))
	}

 	# Transforma dados do Python em um df
	tmp <- data.frame(matrix(serie,
									 ncol = python.get("numCol"),
									 byrow = T))

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

 	python.assign("aux", NULL)
 	python.assign("serie", NULL)
 	python.assign("values", NULL)
 	python.assign("numCol", NULL)
 	python.assign("colNames", NULL)

 	cat(paste(sat[i], "baixado\n"))
}

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
