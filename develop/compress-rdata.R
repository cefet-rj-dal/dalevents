data_path <- "data/"

# Lista todos os arquivos .RData
rdata_files <- list.files(data_path, pattern = "\\.RData$", full.names = TRUE)

# Para cada arquivo .RData
for (file in rdata_files) {
  # Carrega o conteúdo
  load(file)

  # Pega o nome do objeto salvo
  obj_name <- tools::file_path_sans_ext(basename(file))

  # Usa get() para pegar o objeto na memória
  save(list = obj_name, file = file, compress = "xz")  # ou compress = "bzip2"
}

