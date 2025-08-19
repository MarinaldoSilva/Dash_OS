library(RMySQL)
library(dplyr)
library(readr)

main <- function() {
  base_dir <- "/mnt/googledrive/"
  
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  conn <- NULL
  
  tryCatch({
    print("Conectando ao banco de dados...")
    conn <- dbConnect(MySQL(),
    host = "192.168.12.25",
    user = "dev",
    password = "cpd",
    dbname = "scaf")
    
    if (dbIsValid(conn)) {
      print("Conexão com BD ok")
      
      #query do banco das linhas (1-11662)
      print(paste0("\n", strrep("=", 50)))
      print("Extraindo parte 1 (registros 1-11662)")
      query_part1 <- "SELECT * FROM os_externa LIMIT 11662"
      df_part1 <- dbGetQuery(conn, query_part1)
      print(paste0("Extraídos ", nrow(df_part1), " registros"))
      
      #salva aruqivo
      file_part1 <- file.path(base_dir, 'extracao-de-dados1-utf8.csv')
      write_csv(df_part1, file_part1, na = "", append = FALSE, col_names = TRUE) # write_csv from readr for UTF-8
      print(paste0("Arquivo salvo: ", file_part1))
      
      #query do banco das linhas (11663-30000)
      print(paste0("\n", strrep("=", 50)))
      print("Extraindo parte 2 (registros 11663-30000)")
      query_part2 <- "SELECT * FROM os_externa LIMIT 18337 OFFSET 11662"
      df_part2 <- dbGetQuery(conn, query_part2)
      print(paste0("Extraídos ", nrow(df_part2), " registros"))
      
      #salvar aruqivo
      file_part2 <- file.path(base_dir, 'extracao-de-dados2-utf8.csv')
      write_csv(df_part2, file_part2, na = "", append = FALSE, col_names = TRUE)
      print(paste0("Arquivo salvo: ", file_part2))
      
      #fazendo a polimelização dos arquivos
      print(paste0("\n", strrep("=", 50)))
      print("Combinando os arquivos")
      df_combined <- bind_rows(df_part1, df_part2)
      print(paste0("Total de registros combinados: ", nrow(df_combined)))
      
      #Salvar arquivo combinado
      combined_file <- file.path(base_dir, 'extracao-completa-utf8.csv')
      write_csv(df_combined, combined_file, na = "", append = FALSE, col_names = TRUE)
      print(paste0("Arquivo salvo: ", combined_file))
      
    } else {
      print("Não foi possível conectar ao banco de dados.")
    }
  },
  error = function(e) {
    print(paste0("Erro de conexão: ", e$message))
  },
  finally = {
    if (!is.null(conn) && dbIsValid(conn)) {
      dbDisconnect(conn)
      print("Conexão encerrada.")
    }
  })
}

main()