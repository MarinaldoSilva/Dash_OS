library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)

base_dir_linux <- "/mnt/googledrive/" 
#arquivo_xlsx_entrada <- file.path(base_dir_linux, "dados-tratados.xlsx") 
arquivo_xlsx_entrada <- file.path(base_dir_linux, "dados-finais.xlsx") 
arquivo_xlsx_saida <- file.path(base_dir_linux, "dados-finais-formatados.xlsx") 


formatar_datas_df <- function(df_path_in, df_path_out) {
  message("Iniciando a alteração de datas...")
  
  df <- NULL
  retries <- 5
  for (i in 1:retries) {
    tryCatch({
      df <- read_excel(df_path_in)
      break 
    }, error = function(e) {
      message(paste("Tentativa", i, "de leitura de", df_path_in, "falhou:", e$message, ". Aguardando..."))
      Sys.sleep(2) 
      if (i == retries) stop(paste("Falha ao ler o Excel após", retries, "tentativas."))
    })
  }
  
  if (is.null(df)) stop("DataFrame não lido após tentativas de leitura para alteração de datas.")

  message("DataFrame lido com sucesso para alteração de datas.")
  
  df_formatted <- df %>%
    mutate(
      data_ini_clean = na_if(as.character(data_ini), ""), 
      data_ini_clean = na_if(data_ini_clean, "-"),       
      data_ini_parsed = dmy(data_ini_clean, quiet = TRUE),
      hora_ini_clean = na_if(as.character(hora_ini), ""), 
      hora_ini_clean = na_if(hora_ini_clean, "-"),        
      hora_ini_parsed = hms(hora_ini_clean, quiet = TRUE),

      data_hora_completa = if_else(
        !is.na(data_ini_parsed) & !is.na(hora_ini_parsed),
        ymd_hms(paste(format(data_ini_parsed, "%Y-%m-%d"), format(hora_ini_parsed, "%H:%M:%S"))),
        as.POSIXct(NA)
      )
    ) %>%
    select(-any_of(c("data_ini_clean", "hora_ini_clean")))
    
  message("Formatação de datas concluída.")

  message(paste("Salvando arquivo final formatado em:", df_path_out))
  write.xlsx(df_formatted, df_path_out, rowNames = FALSE, overwrite = TRUE)
  message(paste("Arquivo salvo em:", df_path_out))
}

tryCatch({
  formatar_datas_df(arquivo_xlsx_entrada, arquivo_xlsx_saida)
}, error = function(e) {
  message(paste("Erro no script 4_formatacao_datas.R:", e$message))
  stop(e)
})


#sudo kill -9 4510

  #sudo systemctl daemon-reload
  #sudo systemctl restart grafico_OS.service
  #sudo systemctl status grafico_OS.service
