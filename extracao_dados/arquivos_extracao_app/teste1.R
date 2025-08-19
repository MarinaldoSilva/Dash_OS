library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(stringr)

log_file <- "/tmp/shiny_debug_log.txt"
log_message <- function(message) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cat(paste(timestamp, message, "\n"), file = log_file, append = TRUE)
}

DATA_PATH <- "/mnt/googledrive/dados-tratados.xlsx"

CATEGORIAS_OS <- c(
  "computador",
  "impressora",
  "rede",
  "infra",
  "sistema",
  "telefonia",
  "sei",
  "backup",
  "vistoria_diurna",
  "vistoria_noturna",
  "web"
)

DEFAULT_ANO <- "2024" 
DEFAULT_CATEGORIA <- "sei"
dados_base <- reactiveVal(NULL)

carregar_e_processar_dados <- function() {
  log_message(paste0("Tentando carregar arquivo: ", DATA_PATH))
  if (file.exists(DATA_PATH)) {
    tryCatch({
      df <- read_excel(DATA_PATH, sheet = 1)
      log_message(paste0("Arquivo '", DATA_PATH, "' carregado com sucesso. Dimensões: ", nrow(df), "x", ncol(df)))
      if ("data_cad" %in% names(df) && class(df$data_cad) %in% c("Date", "POSIXct", "numeric")) {
        df$data_cad <- as.Date(df$data_cad)
        df$dia <- day(df$data_cad)
        df$mes <- month(df$data_cad)
        df$ano <- year(df$data_cad)
        log_message(paste0("Colunas 'dia', 'mes', 'ano' adicionadas. Exemplos de anos: ", paste(unique(df$ano), collapse = ", ")))
      } else {
        log_message("Coluna 'data_cad' não encontrada ou não está no formato de data esperado. Não foi possível adicionar 'dia', 'mes', 'ano'.")
      }
      df$total_os <- 1

      for (col in CATEGORIAS_OS) {
        if (col %in% names(df)) {
          df[[col]] <- ifelse(is.na(df[[col]]) | str_trim(as.character(df[[col]])) == "", 0, 1)
          log_message(paste0("Coluna '", col, "' convertida para 0/1 (1 se não for vazia, 0 se for vazia/NA)."))
        } else {
          log_message(paste0("Atenção: Coluna de categoria '", col, "' não encontrada no dataframe. Certifique-se de que o nome está correto no XLSX."))
          df[[col]] <- 0
        }
      }

      log_message(paste0("Colunas presentes no df após processamento: ", paste(names(df), collapse = ", ")))
      return(df)
    }, error = function(e) {
      log_message(paste0("Erro ao carregar ou processar dados: ", e$message))
      return(NULL)
    })
  } else {
    log_message(paste0("Arquivo de dados não encontrado em: ", DATA_PATH))
    return(NULL)
  }
}

if (is.null(dados_base())) {
  dados_base(carregar_e_processar_dados())
}