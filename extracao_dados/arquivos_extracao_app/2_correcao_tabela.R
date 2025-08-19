library(readr)
library(dplyr)
library(stringr)
library(openxlsx)

base_dir_linux <- "/mnt/googledrive/"
arquivo_csv_entrada <- file.path(base_dir_linux, "extracao-completa-utf8.csv")
arquivo_xlsx_saida <- file.path(base_dir_linux, "dados-tratados.xlsx")

corrigir_coluna_generica <- function(x, corrections_dict) {
  x_char <- ifelse(is.na(x), "", as.character(x))
  
  for (pattern in names(corrections_dict)) {
    replacement <- corrections_dict[[pattern]]
    if (str_detect(x_char, regex(pattern, ignore_case = TRUE))) {
      return(replacement) 
    }
  }
  return(x_char)
}

correcoes <- list(
  "^(administrativo|administracao)$" = "Administrativo",
  "^(recepcao administrativo|administrativo recepcao|Administrativo Recepcao|Recepcao-Direcao|Administrativo RecepÃ§Ã£o)$" = "Recepção Administrativo",
  "^(agencia transfusional|Banco De Sangue)$" = "Agência Tranfusional",
  "^(Auditoria Prontuario|Auditoria de Prontuarios)$" = "Auditoria de Prontuário",

  "almoxarifado" = "Almoxarifado",
  "biomedicina" = "Biomedicina",
  "centro cirurgico" = "Centro Cirúrgico",
  "centro medico" = "Centro Médico",
  "clinica medica" = "Clínica Médica",
  "direcao" = "Direção",
  "farmacia" = "Farmácia",
  "faturamento" = "Faturamento",
  "lavanderia" = "Lavanderia",
  "nutricao" = "Nutrição",
  "ortopedia" = "Ortopedia",
  "ouvidoria" = "Ouvidoria",
  "pediatria" = "Pediatria",
  "pronto atendimento" = "Pronto Atendimento",
  "psicologia" = "Psicologia",
  "qualidade" = "Qualidade",
  "recursos humanos" = "Recursos Humanos",
  "SAME" = "Same",
  "servico social" = "Serviço Social",
  "UTI" = "UTI"
)
message(paste("Correcoes 'setor' definidas:", length(correcoes), "padrões"))

correcao_computador <- list(
  "Teste Dev" = "Teste Dev",
  "tesla" = "Tesla",
  "cad ad" = "Hugo",
  "machado" = "Marcelo",
  "hugo, wagner" = "Hugo",
  "indefinido" = "Indefinido",
  "Ferista" = "Ferista"
)
message(paste("Correcoes 'computador' definidas:", length(correcao_computador), "padrões"))

correcao_impressora <- list(
  "ricoh" = "RICOH",
  "brother" = "BROTHER",
  "hp" = "HP",
  "epson" = "EPSON",
  "samsung" = "SAMSUNG",
  "lexmark" = "LEXMARK"
)
message(paste("Correcoes 'impressora' definidas:", length(correcao_impressora), "padrões"))

correcao_rede <- list(
  "Ponto Rede" = "Ponto de Rede",
  "cabeamento" = "cabeamento",
  "roteador" = "roteador",
  "wifi" = "wifi"
)
message(paste("Correcoes 'rede' definidas:", length(correcao_rede), "padrões"))

correcao_infra <- list(
  "nobreak" = "Nobreak",
  "energia" = "Energia"
)
message(paste("Correcoes 'infra' definidas:", length(correcao_infra), "padrões"))

correcao_sistema <- list(
  "sysmed" = "Sysmed",
  "MV" = "MV",
  "Soul MV" = "MV",
  "Soul" = "MV",
  "Tasy" = "Tasy",
  "totvs" = "Totvs",
  "prontmed" = "Prontmed",
  "MV-MV" = "MV"
)
message(paste("Correcoes 'sistema' definidas:", length(correcao_sistema), "padrões"))

correcao_telefonia <- list(
  "aparelho telefonico" = "Aparelho Telefônico",
  "pabx" = "PABX",
  "telefone" = "Telefone",
  "conferencia" = "Conferência",
  "sem linha" = "Sem Linha",
  "ramal" = "Ramal",
  "fone" = "Telefone"
)
message(paste("Correcoes 'telefonia' definidas:", length(correcao_telefonia), "padrões"))

correcao_backup <- list(
  "backup" = "Backup",
  "restore" = "Backup",
  "nuvem" = "Backup"
)
message(paste("Correcoes 'backup' definidas:", length(correcao_backup), "padrões"))

correcao_sei <- list(
  "sei" = "SEI",
  "modulo" = "SEI",
  "assinar" = "SEI",
  "tramitar" = "SEI",
  "login" = "SEI",
  "senha" = "SEI",
  "acesso" = "SEI"
)
message(paste("Correcoes 'sei' definidas:", length(correcao_sei), "padrões"))

csv_para_xlsx <- function(arquivo_csv, arquivo_xlsx) {
  tryCatch({
    message(paste("Iniciando a leitura do arquivo:", arquivo_csv))

    df <- NULL
    retries <- 5
    for (i in 1:retries) {
      tryCatch({
        df <- read_csv(arquivo_csv, col_types = cols(.default = col_character()))
        break 
      }, error = function(e) {
        message(paste("Tentativa", i, "de leitura falhou:", e$message, ". Aguardando..."))
        Sys.sleep(2) 
        if (i == retries) stop(paste("Falha ao ler o CSV após", retries, "tentativas."))
      })
    }
    
    if (is.null(df)) stop("DataFrame nulo após tentativas de leitura.")
    
    message("Arquivo CSV lido com sucesso!")

    cols_check <- c("setor", "computador", "impressora", "rede", "infra", "sistema", "telefonia", "backup", "sei")
    missing_cols <- cols_check[!cols_check %in% colnames(df)]
    if (length(missing_cols) > 0) {
      stop(paste("Colunas não encontradas no arquivo CSV:", paste(missing_cols, collapse = ", ")))
    }
    
    message("Iniciando a correção dos setores...")
    df <- df %>%
      mutate(nome_setor_corrigido = sapply(setor, corrigir_coluna_generica, corrections_dict = correcoes, USE.NAMES = FALSE))
    message("Correção dos setores concluída.")
    
    message("Iniciando a correção das categorias...")
    df <- df %>%
      mutate(
        computador = sapply(computador, corrigir_coluna_generica, corrections_dict = correcao_computador, USE.NAMES = FALSE),
        impressora = sapply(impressora, corrigir_coluna_generica, corrections_dict = correcao_impressora, USE.NAMES = FALSE),
        rede = sapply(rede, corrigir_coluna_generica, corrections_dict = correcao_rede, USE.NAMES = FALSE),
        infra = sapply(infra, corrigir_coluna_generica, corrections_dict = correcao_infra, USE.NAMES = FALSE),
        sistema = sapply(sistema, corrigir_coluna_generica, corrections_dict = correcao_sistema, USE.NAMES = FALSE),
        telefonia = sapply(telefonia, corrigir_coluna_generica, corrections_dict = correcao_telefonia, USE.NAMES = FALSE),
        backup = sapply(backup, corrigir_coluna_generica, corrections_dict = correcao_backup, USE.NAMES = FALSE),
        sei = sapply(sei, corrigir_coluna_generica, corrections_dict = correcao_sei, USE.NAMES = FALSE)
      )
    message("Correção das categorias concluída.")

    message(paste("Salvando arquivo corrigido em:", arquivo_xlsx))
    write.xlsx(df, arquivo_xlsx, rowNames = FALSE, overwrite = TRUE)
    message(paste("Arquivo convertido em:", arquivo_xlsx))

  }, error = function(e) {
    message(paste("Erro ao converter arquivo:", e$message))
    stop(e)
  })
}

csv_para_xlsx(arquivo_csv_entrada, arquivo_xlsx_saida)