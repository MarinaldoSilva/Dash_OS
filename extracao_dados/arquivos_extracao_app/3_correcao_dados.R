library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(stringr)
library(tidyr)

base_dir_linux <- "/mnt/googledrive/"
arquivo_xlsx_entrada <- file.path(base_dir_linux, "dados-tratados.xlsx") 
arquivo_xlsx_entrada_setor <- file.path(base_dir_linux, "python-dados.xlsx")
arquivo_xlsx_saida <- file.path(base_dir_linux, "dados-finais.xlsx")

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

dados_nao_preenchidos_ou_faltantes_R <- function(df) {
  message("Aplicando correções de preenchimento e faltantes (Python: dados_nao_preenchidos_ou_faltantes)...")

  df <- df %>%
    mutate(
      executor = case_when(
        tipo_os == "SEI" & setor__corrigido == "Teste DEV" & (executor == "-" | is.na(executor) | str_trim(executor) == "") ~ "Tesla",
        TRUE ~ executor )
    )
  message("Corrigindo executor Teste Dev -> Tesla (se tipo_os='SEI' e setor__corrigido='Teste DEV' e executor='-')")

  df <- df %>%
    mutate(
      executor = case_when(
        str_to_lower(executor) == "tesla" ~ "Tesla",
        TRUE ~ executor
      )
    )
  message("Corrigindo executor 'tesla' -> 'Tesla'")

  df <- df %>%
    mutate(
      solicitante = case_when(
        solicitante == "Marcelo Machado" ~ "Marcelo Rocha Machado",
        TRUE ~ solicitante
      )
    )
  message("Corrigindo solicitante 'Marcelo Machado' -> 'Marcelo Rocha Machado'")

  if ("validador" %in% colnames(df)) {
    df <- df %>%
      mutate(
        validador = case_when(
          validador == "-" | is.na(validador) | str_trim(validador) == "" ~ "Paulo",
          TRUE ~ validador
        )
      )
    message("Corrigindo validador '-' -> 'Paulo'")
  } else {
    message("Coluna 'validador' não encontrada. Pulando correção.")
  }

  if ("enviado" %in% colnames(df)) {
    df <- df %>%
      mutate(
        executor = case_when(
          (executor == "-" | is.na(executor) | str_trim(executor) == "") & tipo_os == "O.S" & enviado == "MV" ~ "MV",
          TRUE ~ executor
        )
      )
    message("Corrigindo executor e enviado MV")
  } else {
    message("Coluna 'enviado' não encontrada. Pulando correção de executor/enviado MV.")
  }

  df <- df %>%
    mutate(
      executor = case_when(
        (executor == "-" | is.na(executor) | str_trim(executor) == "") & tipo_os == "CAD AD" ~ "Hugo",
        TRUE ~ executor
      )
    )
  message("Corrigindo executor 'CAD AD' -> 'Hugo'")

  df <- df %>%
    mutate(
      executor = case_when(
        str_to_lower(executor) == "machado" ~ "Marcelo",
        TRUE ~ executor
      ),
      concluinte = case_when(
        str_to_lower(concluinte) == "machado" ~ "Marcelo",
        TRUE ~ concluinte
      )
    )
  message("Corrigindo executor/concluinte 'Machado' -> 'Marcelo'")

  df <- df %>%
    mutate(
      executor = case_when(
        executor == "-, Hugo, Wagner" ~ "Hugo",
        TRUE ~ executor
      )
    )
  message("Corrigindo executor '-, Hugo, Wagner' -> 'Hugo'")

  df <- df %>%
    mutate(
      executor = case_when(
        executor == "-" | is.na(executor) | str_trim(executor) == "" ~ "Indefinido",
        TRUE ~ executor
      )
    )
  message("Corrigindo executor '-' -> 'Indefinido'")

  if ("statusx" %in% colnames(df)) {
    df <- df %>%
      mutate(
        executor = case_when(
          statusx %in% c(1, 6, 9) ~ "Pendente",
          TRUE ~ executor
        ),
        concluinte = case_when( 
          statusx %in% c(1, 6, 9) ~ "Pendente",
          TRUE ~ concluinte
        )
      )
    message("Corrigindo executor/concluinte para 'Pendente' se statusx for 1, 6 ou 9")
    df <- df %>%
      mutate(
        executor = case_when(
          statusx == 2 ~ "Aguardando Tecnico",
          TRUE ~ executor
        ),
        concluinte = case_when( 
          statusx == 2 ~ "Aguardando Tecnico",
          TRUE ~ concluinte
        )
      )
    message("Corrigindo executor/concluinte para 'Aguardando Tecnico' se statusx for 2")

  } else {
    message("Coluna 'statusx' não encontrada. Pulando correção de statusx.")
  }

  df <- df %>%
    mutate(
      concluinte = case_when(
        concluinte == "-" | is.na(concluinte) | str_trim(concluinte) == "" ~ executor,
        TRUE ~ concluinte
      )
    )
  message("Corrigindo concluinte vazio com valor do executor")

  df <- df %>%
    mutate(
      executor = case_when(
        executor == "Raphael" ~ "Ferista",
        TRUE ~ executor
      ),
      concluinte = case_when(
        concluinte == "Raphael" ~ "Ferista",
        TRUE ~ concluinte
      )
    )
  message("Corrigindo executor/concluinte 'Raphael' -> 'Ferista'")


  message("Corrigindo datas (Python: data_ini, data_fim)...")
  df <- df %>%
    mutate(
      data_ini = as.character(data_ini),
      data_fim = as.character(data_fim),
      data_cad = as.character(data_cad), 
      data_ini = ifelse(data_ini == "-" | is.na(data_ini) | str_trim(data_ini) == "", data_cad, data_ini),
      data_fim = ifelse(data_fim == "-" | is.na(data_fim) | str_trim(data_fim) == "", data_cad, data_fim)
    )

  message("Datas corrigidas.")

  return(df)
}
corrigir_setor_R <- function(df_path_in, df_path_out, dicionario_setores) {
  message("Aplicando correção de setor (Python: corrigir_setor)...")

  df <- NULL
  retries <- 5
  for (i in 1:retries) {
    tryCatch({
      df <- read_excel(df_path_in)
      break
    }, error = function(e) {
      message(paste("Tentativa", i, "de leitura de", df_path_in, "falhou:", e$message, ". Aguardando..."))
      Sys.sleep(4)
      if (i == retries) stop(paste("Falha ao ler o Excel após", retries, "tentativas para correção de setor."))
    })
  }

  if (is.null(df)) stop("DataFrame nulo após tentativas de leitura para correção de setor.")

  if (!"nome_setor_corrigido" %in% colnames(df)) {
    message("ATENÇÃO: Coluna 'nome_setor_corrigido' não encontrada para correção de setor. Pulando a correção.")
    
    if ("setor" %in% colnames(df)) {
      df$setor__corrigido <- df$setor
    } else {
      df$setor__corrigido <- NA_character_
    }

    write.xlsx(df, df_path_out, rowNames = FALSE, overwrite = TRUE)
    return(df)
  }

  df <- df %>%
    mutate(setor__corrigido = nome_setor_corrigido)

  mapeamento_setores <- tibble(
    setor_final = names(dicionario_setores),
    palavras_chave = dicionario_setores
  ) %>%
    rowwise() %>%
    mutate(len_max_palavra = max(nchar(unlist(palavras_chave)))) %>%
    ungroup() %>%
    arrange(desc(len_max_palavra))
  
  for (i in 1:nrow(mapeamento_setores)) {
    setor <- mapeamento_setores$setor_final[i]
    palavras <- unlist(mapeamento_setores$palavras_chave[i])
    padrao <- paste0("^(?:", paste(gsub("([[:punct:]])", "\\\\\\1", palavras), collapse = "|"), ")$")
    
    df <- df %>%
      mutate(
        setor__corrigido = case_when(
          str_detect(nome_setor_corrigido, regex(padrao, ignore_case = TRUE)) ~ setor,
          TRUE ~ setor__corrigido
        )
      )
  }

  write.xlsx(df, df_path_out, rowNames = FALSE, overwrite = TRUE)
  message(paste("Arquivo de setor corrigido salvo em:", df_path_out))
  return(df)
}

chamados_abertos_em_sequencia_R <- function(df) {
  message("Ajustando coluna 'hora_dos_chamados_sequenciais' (Python: chamados_abertos_em_sequencia)...")

  if (!"hora_ini" %in% colnames(df)) {
    message("Coluna 'hora_ini' não encontrada. Pulando ajuste de 'hora_dos_chamados_sequenciais'.")
    df$hora_dos_chamados_sequenciais <- NA_character_
    return(df)
  }

  df <- df %>%
    mutate(
      hora_ini = ifelse(hora_ini == "-" | is.na(hora_ini) | str_trim(hora_ini) == "", NA_character_, as.character(hora_ini)),
      hora_ini_parsed = hms(hora_ini, quiet = TRUE),
      
      hora_dos_chamados_sequenciais = NA_character_
    )

  time_5_am <- hms("05:00:00")
  time_7_am <- hms("07:00:00")
  
  df <- df %>%
    mutate(
      hora_dos_chamados_sequenciais = case_when(
        !is.na(hora_ini_parsed) &
          hora_ini_parsed >= time_5_am &
          hora_ini_parsed <= time_7_am ~ format(hora_ini_parsed, "%H:%M:%S"),
        TRUE ~ "Não se aplica"
      )
    )
  
  df <- df %>% select(-hora_ini_parsed)

  message("Coluna 'hora_dos_chamados_sequenciais' ajustada!")
  return(df)
}

atualizar_campos_final_R <- function(df) {
  message("Aplicando ajustes finais (Python: atualizar_campos)...")

  if (!"solicitante" %in% colnames(df)) {
    message("Coluna 'solicitante' não encontrada. Pulando ajuste final.")
    return(df)
  }

  df <- df %>%
    mutate(
      tipo_os = case_when(
        solicitante == "Marcelo da Silva Firmino Junior" ~ "Indefinido",
        TRUE ~ tipo_os
      ),
      executor = case_when(
        solicitante == "Marcelo da Silva Firmino Junior" ~ "Indefinido",
        TRUE ~ executor
      ),
      concluinte = case_when(
        solicitante == "Marcelo da Silva Firmino Junior" ~ "Indefinido",
        TRUE ~ concluinte
      )
    )
  message("Ajustes finais aplicados para 'Marcelo da Silva Firmino Junior'.")
  return(df)
}

dicionario_setores <- list(
  # Administrativo
  "Administrativo" = c("Recepção Administrativo"),
  
  # Ambulatório
  "Ambulatório" = c("Ambulatório - Sala 01", "Ambulatório - Sala 02", "Ambulatório - Sala 03", "Ambulatório - Sala 04", "Ambulatório - Sala 05", "Ambulatório - Sala 06", "Ambulatório - Sala 07", "Ambulatório - Sala 08", "Ambulatório - Sala 09", "Ambulatório - Sala 10", "Ambulatório - Sala 11", "Ambulatório - Sala 12", "Ambulatório - ECG 1", "Ambulatório - ECG 2", "Ambulatório - ECG 3", "Ambulatório Recepção"),
  
  # Bloco Cirúrgico
  "Bloco Cirúrgico" = c("Bloco Cirúrgico - Gerência de Enfermagem", "Bloco Cirúrgico - Gerência Médica", "Bloco Cirúrgico - Sala 01", "Bloco Cirúrgico - Sala 02", "Bloco Cirúrgico - Sala 03", "Bloco Cirúrgico - Sala 04"),
  
  # Casa de Chagas
  "Casa de Chagas" = c("Casa de Chagas - Associação", "Casa de Chagas - Arquivo", "Casa de Chagas - Gerência Médica", "Casa de Chagas - Gerência de Enfermagem", "Casa de Chagas - ECG 1", "Casa de Chagas - ECG 2", "Casa de Chagas - Sala 1", "Casa de Chagas - Sala 2", "Casa de Chagas - Sala 3", "Casa de Chagas - Recepção"),
  
  # Coordenação de Enfermagem
  "Coordenação de Enfermagem" = c("Gerência de Enfermagem", "Recepção Coordenação de Enfermagem", "Pacientes Externo"),
  
  # Centro de Estudos
  "Centro de Estudos" = c("Centro de Estudo - Sala de Aula 01", "Coordenação de Pesquisa"),
  
  # Direção
  "Direção" = c("Diretor Médico", "Diretória"),
  
  # Emergência
  "Emergência" = c("Emergência - Gerência de Enfermagem", "Emergência - Gerência", "Emergência - ECG", "Emergência - Posto 01", "Emergência - Posto 02", "Emergência - Evolução", "Emergência - Posto de Coleta Laboratórial", "Emergência - Recepção", "Emergência - Sala Amarela", "Emergência - Sala Vermelha", "Emergência - Sala de Prontuário"),
  
  # Farmácia
  "Farmácia" = c("Farmácia", "Farmácia do Ambulatório", "Farmácia DI", "Farmácia do Bloco", "Farmácia CAF", "Farmácia Emergência", "Farmácia da Hemodinânica", "Farmácia de Suprimentos", "Gerência da Farmácia"),
  
  # Ergometria
  "Ergometria" = c("Ergometria - Gerência", "Ergometria - Laudos", "Ergometria - Esteiras"),
  
  # Evolução
  "Evolução" = c("Evolução - Sala Vermelha", "Evolução - Sala Amarela"),
  
  # Enfermarias
  "Enfermaria" = c("Enfermaria 4º Andar", "Enfermaria 5º Andar", "Enfermaria 6º Andar", "Enfermaria 7º Andar"),
  
  # Hemodinâmica
  "Hemodinâmica" = c("Hemodinâmica - Digitação", "Hemodinâmica - Gerência", "Hemodinâmica - Laudos", "Hemodinâmica - Recepção", "Hemodinâmica - Sala A", "Hemodinâmica - Sala B", "Hemodinâmica - Sala C", "Hemodinâmica - Sala Reunião"),
  
  # UCO
  "Coronária" = c("Unidade Coronária", "Coronária I", "Coronária II"),
  "Laboratório de Análises Clínicas" = c("Laboratório de Análises Clínicas"),
  
  # RECEPÇÃO E ADMISSÃO
  "Recepção e Admissão" = c("Recepção", "Admissão", "Alta"),
  
  # Fisioterapia
  "Fisioterapia" = c("Fisioterapia e Terapia Ocupacional"),
  
  # Laboratório
  "Laboratório" = c("Laboratório - Almoxerifado", "Laboratório - Recepção"),
  
  # Laboratório PROLAB
  "Laboratório PROLAB" = c("Laboratório Triagem", "Coleta de Sangue"),
  
  # Manutenção
  "Manutenção" = c("Gerência da Manutenção", "Manutenção Subsolo"),
  
  # Nutrição
  "Nutrição" = c("Gerência da Nutrição", "Nutrição", "Nutrição - Evolução", "Nutrição - Cozinha"),
  
  # Pesquisa Clinica
  "Pesquisa Clínica" = c("Pesquisa Clínica - Consultório"),
  
  # Raio X
  "Raio-X" = c("Raio-X - Administração", "Raio-X - Recepção", "Raio-X - Laudo", "Raio-X - Exame"),
  
  # Zeladoria e RM
  "Zeladoria" = c("Gerência da Zeladoria", "Zeladoria (RM)"),
  
  # Tomografia
  "Tomografia" = c("Tomografia - Sala Exame", "Tomografia - Laudo", "Tomografia - Enfermagem")
)

processar_dados_completamente <- function(path_in_setor, path_in_tratados, path_out_final, dicionario_setores) {
  df_intermediario <- corrigir_setor_R(arquivo_xlsx_entrada_setor, path_in_tratados, dicionario_setores)
  df_intermediario <- read_excel(path_in_tratados)
  df_intermediario <- chamados_abertos_em_sequencia_R(df_intermediario)
  write.xlsx(df_intermediario, path_in_tratados, rowNames = FALSE, overwrite = TRUE)

  df_intermediario <- read_excel(path_in_tratados) 
  df_intermediario <- dados_nao_preenchidos_ou_faltantes_R(df_intermediario)
  write.xlsx(df_intermediario, path_in_tratados, rowNames = FALSE, overwrite = TRUE)
 
  df_intermediario <- read_excel(path_in_tratados)
  df_intermediario <- atualizar_campos_final_R(df_intermediario)
  write.xlsx(df_intermediario, path_in_tratados, rowNames = FALSE, overwrite = TRUE)
  message(paste("Arquivo atualizado com ajustes: ", path_in_tratados))
  message(paste("Salvando arquivo final:", path_out_final))
  write.xlsx(df_intermediario, path_out_final, rowNames = FALSE, overwrite = TRUE)
  message("Processamento completo finalizado!")
}

tryCatch({
  processar_dados_completamente(
    arquivo_xlsx_entrada_setor, 
    arquivo_xlsx_entrada,      
    arquivo_xlsx_saida,      
    dicionario_setores
  )
}, error = function(e) {
  message(paste("Erro no script 3_correcao_dados.R:", e$message))
  stop(e)
})