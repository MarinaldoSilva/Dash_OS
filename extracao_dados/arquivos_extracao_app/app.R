library(shiny)
library(shinydashboard) 
library(dplyr) 
library(ggplot2) 
library(readxl) 
library(lubridate) 
library(stringr)
library(tidyr)

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

DEFAULT_ANO <- "Todos"
DEFAULT_CATEGORIA <- "Todas"

carregar_e_processar_dados <- function() {
  log_message(paste0("Tentando carregar arquivo: ", DATA_PATH))
  if (file.exists(DATA_PATH)) {
    tryCatch({
      df <- read_excel(DATA_PATH, sheet = 1)

      log_message(paste0("Arquivo '", DATA_PATH, "' carregado com sucesso. Dimensões: ", nrow(df), "x", ncol(df)))

      if ("data_cad" %in% names(df)) { 
        df$data_cad_parsed <- dmy(df$data_cad, quiet = TRUE)
        df$dia <- day(df$data_cad_parsed)
        df$mes <- month(df$data_cad_parsed)
        df$ano <- year(df$data_cad_parsed)
        
        df <- df %>% select(-data_cad_parsed)

        log_message(paste0("Colunas 'dia', 'mes', 'ano' adicionadas a partir de 'data_cad'. Exemplos de anos: ", paste(sort(unique(df$ano)), collapse = ", ")))
        if(any(is.na(df$ano))) {
            log_message(paste0("AVISO: Existem ", sum(is.na(df$ano)), " valores NA na coluna 'ano' após o processamento de data_cad."))
        }
      } else {
        log_message("ATENÇÃO: Coluna 'data_cad' não encontrada. Não foi possível adicionar 'dia', 'mes', 'ano'. Verifique o arquivo XLSX.")
        df$dia <- NA
        df$mes <- NA
        df$ano <- NA_integer_
      }

      for (col in CATEGORIAS_OS) {
        if (col %in% names(df)) {
          df[[col]] <- ifelse(is.na(df[[col]]) | str_trim(as.character(df[[col]])) == "", 0, 1)
        } else {
          log_message(paste0("AVISO: Coluna de categoria '", col, "' não encontrada no dataframe. Criando-a com zeros."))
          df[[col]] <- 0
        }
      }

      df$total_os <- 1

      log_message(paste0("Colunas finais no dataframe após processamento: ", paste(names(df), collapse = ", ")))
      return(df)

    }, error = function(e) {
      log_message(paste0("ERRO FATAL: Falha ao carregar ou processar dados do arquivo Excel: ", e$message))
      return(NULL)
    })
  } else {
    log_message(paste0("ERRO FATAL: Arquivo de dados não encontrado no caminho: ", DATA_PATH))
    return(NULL)
  }
}
df_global <- carregar_e_processar_dados()
log_message(paste0("Carregamento global de df_global concluído. Dimensões: ", ifelse(is.null(df_global), "NULL", paste0(nrow(df_global), "x", ncol(df_global)))))

has_tipo_os_column <- !is.null(df_global) && "tipo_os" %in% names(df_global)
if (!has_tipo_os_column) {
    log_message("AVISO: A coluna 'tipo_os' não foi encontrada no arquivo Excel. O gráfico 'Atendimentos por Tipo de OS' pode não funcionar como esperado ou mostrará contagens de bandeiras de categoria.")
}

anos_disponiveis_ui <- if (!is.null(df_global) && "ano" %in% names(df_global)) {
  sort(unique(na.omit(df_global$ano)))
} else {
  numeric(0)
}

meses_disponiveis_ui_numeric <- if (!is.null(df_global) && "mes" %in% names(df_global)) {
  sort(unique(na.omit(df_global$mes)))
} else {
  numeric(0)
}
meses_choices_ui <- c("Todos", setNames(as.list(1:12), month.name[1:12]))

executores_disponiveis_ui <- if (!is.null(df_global) && "executor" %in% names(df_global)) {
  sort(unique(na.omit(as.character(df_global$executor))))
} else {
  character(0) 
}

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard OS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "visao_geral", icon = icon("dashboard")),
      menuItem("Comparativo por Categoria", tabName = "comparativo_categoria", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
          .content-wrapper, .right-side {
            background-color: #f2f2f2;
          }
          .box.box-solid.box-primary > .box-header {
            background-color: #3c8dbc;
            color: #fff;
          }
          .box.box-solid.box-success > .box-header {
            background-color: #00a65a;
            color: #fff;
          }
          .box.box-solid.box-info > .box-header {
            background-color: #00c0ef;
            color: #fff;
          }
          .small-box.bg-aqua { background-color: #00c0ef !important; color: #fff !important; }
          .small-box.bg-green { background-color: #00a65a !important; color: #fff !important; }
          .small-box.bg-yellow { background-color: #f39c12 !important; color: #fff !important; }
          .small-box.bg-red { background-color: #dd4b39 !important; color: #fff !important; }
          .info-box {
            box-shadow: 0 1px 1px rgba(0,0,0,.1);
            border-radius: 2px;
            margin-bottom: 15px;
            background-color: #fff;
            width: 100%;
            padding: 10px;
          }
          .info-box-icon {
            border-radius: 2%;
            display: block;
            float: left;
            height: 60px;
            width: 60px;
            text-align: center;
            font-size: 30px;
            line-height: 60px;
            background: rgba(0,0,0,.2);
          }
          .info-box-content {
            padding-left: 70px;
          }
          .info-box-number {
            display: block;
            font-weight: bold;
            font-size: 24px;
          }
          .info-box-text {
            text-transform: uppercase;
            display: block;
            font-size: 14px;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
        "))
    ),
    tabItems(

      tabItem(tabName = "visao_geral",
              h2("Visão Geral dos Atendimentos de OS"),
              fluidRow(
               
                box(title = "Filtros de Visão Geral", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3, selectInput("ano_filtro", "Ano:", choices = c("Todos", anos_disponiveis_ui), selected = DEFAULT_ANO)),
                      column(3, selectInput("mes_filtro", "Mês:", choices = meses_choices_ui)),
                      column(3, selectInput("categoria_filtro_geral", "Categoria:", choices = c("Todas", CATEGORIAS_OS))),
                      column(3, selectInput("executor_filtro", "Executor:", choices = c("Todos", executores_disponiveis_ui)))
                    )
                )
              ),
              fluidRow(
                infoBoxOutput("total_geral_os", width = 6),
                infoBoxOutput("total_filtrado_os", width = 6)
              ),
              fluidRow(
                box(title = "Atendimentos por Tipo de OS", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("grafico_os", height = "400px")
                )
              )
      ),

      tabItem(tabName = "comparativo_categoria",
              h2("Atendimentos por Categoria (Mensal)"),
              fluidRow(
                box(title = "Filtros por Categoria", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, selectInput("ano_comparativo", "Ano:", choices = c("Todos", anos_disponiveis_ui), selected = DEFAULT_ANO)),
                      column(4, selectInput("categoria_comparativo", "Categoria:", choices = CATEGORIAS_OS, selected = DEFAULT_CATEGORIA)),
                      column(4, selectInput("mes_comparativo", "Mês:", choices = meses_choices_ui, selected = "Todos"))
                    )
                )
              ),
              fluidRow(
                box(title = paste("Atendimentos de Categoria", "por Mês"), status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("grafico_comparativo_categoria", height = "400px")
                )
              ),
              fluidRow(
                box(title = paste("Executores Mais Ativos para Categoria"), status = "success", solidHeader = TRUE, width = 12,
                    tableOutput("tabela_executores_categoria")
                )
              )
      )
    ),
    fluidRow(
      column(12,
             hr(),
             div(style = "text-align: center; font-size: 0.8em; color: gray;",
                 textOutput("info_data_carga"),
                 textOutput("anos_disponiveis")
             )
      )
    ) 
  ) 
) 

server <- function(input, output, session) {
  dados_base <- reactive({
    if (is.null(df_global) || nrow(df_global) == 0) {
      log_message("dados_base reactive: df_global é NULL ou vazio. Retornando dataframe vazio.")
      return(data.frame())
    }
    log_message(paste0("dados_base reactive: Retornando df_global com ", nrow(df_global), " linhas."))
    return(df_global)
  })

  df_filtrado <- reactive({
    log_message("Reactive: df_filtrado acionado.")
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) {
      log_message("df_filtrado: dados_base é NULL ou vazio. Retornando dataframe vazio.")
      return(data.frame())
    }
    
    log_message(paste0("df_filtrado: Colunas disponíveis no dataframe antes do filtro: ", paste(names(df), collapse = ", ")))

    df_filtered <- df

    if (!is.null(input$ano_filtro) && input$ano_filtro != "Todos") {
      df_filtered <- df_filtered %>% filter(ano == as.integer(input$ano_filtro))
      log_message(paste0("df_filtrado: Após filtro por ano (", input$ano_filtro, "), linhas: ", nrow(df_filtered)))
    }

    if (!is.null(input$mes_filtro) && input$mes_filtro != "Todos") {
      df_filtered <- df_filtered %>% filter(mes == as.numeric(input$mes_filtro))
      log_message(paste0("df_filtrado: Após filtro por mês (", input$mes_filtro, "), linhas: ", nrow(df_filtered)))
    }

    if (!is.null(input$executor_filtro) && input$executor_filtro != "Todos") {
      df_filtered <- df_filtered %>% filter(executor == input$executor_filtro)
      log_message(paste0("df_filtrado: Após filtro por executor (", input$executor_filtro, "), linhas: ", nrow(df_filtered)))
    }

    if (!is.null(input$categoria_filtro_geral) && input$categoria_filtro_geral != "Todas") {
      if (input$categoria_filtro_geral %in% names(df_filtered)) {
        df_filtered <- df_filtered %>% filter(!!sym(input$categoria_filtro_geral) == 1)
        log_message(paste0("df_filtrado: Após filtro por categoria (", input$categoria_filtro_geral, "), linhas: ", nrow(df_filtered)))
      } else {
        log_message(paste0("df_filtrado: Aviso - Categoria '", input$categoria_filtro_geral, "' não encontrada no dataframe para filtro. Retornando dataframe vazio."))
        return(data.frame())
      }
    }

    log_message(paste0("df_filtrado: Retornando dataframe com ", nrow(df_filtered), " linhas."))
    return(df_filtered)
  })

  df_comparativo_categoria <- reactive({
    log_message("Reactive: df_comparativo_categoria acionado.")
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) {
      log_message("df_comparativo_categoria: dados_base é NULL ou vazio. Retornando dataframe vazio.")
      return(data.frame())
    }

    log_message(paste0("df_comparativo_categoria: Valores de input - Ano: ", input$ano_comparativo, ", Categoria: ", input$categoria_comparativo, ", Mês: ", input$mes_comparativo))

    df_filtered_by_year <- df
    if (!is.null(input$ano_comparativo) && input$ano_comparativo != "Todos") {
      df_filtered_by_year <- df_filtered_by_year %>% filter(ano == as.integer(input$ano_comparativo))
      log_message(paste0("df_comparativo_categoria: Após filtro por ano (", input$ano_comparativo, "), linhas: ", nrow(df_filtered_by_year)))
      if (nrow(df_filtered_by_year) == 0) {
          log_message("df_comparativo_categoria: Dataframe vazio após filtro por ano.")
          return(data.frame())
      }
    } else {
      log_message("df_comparativo_categoria: Ano 'Todos' selecionado. Não aplicando filtro de ano.")
    }

    if (!is.null(input$mes_comparativo) && input$mes_comparativo != "Todos") {
      df_filtered_by_year <- df_filtered_by_year %>% filter(mes == as.numeric(input$mes_comparativo))
      log_message(paste0("df_comparativo_categoria: Após filtro por mês (", input$mes_comparativo, "), linhas: ", nrow(df_filtered_by_year)))
      if (nrow(df_filtered_by_year) == 0) {
          log_message("df_comparativo_categoria: Dataframe vazio após filtro por mês.")
          return(data.frame())
      }
    } else {
      log_message("df_comparativo_categoria: Mês 'Todos' selecionado. Não aplicando filtro de mês.")
    }

    if (!is.null(input$categoria_comparativo) && input$categoria_comparativo %in% CATEGORIAS_OS) {
      df_filtered_by_category <- df_filtered_by_year %>% filter(!!sym(input$categoria_comparativo) == 1)
      log_message(paste0("df_comparativo_categoria: Após filtro por categoria (", input$categoria_comparativo, "), linhas: ", nrow(df_filtered_by_category)))
    } else {
      log_message(paste0("df_comparativo_categoria: Categoria '", input$categoria_comparativo, "' não é uma categoria válida ou nula. Retornando dataframe vazio."))
      return(data.frame())
    }

    if (nrow(df_filtered_by_category) > 0) {
      df_plot <- df_filtered_by_category %>%
        group_by(mes) %>%
        summarise(TotalOS = n(), .groups = 'drop') %>%
        right_join(data.frame(mes = 1:12), by = "mes") %>%
        replace_na(list(TotalOS = 0)) %>%
        mutate(mes_nome = factor(month(mes, label = TRUE, abbr = FALSE), levels = month.name))
      log_message(paste0("df_comparativo_categoria: Retornando dataframe com ", nrow(df_plot), " linhas para plotagem."))
      return(df_plot)
    } else {
      log_message("df_comparativo_categoria: Retornando dataframe com 0 linhas.")
      return(data.frame())
    }
  })

  df_executores_mes <- reactive({
    log_message("Reactive: df_executores_mes acionado.")
    df_subset <- dados_base()
    if (is.null(df_subset) || nrow(df_subset) == 0) {
      log_message("df_executores_mes: dados_base é NULL ou vazio. Retornando vazio.")
      return(data.frame())
    }

    if (!is.null(input$ano_comparativo) && input$ano_comparativo != "Todos") {
      df_subset <- df_subset %>% filter(ano == as.integer(input$ano_comparativo))
    }

    if (!is.null(input$mes_comparativo) && input$mes_comparativo != "Todos") {
      df_subset <- df_subset %>% filter(mes == as.numeric(input$mes_comparativo))
      log_message(paste0("df_executores_mes: Após filtro por mês (", input$mes_comparativo, "). Linhas restantes: ", nrow(df_subset)))
      if (nrow(df_subset) == 0) {
          log_message("df_executores_mes: Dataframe vazio após filtro por mês.")
          return(data.frame())
      }
    } else {
      log_message("df_executores_mes: Mês 'Todos' selecionado. Não aplicando filtro de mês para a tabela.")
    }

    if (!is.null(input$categoria_comparativo) && input$categoria_comparativo %in% CATEGORIAS_OS) {
      if (input$categoria_comparativo %in% names(df_subset)) {
        df_subset <- df_subset %>% filter(!!sym(input$categoria_comparativo) == 1)
      } else {
        log_message(paste0("df_executores_mes: Aviso - Categoria '", input$categoria_comparativo, "' não encontrada no dataframe para filtro de executor. Retornando vazio."))
        return(data.frame())
      }
    } else {
      log_message(paste0("df_executores_mes: Categoria '", input$categoria_comparativo, "' não é válida ou nula. Retornando dataframe vazio."))
      return(data.frame())
    }

    if (nrow(df_subset) == 0) {
      log_message("df_executores_mes: df_subset (após filtros) está vazio. Retornando vazio.")
      return(data.frame())
    }

    df_exec <- df_subset %>%
      group_by(executor, mes) %>%
      summarise(TotalOS = n(), .groups = 'drop') %>%
      arrange(mes, desc(TotalOS))

    log_message(paste0("df_executores_mes: Retornando dataframe de executores com ", nrow(df_exec), " linhas."))
    return(df_exec)
  })

  output$total_geral_os <- renderInfoBox({
    log_message("Output: total_geral_os acionado.")
    df <- dados_base()
    total_geral <- if (is.null(df)) 0 else nrow(df)
    log_message(paste0("total_geral_os: Calculado total geral. ", total_geral))
    infoBox(
      "Total Geral de OS",
      value = format(total_geral, big.mark = ".", decimal.mark = ","),
      icon = icon("list"),
      color = "aqua",
      fill = TRUE
    )
  })

  output$total_filtrado_os <- renderInfoBox({
    log_message("Output: total_filtrado_os acionado.")
    df <- df_filtrado()
    total_filtrado <- if (is.null(df) || nrow(df) == 0) 0 else nrow(df)
    log_message(paste0("total_filtrado_os: Calculado total filtrado. ", total_filtrado))
    infoBox(
      "Total Filtrado de OS",
      value = format(total_filtrado, big.mark = ".", decimal.mark = ","),
      icon = icon("filter"),
      color = "green",
      fill = TRUE
    )
  })

  output$grafico_os <- renderPlot({
    log_message("Output: grafico_os acionado.")
    df <- df_filtrado()

    if (is.null(df) || nrow(df) == 0) {
      log_message("grafico_os: Sem dados para plotar. Exibindo mensagem.")
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir para os filtros selecionados.",
                        size = 5, color = "gray50") +
               theme_void())
    }

    if (has_tipo_os_column && "tipo_os" %in% names(df)) {
      log_message("grafico_os: Usando 'tipo_os' para agregação do gráfico.")
      df_plot_data <- df %>%
        count(tipo_os) %>%
        rename(Categoria = tipo_os, TotalOS = n) %>%
        arrange(desc(TotalOS))
      plot_title_base <- "Total de Atendimentos por Tipo de OS"
    } else {
      log_message("grafico_os: Coluna 'tipo_os' não encontrada ou não disponível. Recorrendo à contagem de ocorrências de categorias de flag.")
      df_plot_data <- df %>%
        select(all_of(CATEGORIAS_OS)) %>%
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Categoria", values_to = "TotalOS") %>%
        mutate(Categoria = factor(Categoria, levels = CATEGORIAS_OS)) %>%
        arrange(desc(TotalOS))
      plot_title_base <- "Total de Ocorrências de Categorias de OS"
      plot_title_base <- paste(plot_title_base, "(Uma OS pode ter múltiplas categorias)")
    }


    df_plot_data <- df_plot_data %>% filter(TotalOS > 0)

    if (nrow(df_plot_data) == 0) {
        log_message("grafico_os: Nenhuma categoria com dados para plotar após sumarização. Exibindo mensagem.")
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "Nenhuma categoria com atendimentos para exibir.",
                          size = 5, color = "gray50") +
                 theme_void())
    }

    log_message(paste0("grafico_os: Gerando gr├ífico de barras com ", nrow(df_plot_data), " categorias."))

    ggplot(df_plot_data, aes(x = reorder(Categoria, -TotalOS), y = TotalOS, fill = Categoria)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = TotalOS), vjust = -0.5, size = 4) +
      labs(title = paste(plot_title_base,
                          if (!is.null(input$ano_filtro) && input$ano_filtro != "Todos") paste("em", input$ano_filtro) else "",
                          if (!is.null(input$mes_filtro) && input$mes_filtro != "Todos") paste("(", month.name[as.numeric(input$mes_filtro)], ")") else ""),
           x = "Categoria de OS",
           y = "Número de Atendimentos",
           fill = "Categoria de OS") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            legend.position = "none")
  })

  output$grafico_comparativo_categoria <- renderPlot({
    log_message("Output: grafico_comparativo_categoria acionado.")
    df_plot <- df_comparativo_categoria()

    if (is.null(df_plot) || nrow(df_plot) == 0 || sum(df_plot$TotalOS) == 0) {
      log_message("grafico_comparativo_categoria: df_plot vazio ou sem dados. Exibindo mensagem padrão.")
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Não existem valores no dataframe para os filtros informados.",
                        size = 5, color = "gray50") +
               theme_void())
    }

    log_message(paste0("grafico_comparativo_categoria: Gerando gráfico mensal para categoria '", input$categoria_comparativo, "' no ano ", input$ano_comparativo, ". Dados: ", paste(df_plot$TotalOS, collapse = ", ")))

    chart_title <- paste("Atendimentos de", input$categoria_comparativo,
                         "em", input$ano_comparativo)
    if (!is.null(input$mes_comparativo) && input$mes_comparativo != "Todos") {
      chart_title <- paste(chart_title, "(Mês:", month.name[as.numeric(input$mes_comparativo)], ")")
    }


    ggplot(df_plot, aes(x = mes_nome, y = TotalOS, group = 1)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = TotalOS), vjust = -0.8, hjust = 0.5, size = 4) +
      labs(title = chart_title,
           x = "Mês",
           y = "Número de Atendimentos") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$tabela_executores_categoria <- renderTable({
    log_message("Output: tabela_executores_categoria acionado.")
    df_exec <- df_executores_mes()

    if (is.null(df_exec) || nrow(df_exec) == 0) {
      log_message("tabela_executores_categoria: df_exec vazio. Exibindo mensagem padrão com colunas consistentes.")
      no_data_df <- data.frame(
        Mês = "N/A",
        Executor = "Nenhum dado",
        `Total OS` = 0,
        check.names = FALSE
      )
      return(no_data_df)
    }

    df_exec_display <- df_exec %>%
      mutate(Mês = month(mes, label = TRUE, abbr = FALSE),
             Executor = executor,
             `Total OS` = TotalOS) %>%
      select(Mês, Executor, `Total OS`)

    log_message(paste0("tabela_executores_categoria: Retornando tabela de executores com ", nrow(df_exec_display), " linhas."))
    return(df_exec_display)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lcr")

  output$info_data_carga <- renderText({
    log_message("Output: info_data_carga acionado.")
    df <- dados_base()
    if (!is.null(df) && "data_cad" %in% names(df) && nrow(df) > 0 &&
        any(!is.na(df$data_cad))) {
      data_max <- max(df$data_cad, na.rm = TRUE)
      data_min <- min(df$data_cad, na.rm = TRUE)

      if (is.finite(as.numeric(data_min)) && is.finite(as.numeric(data_max))) {
        return(paste("Dados de OS carregados de", format(data_min, "%d/%m/%Y"), "a", format(data_max, "%d/%m/%Y")))
      } else {
        return("N/A: Datas de OS inválidas ou não encontradas nos dados.")
      }
    } else {
      return("N/A: Dados não carregados ou coluna 'data_cad' ausente/vazia.")
    }
  })

  output$anos_disponiveis <- renderText({
    log_message("Output: anos_disponiveis acionado.")
    df <- dados_base()
    if (!is.null(df) && "ano" %in% names(df) && nrow(df) > 0) {
      anos <- sort(unique(df$ano))
      anos <- anos[!is.na(anos)]
      if (length(anos) > 0) {
        return(paste("Anos disponíveis:", paste(anos, collapse = ", ")))
      } else {
        return("Anos disponíveis: Nenhum ano válido encontrado.")
      }
    } else {
      return("Anos disponíveis: N/A")
    }
  })

  observeEvent({
    input$ano_filtro
    input$mes_filtro
    input$categoria_filtro_geral
  }, {
    log_message("ObserveEvent: Atualizando select de executores (Visão Geral).")
    df <- df_filtrado()
    if (!is.null(df) && nrow(df) > 0) {
      executores <- sort(unique(df$executor))
      executores <- executores[!is.na(executores)]
      updateSelectInput(session, "executor_filtro", choices = c("Todos", executores), selected = "Todos")
    } else {
      updateSelectInput(session, "executor_filtro", choices = "Todos", selected = "Todos")
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$ano_filtro, { log_message(paste("Input ano_filtro mudou para:", input$ano_filtro)) })
  observeEvent(input$mes_filtro, { log_message(paste("Input mes_filtro mudou para:", input$mes_filtro)) })
  observeEvent(input$executor_filtro, { log_message(paste("Input executor_filtro mudou para:", input$executor_filtro)) })
  observeEvent(input$categoria_filtro_geral, { log_message(paste("Input categoria_filtro_geral mudou para:", input$categoria_filtro_geral)) })
  observeEvent(input$ano_comparativo, { log_message(paste("Input ano_comparativo mudou para:", input$ano_comparativo)) })
  observeEvent(input$categoria_comparativo, { log_message(paste("Input categoria_comparativo mudou para:", input$categoria_comparativo)) })
  observeEvent(input$mes_comparativo, { log_message(paste("Input mes_comparativo mudou para:", input$mes_comparativo)) }) # NOVO LOG

}

shinyApp(ui, server, options = list(host = "192.168.150.34", port = 8788, launch.browser = FALSE))