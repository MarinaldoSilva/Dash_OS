library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(DT) # Para renderizar tabelas mais interativas

# 🗂️ Definição de diretórios e arquivos
base_dir_linux <- "/mnt/googledrive"
CAMINHO_ARQUIVO_EXCEL <- file.path(base_dir_linux, "dados-tratados.xlsx")

carregar_dados_local <- function() {
  tryCatch({
    if (!file.exists(CAMINHO_ARQUIVO_EXCEL)) {
      message("Erro: arquivo não encontrado: ", CAMINHO_ARQUIVO_EXCEL)
      return(NULL)
    }
    
    df <- read_excel(CAMINHO_ARQUIVO_EXCEL, col_names = TRUE)
    
    df <- df %>%
      mutate(
        data_ini_parsed = dmy(data_ini, quiet = TRUE),
        dia = day(data_ini_parsed),
        mes = month(data_ini_parsed),
        ano = year(data_ini_parsed)
      ) %>%
      select(-data_ini_parsed)
    
    return(df)
  }, error = function(e) {
    message("Erro ao carregar dados localmente: ", e$message)
    return(NULL)
  })
}

df <- carregar_dados_local()

# ---
## Interface do Usuário (UI) com shinydashboard
# ---
ui <- dashboardPage(
  skin = "blue", # Mantém a skin 'blue' como base
  
  dashboardHeader(
    title = ("Painel de Análises"),
    tags$li(class = "dropdown", style = "background-color: #007bff;") # Azul principal para o cabeçalho
  ),
  
  dashboardSidebar(
    tags$head(tags$style(HTML("
      .main-sidebar {
        background-color:rgb(1, 64, 133) !important; /* Azul principal para a sidebar */
      }
      .sidebar-menu > li > a {
        color: white !important; /* Texto do menu em branco */
      }
      .sidebar-menu > li.active > a {
        border-left-color: #0056b3 !important; /* Azul mais escuro para item ativo */
        background-color: #0056b3 !important; /* Azul mais escuro para item ativo */
      }
      .sidebar-menu > li:hover > a {
        background-color: #0056b3 !important; /* Azul mais escuro ao passar o mouse */
        color: white !important;
      }
      .sidebar-menu h4 {
        color: white !important;
      }
      /* Ajuste dos inputs de seleção na sidebar para manter a harmonização */
      .selectize-input {
        border: 1px solid #0056b3 !important; /* Borda dos selects em azul escuro */
        box-shadow: none !important;
      }
      .selectize-dropdown {
        border: 1px solid #0056b3 !important;
      }
    "))),
    sidebarMenu(
      menuItem("Gráfico por OS", tabName = "grafico_os_tab", icon = icon("chart-bar")),
      menuItem("Comparativo por Categoria", tabName = "comparativo_categoria_tab", icon = icon("chart-line")) # Nova aba
    ),
    h4("Filtros", style = "color: white; margin-left: 15px;"),
    selectInput("ano", "Selecione um Ano:",
                choices = c("Todos", if (!is.null(df) && "ano" %in% colnames(df)) sort(unique(na.omit(df$ano))))),
    selectInput("mes", "Selecione um Mês:",
                choices = c("Todos", if (!is.null(df) && "mes" %in% colnames(df)) sort(unique(na.omit(df$mes))))),
    selectInput("executor", "Selecione um Executor:",
                choices = c("Todos", if (!is.null(df) && "executor" %in% colnames(df)) sort(unique(na.omit(df$executor))))),
    selectInput("categoria", "Selecione uma Categoria:",
                choices = c("Todas", "computador", "telefone", "impressora", "rede", "sistema", "telefonia", "sei"))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Cor de fundo do corpo do dashboard (main content) */
        .content-wrapper, .right-side {
          background-color: #F8F9FA; /* Branco */
        }

        /* Estilo da caixa de informações (boxes) para o gráfico */
        .box.box-solid.box-primary > .box-header {
          background-color: #035fc2 !important; /* Azul principal para o cabeçalho do box do gráfico */
          color: white; /* Texto do cabeçalho em branco */
        }
        .box.box-solid.box-primary {
          border-color: #035fc2 !important;
        }

        /* Estilo da caixa de informações (boxes) para Totais Gerais e Totais Filtrados */
        .box.box-solid.box-danger > .box-header { /* Usamos 'danger' para aproveitar o tema vermelho do shinydashboard */
            background-color: #8B0000 !important; /* Vermelho escuro para cabeçalhos de totais */
            color: white; /* Texto do cabeçalho em branco */
        }
        .box.box-solid.box-danger {
            border-color: #8B0000 !important;
        }

        /* Estilo da caixa de informações (boxes) para o novo gráfico (azul mais escuro) */
        .box.box-solid.box-info > .box-header {
            background-color: #0e1a69 !important; /* Azul bem escuro para o novo box */
            color: white;
        }
        .box.box-solid.box-info {
            border-color: #0e1a69 !important;
        }

        /* Ajuste do texto dentro das caixas de saída (verbatimTextOutput) */
        .shiny-text-output {
            white-space: pre-wrap;
            background-color: white;
            padding: 15px;
            border-radius: 8px;
            border: 1px solid #DDDDDD;
            margin-top: 10px;
            font-family: 'Courier New', monospace;
            color: #333;
        }

        /* Estilo para as tabelas DT */
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #007bff !important;
          color: white !important;
          border-color: #007bff !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #0056b3 !important;
          color: white !important;
          border-color: #0056b3 !important;
        }


        /* Cor do título do dashboard no header */
        .main-header .logo {
          background-color:rgb(1, 67, 138) !important; /* Azul mais escuro para o logo do cabeçalho */
          color: white !important;
        }
        .main-header .navbar {
          background-color: #035fc2 !important; /* Azul principal para a barra de navegação superior */
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "grafico_os_tab",
              fluidRow(
                box(
                  title = "Distribuição de OS por Tipo",
                  status = "primary", # Este box usará o tema azul
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("grafico_os", height = "400px")
                ),
                column(width = 4,
                       box(
                         title = "Totais Gerais",
                         status = "danger", # Este box usará o tema vermelho escuro
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("total_geral_os")
                       ),
                       box(
                         title = "Totais Filtrados",
                         status = "danger", # Este box usará o tema vermelho escuro
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("total_filtrado_os")
                       )
                )
              )
      ),
      # Nova Aba: Comparativo por Categoria
      tabItem(tabName = "comparativo_categoria_tab",
              fluidRow(
                box(
                  title = "Atendimentos por Categoria (Mensal)",
                  status = "info", # Novo status para o box do novo gráfico
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("grafico_comparativo_categoria", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Detalhe por Executor (Mês Selecionado)",
                  status = "info", # Novo status para o box da tabela
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("tabela_executores_categoria") # Usando DTOutput para tabela interativa
                )
              )
      )
    )
  )
)

# ---
## Servidor (Server)
# ---
server <- function(input, output, session) {
  
  dados_base <- reactive({
    if (is.null(df)) {
      message("DataFrame 'df' é NULL. Verifique o carregamento dos dados.")
      return(data.frame())
    }
    return(df)
  })
  
  # Dados filtrados para a aba "Gráfico por OS" (mantido como estava)
  df_filtrado <- reactive({
    df_subset <- dados_base()
    
    if (nrow(df_subset) == 0) {
      return(df_subset)
    }
    
    if (input$ano != "Todos") {
      df_subset <- df_subset %>% filter(ano == as.integer(input$ano))
    }
    if (input$mes != "Todos") {
      df_subset <- df_subset %>% filter(mes == as.integer(input$mes))
    }
    if (input$executor != "Todos") {
      df_subset <- df_subset %>% filter(executor == input$executor)
    }
    if (input$categoria != "Todas") {
      if (input$categoria %in% colnames(df_subset)) {
        df_subset <- df_subset %>% filter(!!sym(input$categoria) == 1)
      } else {
        message(paste0("A coluna da categoria '", input$categoria, "' não foi encontrada."))
        df_subset <- df_subset[FALSE,]
      }
    }
    
    return(df_subset)
  })
  
  # NOVO: Dados filtrados para a aba "Comparativo por Categoria"
  df_comparativo_categoria <- reactive({
    df_subset <- dados_base()
    
    if (nrow(df_subset) == 0) {
      return(df_subset)
    }
    
    # O filtro de ano é obrigatório para este gráfico
    if (input$ano == "Todos") {
      return(data.frame()) # Retorna vazio se nenhum ano específico for selecionado para este gráfico
    } else {
      df_subset <- df_subset %>% filter(ano == as.integer(input$ano))
    }
    
    # O filtro de categoria é obrigatório para este gráfico
    if (input$categoria == "Todas" || !(input$categoria %in% colnames(df_subset))) {
      return(data.frame()) # Retorna vazio se nenhuma categoria específica ou categoria inválida for selecionada
    } else {
      df_subset <- df_subset %>% filter(!!sym(input$categoria) == 1)
    }
    
    return(df_subset)
  })
  
  # NOVO: Dados para o detalhe dos executores (baseado nos mesmos filtros, mas incluindo o mês)
  df_executores_mes <- reactive({
    df_subset <- df_comparativo_categoria()
    
    if (nrow(df_subset) == 0) {
      return(df_subset)
    }
    
    if (input$mes != "Todos") {
      df_subset <- df_subset %>% filter(mes == as.integer(input$mes))
    }
    
    return(df_subset)
  })
  
  
  # --- Outputs da aba "Gráfico por OS" (inalterados) ---
  output$total_geral_os <- renderText({
    dados <- dados_base()
    if (nrow(dados) == 0) return("Dados não carregados ou vazios.")
    
    total_geral <- dados %>%
      count(tipo_os) %>%
      rename(Tipo_OS = tipo_os, Total = n)
    
    total_str <- paste0("Total Geral de OS: ", sum(total_geral$Total), "\n\n")
    
    detalhes <- paste(total_geral$Tipo_OS, ":", total_geral$Total, collapse = "\n")
    
    return(paste0(total_str, detalhes))
  })
  
  output$total_filtrado_os <- renderText({
    df_subset <- df_filtrado()
    
    if (nrow(df_subset) == 0) return("Nenhum atendimento encontrado para os filtros selecionados.")
    
    total_filtrado <- df_subset %>%
      count(tipo_os) %>%
      rename(Tipo_OS = tipo_os, Total = n)
    
    total_str <- paste0("Total Filtrado de OS: ", sum(total_filtrado$Total), "\n\n")
    
    detalhes <- paste(total_filtrado$Tipo_OS, ":", total_filtrado$Total, collapse = "\n")
    
    return(paste0(total_str, detalhes))
  })
  
  output$grafico_os <- renderPlot({
    df_subset <- df_filtrado()
    
    if (nrow(df_subset) > 0) {
      df_agrupado <- df_subset %>%
        count(tipo_os) %>%
        rename(Tipo_OS = tipo_os, Quantidade = n)
      return(
        ggplot(df_agrupado, aes(x = Tipo_OS, y = Quantidade)) +
          geom_bar(stat = "identity", fill = "#0e1a69", width = 0.2) + # Azul escuro para as barras do gráfico
          labs(
            title = "Distribuição de OS por Tipo",
            x = "Tipo de OS",
            y = "Quantidade"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
            axis.text.y = element_text(color = "#333333"),
            axis.title.x = element_text(color = "#333333"),
            axis.title.y = element_text(color = "#333333"),
            plot.title = element_text(color = "#007bff", hjust = 0.5, size = 16, face = "bold"), # Título do gráfico em azul
            panel.grid.major = element_line(color = "#E0E0E0"),
            panel.grid.minor = element_line(color = "#F0F0F0")
          )
      )
    }
    return(NULL) # Retorna NULL se não houver dados para plotar
  })
  
  # --- NOVO: Outputs da aba "Comparativo por Categoria" ---
  
  output$grafico_comparativo_categoria <- renderPlot({
    df_plot <- df_comparativo_categoria()
    
    if (nrow(df_plot) == 0) {
      return(ggplot() +
               geom_text(aes(x = 0.5, y = 0.5, label = "Selecione um Ano e uma Categoria para visualizar o gráfico."),
                         size = 6, color = "gray50") +
               theme_void())
    }
    
    # Agrupar por mês para o gráfico
    df_agrupado_mes <- df_plot %>%
      count(mes) %>%
      rename(Mes = mes, Quantidade = n) %>%
      mutate(Mes_Nome = factor(Mes, levels = 1:12, labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")))
    
    ggplot(df_agrupado_mes, aes(x = Mes_Nome, y = Quantidade, group = 1)) +
      geom_line(color = "#035fc2", size = 1.5) + # Azul principal para a linha
      geom_point(color = "#0e1a69", size = 3) + # Azul escuro para os pontos
      labs(
        title = paste0("Atendimentos de '", input$categoria, "' por Mês em ", input$ano),
        x = "Mês",
        y = "Quantidade de Atendimentos"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
        axis.text.y = element_text(color = "#333333"),
        axis.title.x = element_text(color = "#333333"),
        axis.title.y = element_text(color = "#333333"),
        plot.title = element_text(color = "#0e1a69", hjust = 0.5, size = 16, face = "bold"), # Título do gráfico em azul escuro
        panel.grid.major = element_line(color = "#E0E0E0"),
        panel.grid.minor = element_line(color = "#F0F0F0")
      )
  })
  
  output$tabela_executores_categoria <- renderDT({
    df_exec <- df_executores_mes()
    
    if (nrow(df_exec) == 0) {
      # Retorna uma tabela vazia ou com mensagem se não houver dados
      return(datatable(data.frame(Mensagem = "Selecione um Ano e uma Categoria (e opcionalmente um Mês) para ver os detalhes dos executores."),
                       options = list(dom = 't', paging = FALSE, searching = FALSE)))
    }
    
    # Agrupar por executor e tipo_os (categoria já está filtrada)
    df_exec_agrupado <- df_exec %>%
      count(executor, tipo_os) %>%
      rename(Executor = executor, Tipo_OS = tipo_os, Atendimentos = n) %>%
      arrange(Executor, desc(Atendimentos))
    
    # Formatar o nome do mês se um mês específico for selecionado
    titulo_mes <- if (input$mes != "Todos") {
      paste0(" (Mês: ", month.name[as.integer(input$mes)], ")")
    } else {
      ""
    }
    
    datatable(df_exec_agrupado,
              options = list(pageLength = 10,  # Número de linhas por página
                             lengthMenu = c(5, 10, 15, 20), # Opções de número de linhas
                             dom = 'lfrtip'), # Layout padrão com filtros e paginação
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color: #0e1a69; font-size: 16px; font-weight: bold;',
                paste0("Detalhamento de Atendimentos por Executor para a Categoria '", input$categoria, "' em ", input$ano, titulo_mes)
              )
    )
  })
}

# 🚀 Rodar o aplicativo
shinyApp(ui, server, options = list(host = "192.168.150.34", port = 8788, launch.browser = FALSE))