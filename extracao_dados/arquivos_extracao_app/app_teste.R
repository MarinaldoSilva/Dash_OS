library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

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

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span("Painel de Análises", style = "color: white;"),
    tags$li(class = "dropdown", style = "background-color: #007bff;") 
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
      menuItem("Gráfico por OS", tabName = "grafico_os_tab", icon = icon("chart-bar"))
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
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("grafico_os", height = "400px")
                ),
                column(width = 4,
                       box(
                         title = "Totais Gerais",
                         status = "danger",
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("total_geral_os")
                       ),
                       box(
                         title = "Totais Filtrados",
                         status = "danger",
                         solidHeader = TRUE,
                         width = 12,
                         verbatimTextOutput("total_filtrado_os")
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  dados_base <- reactive({
    if (is.null(df)) {
      message("DataFrame 'df' é NULL. Verifique o carregamento dos dados.")
      return(data.frame())
    }
    return(df)
  })

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
          geom_bar(stat = "identity", fill = "#0e1a69", width = 0.2) +
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
            plot.title = element_text(color = "#007bff", hjust = 0.5, size = 16, face = "bold"),
            panel.grid.major = element_line(color = "#E0E0E0"),
            panel.grid.minor = element_line(color = "#F0F0F0")
          )
      )
    }

    return(NULL)
  })
}

shinyApp(ui, server, options = list(host = "192.168.150.34", port = 8788, launch.browser = FALSE))