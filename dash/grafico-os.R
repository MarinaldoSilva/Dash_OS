library(shiny)
library(ggplot2)
library(readxl)
library(RCurl)

base_dir_linux <- "/tratar_dados"
base_dir_windows <- "C:/tratar_dados"
FTP_HOST <- "127.0.0.1"
FTP_USER <- "tesla"
FTP_PASS <- "tesla"
FTP_FILE <- "dados-tratados.xlsx"

CAMINHO_ARQUIVO <- if (file.exists(file.path(base_dir_linux, FTP_FILE))) {
  file.path(base_dir_linux, FTP_FILE)
} else {
  file.path(base_dir_windows, FTP_FILE)
}

carregar_dados_local <- function() {
  read_excel(CAMINHO_ARQUIVO)
}

carregar_dados_ftp <- function() {
  tryCatch({
    ftp_url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, "/", FTP_FILE)
    temp_file <- tempfile(fileext = ".xlsx")
    bin_data <- getBinaryURL(ftp_url, ssl.verifypeer = FALSE)
    writeBin(bin_data, temp_file)
    read_excel(temp_file)
  }, error = function(e) {
    message("Erro ao carregar dados do FTP: ", e)
    carregar_dados_local()
  })
}

df <- carregar_dados_ftp()

navbar <- page_navbar(
  #title = tagList(icon(tags$i(class = "bi bi-home")), "Menu"),
  title = "Ordem de serviço por usuário - ",
  navbar_options = navbar_options(bg = "#2D89C8", theme = "dark"),
  nav_panel(tagList(tags$i(class = "bi bi-hurricane"), " Home"), p("")),
  #nav_panel("casa", p("")),
  #nav_panel("principal", p("")),
  
  #nav_menu(title = "Links úteis", align = "right",nav_item(tags$a("Posit", href = "https://posit.co")),nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))),
  #nav_menu(title = "Link copilot", align = "right",nav_item(tags$a("copilot", href = "https://copilot.microsoft.com/chats/RQTKmxb7Pu7w1DKn1i3Zk")),
    
    #nav_item(tags$a("Guia layout shiny",href = "https://shiny.posit.co/r/articles/build/layout-guide/")),
    
    #nav_item(tags$a("Curso udemy",href = "https://www.udemy.com/course/curso-aprenda-shiny-na-linguagem-r-paineis-para-analise-de-dados/learn/lecture/19484284#questions"))
  )
  #os cods estão comentados devido a quebra no sistema após add os layouts, Romulo vê dps como foi feito e faz a correção junto a paulo

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        body { font-family: Arial, sans-serif; margin: 0; padding: 0; }
        .panel { padding: 15px; border-radius: 8px; background-color: #f8f9fa; }
    "))
  ),
  
  tags$head(
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css")),
  navbar,
  
  titlePanel(tagList(tags$i(class = "bi bi-file-bar-graph-fill"), " Grafico por OS")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("executor", "Selecione um Executor:", choices = unique(na.omit(df$executor)))
    ),
    mainPanel(
      textOutput("total_os"),
      plotOutput("grafico_os")
    )
  )
)

server <- function(input, output, session) {
  
  output$total_os <- renderText({
    if (is.null(input$executor) || input$executor == "") return("Selecione um Executor para ver o total de OS")
    
    total <- nrow(df[df$executor == input$executor, ])
    paste("Total de OS para", input$executor, ":", total)
  })
  
  output$grafico_os <- renderPlot({
    if (is.null(input$executor) || input$executor == "") return(NULL)
    
    df_filtrado <- df[df$executor == input$executor, ]
    df_agrupado <- as.data.frame(table(df_filtrado$tipo_os))
    
    ggplot(df_agrupado, aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "#2D89C8") +
      labs(title = paste("Distribuição de OS -", input$executor),
           x = "Tipo de OS", y = "Quantidade") +
      theme_minimal()
  })
}

shinyApp(ui, server, options = list(port=3779, launch.browser = TRUE))
