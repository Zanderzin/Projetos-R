#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinythemes)
library(shiny)
library(readxl)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$style(HTML("
    .btn-custom {
      background-color: #0090f1;
      border: none;
      color: white;
      padding: 10px 20px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      margin: 4px 2px;
      cursor: pointer;
      border-radius: 12px;
    }
    .btn-custom:hover {
      background-color: #0062a5;
      color: #fceb30;
    }
    .btn-custom:active {
      background-color: #0090f1;
      color: #fceb30;
    }
    .nav-tabs > li > a {
      background-color: #95a5a6; /* Cor de fundo dos botões */
      color: white; /* Cor do texto dos botões */
    }
    .nav-tabs > li > a:hover {
      background-color: #0062a5; /* Cor de fundo dos botões ao passar o mouse */
      color: #fceb30; /* Cor do texto dos botões ao passar o mouse */
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      background-color: #0090f1; /* Cor de fundo do botão ativo */
      color: #fceb30; /* Cor do texto do botão ativo */
    }
  ")),
  titlePanel("CTR"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Escolha a planilha Excel", accept = c(".xlsx")),
      uiOutput("selectUI"),
      numericInput("charLimit", "Limite de caracteres", value = 200, min = 1),
      actionButton("generate", "Gerar TXT", class = "btn-custom"),
      downloadButton("downloadData", "Baixar TXT", class = "btn-custom")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizar Dados", tableOutput("contents")),
        tabPanel("Montar TXT", tableOutput("txtContents")),
        tabPanel("Visualizar TXT", 
                 verbatimTextOutput("txtViewer"),
                 textOutput("charCount"),
                 actionButton("copy", "Copiar Texto", class = "btn-custom"),
                 tags$textarea(id = "clipboard", style = "position: absolute; left: -9999px;")
        ),
        tabPanel("Mesclar TXT", 
                 fileInput("file2", "Escolha o primeiro arquivo TXT", accept = c(".txt")),
                 fileInput("file3", "Escolha o segundo arquivo TXT", accept = c(".txt")),
                 actionButton("merge", "Mesclar TXT", class = "btn-custom"),
                 downloadButton("downloadMerged", "Baixar TXT Mesclado", class = "btn-custom"),
                 verbatimTextOutput("mergedViewer")
        )
      )
    )
  ),
  tags$script(HTML("
    document.getElementById('copy').addEventListener('click', function() {
      var text = document.getElementById('txtViewer').innerText;
      var clipboard = document.getElementById('clipboard');
      clipboard.value = text;
      clipboard.select();
      document.execCommand('copy');
      alert('Texto copiado para a área de transferência!');
    });
  "))
)

server <- function(input, output, session) {
  txtContent <- reactiveVal(NULL)  # Variável reativa para armazenar o conteúdo do TXT
  
  data <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath)
    names(df)[names(df) == "Valor total por NPJ"] <- "VALOR TOTAL DE HONORARIOS DEVIDOS POR NPJ(SEM RATEIO)"
    names(df)[names(df) == "Credor"] <- "ADVOGADO"
    names(df)[names(df) == "DMI de Rateio"] <- "DMI"
    df$CTR <- as.character(df$CTR)
    df <- df %>% relocate(CTR, .before = Motivo)
    df
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$selectUI <- renderUI({
    req(data())
    tagList(
      selectInput("columns", "Selecione as colunas", choices = names(data()), multiple = TRUE, 
                  selected = c("VALOR TOTAL DE HONORARIOS DEVIDOS POR NPJ(SEM RATEIO)", "ADVOGADO", "CNPJ/CPF", "DMI", "Motivo", "CTR")),
      selectInput("rows", "Selecione as linhas", choices = 1:nrow(data()), multiple = TRUE)
    )
  })
  
  selected_data <- reactive({
    req(input$rows, input$columns)
    df <- data()
    valid_rows <- as.numeric(input$rows[input$rows <= nrow(df)])
    valid_columns <- input$columns[input$columns %in% names(df)]
    df[valid_rows, valid_columns, drop = FALSE]
  })
  
  output$txtContents <- renderTable({
    req(selected_data())
    selected_data()
  })
  
  observeEvent(input$generate, {
    req(selected_data())
    df <- selected_data()
    npj_values <- data()[input$rows, "NPJ", drop = TRUE]
    num <- "000030181"
    today <- format(Sys.Date(), "%d.%m.%Y")
    
    txt_data <- lapply(1:nrow(df), function(i) {
      row <- df[i, ]
      npj <- as.character(npj_values[i])
      if (is.na(npj)) {
        showModal(modalDialog(
          title = "Erro",
          paste("NPJ não encontrado para a linha", i, ". Por favor, verifique os dados.")
        ))
        return(NULL)
      }
      header <- paste0(npj, num, today)
      row_text <- paste(paste(names(row), as.character(row), sep = ":"), collapse = " ")
      #tornando o total_length global
      total_length <<- nchar(row_text)  # Contar apenas o texto após o cabeçalho
      if (total_length > input$charLimit) {
        showModal(modalDialog(
          title = "Erro",
          paste("A linha", i, "excede o limite de", input$charLimit, "caracteres. Ajuste a seleção.")
        ))
        return(NULL)
      }
      # Preencher com espaços se o total_length for menor que o limite
      if (total_length < input$charLimit) {
        row_text <- paste0(row_text, strrep(" ", input$charLimit - total_length))
      }
      paste0(header, row_text, "0000085532")
    })
    
    if (any(sapply(txt_data, is.null))) {
      return()
    }
    
    txt_content <- paste(txt_data, collapse = "\n")
    writeLines(txt_content, "dados_selecionados.txt")
    txtContent(txt_content)  # Armazena o texto gerado
    showModal(modalDialog(
      title = "Sucesso",
      paste("O arquivo TXT foi gerado com sucesso! Ele contém", total_length, "caracteres (excluindo o cabeçalho).")
    ))
    output$txtViewer <- renderText({
      txtContent()
    })
    output$charCount <- renderText({
      paste("Número de caracteres no TXT (excluindo o cabeçalho):", total_length)
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "dados_selecionados.txt"
    },
    content = function(file) {
      txt_content <- readLines("dados_selecionados.txt")
      writeLines(txt_content, file)
    }
  )
  
  observeEvent(input$merge, {
    req(input$file2, input$file3)
    txt1 <- readLines(input$file2$datapath)
    txt2 <- readLines(input$file3$datapath)
    merged_txt <- c(txt1, txt2)
    writeLines(merged_txt, "merged.txt")
    showModal(modalDialog(
      title = "Sucesso",
      "Os arquivos TXT foram mesclados com sucesso!"
    ))
    output$mergedViewer <- renderText({
      paste(merged_txt, collapse = "\n")
    })
  })
  
  output$downloadMerged <- downloadHandler(
    filename = function() {
      "merged.txt"
    },
    content = function(file) {
      txt1 <- readLines(input$file2$datapath)
      txt2 <- readLines(input$file3$datapath)
      merged_txt <- c(txt1, txt2)
      writeLines(merged_txt, file)
    }
  )
}

shinyApp(ui = ui, server = server)
