# SIGA-SOJA Mobile — Plataforma do Produtor | Paraná
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(tidyverse)
library(sf)
library(scales)
library(googlesheets4)

options(OutDec = ",")

# Configura o googlesheets4 para acessar planilhas públicas sem pedir login/senha
gs4_deauth() 

# 1. DATASETS E CONFIGURAÇÕES GLOBAIS

# Carrega a base de dados oficial para extrair a lista completa de municípios do PR
dados_carregados <- readRDS("dados/dados_sinistro_final.rds")

if (!inherits(dados_carregados, "sf")) {
  if ("geometry" %in% names(dados_carregados) && is.character(dados_carregados$geometry)) {
    dados_sinistro_final <- st_as_sf(dados_carregados, wkt = "geometry", crs = 4674)
  } else {
    dados_sinistro_final <- st_as_sf(dados_carregados, sf_column_name = "geometry", crs = 4674)
  }
} else {
  dados_sinistro_final <- dados_carregados
}

dados_sinistro_final <- st_drop_geometry(dados_sinistro_final)
lista_municipios <- sort(unique(dados_sinistro_final$municipio_estacao))

# Equação de Del Ponte para Severidade
calc_sev <- function(chuva) pmin(pmax(-3.3983 + 0.3777 * chuva - 0.0003 * chuva^2, 0), 100)

# Sincronização com as Planilhas do Google Drive
link_planilha_defensivos <- "https://docs.google.com/spreadsheets/d/1Pcec8dp5NJ9Y8ReXetyDKA9U9ae6BqD0tOX6W7Zlg74/edit?usp=sharing"
df_defensivos <- read_sheet(link_planilha_defensivos)

link_planilha_eficiencia <- "https://docs.google.com/spreadsheets/d/1mmK6xPqS_v5nQajqBEiauWJy8c4hk_P64ULzh1qKXmw/edit?usp=sharing"
df_eficiencia_godoy <- read_sheet(link_planilha_eficiencia)


# 2. INTERFACE DO USUÁRIO (UI)
ui <- page_fillable(
  tags$head(tags$style(HTML("
    .bslib-value-box { padding: 8px 15px !important; min-height: 80px !important; }
    .bslib-value-box .value-box-title { font-size: 12px !important; margin-bottom: 2px !important; color: #475569 !important; }
    .bslib-value-box .value-box-value { font-size: 20px !important; font-weight: bold !important; line-height: 1.1 !important; }
    .nav-pills .nav-link { white-space: nowrap !important; }
  "))),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty", 
    primary = "#10B981", 
    success = "#22C55E",
    warning = "#F59E0B",
    danger = "#EF4444",
    base_font = font_google("Inter"),
    "navbar-bg" = "#0F172A"
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "desktop", bg = "#0F172A", fg = "#E2E8F0",
      tags$div(style = "padding:18px 0; border-bottom:1px solid #1E293B; margin-bottom:20px;",
               tags$h2(style = "margin:0; font-size:18px; font-weight:700; color:#10B981;", "SIGA-SOJA"),
               tags$p(style = "margin:4px 0 0; font-size:11px; color:#94A3B8;", "Plataforma de Monitoramento do Produtor")),
      
      navset_pill_list(
        id = "abas", 
        nav_panel("A Doença"),
        nav_panel("Alerta Ferrugem"),
        nav_panel("Projeção de Risco"),
        nav_panel("Vazio Sanitário"),
        nav_panel("Defensivos"),
        nav_panel("Eficiência Fungicidas"),
        nav_panel("Contato da Seguradora")
      ),
      hr(style = "border-color:#1E293B; margin:20px 0 10px;")
    ),
    
    card(style = "border: none; background: transparent; overflow-y: auto;", class = "p-3",
         
         # --- ABA 1: A DOENÇA ---
         conditionalPanel(condition = "input.abas == 'A Doença'",
                          fluidPage(
                            card(
                              card_header(class = "bg-success text-white", "Ferrugem Asiática da Soja"),
                              card_body(
                                p(strong("A Ferrugem Asiática"), " é o principal desafio fitossanitário da soja. O fungo causa desfolha precoce drástica, impedindo o enchimento correto das vagens e derrubando a produtividade em níveis severos."),
                                tags$h5("Sintomas no Campo:", class = "text-success mt-2"),
                                tags$ul(
                                  tags$li("Pequenas pontuações de coloração escura/marrom localizadas no baixeiro da planta."),
                                  tags$li("Presença visível de estruturas de esporulação (urédias) na face inferior da folha afetada.")
                                ),
                                hr(),
                                tags$h5("Galeria de Identificação Visual:", class = "text-muted"),
                                tags$div(
                                  class = "text-center",
                                  tags$img(src = "https://altadefensivos.com.br/wp-content/uploads/2025/05/Design-sem-nome-3.jpg", class = "img-fluid rounded mb-1", style = "max-height: 220px;"),
                                  br(),
                                  tags$small(class = "text-muted d-block mb-3", "Fonte: alta defensivos"),
                                  hr(),
                                  tags$img(src = "https://opresenterural.com.br/wp-content/uploads/2025/09/Sinais-quando-o-fungo-Phakopsora-pachyrhizi-ataca-a-planta-.jpg", class = "img-fluid rounded mb-1", style = "max-height: 220px;"),
                                  br(),
                                  tags$small(class = "text-muted d-block", "Fonte: o presente rural")
                                )
                              )
                            )
                          )
         ),
         
         # --- ABA 2: ALERTA FERRUGEM (FORMULÁRIO DE NOTIFICAÇÃO) ---
         conditionalPanel(condition = "input.abas == 'Alerta Ferrugem'",
                          fluidPage(
                            card(
                              card_header(class = "bg-danger text-white", icon("bell"), "Notificar Ocorrência de Ferrugem"),
                              card_body(
                                p("Preencha os dados abaixo para comunicar o foco da doença. Ao clicar em enviar, o aplicativo do seu e-mail será aberto para concluir o envio do relatório."),
                                hr(),
                                textInput("alerta_nome", "Nome do Produtor:", placeholder = "Digite seu nome completo"),
                                textInput("alerta_documento", "CPF ou Número do Seguro:", placeholder = "Digite o documento ou nº da apólice"),
                                selectizeInput("alerta_cidade", "Cidade da Ocorrência:", choices = lista_municipios, selected = "CASCAVEL", width = "100%"),
                                dateInput("alerta_data", "Data da Ocorrência:", value = Sys.Date(), format = "dd/mm/yyyy", language = "pt-BR"),
                                textAreaInput("alerta_obs", "Observação (Opcional):", placeholder = "Adicione detalhes sobre o talhão ou gravidade, se desejar...", rows = 3),
                                hr(),
                                # Botão dinâmico renderizado no Servidor que gera o link mailto atualizado
                                uiOutput("botao_enviar_email")
                              ),
                              class = "shadow-sm"
                            )
                          )
         ),
         
         # --- ABA 3: PROJEÇÃO DE RISCO ---
         conditionalPanel(condition = "input.abas == 'Projeção de Risco'",
                          layout_columns(col_widths = c(4, 8),
                                         card(class = "p-3 shadow-sm border-0", card_header(tags$b(icon("chart-line"), " Configuração do Cenário")),
                                              selectizeInput("proj_muni", "Município Alvo:", choices = lista_municipios, selected = "CASCAVEL"),
                                              numericInput("proj_area", "Área da Propriedade (Hectares):", value = 150, min = 1),
                                              numericInput("proj_preco", "Preço Estimado da Saca (R$):", value = 135, min = 1),
                                              selectInput("proj_cenario", "Cenário Climático Global:", 
                                                          choices = c("Normal" = "normal", "El Niño (Mais Úmido / Risco Alto)" = "elnino", "La Niña (Mais Seco / Risco Baixo)" = "lanina"))),
                                         card(class = "shadow-sm border-0", card_header(tags$b(icon("table"), " Comparativo de Riscos e Perdas Financeiras")),
                                              tableOutput("tabela_projecao"))
                          ),
                          layout_columns(col_widths = c(4, 4, 4), class = "mt-3",
                                         value_box(title = "Severidade Simulada", value = uiOutput("txt_proj_sev"), showcase = icon("biohazard"), theme = "danger", class = "border-0 shadow-sm"),
                                         value_box(title = "Perda por Hectare", value = uiOutput("txt_proj_sc_ha"), showcase = icon("wheat-awn"), theme = "warning", class = "border-0 shadow-sm"),
                                         value_box(title = "Prejuízo de Produção Total", value = uiOutput("txt_proj_reais_total"), showcase = icon("dollar-sign"), theme = "dark", class = "border-0 shadow-sm")
                          ),
                          card(class = "shadow-sm border-0 mt-3", card_header(tags$b(icon("chart-bar"), " Impacto Financeiro Total por Cenário (R$)")),
                               plotlyOutput("grafico_projecao", height = "350px"))
         ),
         
         # --- ABA 4: VAZIO SANITÁRIO ---
         conditionalPanel(condition = "input.abas == 'Vazio Sanitário'",
                          card(
                            card_header(class = "bg-dark text-white", "Zoneamento Obrigatório ADAPAR"),
                            card_body(
                              p("Consulte os períodos de pousio forçado para quebra do ciclo do patógeno:"),
                              selectizeInput("busca_cidade", "Selecione sua Cidade:", choices = lista_municipios, selected = "CASCAVEL", width = "100%"),
                              hr(),
                              uiOutput("painel_vazio_resultado")
                            )
                          )
         ),
         
         # --- ABA 5: DEFENSIVOS ---
         conditionalPanel(condition = "input.abas == 'Defensivos'",
                          card(
                            card_header(class = "bg-info text-white", "Consulta de Fungicidas Registrados no Ministério da Agricultura"),
                            card_body(
                              p(tags$small("Exibindo catálogo completo sincronizado em tempo real com a planilha do Google Drive:")),
                              DT::dataTableOutput("tabela_defensivos")
                            )
                          )
         ),
         
         # --- ABA 6: EFICIÊNCIA DE FUNGICIDAS ---
         conditionalPanel(condition = "input.abas == 'Eficiência Fungicidas'",
                          card(
                            card_header(class = "bg-secondary text-white", "Resultados de Controle - Ensaios Cooperativos (Embrapa)"),
                            card_body(
                              p(tags$small("Dados de eficácia média sincronizados diretamente com a planilha do Google Drive:")),
                              DT::dataTableOutput("tabela_eficiencia"),
                              hr(),
                              tags$div(
                                class = "p-2 bg-light rounded border",
                                tags$small(strong("Fonte bibliográfica: "), "GODOY, C. V., et al. (2024). Eficiência de fungicidas para o controle da ferrugem-asiática da soja na safra 2023/2024: resultados sumarizados dos ensaios cooperativos.")
                              )
                            )
                          )
         ),
         
         # --- ABA 7: CONTATO DA SEGURADORA ---
         conditionalPanel(condition = "input.abas == 'Contato da Seguradora'",
                          card(
                            card_header(class = "bg-dark text-white", "Canais de Atendimento Oficial"),
                            card_body(
                              tags$div(
                                class = "text-center mb-4",
                                tags$img(src = "https://lh3.googleusercontent.com/d/1hV4rJ28gAUsU1dBztLdT2cjU36wFMbSt", class = "img-fluid mb-2", style = "max-height: 200px;")
                              ),
                              hr(),
                              tags$div(
                                class = "p-3 bg-light rounded border mb-4",
                                p(icon("envelope"), strong(" E-mail Comercial: "), tags$a(href = "mailto:contato@sigasoja.com", "contato@sigasoja.com")),
                                p(icon("phone"), strong(" Telefone Fixo: "), tags$a(href = "tel:4533217070", "(45) 3321-7070")),
                                p(icon("whatsapp"), strong(" Suporte WhatsApp: "), tags$a(href = "https://wa.me/5545999999999", "(45) 99999-9999", target = "_blank"))
                              ),
                              layout_column_wrap(
                                width = 1/2,
                                tags$a(href = "https://wa.me/5545999999999", class = "btn btn-success fw-bold p-3 mb-2", icon("whatsapp"), " Chamar no WhatsApp", target = "_blank"),
                                tags$a(href = "tel:4533217070", class = "btn btn-primary fw-bold p-3 mb-2", icon("phone"), " Ligar no Fixo")
                              )
                            )
                          )
         )
    )
  )
)

# 3. SERVIDOR (LOGIC)
server <- function(input, output, session) {
  
  # LÓGICA DA ABA: ALERTA FERRUGEM (CONSTRUÇÃO DO LINK DE E-MAIL)
  output$botao_enviar_email <- renderUI({
    # Constrói o texto do corpo do e-mail codificado para URL
    destinatario <- "alicebarbutti@hotmail.com"
    assunto <- URLencode(paste0("Alerta de Ferrugem - ", input$alerta_cidade), reserved = TRUE)
    
    corpo_texto <- paste0(
      "Notificação de Ocorrência - Ferrugem Asiática\n\n",
      "Nome do Produtor: ", input$alerta_nome, "\n",
      "CPF / Seguro: ", input$alerta_documento, "\n",
      "Cidade: ", input$alerta_cidade, "\n",
      "Data da Ocorrência: ", format(input$alerta_data, "%d/%m/%Y"), "\n",
      "Observações: ", input$alerta_obs
    )
    corpo_codificado <- URLencode(corpo_texto, reserved = TRUE)
    
    # Cria a URL final mailto
    url_mailto <- paste0("mailto:", destinatario, "?subject=", assunto, "&body=", corpo_codificado)
    
    # Retorna o botão estilizado estruturado como link HTML
    tags$a(
      href = url_mailto,
      class = "btn btn-danger btn-lg w-100 fw-bold",
      icon("paper-plane"), " Enviar Notificação"
    )
  })
  
  # PROJEÇÃO DE RISCO - CÁLCULOS
  calculo_projecao_reativa <- reactive({
    req(input$proj_muni, input$proj_area, input$proj_preco, input$proj_cenario)
    m_data <- dados_sinistro_final[dados_sinistro_final$municipio_estacao == input$proj_muni, ]
    
    if(nrow(m_data) == 0) {
      return(list(sev = 0, perda_sc_ha = 0, perda_sc_total = 0, reais_total = 0))
    }
    m_data <- m_data[1, ]
    
    coluna_sev <- names(m_data)[grep("severidade", names(m_data), ignore.case = TRUE)]
    sev_normal <- if(length(coluna_sev) > 0) as.numeric(m_data[[coluna_sev[1]]]) else 25
    if(is.na(sev_normal)) sev_normal <- 25
    
    sev_simulada <- switch(input$proj_cenario,
                           "normal" = sev_normal,
                           "elnino" = pmin(100, sev_normal * 1.35),
                           "lanina" = pmax(0, sev_normal * 0.65))
    
    prod_base <- if("prod_atingivel_kg_ha" %in% names(m_data)) m_data$prod_atingivel_kg_ha else 3300
    if(is.na(prod_base) || prod_base <= 0) prod_base <- 3300
    
    perda_sc_ha <- pmin((prod_base * sev_simulada * 0.0055) / 60, prod_base / 60)
    perda_sc_total <- perda_sc_ha * input$proj_area
    reais_total <- perda_sc_total * input$proj_preco
    
    list(sev = sev_simulada, perda_sc_ha = perda_sc_ha, perda_sc_total = perda_sc_total, reais_total = reais_total)
  })
  
  output$txt_proj_sev <- renderUI({
    paste0(format(round(calculo_projecao_reativa()$sev, 1), decimal.mark = ","), "%")
  })
  output$txt_proj_sc_ha <- renderUI({
    paste0(format(round(calculo_projecao_reativa()$perda_sc_ha, 1), decimal.mark = ","), " sc/ha")
  })
  output$txt_proj_reais_total <- renderUI({
    paste0("R$ ", format(round(calculo_projecao_reativa()$reais_total, 2), big.mark = ".", decimal.mark = ","))
  })
  
  dados_projecao_tabela_comparativa <- reactive({
    req(input$proj_muni, input$proj_area, input$proj_preco)
    m_data <- dados_sinistro_final[dados_sinistro_final$municipio_estacao == input$proj_muni, ]
    
    if(nrow(m_data) == 0) {
      return(tibble(Cenário = c("Normal", "El Niño (Alto Risco)", "La Niña (Baixo Risco)"), `Severidade (%)` = 0, `Perda (sc/ha)` = 0, `Prejuízo Total (R$)` = 0))
    }
    m_data <- m_data[1, ]
    
    coluna_sev <- names(m_data)[grep("severidade", names(m_data), ignore.case = TRUE)]
    sev_normal <- if(length(coluna_sev) > 0) as.numeric(m_data[[coluna_sev[1]]]) else 25
    if(is.na(sev_normal)) sev_normal <- 25
    
    sev_elnino <- pmin(100, sev_normal * 1.35)
    sev_lanina <- pmax(0, sev_normal * 0.65)
    
    prod_base <- if("prod_atingivel_kg_ha" %in% names(m_data)) m_data$prod_atingivel_kg_ha else 3300
    if(is.na(prod_base) || prod_base <= 0) prod_base <- 3300
    
    calc_perdas_cenario <- function(sev) {
      sc_ha <- pmin((prod_base * sev * 0.0055) / 60, prod_base / 60)
      total_financeiro <- sc_ha * input$proj_area * input$proj_preco
      list(sc_ha = sc_ha, total = total_financeiro)
    }
    
    p_norm <- calc_perdas_cenario(sev_normal)
    p_el   <- calc_perdas_cenario(sev_elnino)
    p_la   <- calc_perdas_cenario(sev_lanina)
    
    tibble(
      Cenário = c("Normal", "El Niño (Alto Risco)", "La Niña (Baixo Risco)"),
      `Severidade (%)` = c(sev_normal, sev_elnino, sev_lanina),
      `Perda (sc/ha)` = c(p_norm$sc_ha, p_el$sc_ha, p_la$sc_ha),
      `Prejuízo Total (R$)` = c(p_norm$total, p_el$total, p_la$total)
    )
  })
  
  output$tabela_projecao <- renderTable({
    df <- dados_projecao_tabela_comparativa()
    df |> mutate(
      `Severidade (%)` = format(round(`Severidade (%)`, 1), decimal.mark = ","),
      `Perda (sc/ha)` = format(round(`Perda (sc/ha)`, 1), decimal.mark = ","),
      `Prejuízo Total (R$)` = paste0("R$ ", format(round(`Prejuízo Total (R$)`, 2), big.mark = ".", decimal.mark = ","))
    )
  }, digits = 2, align = 'l')
  
  output$grafico_projecao <- renderPlotly({
    df <- dados_projecao_tabela_comparativa()
    p <- ggplot(df, aes(x = Cenário, y = `Prejuízo Total (R$)`, fill = Cenário)) +
      geom_bar(stat = "identity", width = 0.4, show.legend = FALSE) +
      scale_fill_manual(values = c("Normal" = "#10B981", "El Niño (Alto Risco)" = "#EF4444", "La Niña (Baixo Risco)" = "#3B82F6")) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",", prefix = "R$ ")) +
      theme_minimal() + labs(x = NULL, y = NULL)
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  # VAZIO SANITÁRIO - LOCALIZAÇÃO E DATAS ADAPAR
  output$painel_vazio_resultado <- renderUI({
    req(input$busca_cidade)
    res <- dados_sinistro_final[dados_sinistro_final$municipio_estacao == input$busca_cidade, ]
    
    if(nrow(res) > 0) {
      res <- res[1, ]
      meso_nome <- if("name_meso" %in% names(res)) res$name_meso else "Não Identificada"
      
      v_inicio <- "10/06"
      v_fim    <- "10/09"
      s_inicio <- "11/09"
      s_fim    <- "20/12"
      
      if(grepl("Sul|Centro-Sul", meso_nome, ignore.case = TRUE)) {
        v_inicio <- "20/06"
        v_fim    <- "20/09"
        s_inicio <- "21/09"
        s_fim    <- "30/12"
      }
      
      div(class = "p-3 border rounded bg-white shadow-sm",
          p(strong("Mesorregião Mapeada: "), meso_nome),
          p(strong("🚫 Período do Vazio Sanitário: "), tags$span(class="text-danger fw-bold", paste(v_inicio, "a", v_fim))),
          p(strong("🚜 Janela de Semeadura Autorizada: "), tags$span(class="text-success fw-bold", paste(s_inicio, "a", s_fim)))
      )
    } else {
      p("Município não localizado na base regional.")
    }
  })
  
  # TABELAS DO GOOGLE DRIVE
  output$tabela_defensivos <- DT::renderDataTable({
    DT::datatable(
      df_defensivos,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'ftp',
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      ),
      rownames = FALSE
    )
  })
  
  output$tabela_eficiencia <- DT::renderDataTable({
    DT::datatable(
      df_eficiencia_godoy,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'ftp',
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)