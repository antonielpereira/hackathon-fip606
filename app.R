# SIGA-SOJA v6.6 — Plataforma de Risco de Ferrugem Asiática | Paraná 2020–2026
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(tidyverse)
library(sf)
library(scales)
library(stringi)
library(lubridate)
library(grid)

options(OutDec = ",")

# 1. DATASETS E MAPEAMENTO GLOBAL
calendario_esporos <- tibble(
  regiao_idr  = c("Oeste", "Sudoeste", "Norte", "Noroeste", "Centro-sul", "Centro", "Metropolitana"),
  data_inicio_esporo = as.Date(c("2024-11-06", "2024-11-14", "2024-11-22", "2024-12-02", "2024-12-05", "2024-12-17", "2024-12-29"))
)

# Equação de Del Ponte para Severidade
calc_sev <- function(chuva) pmin(pmax(-3.3983 + 0.3777 * chuva - 0.0003 * chuva^2, 0), 100)

# Diretrizes de Recomendação Técnico-Atuarial
recomendar <- function(categoria, data_inicio_esporo) {
  dt <- if (!is.na(data_inicio_esporo)) format(as.Date(data_inicio_esporo), "%d/%m/%Y") else "–"
  switch(as.character(categoria),
         "Risco Crítico" = paste0("🔴 ALTO RISCO (Vulnerabilidade >= 50%). Janela de esporos regional: ", dt, ". Alta correlação com quebra severa se não houver controle preventivo rigoroso."),
         "Risco Moderado" = paste0("🟠 Risco moderado (Vulnerabilidade entre 20% e 49,9%). Janela de esporos regional: ", dt, ". Exige atenção constante no monitoramento visual."),
         "Baixo Risco" = paste0("🟡 Baixo Risco (Vulnerabilidade entre 10% e 19,9%). Condições pouco favoráveis ao fungo na safra selecionada."),
         "Risco Desprezível" = paste0("🟢 Risco Desprezível (Vulnerabilidade < 10%). Condições climáticas hostis ao avanço do patógeno."),
         "Município sem Relevância Produtiva" = paste0("⚪ Área sem relevância produtiva mapeada para a cultura da soja comercial."),
         "Diretriz não catalogada para este cenário de risco.")
}

# Gerador de PDF para a Tarifação Dinâmica
gerar_pdf_tarifacao <- function(file, safra, muni, area, prod_seg, preco_sc, clima, midsoja, historico, taxa_base, ajuste_clima, bonus_tech, taxa_seguro, premio_total) {
  pdf(file, width = 8.5, height = 11)
  on.exit(dev.off())
  
  grid.newpage()
  grid.rect(gp = gpar(fill = "#F8FAFC", col = NA))
  grid.rect(y = 0.9, height = 0.2, gp = gpar(fill = "#0F172A", col = NA))
  
  grid.text("SIGA-SOJA", x = 0.08, y = 0.93, just = c("left", "center"),
            gp = gpar(col = "#10B981", fontsize = 24, fontface = "bold"))
  grid.text("Relatório de Tarifação Dinâmica e Apólice", x = 0.08, y = 0.88, just = c("left", "center"),
            gp = gpar(col = "#94A3B8", fontsize = 12))
  grid.text(paste("Gerado em:", format(Sys.time(), "%d/%m/%Y %H:%M:%S")), x = 0.92, y = 0.93, just = c("right", "center"),
            gp = gpar(col = "#64748B", fontsize = 9))
  
  grid.lines(x = c(0.08, 0.92), y = c(0.8, 0.8), gp = gpar(col = "#E2E8F0", lwd = 1.5))
  
  grid.text("1. Configurações da Apólice", x = 0.08, y = 0.77, just = c("left", "center"),
            gp = gpar(col = "#0F172A", fontsize = 14, fontface = "bold"))
  
  inputs <- list(
    c("Safra de Trabalho:", safra),
    c("Município:", muni),
    c("Área da Lavoura:", paste(format(area, big.mark=".", decimal.mark=","), "ha")),
    c("Produtividade Segurada:", paste(format(prod_seg, big.mark=".", decimal.mark=","), "sc/ha")),
    c("Preço da Saca:", paste("R$", format(preco_sc, big.mark=".", decimal.mark=","))),
    c("Macroclima Corrente (ENOS):", clima),
    c("Coletor MID-Soja:", if(midsoja) "Ativo (Sim)" else "Inativo (Não)"),
    c("Histórico de Risco Limpo:", if(historico) "Sim" else "Não")
  )
  
  y_pos <- 0.73
  for (i in seq_along(inputs)) {
    grid.text(inputs[[i]][1], x = 0.1, y = y_pos, just = c("left", "center"), gp = gpar(col = "#475569", fontsize = 11, fontface = "bold"))
    grid.text(inputs[[i]][2], x = 0.45, y = y_pos, just = c("left", "center"), gp = gpar(col = "#0F172A", fontsize = 11))
    y_pos <- y_pos - 0.03
  }
  
  y_pos <- y_pos - 0.02
  grid.lines(x = c(0.08, 0.92), y = c(y_pos+0.02, y_pos+0.02), gp = gpar(col = "#E2E8F0", lwd = 1.5))
  
  grid.text("2. Detalhamento e Justificativa Atuarial", x = 0.08, y = y_pos, just = c("left", "center"),
            gp = gpar(col = "#0F172A", fontsize = 14, fontface = "bold"))
  
  details <- list(
    c("Taxa Base Regional:", paste(format(round(taxa_base, 2), decimal.mark=","), "%")),
    c("Ajuste Climático (ENOS):", paste(format(round(ajuste_clima, 2), decimal.mark=","), "%")),
    c("Bônus Tecnológico:", bonus_tech)
  )
  
  y_pos <- y_pos - 0.04
  for (i in seq_along(details)) {
    grid.text(details[[i]][1], x = 0.1, y = y_pos, just = c("left", "center"), gp = gpar(col = "#475569", fontsize = 11, fontface = "bold"))
    grid.text(details[[i]][2], x = 0.45, y = y_pos, just = c("left", "center"), gp = gpar(col = "#0F172A", fontsize = 11))
    y_pos <- y_pos - 0.03
  }
  
  y_pos <- y_pos - 0.03
  grid.lines(x = c(0.08, 0.92), y = c(y_pos+0.02, y_pos+0.02), gp = gpar(col = "#E2E8F0", lwd = 1.5))
  
  grid.text("3. Resumo de Prêmios e Taxas", x = 0.08, y = y_pos, just = c("left", "center"),
            gp = gpar(col = "#0F172A", fontsize = 14, fontface = "bold"))
  
  y_pos <- y_pos - 0.14
  grid.rect(x = 0.5, y = y_pos + 0.05, width = 0.84, height = 0.12, gp = gpar(fill = "#ECFDF5", col = "#10B981", lwd = 1.5))
  
  grid.text("TAXA COMERCIAL DINÂMICA", x = 0.12, y = y_pos + 0.08, just = c("left", "center"),
            gp = gpar(col = "#065F46", fontsize = 11, fontface = "bold"))
  grid.text(taxa_seguro, x = 0.12, y = y_pos + 0.04, just = c("left", "center"),
            gp = gpar(col = "#047857", fontsize = 20, fontface = "bold"))
  
  grid.text("PRÊMIO TOTAL ESTIMADO", x = 0.52, y = y_pos + 0.08, just = c("left", "center"),
            gp = gpar(col = "#065F46", fontsize = 11, fontface = "bold"))
  grid.text(premio_total, x = 0.52, y = y_pos + 0.04, just = c("left", "center"),
            gp = gpar(col = "#047857", fontsize = 20, fontface = "bold"))
  
  grid.text("Nota: Este documento é uma simulação gerada de forma automática e não constitui uma proposta final de seguro.", 
            x = 0.5, y = 0.05, just = "center", gp = gpar(col = "#94A3B8", fontsize = 9, fontitalic = TRUE))
}

# --- RECONSTRUTOR E HIGIENIZADOR GEOESPACIAL ---
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

if (is.na(st_crs(dados_sinistro_final))) {
  dados_sinistro_final <- st_set_crs(dados_sinistro_final, 4674)
}
dados_sinistro_final <- st_transform(dados_sinistro_final, crs = 4326)
dados_sinistro_final <- dados_sinistro_final[!st_is_empty(dados_sinistro_final$geometry), ]
dados_sinistro_final <- st_make_valid(dados_sinistro_final)

colunas_disponiveis <- names(dados_sinistro_final)

coluna_sev_encontrada <- colunas_disponiveis[grep("severidade", colunas_disponiveis, ignore.case = TRUE)]
if (length(coluna_sev_encontrada) > 0) {
  dados_sinistro_final$severidade_calculada_pct <- as.numeric(dados_sinistro_final[[coluna_sev_encontrada[1]]])
} else {
  dados_sinistro_final$severidade_calculada_pct <- 0
}

if (!"vulnerabilidade" %in% names(dados_sinistro_final)) {
  dados_sinistro_final$vulnerabilidade <- dados_sinistro_final$severidade_calculada_pct * 1.12
}

coluna_safra_encontrada <- colunas_disponiveis[grep("safra", colunas_disponiveis, ignore.case = TRUE)]
if (length(coluna_safra_encontrada) > 0) {
  dados_sinistro_final$safra_string <- as.character(dados_sinistro_final[[coluna_safra_encontrada[1]]])
} else {
  dados_sinistro_final$safra_string <- "2024/2025"
}

if (!"categoria_sinistro" %in% names(dados_sinistro_final)) {
  dados_sinistro_final$categoria_sinistro <- "Risco Desprezível"
}

dados_sinistro_final <- dados_sinistro_final |> 
  mutate(
    safra_limpa = case_when(
      safra_string %in% c("2024", "2024_2025", "2024-2025", "2024/2025") ~ "2024/2025",
      safra_string %in% c("2023", "2023_2024", "2023-2024", "2023/2024") ~ "2023/2024",
      safra_string %in% c("2022", "2022_2023", "2022-2023", "2022/2023") ~ "2022/2023",
      safra_string %in% c("2021", "2021_2022", "2021-2022", "2021/2022") ~ "2021/2022",
      safra_string %in% c("2020", "2020_2021", "2020-2021", "2020/2021") ~ "2020/2021",
      TRUE ~ safra_string
    ),
    categoria_sinistro = case_when(
      categoria_sinistro == "Município sem Relevância Produtiva" ~ "Município sem Relevância Produtiva",
      vulnerabilidade < 10 ~ "Risco Desprezível",
      vulnerabilidade < 20 ~ "Baixo Risco",
      vulnerabilidade < 50 ~ "Risco Moderado",
      TRUE                 ~ "Risco Crítico"
    )
  )

if (!"perda_total_sacas" %in% names(dados_sinistro_final)) dados_sinistro_final$perda_total_sacas <- 0
if (!"perda_sacas_por_ha" %in% names(dados_sinistro_final)) dados_sinistro_final$perda_sacas_por_ha <- 0
if (!"prod_atingivel_kg_ha" %in% names(dados_sinistro_final)) dados_sinistro_final$prod_atingivel_kg_ha <- 3300
if (!"prod_perda_estimada_kgha" %in% names(dados_sinistro_final)) dados_sinistro_final$prod_perda_estimada_kgha <- 0
if (!"name_meso" %in% names(dados_sinistro_final)) dados_sinistro_final$name_meso <- "Não Mapeada"

lista_municipios = sort(unique(dados_sinistro_final$municipio_estacao))

CORES_RISCO <- c(
  "Risco Crítico"                      = "#C50000", 
  "Risco Moderado"                     = "#FF9F00", 
  "Baixo Risco"                        = "#F7D002", 
  "Risco Desprezível"                  = "#b5ecb1", 
  "Município sem Relevância Produtiva" = "#dcdde1"
)

# 2. INTERFACE GRÁFICA (UI)
ui <- page_fillable(
  tags$head(tags$style(HTML("
    .bslib-value-box { padding: 5px 12px !important; min-height: 70px !important; }
    .bslib-value-box .value-box-title { font-size: 11px !important; margin-bottom: 2px !important; color: #475569 !important; }
    .bslib-value-box .value-box-value { font-size: 18px !important; font-weight: bold !important; line-height: 1.1 !important; }
    .sub-value-text { font-size: 11px !important; color: #64748B; font-weight: normal; margin-top: 2px; display: block; }
    .nav-pills .nav-link { white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
    .bg-verde-app { background-color: #00bc71 !important; color: white !important; }
  "))),
  
  theme = bs_theme(version = 5, bg = "#F8FAFC", fg = "#0F172A", primary = "#10B981", success = "#22C55E", warning = "#F59E0B", danger = "#EF4444", base_font = font_google("Inter"), "navbar-bg" = "#0F172A"),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "desktop", bg = "#0F172A", fg = "#E2E8F0",
      tags$div(style = "padding:18px 0; border-bottom:1px solid #1E293B; margin-bottom:20px;",
               tags$h2(style = "margin:0; font-size:18px; font-weight:700; color:#10B981;", "SIGA-SOJA"),
               tags$p(style = "margin:4px 0 0; font-size:11px; color:#94A3B8;", "Auditor de Sinistros & Perdas")),
      
      tags$div(style = "margin-bottom: 15px; padding: 0 10px;",
               selectizeInput("seleciona_safra", "Safra de Trabalho:", 
                              choices = c("2024/2025", "2023/2024", "2022/2023", "2021/2022", "2020/2021"), 
                              selected = "2024/2025", width = "100%")),
      
      navset_pill_list(id = "abas", 
                       nav_panel("Painel de Exposição"), 
                       nav_panel("Inspeção de Risco"),
                       nav_panel("Projeção de Risco"),
                       nav_panel("Tarifação Dinâmica"),
                       nav_panel("Portfólio de Riscos")),
      
      hr(style = "border-color:#1E293B; margin:20px 0 10px;")
    ),
    
    card(style = "border: none; background: transparent; overflow-y: auto;", class = "p-3",
         
         # PAINEL 1: PAINEL DE EXPOSIÇÃO
         conditionalPanel(condition = "input.abas == 'Painel de Exposição'",
                          layout_columns(col_widths = c(4, 4, 4),
                                         value_box(title = "Municípios Filtrados", value = uiOutput("vb1_n_muni"), showcase = icon("map-location-dot"), theme = "primary", class = "border-0"),
                                         value_box(title = "Em Risco Crítico", value = uiOutput("vb1_critico"), showcase = icon("triangle-exclamation"), theme = "danger", class = "border-0"),
                                         value_box(title = "Sacas Totais em Perda", value = uiOutput("vb1_perda"), showcase = icon("wheat-awn"), theme = "warning", class = "border-0")
                          ),
                          layout_columns(col_widths = c(3, 9), 
                                         tags$div(
                                           card(card_header("Métrica do Mapa"), 
                                                radioButtons("tipo_mapa_panorama", NULL,
                                                             choices = c("Vulnerabilidade" = "classe", 
                                                                         "Perdas Potenciais (kg/ha)" = "perda", 
                                                                         "Severidade Estimada (%)" = "severidade"),
                                                             selected = "classe")),
                                           conditionalPanel(
                                             condition = "input.tipo_mapa_panorama == 'classe'",
                                             card(card_header("Filtrar Classes de Risco"), 
                                                  checkboxGroupInput("filtro_panorama", NULL, choices = names(CORES_RISCO), selected = names(CORES_RISCO)))
                                           )
                                         ),
                                         card(full_screen = TRUE, card_header("Mapa Dinâmico Interativo"), leafletOutput("map_panorama", height = "450px"))
                          )
         ),
         
         # PAINEL 2: INSPEÇÃO DE RISCO (RESTRUTURADO)
         conditionalPanel(condition = "input.abas == 'Inspeção de Risco'",
                          layout_columns(col_widths = c(4, 4, 4),
                                         value_box(title = "Perda Total (kg)", value = uiOutput("vb3_kg_total"), showcase = icon("weight-hanging"), theme = "warning", class = "border-0 shadow-sm"),
                                         value_box(title = "Volume Total de Quebra (sacas)", value = uiOutput("vb3_sc_total"), showcase = icon("boxes-stacked"), theme = "success", class = "border-0 shadow-sm"),
                                         value_box(title = "Prejuízo Estimado", value = uiOutput("vb3_reais"), showcase = icon("hand-holding-dollar"), theme = "danger", class = "border-0 shadow-sm")
                          ),
                          
                          layout_columns(col_widths = c(4, 4, 4),
                                         # Coluna 1: Localização
                                         card(card_header("Localização & Configuração"), style = "height: 440px !important;", class = "shadow-sm border-0",
                                              selectizeInput("prod_muni", "Município Analisado:", choices = lista_municipios, selected = "CASCAVEL", width = "100%"),
                                              numericInput("prod_area", "Área da Lavoura de Soja (ha):", value = 150, min = 1, width = "100%"),
                                              numericInput("prod_preco", "Preço da Saca de Soja (R$/sc):", value = 135, min = 1, width = "100%")),
                                         
                                         # Coluna 2: Dados Climáticos e Ocorrência (Separados conforme solicitado)
                                         card(card_header("Dados Climáticos e de Ocorrência"), style = "height: 440px !important;", class = "shadow-sm border-0",
                                              # Seção Ocorrência
                                              tags$b("Ocorrência de Ferrugem Asiática:", style = "font-size: 13px; color: #334155; display:block; margin-bottom:5px;"),
                                              radioButtons("origem_ocorrencia", NULL, 
                                                           choices = c("Utilizar padrão da região" = "banco", "Informar observação em campo" = "manual"), 
                                                           selected = "banco", inline = FALSE),
                                              conditionalPanel(condition = "input.origem_ocorrencia == 'manual'",
                                                               dateInput("data_manual", "Data da 1ª Ocorrência:", value = Sys.Date(), width = "100%")),
                                              
                                              hr(style = "margin: 10px 0; border-color: #CBD5E1;"),
                                              
                                              # Seção Chuva
                                              tags$b("Chuva Acumulada (mm):", style = "font-size: 13px; color: #334155; display:block;"),
                                              tags$span("30 dias após a primeira observação", style = "font-size: 11px; color: #64748B; display:block; margin-bottom:5px;"),
                                              radioButtons("origem_chuva", NULL, 
                                                           choices = c("Utilizar histórico municipal" = "banco", "Informar acumulado do campo" = "manual"), 
                                                           selected = "banco", inline = FALSE),
                                              conditionalPanel(condition = "input.origem_chuva == 'manual'",
                                                               numericInput("chuva_manual", "Chuva acumulada (mm):", value = 180, min = 0, width = "100%")),
                                              
                                              # Bloco informativo discreto do Banco
                                              conditionalPanel(condition = "input.origem_ocorrencia == 'banco' || input.origem_chuva == 'banco'",
                                                               uiOutput("dados_carregados_banco"))),
                                         
                                         # Coluna 3: Quadro de Risco promovido para o lugar dos Dados de Referência
                                         tags$div(style = "display: flex; flex-direction: column; gap: 10px; height: 440px;",
                                                  uiOutput("prod_alerta_box"),
                                                  # Dados de Referência remanejados para baixo com menor destaque
                                                  card(card_header("Dados de Referência Regional", style = "font-size: 12px; padding: 6px 10px;"), 
                                                       class = "shadow-sm border-0", style = "flex-grow: 1; min-height: 100px;",
                                                       uiOutput("dados_referencia_bloco"))
                                         )
                          )
         ),
         
         # PAINEL 3: PROJEÇÃO DE RISCO (INALTERADO)
         conditionalPanel(condition = "input.abas == 'Projeção de Risco'",
                          layout_columns(col_widths = c(4, 8),
                                         card(class = "p-3 shadow-sm border-0", card_header(tags$b(icon("chart-line"), " Configuração do Cenário")),
                                              selectizeInput("proj_muni", "Município Alvo:", choices = lista_municipios, selected = "CASCAVEL"),
                                              numericInput("proj_area", "Área da Propriedade (Hectares):", value = 150, min = 1),
                                              numericInput("proj_preco", "Preço Estimado da Saca (R$):", value = 135, min = 1)),
                                         card(class = "shadow-sm border-0", card_header(tags$b(icon("table"), " Comparativo de Perdas por Cenário Climático")),
                                              tableOutput("tabela_projecao"))
                          ),
                          card(class = "shadow-sm border-0 mt-3", card_header(tags$b(icon("chart-bar"), " Impacto Financeiro Total por Cenário (R$)")),
                               plotlyOutput("grafico_projecao", height = "350px"))
         ),
         
         # PAINEL 4: TARIFAÇÃO DINÂMICA (INALTERADO)
         conditionalPanel(condition = "input.abas == 'Tarifação Dinâmica'",
                          layout_columns(col_widths = c(5, 7),
                                         card(class = "p-2 shadow-sm border-0", style = "max-height: 480px;",
                                              card_header(class = "py-1", tags$b(icon("user-shield"), " Simulador de Apólice")),
                                              layout_columns(col_widths = c(7, 5),
                                                             selectizeInput("tar_municipio", "Município:", choices = lista_municipios, selected = "CASCAVEL", width = "100%"),
                                                             numericInput("tar_area", "Área (ha):", 200, min = 10, width = "100%")),
                                              layout_columns(col_widths = c(6, 6),
                                                             numericInput("tar_prod_esp", "Prod. Segurada (sc/ha):", 65, min = 10, width = "100%"),
                                                             numericInput("tar_preco_sc", "Preço (R$/sc):", 135, min = 50, width = "100%")),
                                              selectInput("tar_clima_global", strong("Macroclima Corrente (ENOS):"), 
                                                          choices = c("Neutralidade Climática" = "neutro", "El Niño (Risco Alto)" = "elnino", "La Niña (Risco Baixo)" = "lanina"), 
                                                          selected = "neutro", width = "100%"),
                                              layout_columns(col_widths = c(6, 6), style = "margin-top: 5px; margin-bottom: 5px;",
                                                             checkboxInput("tar_midsoja", label = span("Coletor MID-Soja", style = "font-size: 12px; font-weight: bold;"), value = FALSE),
                                                             checkboxInput("tar_historico", label = span("Histórico Limpo", style = "font-size: 12px; font-weight: bold;"), value = TRUE))
                                         ),
                                         tags$div(style = "display: flex; flex-direction: column; gap: 10px;",
                                                  layout_columns(col_widths = c(6, 6),
                                                                 value_box(title = "Taxa Comercial Dinâmica", value = uiOutput("txt_taxa_seguro"), showcase = icon("percent"), theme = "success"),
                                                                 value_box(title = "Prêmio Total Estimado", value = uiOutput("txt_premio_total"), showcase = icon("file-invoice-dollar"), theme = "primary")),
                                                  card(class = "shadow-sm border-0", style = "max-height: 250px;",
                                                       card_header(class = "py-1", tags$b(icon("receipt"), " Justificativa Atuarial")),
                                                       card_body(style = "line-height: 1.6; font-size: 13px; padding-top: 5px; padding-bottom: 5px;",
                                                                 tags$p(class = "mb-1", tags$b("Taxa Base Regional: "), uiOutput("txt_taxa_base", inline = TRUE)),
                                                                 tags$p(class = "mb-1", tags$b("Ajuste Climático (ENOS): "), uiOutput("txt_ajuste_clima", inline = TRUE)),
                                                                 tags$p(class = "mb-0", tags$b("Bônus Tecnológico: "), uiOutput("txt_bonus_tech", inline = TRUE)))),
                                                  layout_columns(col_widths = c(6, 6),
                                                                 downloadButton("exportar_tar_pdf", "Exportar PDF", class = "btn btn-danger btn-sm fw-bold w-100", icon = icon("file-pdf")),
                                                                 downloadButton("exportar_tar_excel", "Exportar Excel", class = "btn btn-success btn-sm fw-bold w-100", icon = icon("file-excel")))
                                         )
                          )
         ),
         
         # PAINEL 5: PORTFÓLIO DE RISCO (INALTERADO)
         conditionalPanel(condition = "input.abas == 'Portfólio de Riscos'",
                          card(class = "h-100", style = "display:flex; flex-direction:column; gap:12px; height: calc(100vh - 150px);",
                               card_header("Painel Geral de Monitoramento por Município"), DT::dataTableOutput("tabela_portfolio"))
         )
    )
  )
)

# 3. SERVIDOR (LOGIC)
server <- function(input, output, session) {
  
  dados_safra_selecionada <- reactive({
    req(input$seleciona_safra)
    dados_sinistro_final[dados_sinistro_final$safra_limpa == input$seleciona_safra, ]
  })
  
  dados_safra_tabela <- reactive({
    req(input$seleciona_safra)
    st_drop_geometry(dados_sinistro_final[dados_sinistro_final$safra_limpa == input$seleciona_safra, ])
  })
  
  dados_municipio_banco <- reactive({
    req(input$prod_muni)
    df_clean <- dados_safra_tabela()
    res <- df_clean[df_clean$municipio_estacao == input$prod_muni, ]
    if(nrow(res) == 0) return(NULL)
    res[1, ]
  })
  
  output$vb1_n_muni <- renderUI({
    df <- dados_safra_tabela()
    if (input$tipo_mapa_panorama == "classe" && !is.null(input$filtro_panorama)) {
      df <- df[df$categoria_sinistro %in% input$filtro_panorama, ]
    }
    format(nrow(df), big.mark = ".", decimal.mark = ",")
  })
  
  output$vb1_critico <- renderUI({
    df <- dados_safra_tabela()
    format(sum(df$categoria_sinistro == "Risco Crítico", na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  
  output$vb1_perda <- renderUI({
    df <- dados_safra_tabela()
    format(sum(df$perda_total_sacas, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  
  output$dados_carregados_banco <- renderUI({
    d <- dados_municipio_banco()
    if (is.null(d) || nrow(d) == 0 || is.na(d$municipio_estacao)) return(p("Nenhum registro no banco."))
    
    chuva_val <- if("chuva_total_mm" %in% names(d)) d$chuva_total_mm else 0
    dt_exibir <- if("data_inicio_esporo" %in% names(d) && !is.na(d$data_inicio_esporo)) format(as.Date(d$data_inicio_esporo), "%d/%m/%Y") else "Não Identificada"
    
    tagList(
      tags$div(
        style = "margin-top: 5px; padding: 5px 8px; background: #F1F5F9; border-radius: 4px; font-size: 11px;",
        p(style="margin:0;", tags$b("Banco -> "), paste0("Chuva: ", round(chuva_val), " mm | 1ª Ocorrência: ", dt_exibir))
      )
    )
  })
  
  output$dados_referencia_bloco <- renderUI({
    d <- dados_municipio_banco()
    if (is.null(d) || nrow(d) == 0 || is.na(d$municipio_estacao)) return(p("Sem dados regionais."))
    teto_texto <- if(d$categoria_sinistro == "Município sem Relevância Produtiva") "Sem Cadastro" else paste0(format(round(d$prod_atingivel_kg_ha), big.mark = ".", decimal.mark = ","), " kg/ha")
    div(style = "font-size: 12px; line-height: 1.3;",
        p(style="margin-bottom:3px;", tags$b("Mesorregião: "), span(class = "text-primary fw-bold", d$name_meso)),
        p(style="margin:0;", tags$b("Teto Produtivo: "), span(class = "text-success fw-bold", teto_texto)))
  })
  
  # Lógica de Inspeção adaptada para as novas origens independentes
  calculo_inspecao_ativo <- reactive({
    d <- dados_municipio_banco()
    req(!is.null(d), input$prod_area, input$prod_preco)
    produtividade_base <- d$prod_atingivel_kg_ha
    
    # Tratamento da Chuva (Manual vs Banco)
    chuva_considerada <- if (input$origem_chuva == "manual") {
      req(input$chuva_manual)
      input$chuva_manual
    } else {
      if("chuva_total_mm" %in% names(d)) d$chuva_total_mm else 0
    }
    
    # Tratamento da Ocorrência/Data (Manual vs Banco)
    data_ocorrencia <- if (input$origem_ocorrencia == "manual") {
      req(input$data_manual)
      input$data_manual
    } else {
      if("data_inicio_esporo" %in% names(d)) d$data_inicio_esporo else NA
    }
    
    # Cálculo da severidade sempre responde à chuva selecionada
    sev_calculada <- calc_sev(chuva_considerada)
    
    if (d$categoria_sinistro == "Município sem Relevância Produtiva") {
      perda_sc_ha <- 0
      categoria_atual <- "Município sem Relevância Produtiva"
    } else {
      perda_sc_ha <- pmin((produtividade_base * sev_calculada * 0.0055) / 60, produtividade_base / 60)
      
      # Calculamos a vulnerabilidade antes para usar no case_when
      vulnerabilidade_calc <- sev_calculada * 1.12 
      
      categoria_atual <- case_when(
        vulnerabilidade_calc < 10 ~ "Risco Desprezível",
        vulnerabilidade_calc < 20 ~ "Baixo Risco",
        vulnerabilidade_calc < 50 ~ "Risco Moderado",
        TRUE                      ~ "Risco Crítico"
      )
    }
    
    list(perda_sc_ha = perda_sc_ha, perda_kg_ha = perda_sc_ha * 60, total_sacas = perda_sc_ha * input$prod_area, total_kg = perda_sc_ha * 60 * input$prod_area, prejuizo = (perda_sc_ha * input$prod_area) * input$prod_preco, categoria = categoria_atual, data_esporos = data_ocorrencia, severidade = sev_calculada)
  })
  
  output$vb3_kg_total <- renderUI({
    res <- calculo_inspecao_ativo()
    tagList(format(round(res$total_kg), big.mark = ".", decimal.mark = ","), span(class = "sub-value-text", paste0(format(round(res$perda_kg_ha, 1), decimal.mark = ","), " kg/ha")))
  })
  output$vb3_sc_total <- renderUI({
    res <- calculo_inspecao_ativo()
    tagList(format(round(res$total_sacas), big.mark = ".", decimal.mark = ","), span(class = "sub-value-text", paste0(format(round(res$perda_sc_ha, 1), decimal.mark = ","), " sc/ha")))
  })
  output$vb3_reais <- renderUI({ paste0("R$ ", format(round(calculo_inspecao_ativo()$prejuizo, 2), big.mark = ".", decimal.mark = ",")) })
  
  output$prod_alerta_box <- renderUI({
    res <- calculo_inspecao_ativo()
    c_bg <- c("Risco Desprezível"="#F0FDF4", "Baixo Risco"="#FEFCE8", "Risco Moderado"="#FFF7ED", "Risco Crítico"="#FEF2F2", "Município sem Relevância Produtiva"="#F8FAFC")
    c_bd <- c("Risco Desprezível"="#86EFAC", "Baixo Risco"="#FDE047", "Risco Moderado"="#FDBA74", "Risco Crítico"="#FCA5A5", "Município sem Relevância Produtiva"="#94A3B8")
    
    card(class = "border-0 border-start text-dark shadow-sm", style = paste0("background:", c_bg[res$categoria], "; border-left: 6px solid ", c_bd[res$categoria], "; flex-grow: 2;"),
         card_body(class = "p-3",
                   tags$h6(class = "fw-bold mb-1", style = paste0("color:", CORES_RISCO[res$categoria], ";"), toupper(res$categoria)), 
                   tags$p(class = "mb-2 small", recomendar(res$categoria, res$data_esporos)),
                   tags$div(style = "border-top: 1px solid rgba(0,0,0,0.06); padding-top: 5px; margin-top: 5px;",
                            tags$span(tags$b("Severidade Estimada: "), paste0(format(round(res$severidade, 1), decimal.mark=","), " %"))
                   )
         )
    )
  })
  
  # PROJEÇÃO DE RISCO (INALTERADO)
  dados_projecao_reativa <- reactive({
    req(input$proj_muni, input$proj_area, input$proj_preco)
    df_clean <- dados_safra_tabela()
    m_data <- df_clean[df_clean$municipio_estacao == input$proj_muni, ]
    
    if(nrow(m_data) == 0) {
      return(tibble(Cenário = c("Normal", "El Niño", "La Niña"), `Severidade (%)` = 0, `Perda (R$/ha)` = 0, `Prejuízo Total (R$)` = 0))
    }
    m_data <- m_data[1, ]
    
    sev_normal <- m_data$severidade_calculada_pct
    sev_elnino <- pmin(100, sev_normal * 1.35)
    sev_lanina <- pmax(0, sev_normal * 0.65)
    prod_base <- m_data$prod_atingivel_kg_ha
    
    calc_prejuizo <- function(sev) {
      sc_ha <- pmin((prod_base * sev * 0.0055) / 60, prod_base / 60)
      perda_financeira_ha <- sc_ha * input$proj_preco
      total_financeiro <- perda_financeira_ha * input$proj_area
      list(ha = perda_financeira_ha, total = total_financeiro)
    }
    
    p_norm <- calc_prejuizo(sev_normal)
    p_el   <- calc_prejuizo(sev_elnino)
    p_la   <- calc_prejuizo(sev_lanina)
    
    tibble(
      Cenário = c("Normal", "El Niño (Alto Risco)", "La Niña (Baixo Risco)"),
      `Severidade (%)` = c(sev_normal, sev_elnino, sev_lanina),
      `Perda (R$/ha)` = c(p_norm$ha, p_el$ha, p_la$ha),
      `Prejuízo Total (R$)` = c(p_norm$total, p_el$total, p_la$total)
    )
  })
  
  output$tabela_projecao <- renderTable({
    df <- dados_projecao_reativa()
    df |> mutate(
      `Severidade (%)` = format(round(`Severidade (%)`, 1), decimal.mark = ","),
      `Perda (R$/ha)` = paste0("R$ ", format(round(`Perda (R$/ha)`, 2), big.mark = ".", decimal.mark = ",")),
      `Prejuízo Total (R$)` = paste0("R$ ", format(round(`Prejuízo Total (R$)`, 2), big.mark = ".", decimal.mark = ","))
    )
  }, digits = 2, align = 'l')
  
  output$grafico_projecao <- renderPlotly({
    df <- dados_projecao_reativa()
    p <- ggplot(df, aes(x = Cenário, y = `Prejuízo Total (R$)`, fill = Cenário)) +
      geom_bar(stat = "identity", width = 0.4, show.legend = FALSE) +
      scale_fill_manual(values = c("Normal" = "#10B981", "El Niño (Alto Risco)" = "#EF4444", "La Niña (Baixo Risco)" = "#3B82F6")) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",", prefix = "R$ ")) +
      theme_minimal() + labs(x = NULL, y = NULL)
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  dados_filtrados_panorama <- reactive({
    df <- dados_safra_selecionada()
    if (nrow(df) == 0) return(df)
    if (input$tipo_mapa_panorama == "classe") {
      if (is.null(input$filtro_panorama) || length(input$filtro_panorama) == 0) return(df[FALSE, ])
      return(df[df$categoria_sinistro %in% input$filtro_panorama, ])
    }
    return(df)
  })
  
  output$map_panorama <- renderLeaflet({
    leaflet() |> addProviderTiles(providers$CartoDB.Positron) |> setView(lng = -51.6, lat = -24.8, zoom = 7)
  })
  
  # OBSERVER INTERATIVO DO LEAFLET MAP - DEGRADÊ CONTÍNUO E SEGURO
  observe({
    df <- dados_filtrados_panorama()
    req(input$tipo_mapa_panorama)
    proxy <- leafletProxy("map_panorama") |> clearShapes() |> clearControls()
    if (is.null(df) || nrow(df) == 0) return()
    
    data_vector <- if("data_inicio_esporo" %in% names(df)) df$data_inicio_esporo else rep(NA, nrow(df))
    
    if (input$tipo_mapa_panorama == "classe") {
      pal <- colorFactor(palette = as.character(CORES_RISCO), domain = names(CORES_RISCO), ordered = TRUE)
      
      proxy |> addPolygons(data = df, fillColor = pal(df$categoria_sinistro), weight = 1.0, color = "#ffffff", fillOpacity = 0.80,
                           highlightOptions = highlightOptions(weight = 2, color = "#0F172A", fillOpacity = 0.95, bringToFront = TRUE),
                           label = df$municipio_estacao, 
                           popup = map_chr(1:nrow(df), function(i) {
                             row <- df[i, ]
                             paste0("<div style='font-size:12px;'><b>", row$municipio_estacao, "</b><br><b>Classe:</b> ", row$categoria_sinistro, "<br><b>Vulnerabilidade:</b> ", format(round(row$vulnerabilidade, 1), decimal.mark=","), "%<br><b>Perdas:</b> ", round(row$prod_perda_estimada_kgha), " kg/ha<br><b>Severidade:</b> ", round(row$severidade_calculada_pct, 1), "%<br><b>Ação:</b> ", recomendar(row$categoria_sinistro, data_vector[i]), "</div>")
                           })) |>
        addLegend(pal = pal, values = factor(names(CORES_RISCO), levels = names(CORES_RISCO)), title = "Vulnerabilidade", position = "bottomright", opacity = 0.85)
      
    } else if (input$tipo_mapa_panorama == "perda") {
      valores_perda <- as.numeric(df$prod_perda_estimada_kgha)
      valores_perda[is.na(valores_perda) | is.infinite(valores_perda)] <- 0
      
      # MUDANÇA AQUI: Usando colorBin em vez de colorNumeric. 
      # O parâmetro 'bins = 5' divide as perdas em 5 intervalos de cores seguros e estáveis.
      pal_num <- colorBin(palette = "YlOrRd", domain = valores_perda, bins = 5, na.color = "#dcdde1")
      
      proxy |> addPolygons(data = df, fillColor = pal_num(valores_perda), weight = 1.0, color = "#ffffff", fillOpacity = 0.80,
                           highlightOptions = highlightOptions(weight = 2, color = "#0F172A", fillOpacity = 0.95, bringToFront = TRUE),
                           label = df$municipio_estacao,
                           popup = map_chr(1:nrow(df), function(i) {
                             row <- df[i, ]
                             paste0("<div style='font-size:12px;'><b>", row$municipio_estacao, "</b><br><b>Classe:</b> ", row$categoria_sinistro, "<br><b>Vulnerabilidade:</b> ", format(round(row$vulnerabilidade, 1), decimal.mark=","), "%<br><b>Perdas:</b> ", round(valores_perda[i]), " kg/ha<br><b>Severidade:</b> ", round(row$severidade_calculada_pct, 1), "%</div>")
                           })) |>
        # A legenda vai renderizar os 5 bloquinhos de intervalo sem nenhum bug visual
        addLegend(pal = pal_num, values = valores_perda, title = "Perda Potencial (kg/ha)", position = "bottomright", opacity = 0.85,
                  labFormat = labelFormat(big.mark = ".", digits = 0))
      
    } else if (input$tipo_mapa_panorama == "severidade") {
      valores_sev <- as.numeric(df$severidade_calculada_pct)
      valores_sev[is.na(valores_sev) | is.infinite(valores_sev)] <- 0
      
      pal_num <- colorNumeric(palette = "Reds", domain = c(0, 100), na.color = "#dcdde1")
      
      proxy |> addPolygons(data = df, fillColor = pal_num(valores_sev), weight = 1.0, color = "#ffffff", fillOpacity = 0.80,
                           highlightOptions = highlightOptions(weight = 2, color = "#0F172A", fillOpacity = 0.95, bringToFront = TRUE),
                           label = df$municipio_estacao,
                           popup = map_chr(1:nrow(df), function(i) {
                             row <- df[i, ]
                             paste0("<div style='font-size:12px;'><b>", row$municipio_estacao, "</b><br><b>Classe:</b> ", row$categoria_sinistro, "<br><b>Vulnerabilidade:</b> ", format(round(row$vulnerabilidade, 1), decimal.mark=","), "%<br><b>Severidade:</b> ", round(valores_sev[i], 1), "%</div>")
                           })) |>
        # CORREÇÃO: "bins" removido. O Leaflet desenhará a barra em degradê contínuo automaticamente
        addLegend(pal = pal_num, values = c(0, 100), title = "Severidade Estimada (%)", position = "bottomright", opacity = 0.85,
                  labFormat = labelFormat(big.mark = ".", suffix = " %", digits = 1))
    }
  })
  
    # TARIFAÇÃO ATUARIAL (INALTERADO)
  calc_tarifacao <- reactive({
    req(input$tar_municipio)
    df_clean <- dados_safra_tabela()
    m_data <- df_clean[df_clean$municipio_estacao == input$tar_municipio, ]
    if(nrow(m_data) == 0) return(list(final = 4, premio = 0, base = 4, clima = 0))
    m_data <- m_data[1, ]
    
    taxa_base <- 4.0 + (m_data$severidade_calculada_pct * 0.04)
    ajuste_clima <- switch(input$tar_clima_global, "elnino" = 1.5, "lanina" = -0.8, 0.0)
    subtracao_midsoja  <- if(input$tar_midsoja) -1.2 else 0.0
    subtracao_historico <- if(input$tar_historico) -0.5 else 0.0
    
    taxa_final <- max(3.0, taxa_base + ajuste_clima + subtracao_midsoja + subtracao_historico)
    list(final = taxa_final, premio = (input$tar_area * input$tar_prod_esp * input$tar_preco_sc) * (taxa_final / 100), base = taxa_base, clima = ajuste_clima)
  })
  
  output$txt_taxa_seguro <- renderUI({ paste0(format(round(calc_tarifacao()$final, 2), decimal.mark = ","), " %") })
  output$txt_premio_total <- renderUI({ paste0("R$ ", format(round(calc_tarifacao()$premio, 2), big.mark = ".", decimal.mark = ",")) })
  output$txt_taxa_base <- renderUI({ paste0(format(round(calc_tarifacao()$base, 2), decimal.mark = ","), " %") })
  output$txt_ajuste_clima <- renderUI({ paste0(format(round(calc_tarifacao()$clima, 2), decimal.mark = ","), " %") })
  output$txt_bonus_tech <- renderUI({ 
    tem_midsoja  <- isTRUE(input$tar_midsoja)
    tem_historico <- isTRUE(input$tar_historico)
    if (tem_midsoja && tem_historico) return("-1,70 % (MID-Soja + Histórico Limpo)")
    if (tem_midsoja) return("-1,20 % (MID-Soja Ativo)")
    if (tem_historico) return("-0,50 % (Histórico Limpo Ativo)")
    return("0,00 % (Nenhum bônus aplicado)")
  })
  
  output$exportar_tar_pdf <- downloadHandler(
    filename = function() { paste0("Simulacao_Tarifacao_", input$tar_municipio, "_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) {
      req(input$tar_municipio)
      clima_str <- switch(input$tar_clima_global, "elnino" = "El Niño (Risco Alto)", "lanina" = "La Niña (Risco Basixo)", "Neutralidade Climática")
      taxa_base_val <- calc_tarifacao()$base
      ajuste_clima_val <- calc_tarifacao()$clima
      bonus_tech_str <- if(input$tar_midsoja) "-1,20 % (MID-Soja Ativo)" else "0,00 % (Nenhuma)"
      taxa_seguro_str <- paste0(format(round(calc_tarifacao()$final, 2), decimal.mark = ","), " %")
      premio_total_str <- paste0("R$ ", format(round(calc_tarifacao()$premio, 2), big.mark = ".", decimal.mark = ","))
      
      gerar_pdf_tarifacao(file = file, safra = input$seleciona_safra, muni = input$tar_municipio, area = input$tar_area, prod_seg = input$tar_prod_esp, preco_sc = input$tar_preco_sc, clima = clima_str, midsoja = input$tar_midsoja, historico = input$tar_historico, taxa_base = taxa_base_val, ajuste_clima = ajuste_clima_val, bonus_tech = bonus_tech_str, taxa_seguro = taxa_seguro_str, premio_total = premio_total_str)
    }
  )
  
  output$exportar_tar_excel <- downloadHandler(
    filename = function() { paste0("Simulacao_Tarifacao_", input$tar_municipio, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx") },
    content = function(file) {
      req(input$tar_municipio)
      clima_str <- switch(input$tar_clima_global, "elnino" = "El Niño (Risco Alto)", "lanina" = "La Niña (Risco Baixo)", "Neutralidade Climática")
      df_export <- tibble::tibble(
        `Variável` = c("Safra de Trabalho", "Município", "Área da Lavoura (ha)", "Produtividade Segurada (sc/ha)", "Preço da Saca (R$/sc)", "Macroclima Corrente (ENOS)", "Coletor MID-Soja Ativo", "Histórico de Risco Limpo", "Taxa Base Regional", "Ajuste Climático (ENOS)", "Bônus Tecnológico", "Taxa Comercial Dinâmica", "Prêmio Total Estimado"),
        `Valor` = c(input$seleciona_safra, input$tar_municipio, as.character(input$tar_area), as.character(input$tar_prod_esp), paste0("R$ ", format(round(input$tar_preco_sc, 2), big.mark = ".", decimal.mark = ",")), clima_str, if(input$tar_midsoja) "Sim" else "Não", if(input$tar_historico) "Sim" else "Não", paste0(format(round(calc_tarifacao()$base, 2), decimal.mark = ","), " %"), paste0(format(round(calc_tarifacao()$clima, 2), decimal.mark = ","), " %"), if(input$tar_midsoja) "-1,20 % (MID-Soja Ativo)" else "0,00 % (Nenhuma)", paste0(format(round(calc_tarifacao()$final, 2), decimal.mark = ","), " %"), paste0("R$ ", format(round(calc_tarifacao()$premio, 2), big.mark = ".", decimal.mark = ",")))
      )
      openxlsx::write.xlsx(df_export, file, sheetName = "Simulação", colNames = TRUE)
    }
  )
  
  # PORTFÓLIO DE RISCO (INALTERADO)
  output$tabela_portfolio <- DT::renderDataTable({
    df <- dados_safra_tabela() |> 
      select(municipio_estacao, name_meso, categoria_sinistro, vulnerabilidade, severidade_calculada_pct, perda_total_sacas) |> 
      rename(Município = municipio_estacao, Mesorregião = name_meso, Status = categoria_sinistro, `Vulnerabilidade (%)` = vulnerabilidade, `Severidade (%)` = severidade_calculada_pct, `Perda Total (Sacas)` = perda_total_sacas)
    
    DT::datatable(
      df, extensions = 'Buttons', 
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', 
        buttons = list(
          list(extend = 'pdfHtml5', text = '📄 Exportar para PDF', title = paste0("Siga-Soja - Portfolio de Riscos (Safra ", input$seleciona_safra, ")"), orientation = 'landscape', pageSize = 'A4', className = 'btn btn-danger btn-sm fw-bold'),
          list(extend = 'excelHtml5', text = '🟢 Exportar para Excel', title = paste0("Siga-Soja - Portfolio de Riscos (Safra ", input$seleciona_safra, ")"), className = 'btn btn-success btn-sm fw-bold')
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      )
    ) |> 
      formatRound(columns = c('Vulnerabilidade (%)', 'Severidade (%)'), digits = 1) |> 
      formatRound(columns = c('Perda Total (Sacas)'), digits = 0, mark = ".", dec.mark = ",")
  })
}

shinyApp(ui, server)
