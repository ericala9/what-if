# ==============================================================================
#                      Mapa das línguas indígenas no Brasil 
# ==============================================================================
# Título: Mapa das línguas indígenas no Brasil 
# Autor: Érica Ambrosio
# Objetivo: Criação de mapa interativo que mostre as línguas indígenas mais
# faladas em cada município do Brasil a partir dos dados do Censo 2022.
# Data: 2026-02-20
# ==============================================================================

# ------------------------- Configuração do ambiente  -------------------------- 

rm(list = ls()) 

options(encoding = "UTF-8")
options(scipen = 999)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

library(dplyr)
library(geobr)
library(leaflet)
library(readxl)
library(sf)
library(stringr)

# ----------------------------- Leitura dos dados ------------------------------ 

# Arquivos disponíveis no Sidra e em: https://www.ibge.gov.br/estatisticas/sociais/trabalho/22827-censo-demografico-2022.html?edicao=44827&t=resultados
# Links diretos: 

# Apêndice 2. Proposta de agrupamentos de troncos, famílias linguísticas e línguas indígenas – Brasil – 2022						
# https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Etnias_e_Linguas_Indigenas_principais_caracteristicas_sociodemograficas_Resultados_do_universo/Apendices/xlsx/Apendice_02.xlsx

# Tabela 14 - Pessoas indígenas de 15 anos ou mais de idade falantes de língua indígena, por alfabetização, segundo os municípios – 2022
# https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Etnias_e_Linguas_Indigenas_principais_caracteristicas_sociodemograficas_Resultados_do_universo/Tabelas_complementares/xlsx/Tabela_complementar_14.xlsx

# Tabela complementar 26 - Pessoas indígenas de 2 anos ou mais de idade, por língua indígena falada ou utilizada no domicílio, segundo os Municípios - Brasil – 2022						
# https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Etnias_e_Linguas_Indigenas_principais_caracteristicas_sociodemograficas_Resultados_do_universo/Tabelas_complementares/xlsx/Tabela_complementar_26.xlsx

# Tabela 9514 - População residente, por sexo, idade e forma de declaração da idade (Vide Notas)
# https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela9514.xlsx&terr=N&rank=-&query=t/9514/n1/all/n6/all/v/allxp/p/all/c2/6794/c287/93070,93084,93085,100362/c286/113635/l/v,p%2Bc2%2Bc287,t%2Bc286

# Tabela 9718 - População residente, total e indígena, por localização do domicílio e quesito de declaração indígena nos Censos Demográficos - Primeiros Resultados do Universo 
# https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela9718.xlsx&terr=N&rank=-&query=t/9718/n6/all/v/allxp/p/all/c1714/60024/c2661/32776/d/v4727%202/l/v,p%2Bc1714,t%2Bc2661
# Acessados em 20 de fevereiro de 2026.

ap2 <- read_excel("dados/Apendice_02.xlsx", skip = 5)
tab14 <- read_excel("dados/Tabela_complementar_14.xlsx", skip = 7, na = "-")
tab26 <- read_excel("dados/Tabela_complementar_26.xlsx", skip = 7)
tab9514 <- read_excel("dados/tabela9514.xlsx", skip = 5)
tab9718_1 <- read_excel("dados/tabela9718.xlsx", skip = 2, na = "-")
tab9718_2 <- read_excel("dados/tabela9718.xlsx", sheet = 2, skip = 2, na = "-")
tab9718_3 <- read_excel("dados/tabela9718.xlsx", sheet = 3, skip = 2, na = "-")

malha_brasil <- read_municipality(year = 2022, showProgress = FALSE)

# --------------------------- Transformação de dados --------------------------- 

## Apêndice 2 ------------------------------------------------------------------
names(ap2) <- c("codigo_tronco", "tronco", "ordem_familia", "seq_familia", 
                "familia", "lingua", "codigo_lingua")

ap2 <- ap2 |> 
  filter(is.na(lingua) == FALSE) |> 
  mutate(
    #     Retirada da nota de rodapé que existe em algumas línguas.
    lingua = gsub(" \\(\\*+\\)$", "", lingua),
    #     Ajuste nas línguas sem família e sem tronco para que a informação de
    #     família seja igual ao nome da própria língua.
    familia = ifelse(codigo_tronco >= 4, lingua, familia)
  )

## Tabela 14 -------------------------------------------------------------------
names(tab14) <- c("codigo_mun", "mun", "total", "alfabet", "nao_alfabet", 
                  "tx_alfabet", "tx_analfabet", "ind_total", "ind_alfabet", 
                  "ind_nao_alfabet", "ind_tx_alfabet", "ind_tx_analfabet")
# alfabet = alfabetizados
# nao_alfabet = não alfabetizados
# tx_alfabet =  taxa de alfabetização
# tx_analfabet =  taxa de analfabetismo
# ind_* = falantes de língua indígena podem ou não ser falantes de português

tab14 <- tab14 |> 
#   Deixando apenas as linhas de municípios
  filter(nchar(codigo_mun) == 7) 

## Tabela 26 -------------------------------------------------------------------
names(tab26) <- c("coduf", "uf", "codigo_mun", "mun", "codigo_lingua", "lingua", "total")

tab26 <- tab26 |> 
  select(-coduf, -uf) |> 
  # Deixando apenas as linhas de municípios
  filter(nchar(codigo_mun) == 7) |> 
  # Retirada da nota de rodapé que existe em algumas línguas
  mutate(lingua = gsub(" \\(\\*+\\)$", "", lingua))


## Tabela 9514 -----------------------------------------------------------------
# O objetivo é ter saber a população maior de 15 anos, assim, na consolidação,
# posso calcular a proporção desta popução falante de língua indígena.
names(tab9514) <- c("mun", "desc", "pop_total", "fx_0a4", "fx_5a9", "fx_10a14")

tab9514 <- tab9514 |> 
  filter(is.na(desc) == FALSE & mun != "Brasil") |> 
  mutate(pop_15mais = pop_total - fx_0a4 - fx_5a9 - fx_10a14) |> 
  select(mun, pop_15mais)

## Tabela 9718 -----------------------------------------------------------------

# É necessário juntar as diferentes em uma única base.
names(tab9718_1) <- c("mun", "desc", "pop_indigena")
names(tab9718_2) <- c("mun", "desc", "pop_total")
names(tab9718_3) <- c("mun", "desc", "prop_indigena")

tab9718 <- tab9718_1 |> 
# Retirada de linhas que não contém dados  
  filter(is.na(desc) == FALSE) |> 
  select(-desc) |> 
  mutate(pop_indigena = as.numeric(pop_indigena)) |> 
  left_join(
    tab9718_2 |> 
      # Retirada de linhas que não contém dados  
      filter(is.na(desc) == FALSE) |> 
      select(-desc) |> 
      mutate(pop_total = as.numeric(pop_total)),
    by = "mun"
  ) |> 
  left_join(
    tab9718_3 |> 
      # Retirada de linhas que não contém dados  
      filter(is.na(desc) == FALSE) |> 
      select(-desc) |> 
      mutate(prop_indigena = as.numeric(prop_indigena)),
    by = "mun"
  ) 
rm(tab9718_1, tab9718_2, tab9718_3)

# Teste se a proporção divulgada é a proporção que tenho em mente.
tab9718 |> 
  mutate(
    teste_prop = round(pop_indigena / pop_total * 100, 2),
    dif_prop = teste_prop - prop_indigena
  ) |> 
  pull(dif_prop) |> 
  summary()



# --------------------------- Consolidação das bases --------------------------- 

## perfil_indigenas ------------------------------------------------------------

# Criação de base que traz informações sobre a população indígena e número de
# falantes de línguas indígenas ali.
perfil_indigenas <- tab9718 |> 
  left_join(
# População maior de 15 anos falante de língua indígena
    tab14 |> 
      select(codigo_mun, mun, total, ind_total),
  by = "mun"
  ) |> 
# População total maior de 15 anos
  left_join(tab9514, by = "mun") |> 
  relocate(codigo_mun, mun, pop_total, pop_15mais) |> 
  rename(
    "pop_ind_15mais" = total,
    "falantes_ind_15mais" = ind_total
  ) |> 
  mutate(prop_falantes_ind_15mais = round(falantes_ind_15mais / pop_15mais * 100, 2))

## perfil_linguas --------------------------------------------------------------

perfil_linguas <- tab26 |> 
  filter(!lingua %in% c("Não determinada", "Não sabe", "Sem declaração", "Mal definida")) |> 
  rename("falantes_ind_02mais" = total) |> 
  left_join(
    ap2 |> 
#     Variável que indica se a língua tem família classificada, vai ser útil na
#     construção do pop-up
      mutate(ind_familia = ifelse(codigo_tronco <= 3, 1, 0)) |> 
      select(codigo_lingua, familia, ind_familia),
    by = "codigo_lingua"
  ) |> 
  relocate(falantes_ind_02mais, .after = last_col()) |> 
  select(-codigo_lingua) |> 
# Retirando as línguas faladas apenas por uma pessoa, para evitar empate cheio de ruído.
  filter(falantes_ind_02mais > 1) |> 
# Deixando apenas as três línguas mais faladas em cada município
  slice_max(falantes_ind_02mais, n = 3, by = codigo_mun) |> 
# Junção com o número total de indígenas no município
  left_join(tab9718, by = "mun") |> 
# Proporção de indígenas que falam a língua
  mutate(prop_ind_lingua = round(falantes_ind_02mais  / pop_indigena  *100, 2))

# ------------------------------- Pop-up do mapa ------------------------------- 

# Criação do pop-up do mapa, que traz informações sobre a população total,
# indígena e as principais línguas faladas.

# Pop-up sobre as línguas mais faladas.
popup_linguas <- perfil_linguas |> 
  mutate(
    item_lista = paste0(
      "<li><b>", 
      lingua, 
      "</b>",
      if_else(ind_familia == 1 & !grepl("não especificado$", familia), paste0(" (família: ", familia, ")"), ""),
      ": ", 
      falantes_ind_02mais , 
      " falantes (", 
      format(prop_ind_lingua, decimal.mark = ",", trim = TRUE),
      "%) </li>"
    )
  ) |> 
#   Junção de todas as informações do município em uma linha só.
  group_by(codigo_mun) |> 
  summarise(
    lista_completa = paste(item_lista, collapse = ""),
    .groups = "drop"
  ) |> 
  mutate(
    item_lingua = paste0("<ul>", lista_completa, "</ul>")
  ) |> 
  select(codigo_mun, item_lingua)

# Pop-up que efetivamente vai para o mapa.
popup_mapa <- perfil_indigenas |> 
  left_join(popup_linguas, by = "codigo_mun") |> 
  mutate(
    # 1. BLOCO CABEÇALHO (Adicionei um line-height para as linhas respirarem)
    bloco_cabecalho = paste0(
      "<h2 style='margin: 0 0 8px 0;'><b>", mun, "</b></h4>",
      "<div style='line-height: 1.5;'>",
      "<b>População Total:</b> ", format(pop_total, big.mark = ".", decimal.mark = ",", scientific = FALSE), " pessoas<br>"
    ),
    
    # 2. BLOCO POPULAÇÃO INDÍGENA
    bloco_indigena = if_else(
      is.na(pop_indigena),
      "<i style='color: #777;'>Sem registros de população indígena residente.</i></div>", 
      paste0("<b>População Indígena:</b> ", format(pop_indigena, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
             " pessoas (", prop_indigena, "%)</div>")
    ),
    
    # 3. BLOCO CENÁRIO LINGUÍSTICO (Com a linha divisória elegante)
    bloco_cenario = if_else(
      is.na(pop_indigena), 
      "",
      if_else(
        is.na(falantes_ind_15mais),
        paste0("<hr style='margin: 12px 0; border: 0; border-top: 1px solid #ddd;'>",
               "<h5 style='margin: 0 0 6px 0; color: #2c3e50; font-size: 13px;'><b>Cenário das línguas indígenas</b></h5>",
               "<i style='color: #777;'>Sem dados de falantes (15+ anos).</i><br>"),
        paste0("<hr style='margin: 12px 0; border: 0; border-top: 1px solid #ddd;'>",
               "<h5 style='margin: 0 0 6px 0; color: #2c3e50; font-size: 13px;'><b>Cenário das línguas indígenas</b></h5>",
               "<b>Indígenas falantes (15+ anos):</b> ", format(falantes_ind_15mais, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
               " (", prop_falantes_ind_15mais, "%)<br>")
      )
    ),
    
    # 4. BLOCO LÍNGUAS (Ajustando a mensagem e o texto miúdo)
    bloco_linguas = if_else(
      is.na(pop_indigena), 
      "", 
      if_else(
        is.na(item_lingua),
        paste0("<b style='margin-top: 8px; display: block;'>Principais línguas:</b>",
               "<i style='color: #777;'>Nenhuma língua com mínimo de falantes registrada.</i>"),
        paste0("<b style='margin-top: 8px; display: block;'>Principais línguas:</b>",
               "<span style='font-size: 10px; color: #777; line-height: 1.1; display: block; margin-bottom: 4px;'>",
               "*Faladas por pessoas de 2 anos ou mais. O Censo permite a declaração de até duas línguas.</span>",
               
               # O truque final: Substituir o <ul> padrão que veio da outra tabela por um estilizado
               str_replace(item_lingua, "<ul>", "<ul style='margin: 0; padding-left: 20px;'>")
        )
      )
    ),
    
    # 5. A GRANDE JUNÇÃO
    item_final = paste0(bloco_cabecalho, bloco_indigena, bloco_cenario, bloco_linguas)
  ) |> 
  select(codigo_mun, item_final)

# ----------------------------- Construção do mapa ----------------------------- 

# Construção da base do mapa.
mapa_falantes_15mais <- malha_brasil |> 
  left_join(perfil_indigenas, by = c("code_muni" = "codigo_mun")) |> 
  left_join(popup_mapa, by = c("code_muni" = "codigo_mun"))

# Ajuste na malha do mapa
mapa_falantes_15mais <- mapa_falantes_15mais |> 
  st_transform(crs = 4326)

paleta_cores <- colorNumeric(
  palette = "BuGn",
  domain = mapa_falantes_15mais$prop_falantes_ind_15mais,
  na.color = "transparent" 
)

mapa_falantes_15mais_final <- leaflet(data = mapa_falantes_15mais) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    fillColor = ~paleta_cores(prop_falantes_ind_15mais),
    fillOpacity = 0.8,
    weight = 0.5,
    color = "#444444",
    popup = ~item_final, # Variável com a informação do popup
    
    # Destaca o município quando o mouse passa por cima
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) |> 
  
  # Legenda
  addLegend(
    pal = paleta_cores, 
    values = ~prop_falantes_ind_15mais, 
    opacity = 0.8, 
    title = "Proporção de falantes de língua indígena entre os maiores de 15 anos (%)", 
    position = "bottomright"
  )

# Chama o mapa para ver o resultado no painel Viewer do RStudio
mapa_falantes_15mais_final

# ---------------------------- Exportação dos dados ---------------------------- 

# Criação da pasta de output se ela ainda não existir.
if (!dir.exists("./output")) {
  dir.create("./output")
}

write.xlsx(
  objeto_final, 
  file = paste0("output/nomeDoArquivo", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
  colWidths = "auto",
  headerStyle = createStyle(textDecoration = "bold")
)

# ------------------------------------------------------------------------------
#                                    Entregas 
# ------------------------------------------------------------------------------
#
# Descrição dos produtos, incluindo:
# - O que foi entregue
# - Quem recebeu
# - Quando e como a entrega foi feita (email, Redmine, WhatsApp, etc.)
#
# Exemplo:
# - tabela_resumo.xlsx enviada para X em 2025.10.21 por email
#
# Exemplo:
# Produto:       tabela_resumo.xlsx
# Destinatário:  X
# Data:          2025-10-21
# Meio de envio: email
# Notas:         Enviado junto com o relatório de resumo do projeto

# ------------------------------------------------------------------------------
#                                Próximos passos
# ------------------------------------------------------------------------------
# - Colocar título
# - Colocar aba pesquisável
# - Arrumar a legenda
# - Destacar o limite dos estados
# - Tirar as legendas
# - Hover com nome do município
# - Mapa de calor e/ou aquele de bolinhas com a distribuição das línguas pelo municípios
# ------------------------------------------------------------------------------
