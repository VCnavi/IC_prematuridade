# pacotes ####
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(janitor)
library(writexl)
library(purrr)
library(stringr)

# carregando dados ####
dados_resumo_ano <- read_excel("f2_tabelas/tabela_resumo_prematuridade_por_ano.xlsx")
dados_resumo_ano_rras <- read_excel("f2_tabelas/tabela_resumo_prematuridade_por_ano_e_rras.xlsx")
dados_obito  <- read_excel("f2_tabelas/tabela_obitos_por_variavel_ano_e_rras.xlsx")
dados_idh    <- read_excel("f2_tabelas/tabela_idh_médio_por_rras.xlsx")
dados_carac <- read_excel("f2_tabelas/tabela_prematuridade_por_ano_e_rras.xlsx")
dados_carac_obito <- read_excel("f2_tabelas/tabela_obitos_por_variavel_ano_e_rras.xlsx")

# Criando novas tabelas excel ####

# # agrupando dados carac
# arquivo <- "f2_tabelas/tabela_prematuridade_por_ano_e_rras.xlsx"
# abas <- excel_sheets(arquivo)
# dados_carac <- map_df(abas, function(aba) {
#   read_excel(arquivo, sheet = aba) %>%
#     mutate(ANO_RRAS = aba, .before = 1)
# }) %>%
#   filter(!is.na(Tipo))%>%
#   mutate(ANO = str_extract(ANO_RRAS, "^\\d{4}"),
#          RRAS_num = str_extract(ANO_RRAS, "\\d+$"),
#          RRAS = paste0("RRAS", RRAS_num)
#   ) %>%
#   select(ANO, RRAS, everything(), -RRAS_num, -ANO_RRAS, -NA.)
# write_xlsx(dados_carac, "f2_tabelas/tabela_prematuridade_por_ano_e_rras.xlsx")

# # taxas de prematuridade
# dados_resumo_ano_rras <- dados_resumo_ano_rras %>%
#   filter(!is.na(RRAS)) %>%
#   mutate(
#     taxa_PP = `Total de partos prematuros (PP)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPel_por_NV = `Total de PP eletivos (PPel)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPel_por_PP = `Total de PP eletivos (PPel)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PPes_por_NV = `Total de PP espontâneos (PPes)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPes_por_PP = `Total de PP espontâneos (PPes)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PTP = `Total de partos termo precoce (PTP)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPel_por_NV = `Total de partos termo precoce eletivos (PTPel)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPel_por_PP = `Total de partos termo precoce eletivos (PTPel)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PTPes_por_NV = `Total de partos termo precoce espontâneos (PTPes)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPes_por_PP = `Total de partos termo precoce espontâneos (PTPes)` / `Total de partos prematuros (PP)` * 100,
#   )
# 
# # salvando em excel
# write_xlsx(dados_resumo, "f2_tabelas/tabela_resumo_prematuridade_por_ano_e_rras.xlsx")

# # taxas de prematuridade
# dados_resumo_ano <- dados_resumo_ano_rras %>%
#   mutate(
#     taxa_PP = `Total de partos prematuros (PP)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPel_por_NV = `Total de PP eletivos (PPel)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPel_por_PP = `Total de PP eletivos (PPel)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PPes_por_NV = `Total de PP espontâneos (PPes)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PPes_por_PP = `Total de PP espontâneos (PPes)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PTP = `Total de partos termo precoce (PTP)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPel_por_NV = `Total de partos termo precoce eletivos (PTPel)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPel_por_PP = `Total de partos termo precoce eletivos (PTPel)` / `Total de partos prematuros (PP)` * 100,
#     taxa_PTPes_por_NV = `Total de partos termo precoce espontâneos (PTPes)` / `Total de nascidos vivos (NV) por residência` * 100,
#     taxa_PTPes_por_PP = `Total de partos termo precoce espontâneos (PTPes)` / `Total de partos prematuros (PP)` * 100,
#   )
# 
# # salvando em excel
# write_xlsx(dados_resumo, "f2_tabelas/tabela_resumo_prematuridade_por_ano.xlsx")



# limpando dados ####
dados_carac <- dados_carac %>% 
  filter(!is.na(Categoria),
         Categoria != "NA",
         Categoria != "Ignorado",
         Categoria != "99")

# análises ####

dados_resumo_comp <- dados_resumo %>%
  select(ANO, RRAS,
         `Total de partos prematuros (PP)`,
         `Total de partos termo precoce (PTP)`,
         `Total de PP eletivos (PPel)`,
         `Total de PP espontâneos (PPes)`) %>%
  pivot_longer(
    cols = c(`Total de partos prematuros (PP)`,
             `Total de partos termo precoce (PTP)`),
    names_to = "PP_e_PTP",
    values_to = "valor_PP_e_PTP"
  ) %>%
  pivot_longer(
    cols = c(`Total de PP eletivos (PPel)`,
             `Total de PP espontâneos (PPes)`),
    names_to = "PPel_e_PPes",
    values_to = "valor_PPel_e_PPes"
  ) %>%
  left_join(dados_idh, by = "RRAS")
# comparação PP x PTP e PPel x PPes por RRAS e ano

PPxPTP <- ggplot(dados_resumo_comp,
                 aes(ANO, valor_PP_e_PTP, color = PP_e_PTP)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~RRAS) +
  theme_minimal() +
  labs(
    title = "PP x PTP por RRAS",
    x = "Ano", y = "Número de casos", color = ""
  ) +
  theme(legend.position = "bottom")

PPelxPPes <- ggplot(dados_resumo_comp,
                    aes(ANO, valor_PPel_e_PPes, color = PPel_e_PPes)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~RRAS) +
  theme_minimal() +
  labs(
    title = "PPel x PPes por RRAS",
    x = "Ano", y = "Número de casos", color = ""
  ) +
  theme(legend.position = "bottom")

PPxPTP

PPelxPPes
