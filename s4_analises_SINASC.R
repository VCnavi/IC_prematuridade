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

# RODAR ESTA ETAPA APENAS QUANDO NOVOS DADOS FOREM ADICIONADOS AOS ARQUIVOS "dados_SINASC" e "dados_SIM" ####

# # Criando novas tabelas excel 
# # limpando dados_carac (fazer apenas quando necessário para trabalhar com dados de características)
# dados_carac <- dados_carac %>%
#   filter(!is.na(Categoria),
#          Categoria != "NA",
#          Categoria != "Ignorado",
#          Categoria != "99")
# write_xlsx(dados_carac, "f2_tabelas/tabela_prematuridade_por_ano_e_rras.xlsx")

# taxas de prematuridade
dados_resumo_ano_rras <- dados_resumo_ano_rras %>%
  filter(!is.na(RRAS)) %>%
  mutate(
    taxa_PP = `Total de partos prematuros (PP)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPel_por_NV = `Total de PP eletivos (PPel)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPel_por_PP = `Total de PP eletivos (PPel)` / `Total de partos prematuros (PP)` * 100,
    taxa_PPes_por_NV = `Total de PP espontâneos (PPes)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPes_por_PP = `Total de PP espontâneos (PPes)` / `Total de partos prematuros (PP)` * 100,
    taxa_PTP = `Total de partos termo precoce (PTP)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPel_por_NV = `Total de partos termo precoce eletivos (PTPel)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPel_por_PP = `Total de partos termo precoce eletivos (PTPel)` / `Total de partos prematuros (PP)` * 100,
    taxa_PTPes_por_NV = `Total de partos termo precoce espontâneos (PTPes)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPes_por_PP = `Total de partos termo precoce espontâneos (PTPes)` / `Total de partos prematuros (PP)` * 100,
  )

# salvando em excel
write_xlsx(dados_resumo_ano_rras, "f2_tabelas/tabela_resumo_prematuridade_por_ano_e_rras.xlsx")

# taxas de prematuridade
dados_resumo_ano <- dados_resumo_ano %>%
  mutate(
    taxa_PP = `Total de partos prematuros (PP)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPel_por_NV = `Total de PP eletivos (PPel)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPel_por_PP = `Total de PP eletivos (PPel)` / `Total de partos prematuros (PP)` * 100,
    taxa_PPes_por_NV = `Total de PP espontâneos (PPes)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PPes_por_PP = `Total de PP espontâneos (PPes)` / `Total de partos prematuros (PP)` * 100,
    taxa_PTP = `Total de partos termo precoce (PTP)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPel_por_NV = `Total de partos termo precoce eletivos (PTPel)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPel_por_PP = `Total de partos termo precoce eletivos (PTPel)` / `Total de partos prematuros (PP)` * 100,
    taxa_PTPes_por_NV = `Total de partos termo precoce espontâneos (PTPes)` / `Total de nascidos vivos (NV) por residência` * 100,
    taxa_PTPes_por_PP = `Total de partos termo precoce espontâneos (PTPes)` / `Total de partos prematuros (PP)` * 100,
  )

# salvando em excel
write_xlsx(dados_resumo_ano, "f2_tabelas/tabela_resumo_prematuridade_por_ano.xlsx")
