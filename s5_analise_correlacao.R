
library(readxl)
library(writexl)
library(dplyr)
library(corrplot)
library(tibble)

##

dados_resumo_ano_rras <- read_excel("f2_tabelas/tabela_resumo_prematuridade_por_ano_e_rras.xlsx")

dados_resumo_rras_2024 <- dados_resumo_ano_rras |> 
  filter(ANO == 2024) |>
  select(RRAS, taxa_PP, taxa_PPel_por_NV, taxa_PPes_por_NV) |> 
  mutate(across(is.numeric, ~ round(.x, 2)))

rm(dados_resumo_ano_rras)

##

dados_caracteristicas_rras <- read_excel("f4_dados_julia/CARACTERÍSTICAS DAS RRAS 2022.xlsx") |>
  select(-'RRAS label') |>
  mutate(
    RRAS = paste("RRAS", RRAS)
  )

dados_rras <- left_join(dados_resumo_rras_2024, dados_caracteristicas_rras) |>
  mutate(
    RRAS = factor(RRAS, 
                  levels = paste("RRAS", 1:18))
  )

rm(dados_resumo_rras_2024, dados_caracteristicas_rras)

write_xlsx(dados_rras, "f2_tabelas/dados_rras_2024.xlsx")

##

dados_rras_num <- dados_rras |>
  select(-RRAS) |>
  rename(
    tx_PP = taxa_PP,
    tx_PPel = taxa_PPel_por_NV,
    tx_PPes = taxa_PPes_por_NV,
    rz_mort_mat = 'Razão de Mortalidade materna oficial 2024',
    IDH_2022 = 'IDH 2022',
    porc_exclusivas_SUS = '% de mulheres de 10-49 anos usuárias exclusivas do SUS 2024',
    cob_pop_AB = 'CObertura populacional de atenção básica 2024',
    cob_pop_SF = 'Cobertura populacional com equipes de Saúde da Família 2020',
    porc_mais_7_pre_natal = '% de Mulheres com mais de 7 consultas de pré natal 2024',
    porc_pre_natal_precoce = '% de mulheres com inicio do pré-natal precoce (<12 semanas) 2024',
    porc_pre_natal_adequado = '% de mulheres con número adequado de consultas de pré-natal para a idade gestacional no parto',
    tx_mort_fetal = 'taxa de mortalidade fetal 2024 (por  1000 NV)',
    porc_obitos_fetais_evitaveis = 'Porcentagem de óbitos fetais potencialmente evitáveis'
  ) 

##

cor_matrix <- cor(dados_rras_num, 
                  method = "spearman") 

cor_matrix_aux <- cor_matrix |>
  as.data.frame() |>
  rownames_to_column("variavel") 

write_xlsx(cor_matrix_aux, "f2_tabelas/matriz_correlacao.xlsx")

rm(cor_matrix_aux)

##

jpeg("f3_graficos/corrplot.jpg", width = 2500, height = 2500, res = 300)

corrplot(cor_matrix, type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         insig = "label_sig")

dev.off()

##

jpeg("f3_graficos/pairs_panels.jpg", width = 2000, height = 2000, res = 300)

pairs.panels(dados_rras_num, 
             method = "spearman", 
             hist.col = "#00AFBB",
             density = FALSE,  
             ellipses = FALSE)

dev.off()

##

res <- corr.test(dados_rras_num, method = "spearman", adjust = "none")

p_matrix <- res$p |>
  as.data.frame() |>
  rownames_to_column("variavel") 

rm(res)

write_xlsx(p_matrix, "f2_tabelas/matriz_p_valor_correlacao.xlsx")


