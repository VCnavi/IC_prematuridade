# pacotes
{
  library(remotes)
  library(tidyverse)
  library(data.table)
  library(microdatasus)
}

variaveis_sinasc <- c("DTNASC","SEMAGESTAC","CODMUNRES","ESTCIVMAE","STTRABPART", 
               "STCESPARTO","CODMUNNASC","GESTACAO","IDADEMAE","RACACORMAE", 
               "CONSPRENAT","MESPRENAT","GRAVIDEZ","ESCMAE2010","PESO", 
               "QTDPARTCES","QTDFILVIVO","QTDFILMORT")

variaveis_sim <- c("DTOBITO","CODMUNRES","CODMUNOCOR","GESTACAO","SEMAGESTAC",
                   "OBITOPARTO","PESO","ESCMAE","ESCMAE2010","GRAVIDEZ",
                   "QTDFILVIVO","QTDFILMORT")

# for (ano in 2012:2023) {
#   nome_df <- paste0("sinasc_", ano)
#   assign(nome_df,
#     fetch_datasus(
#       year_start = ano,
#       year_end = ano,
#       uf = "SP",
#       vars = variaveis_sinasc,
#       information_system = "SINASC"
#     )
#   )
# }
# 
# for (ano in 2012:2023) {
#   nome_df <- paste0("sinasc_", ano)
#   if (exists(nome_df)) {
#     df <- get(nome_df)
#     nome_arquivo <- paste0("sinasc_", ano, ".csv")
#     write.csv(df, nome_arquivo, row.names = FALSE)
#   }
# }
# 
# for (ano in 2012:2023) {
#   nome_df <- paste0("sim_dofet_", ano)
#   assign(nome_df,
#          fetch_datasus(
#            year_start = ano,
#            year_end = ano,
#            uf = "SP",
#            information_system = "SIM-DOFET"
#          )
#   )
# }
# 
# for (ano in 2012:2023) {
#   nome_df <- paste0("sim_dofet_", ano)
#   if (exists(nome_df)) {
#     df <- get(nome_df)
#     nome_arquivo <- paste0("sim_dofet_", ano, ".csv")
#     write.csv(df, nome_arquivo, row.names = FALSE)
#   }
# }

dados <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  uf = "SP", 
  information_system = "SINASC",
  vars = variaveis_sinasc)

write.csv(dados, "dados_SINASC.csv")

dados <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  information_system = "SIM-DOFET",
  vars = variaveis_sim)

write.csv(dados, "dados_SIM.csv")
