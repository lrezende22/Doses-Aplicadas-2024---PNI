#### --------------- Análise de doses aplicadas --------------------------------

### ---- Período: janeiro 2024 até dezembro de 204 -----------------------------



library(data.table)
library(dplyr)
library(purrr)
library(writexl)
library(lubridate)

diretorio <- "C:\\Users\\Lucas\\Downloads\\Dados Vacina\\Todos os meses"




vacinas_polio <- c("VIP", "dTpa/VIP", "PENTA acelular", "HEXA")


var_inter <- c("co_documento", "co_paciente", "sg_vacina", "co_dose_vacina", 
               "nu_idade_paciente", "ds_dose_vacina", "co_lote_vacina", 
               "dt_vacina", "dt_entrada_rnds", "ds_vacina_fabricante", "no_municipio_estabelecimento",
               "no_municipio_paciente")


arquivos <- list.files(diretorio, pattern = "vacinacao_.*_2024.csv", full.names = TRUE)



processar_dados_polio <- function(arquivo, vacinas_polio) {
  fread(arquivo, sep = ";", select = var_inter) %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    filter(sg_vacina %in% vacinas_polio)
}

# todos os dados em uma lista e juntos
dados_polio <- map_dfr(arquivos, processar_dados_polio, vacinas_polio = vacinas_polio)

dados_polio <- dados_polio %>%
  filter(!(is.na(co_paciente) | co_paciente == ""))


dados_polio <- dados_polio %>%
  mutate(dt_vacina = as.Date(dt_vacina)) %>%         
  arrange(co_paciente, dt_vacina) %>%                
  group_by(co_paciente) %>%                          
  mutate(
    idade_inicial = first(nu_idade_paciente),        # Pega a idade da primeira dose
    meses_após_primeira = as.integer(difftime(dt_vacina, first(dt_vacina), units = "days") / 30.44),
    idade_estimada = idade_inicial + meses_após_primeira
  ) %>%
  ungroup() %>%
  select(-idade_inicial, -meses_após_primeira) %>%   # Removendo colunas auxiliares
  relocate(idade_estimada, .after = nu_idade_paciente) # Colocando a nova coluna logo após a original







frequencias_polio <- dados_polio %>%
group_by(co_paciente) %>%
summarise(
 n_doses = n(),  
 vacinas = paste(sg_vacina, collapse = ", "),  # lista todas as vacinas aplicadas
groups = "drop"
   ) %>%
   arrange(desc(n_doses))






dados_limpos <- dados_polio %>%
  mutate(dt_vacina = as.Date(dt_vacina)) %>%
  arrange(co_paciente, sg_vacina, dt_vacina) %>%
  group_by(co_paciente, sg_vacina) %>%
  mutate(intervalo_dias = as.numeric(dt_vacina - lag(dt_vacina))) %>%
  filter(is.na(intervalo_dias) | intervalo_dias >= 30) %>% 
  ungroup()
  #select(-intervalo_dias)   




 contagem <- dados_limpos %>%
   mutate(idade_anos = floor(idade_estimada / 12)) %>%
   group_by(no_municipio_paciente, sg_vacina, co_dose_vacina, idade_anos) %>%
   summarise(
     n_aplicacoes = n(),
     .groups = "drop"
   ) %>%
   arrange(no_municipio_paciente, sg_vacina, co_dose_vacina, idade_anos)






head(dados_limpos)



contagem_individual <- dados_limpos %>%
  mutate(idade_anos = floor(idade_estimada / 12)) %>%
  arrange(co_paciente, sg_vacina, co_dose_vacina) %>%
  group_by(co_paciente) %>%
  mutate(freq_acumulada = row_number()) %>%
  select(co_paciente, sg_vacina, co_dose_vacina, idade_anos, no_municipio_paciente, freq_acumulada) %>%
  ungroup()

head(contagem_acumulada, 10)




colnames(dados_limpos)
# freq_por_idade_mun <- dados_limpos %>%
#   filter(!is.na(nu_idade_paciente)) %>%
#   mutate(idade_anos = floor(nu_idade_paciente / 12)) %>%
#   group_by(
#     no_municipio_paciente,
#     sg_vacina,
#     co_dose_vacina,
#     idade_anos
#   ) %>%
#   summarise(n_aplicacoes = n(), .groups = "drop") %>%
#   rename(
#     Municipio_Paciente = no_municipio_paciente
#   ) %>%
#   arrange(Municipio_Paciente, sg_vacina, co_dose_vacina, idade_anos)
# 
# 
# 
# 
# 
# 
# frequencias_polio2 <- dados_limpos %>%
#   group_by(co_paciente, sg_vacina) %>%
#   mutate(n_dose_est = 1) %>% 
#   mutate(n_dose_est = cumsum(n_dose_est)) %>% 
#     group_by(n_dose_est,no_municipio_paciente, sg_vacina) %>% 
#     summarise(
#     doses_estimadas = n()
#   ) #%>%
#   #arrange(desc(n_doses), no_municipio_paciente)
# 
# 



# aplicacoes <- dados_polio %>%
#   filter(!is.na(nu_idade_paciente)) %>%                  # opcional: descarta idades ausentes
#   mutate(idade_anos = floor(nu_idade_paciente / 12)) %>% # meses -> anos inteiros
#   group_by(
#     no_municipio_paciente,
#     no_municipio_estabelecimento,
#     sg_vacina,
#     co_dose_vacina,
#     idade_anos
#   ) %>%
#   summarise(n_aplicacoes = n(), .groups = "drop") %>%
#   rename(
#     Municipio_Paciente = no_municipio_paciente,
#     Municipio_Estabelecimento = no_municipio_estabelecimento
#   ) %>%
#   arrange(Municipio_Paciente, Municipio_Estabelecimento,
#           sg_vacina, co_dose_vacina, idade_anos)



#sem_registro <- dados %>%
#  # filter(is.na(co_paciente) | co_paciente == "")
# 
# dados_polio2 = filter(dados_polio, co_paciente %in% c("4c88b9637fcf04fbec412beb0fd3a2920b862123d2e0e360a9ee82c9aab6d0a8",
#                                                       "708c68d36f158a3bb651dd4e3e0c5ef188b72fd11d5547fd625e0ab737d918b4"))
# 
# dados_limp1os <- dados_polio %>% 
#   mutate(
#     dt_vacina = as.Date(dt_vacina),
#     ano_mes = format(dt_vacina, "%Y-%m")   
#   ) %>%
#   group_by(co_paciente, sg_vacina) %>% 
#   mutate(dt_ultim_dose = lag(dt_vacina,1)) %>% 
#   mutate(interv_dias = days(dt_vacina - dt_ultim_dose)) %>% 
#   filter(interv_dias >= days(30) | is.na(interv_dias)) %>% 
#   arrange(dt_vacina) %>%                   # ordena por data                          # mantém só a primeira do mês
#   ungroup() %>%
#   select(-ano_mes, -dt_ultim_dose, -interv_dias)                         # remove a coluna auxiliar
# 
# 
# 
# polio_por_munic <- dados_limpos %>%
#   mutate(idade_em_anos = floor(nu_idade_paciente / 12)) %>%  # meses -> anos
#   group_by(no_municipio_paciente, 
#            sg_vacina, co_dose_vacina, idade_em_anos) %>%
#   summarise(n_aplicacoes = n(), .groups = "drop") %>%
#   rename(
#     Municipio_Paciente = no_municipio_paciente,
#    # Municipio_Estabelecimento = no_municipio_estabelecimento
#   ) %>%
#   arrange(Municipio_Paciente, sg_vacina, co_dose_vacina, idade_em_anos)






##### --------------- para a vacina bcg ----------------------------------------


vacina_bcg <- "BCG"

var_inter <- c("co_documento", "co_paciente", "sg_vacina", "co_dose_vacina", 
               "nu_idade_paciente", "ds_dose_vacina", "co_lote_vacina", 
               "dt_vacina", "dt_entrada_rnds", "ds_vacina_fabricante", "no_municipio_estabelecimento",
               "no_municipio_paciente")


processar_dados_bcg <- function(arquivo, vacina_bcg) {
  fread(arquivo, sep = ";", select = var_inter) %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    filter(sg_vacina == vacina_bcg)
}


dados_bcg <- map_dfr(arquivos, processar_dados_bcg, vacina_bcg = vacina_bcg)


dados_bcg <- dados_bcg %>%
  filter(!(is.na(co_paciente) | co_paciente == ""))


dados_bcg <- dados_bcg %>%
  mutate(dt_vacina = as.Date(dt_vacina)) %>%
  arrange(co_paciente, dt_vacina) %>%
  group_by(co_paciente) %>%
  mutate(
    idade_inicial = first(nu_idade_paciente),
    meses_após_primeira = as.integer(difftime(dt_vacina, first(dt_vacina), units = "days") / 30.44),
    idade_estimada = idade_inicial + meses_após_primeira
  ) %>%
  ungroup() %>%
  select(-idade_inicial, -meses_após_primeira) %>%
  relocate(idade_estimada, .after = nu_idade_paciente)





frequencias_bcg <- dados_bcg %>%
  group_by(co_paciente) %>%
  summarise(
    n_doses = n(),  
    vacinas = paste(sg_vacina, collapse = ", "),  
  ) %>%
  arrange(desc(n_doses))



#sem_registro <- dados %>%
  #filter(is.na(co_paciente) | co_paciente == "")



dados_bcg_limpos <- dados_bcg %>%
  mutate(
    dt_vacina = as.Date(dt_vacina),
    ano_mes = format(dt_vacina, "%Y-%m")   
  ) %>%
  group_by(co_paciente, sg_vacina, ano_mes) %>%
  arrange(dt_vacina) %>%
  slice(1) %>%     
  ungroup() %>%
  select(-ano_mes) 




bcg_por_munic <- dados_bcg_limpos %>%
  mutate(idade_em_anos = floor(nu_idade_paciente / 12)) %>%  
  group_by(no_municipio_paciente, no_municipio_estabelecimento, 
           sg_vacina, co_dose_vacina, idade_em_anos) %>%
  summarise(n_aplicacoes = n(), .groups = "drop") %>%
  rename(
    Municipio_Paciente = no_municipio_paciente,
    Municipio_Estabelecimento = no_municipio_estabelecimento
  ) %>%
  arrange(Municipio_Estabelecimento,Municipio_Paciente, sg_vacina, co_dose_vacina, idade_em_anos)







#### -------- investigando pacientes -------------------------------------------

#paciente_alvo <- "15548f4af0d44d22ed3a26f0472ba747cd0ff25ba6615715407b32bae64c5532"


#arquivos <- list.files(diretorio, pattern = "vacinacao_.*_2024.csv", full.names = TRUE)


#process_paciente <- function(arquivo, vacinas_polio, paciente_alvo) {
 # fread(arquivo, sep = ";", select = var_inter) %>%
 #   mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
  #  filter(
  #    sg_vacina %in% vacinas_polio,
   #   co_paciente == paciente_alvo
  #  )
#}

# # Aplica a função a todos os arquivos
# dados_paciente <- map_dfr(
#   arquivos,
#   process_paciente,
#   vacinas_polio = vacinas_polio,
#   paciente_alvo = paciente_alvo
# )


# dados_paciente %>%
#   distinct(nu_idade_paciente, dt_vacina) %>%
#   arrange(dt_vacina)
# 
# 
# 
# paciente <- dados_paciente %>%
#   mutate(dt_vacina = as.Date(dt_vacina)) %>%
#   arrange(co_paciente, dt_vacina) %>%
#   group_by(co_paciente) %>%
#   mutate(
#     idade_inicial = first(nu_idade_paciente),
#     meses_após_primeira = as.integer(difftime(dt_vacina, first(dt_vacina), units = "days") / 30.44),
#     idade_estimada = idade_inicial + meses_após_primeira
#   ) %>%
#   ungroup() %>%
#   select(-idade_inicial, -meses_após_primeira) %>%
#   relocate(idade_estimada, .after = nu_idade_paciente)
# 
# 
# idades = paciente[,c(5,6,9)]
# 
# write_xlsx(dados_paciente, path = "dados_4_paciente_polio_2024.xlsx")

