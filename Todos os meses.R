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




#dados_polio = dados_polio[,-6]




contagem_por_muni <- dados_limpos %>%
  filter(!is.na(no_municipio_paciente) & no_municipio_paciente != "") %>%
  arrange(co_paciente, dt_vacina) %>%
  group_by(co_paciente) %>%
  mutate(dose_estimada = row_number()) %>%
  ungroup() %>%
  group_by(no_municipio_paciente, sg_vacina, co_dose_vacina, dose_estimada) %>%
  summarise(qtd_doses = n(), .groups = "drop") %>%
  arrange(no_municipio_paciente, co_dose_vacina, dose_estimada, sg_vacina) %>% 
  mutate(divergencia_dose = if_else(co_dose_vacina == dose_estimada, 
                                    "correta", "divergente"))





contagem_por_muni_ABD_GO <- dados_limpos %>%
  filter(no_municipio_paciente == "ABADIA DE GOIAS",
         !is.na(no_municipio_paciente), 
         no_municipio_paciente != "") %>%
  arrange(co_paciente, dt_vacina) %>%
  group_by(co_paciente) %>%
  mutate(dose_estimada = row_number()) %>%
  ungroup() %>%
  group_by(no_municipio_paciente, sg_vacina, co_dose_vacina, dose_estimada) %>%
  summarise(qtd_doses = n(), .groups = "drop") %>%
  arrange(no_municipio_paciente, co_dose_vacina, dose_estimada, sg_vacina) %>%
  mutate(divergencia_dose = if_else(co_dose_vacina == dose_estimada, 
                                    "correta", "divergente"))

abd_go_sem3dose <- contagem_por_muni_ABD_GO %>%
  filter(dose_estimada != 3)