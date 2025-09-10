#### --------------- Análise de doses aplicadas --------------------------------

### ---- Período: janeiro 2024 até dezembro de 204 -----------------------------



library(data.table)
library(dplyr)
library(purrr)
library(writexl)
library(lubridate)
library(ggplot2)

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


#### ---------------- análise do munícipio de SP -------------------------------

dados_sp_vip <- dados_limpos %>%
  filter(no_municipio_paciente == "SAO PAULO",
         sg_vacina == "dTpa/VIP")

ggplot(dados_sp_vip, aes(x = nu_idade_paciente)) +
  geom_histogram(binwidth = 3, fill = "navy", color = "black", alpha = 0.7) +
  stat_bin(binwidth = 3, geom = "text", aes(label = ..count..), 
           vjust = -0.5, color = "black", size = 3) +
  labs(
    title = "",
      x = "Idade",
    y = "Frequência"
  ) +
  theme_classic()


# ------------ rio de janeiro --------------------------------------------------

dados_rj_vip <- dados_limpos %>%
  filter(no_municipio_paciente == "RIO DE JANEIRO",
         sg_vacina == "dTpa/VIP")

ggplot(dados_rj_vip, aes(x = nu_idade_paciente)) +
  geom_histogram(binwidth = 3, fill = "navy", color = "black", alpha = 0.7) +
  stat_bin(binwidth = 3, geom = "text", aes(label = ..count..), 
           vjust = -0.5, color = "black", size = 3) +
  labs(
    title = "",
    x = "Idade",
    y = "Frequência"
  ) +
  theme_classic()






pacientes_100m <- dados_limpos %>%
  filter(nu_idade_paciente > 100) %>%
  select(co_paciente, sg_vacina, co_dose_vacina, 
         nu_idade_paciente, idade_estimada, dt_vacina, no_municipio_paciente) %>%
  arrange(desc(nu_idade_paciente))

# Visualizar as primeiras linhas
head(pacientes_100m, 20)




ggplot(pacientes_100m, aes(x = sg_vacina, y = nu_idade_paciente, fill = sg_vacina)) +
  geom_boxplot(alpha = 0.7, color = "black", outlier.size = 2) + # Ajuste na borda do boxplot
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") + # Linha de média
  labs(
    title = "",
    x = "Tipo de Vacina",
    y = "Idade (em meses)"
  ) +
  scale_fill_brewer(palette = "Set3") + # Paleta de cores mais suave
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1) # Melhora na legibilidade do eixo X
  )



  
  

## ----------------- Alguns Municipios pequenos --------------------------------



municipios_pequenos <- c(
  "ACU", "VARZEDO", "AFONSO BEZERRA", 
  "XIQUE-XIQUE", "WAGNER", "ANTONIO DIAS", 
  "JOAO COSTA", "JOAO DIAS"
)



muni_pequenos <- dados_limpos %>%
  filter(no_municipio_paciente %in% municipios_pequenos)




freq_muni_pequenos <- muni_pequenos %>%
  group_by(co_paciente) %>%
  summarise(
    n_doses = n(),
    vacinas = paste(sg_vacina, collapse = ", "),  # lista todas as vacinas aplicadas
    .groups = "drop"
  ) %>%
  arrange(desc(n_doses))



contagem_por_muni_pequenos <- muni_pequenos %>%
  arrange(co_paciente, sg_vacina, dt_vacina) %>%
  group_by(co_paciente, sg_vacina) %>%
  mutate(freq_acumulada = row_number()) %>%
  ungroup() %>%
  group_by(no_municipio_paciente, sg_vacina, co_dose_vacina, freq_acumulada) %>%
  summarise(qtd_doses = n(), .groups = "drop") %>%
  arrange(no_municipio_paciente, sg_vacina, co_dose_vacina, freq_acumulada)


# ---- Divergências (municípios pequenos) ----
contagem_por_muni_pequenos <- contagem_por_muni_pequenos %>%
  mutate(divergencia_dose = if_else(co_dose_vacina == freq_acumulada, "correta", "divergente"))

divergencias_muni_peq <- contagem_por_munic_pequenos %>%
  group_by(divergencia_dose) %>%
  summarise(
    n_obs = n(),                 
  #  total_doses = sum(qtd_doses)
    .groups = "drop"
  )

















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

