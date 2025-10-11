# As duas bases de dados se referem às 3ª doses aplicadas.
# O arquivo "Cobertura vacinal 24.xlsx" contém:
#  A cobertura vacinal;
#  O numerador (quantidade de doses aplicadas);
#  O denominador (nascidos vivos).


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/Lucas/Downloads/Dados Vacina/Cv_nascidos_vivos e Calendario Vacinal")


# --------------- montagem de cabeçalho em duas linhas -------------------------


raw <- read_excel("Cobertura vacinal 24.xlsx", col_names = FALSE)
hdr1 <- as.character(raw[1, ])
hdr2 <- as.character(raw[2, ])
colnames(raw) <- ifelse(is.na(hdr2) | hdr2 == "", hdr1, paste0(hdr1, "_", hdr2))
dat <- raw[-c(1, 2), ]
colnames(dat)[1] <- "Municipio"

#limpando valores ausentes da coluna de municipios
dat <- dat %>%
  filter(!Municipio %in% c("Município Residência", "Totais"), !is.na(Municipio))


# deixando no formato long
long <- dat %>%
  pivot_longer(cols = -Municipio, names_to = "Nome", values_to = "Valor") %>%
  mutate(
    Nome = as.character(Nome),
    Vacina = sub("_(?!.*_).*", "", Nome, perl = TRUE),
    Tipo_raw = sub(".*_(?!.*_)", "", Nome, perl = TRUE),
    Tipo = case_when(
      str_detect(Tipo_raw, regex("cobertura|%", ignore_case = TRUE)) ~ "Cobertura",
      str_detect(Tipo_raw, regex("doses aplicadas|aplicadas|numerador", ignore_case = TRUE)) ~ "Numerador",
      str_detect(Tipo_raw, regex("popula[cç][aã]o|populacao|denominador|nascidos vivos|nv|alvo", ignore_case = TRUE)) ~ "Denominador",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Tipo)) %>%
  mutate(
    Valor = Valor %>% as.character() %>%
      str_replace_all("[\\s%]", "") %>%
      str_replace_all("\\.", "") %>%
      str_replace_all(",", ".") %>%
      na_if("") %>% as.numeric()
  )

# Agregando duplicidades e pivotando para long
wide <- long %>%
  group_by(Municipio, Vacina, Tipo) %>%
  summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Tipo, values_from = Valor)


# recalculando cv
cv_nv_24 <- wide %>%
  mutate(Cobertura_vacinal = if_else(!is.na(Numerador) & !is.na(Denominador) & Denominador > 0,
                                     (Numerador / Denominador) * 100, NA_real_)) %>%
  select(Municipio, Vacina, Numerador, Denominador, Cobertura_vacinal)


# removendo dfs auxiliares

dfs <- c("dat", "long", "raw", "wide")

rm(list = dfs); gc()





# -------- 3ªs doses aplicadas: calendário vacinal MS --------------------------



terc_doses_aplicadas_cv <- read_excel("3ªs doses aplicadas - calendario vacinal - 2024.xlsx") %>%
  mutate(across(-`Município Residência`, as.character)) %>%
  pivot_longer(
    cols = -`Município Residência`,
    names_to = "Imunobiologico",
    values_to = "Quantidade"
  ) %>%
  mutate(
    Quantidade = Quantidade %>%
      str_replace_all("\\s", "") %>%          # remove espaços
      str_replace_all("[^0-9,\\.]", "") %>%   # remove traços, letras, etc.
      str_replace_all(",", ".") %>%           # vírgula → ponto
      na_if("") %>%                           # vazio vira NA
      as.numeric()
  )







#### --------------- Análise de doses aplicadas --------------------------------

### ---- Período: janeiro 2024 até dezembro de 204 -----------------------------



library(data.table)
library(dplyr)
library(purrr)
library(writexl)
library(lubridate)
library(ggplot2)
library(scales)



setwd("C:\\Users\\Lucas\\Downloads\\Dados Vacina\\Todos os meses")



vacinas_polio <- c("VIP", "dTpa/VIP", "PENTA acelular", "HEXA")


var_inter <- c("co_documento", "co_paciente", "sg_vacina", "co_dose_vacina", 
               "nu_idade_paciente", "ds_dose_vacina", "co_lote_vacina", 
               "dt_vacina", "dt_entrada_rnds", "ds_vacina_fabricante", "no_municipio_estabelecimento",
               "no_municipio_paciente", "co_municipio_paciente")


arquivos <- list.files(pattern = "vacinacao_.*_2024.csv", full.names = TRUE)



processar_dados_polio <- function(arquivo, vacinas_polio) {
  fread(arquivo, sep = ";", select = var_inter) %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    filter(sg_vacina %in% vacinas_polio)
}

# todos os dados em uma lista e juntos
dados_brutos = map_dfr(arquivos, processar_dados_polio, vacinas_polio = vacinas_polio)

nas_vivos_23 = readRDS("nas_vivos_2023-23.rds")

nas_vivos_23 = nas_vivos_23 %>% filter(Ano == 2023)



dados_polio <- dados_brutos %>% 
  filter(!(is.na(co_paciente) | co_paciente == ""))



dados_polio <- dados_polio %>%
  mutate(dt_vacina = as.Date(dt_vacina)) %>%         
  arrange(co_paciente, dt_vacina) %>%                
  group_by(co_paciente) %>%                          
  mutate(
    idade_inicial = first(nu_idade_paciente),        # Idade da primeira dose (em anos)
    meses_após_primeira = as.integer(difftime(dt_vacina, first(dt_vacina), units = "days") / 30.44),
    idade_estimada = round(idade_inicial + (meses_após_primeira / 12), 0)  # Arredondando para 2 casas decimais
  ) %>%
  ungroup() %>%
  select(-idade_inicial, -meses_após_primeira) %>%  # Removendo as colunas auxiliares
  relocate(idade_estimada, .after = nu_idade_paciente) # Colocando a nova coluna após a original


# criando a coluna de dose estimada 
dados_polio <- dados_polio %>%
  arrange(co_paciente, dt_vacina) %>%                
  group_by(co_paciente) %>%                          
  mutate(dose_estimada = row_number()) %>%            
  ungroup() %>%                                     
  relocate(dose_estimada, .after = co_dose_vacina)                                          





dados_limpos <- dados_polio %>%
  mutate(dt_vacina = as.Date(dt_vacina)) %>%
  arrange(co_paciente, sg_vacina, dt_vacina) %>%
  group_by(co_paciente, sg_vacina) %>%
  mutate(intervalo_dias = as.numeric(dt_vacina - lag(dt_vacina))) %>%
  filter(is.na(intervalo_dias) | intervalo_dias >= 30) %>% 
  ungroup()
#select(-intervalo_dias)   



# ----- Duas maneiras para contar qtd de doses aplicadas -----------------------




# contagem_por_muni <- dados_limpos %>%
#   filter(!is.na(no_municipio_paciente) & no_municipio_paciente != "") %>%
#   arrange(co_paciente, dt_vacina) %>%
#   group_by(co_paciente, sg_vacina) %>%  
#   mutate(dose_estimada = row_number()) %>%
#   ungroup() %>%
#   filter(co_dose_vacina == 3 | dose_estimada == 3) %>%
#   mutate(dose_final = 3) %>%   # todas as que passam são consideradas "3ª dose"
#   group_by(no_municipio_paciente, co_municipio_paciente, sg_vacina, dose_final) %>%
#   summarise(qtd_doses = n(), .groups = "drop")



# conta cada paciente somente uma vez
contagem_por_muni <- dados_limpos %>%
  filter(!is.na(no_municipio_paciente) & no_municipio_paciente != "") %>%
  arrange(co_paciente, sg_vacina, dt_vacina) %>%
  group_by(co_paciente, sg_vacina) %>%
  mutate(dose_estimada = row_number()) %>%
  ungroup() %>%
  # mantém apenas se for 3ª dose registrada OU 3ª dose estimada
  filter(co_dose_vacina == 3 | dose_estimada == 3) %>%
  # garante apenas 1 registro por paciente/vacina
  distinct(co_paciente, sg_vacina, .keep_all = TRUE) %>%
  # define dose_final = 3 para todos que passaram no filtro
  mutate(dose_final = 3) %>%
  # conta por município + vacina
  group_by(no_municipio_paciente, co_municipio_paciente, sg_vacina, dose_final) %>%
  summarise(qtd_doses = n(), .groups = "drop")




# ------------------------- COBERTURA VACINA -----------------------------------

cv_muni <- contagem_por_muni %>%
  left_join(
    nas_vivos_23 %>%
      mutate(codigo_municipio = as.numeric(Município)),
    by = c("co_municipio_paciente" = "codigo_municipio")
  ) %>%
  mutate(cv_iqc = round((qtd_doses / Nascidos_vivos), 4))

cv_nv_24 <- cv_nv_24 %>%
  separate(Municipio, into = c("cod_mun", "nome_mun"), sep = " - ") %>%
  mutate(
    codigo_municipio = as.numeric(cod_mun),
    nome_municipio   = stringr::str_to_upper(trimws(nome_mun)),
    Vacina           = trimws(Vacina)
  )

# -------------------- COMPARAÇÃO ----------------------------------------------

# Padronizando cv_muni
cv_muni <- cv_muni %>%
  mutate(
    co_municipio_paciente = as.numeric(co_municipio_paciente),
    Vacina = trimws(sg_vacina)
  )

# Dicionário de vacinas para padronizar nomes
nomes_vacinas <- tibble::tribble(
  ~sg_vacina,       ~Vacina_padronizada,
  "HEXA",           "Penta (DTP/HepB/Hib)",
  "PENTA acelular", "DTP",
  "VIP",            "Polio Injetável (VIP)",
  "dTpa/VIP",       "dTpa Adulto - Gestantes"
)

# Padronizando cv_muni com nomes de vacinas
cv_muni_pad <- cv_muni %>%
  left_join(nomes_vacinas, by = "sg_vacina") %>%
  mutate(
    Vacina = coalesce(Vacina_padronizada, Vacina)
  ) %>%
  select(
    co_municipio_paciente,
    Vacina,
    qtd_doses,
    Ano,
    Nascidos_vivos,
    cv_iqc
  )

# Comparação IQC x MS (join pelo código do município e vacina)
comparacao_cv <- cv_muni_pad %>%
  inner_join(
    cv_nv_24,
    by = c("co_municipio_paciente" = "codigo_municipio",
           "Vacina" = "Vacina"),
    suffix = c("_iqc", "_ms")
  ) %>%
  select(
    co_municipio_paciente,
    nome_municipio,
    Vacina,
    qtd_doses,
    Numerador,
    Denominador,
    cv_iqc,
    Cobertura_vacinal
  )

# -------------------- TERCEIRAS DOSES -----------------------------------------

# Criar tabela única de municípios para evitar many-to-many
municipios_unicos <- cv_nv_24 %>%
  select(codigo_municipio, nome_municipio) %>%
  distinct(nome_municipio, .keep_all = TRUE) %>%   # garante apenas 1 código por nome
  mutate(nome_municipio = stringr::str_to_upper(trimws(nome_municipio)))

# Padronizando base de terceiras doses aplicadas
terc_doses_aplicadas_cv <- terc_doses_aplicadas_cv %>%
  rename(
    nome_municipio = `Município Residência`,
    Vacina_raw     = Imunobiologico
  ) %>%
  mutate(
    nome_municipio = stringr::str_to_upper(trimws(nome_municipio)),
    Vacina_raw     = trimws(Vacina_raw)
  ) %>%
  left_join(municipios_unicos, by = "nome_municipio")

# Dicionário de vacinas para terceiras doses
nomes_vacinas_terc <- tibble::tribble(
  ~Vacina_raw,             ~Vacina_padronizada,
  "PENTA",                 "Penta (DTP/HepB/Hib)",
  "PENTA ACELULAR",        "DTP",
  "DTP",                   "DTP",
  "VIP",                   "Polio Injetável (VIP)",
  "dTpa",                  "dTpa Adulto - Gestantes",
  "dTpa/VIP",              "dTpa Adulto - Gestantes",
  "Polio Injetável (VIP)", "Polio Injetável (VIP)",
  "Penta (DTP/HepB/Hib)",  "Penta (DTP/HepB/Hib)"
)

# Padronizando vacinas e consolidando quantidades
terc_doses_limpo <- terc_doses_aplicadas_cv %>%
  left_join(nomes_vacinas_terc, by = "Vacina_raw") %>%
  mutate(
    Vacina = coalesce(Vacina_padronizada, Vacina_raw)
  ) %>%
  group_by(codigo_municipio, Vacina) %>%
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE), .groups = "drop")

# -------------------- JUNÇÃO FINAL --------------------------------------------

comparacao_cv <- comparacao_cv %>%
  left_join(
    terc_doses_limpo,
    by = c("co_municipio_paciente" = "codigo_municipio",
           "Vacina" = "Vacina")
  )
