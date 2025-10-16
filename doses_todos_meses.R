#### --------------- Análise de doses aplicadas --------------------------------

### ---- Período: janeiro 2024 até dezembro de 204 -----------------------------



library(data.table)
library(dplyr)
library(purrr)
library(writexl)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)



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
  mutate(cv_iqc = round((100*(qtd_doses / Nascidos_vivos)), 4))

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



glimpse(comparacao_cv)










# ==============================================================================

estat_vacina <- comparacao_cv %>%
  group_by(Vacina) %>%
  summarise(
    n_municipios = n_distinct(nome_municipio),
    total_doses = sum(qtd_doses, na.rm = TRUE),
    media_cv_iqc = mean(cv_iqc, na.rm = TRUE),
    media_cv_ms = mean(Cobertura_vacinal, na.rm = TRUE),
    correlacao_iqc_ms = cor(cv_iqc, Cobertura_vacinal, use = "complete.obs")
  ) %>%
  arrange(desc(total_doses))




# 1) Preparar dados: filtrar Polio Injetável (VIP) e calcular diferença
top_diff_doses <- comparacao_cv %>%
  filter(Vacina == "Polio Injetável (VIP)") %>%
  filter(!is.na(qtd_doses) & !is.na(Numerador)) %>%
  mutate(
    diff = qtd_doses - Numerador,
    absdiff = abs(diff),
    nome_municipio = str_to_title(nome_municipio)
  ) %>%
  arrange(desc(absdiff))

if (nrow(top_diff_doses) == 0)
  stop("Não há observações para 'Polio Injetável (VIP)' com qtd_doses e Numerador válidos.")

# 2) Selecionar top 10 municípios com maior diferença absoluta
n_top <- min(10, nrow(top_diff_doses))
top_n <- top_diff_doses %>% slice_head(n = n_top)

# 3) Converter para formato longo
plot_data <- top_n %>%
  select(nome_municipio, qtd_doses, Numerador, diff, absdiff) %>%
  pivot_longer(
    cols = c("qtd_doses", "Numerador"),
    names_to = "Fonte", values_to = "Doses"
  ) %>%
  mutate(
    Fonte = recode(
      Fonte,
      "qtd_doses" = "IQC (Nós)",
      "Numerador" = "Painel CV - Residência"
    )
  )

# 4) Ordem dos municípios por diferença (maior diferença em cima)
ordem <- top_n %>% arrange(desc(absdiff)) %>% pull(nome_municipio)
plot_data <- plot_data %>%
  mutate(nome_municipio = factor(nome_municipio, levels = rev(ordem)))

# 5) Plot
# deslocamento dinâmico (em função da escala do eixo x)
offset <- max(plot_data$Doses, na.rm = TRUE) * 0.007



# preparar dados para textos IQC e MS
iqc_labels <- plot_data %>%
  filter(Fonte == "IQC (Nós)") %>%
  left_join(
    plot_data %>%
      filter(Fonte == "Painel CV - Residência") %>%
      select(nome_municipio, Doses_MS = Doses),
    by = "nome_municipio"
  ) %>%
  mutate(nudge = ifelse(abs(Doses - Doses_MS) < offset * 8, -offset * 4, -offset))

ms_labels <- plot_data %>%
  filter(Fonte == "Painel CV - Residência") %>%
  left_join(
    plot_data %>%
      filter(Fonte == "IQC (Nós)") %>%
      select(nome_municipio, Doses_IQC = Doses),
    by = "nome_municipio"
  ) %>%
  mutate(nudge = ifelse(abs(Doses - Doses_IQC) < offset * 8, offset * 4, offset))

# plot
p <- ggplot(plot_data, aes(x = Doses, y = nome_municipio, group = nome_municipio)) +
  geom_line(color = "gray70", size = 0.6) +
  geom_point(aes(color = Fonte, shape = Fonte), size = 3) +
  geom_text(
    data = iqc_labels,
    aes(label = comma(Doses, big.mark = ".", decimal.mark = ",")),
    position = position_nudge(x = iqc_labels$nudge),
    size = 2.6, hjust = 1, show.legend = FALSE
  ) +
  geom_text(
    data = ms_labels,
    aes(label = comma(Doses, big.mark = ".", decimal.mark = ",")),
    position = position_nudge(x = ms_labels$nudge),
    size = 2.6, hjust = 0, show.legend = FALSE
  ) +
  scale_x_continuous(
    name = "Doses Aplicadas",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    limits = c(0, max(plot_data$Doses, na.rm = TRUE) * 1.15)
  ) +
  scale_color_manual(values = c("IQC (Nós)" = "#1b9e77", "Painel CV - Residência" = "#d95f02")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(
    y = NULL,
    #title = paste0("Top ", n_top, " municípios com maior diferença no número de doses — VIP"),
    #subtitle = "Comparação: Doses estimadas por nós (IQC) vs Painel CV - Residência (MS)",
    color = "", shape = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

print(p)

