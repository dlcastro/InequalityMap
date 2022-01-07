# Author: Daniel Castro - https://dlcastro.com
# Distribuição do Nível Socioeconômico ESCOLAR pelos municípios do Brasil: Escolas Públicas

library(tidyverse)
library(sf)
library(geobr)
library(readxl)
library(viridis)


# DOWNLOAD DADOS ----------------------------------------------------------

# Download Indicador de Nível Socioeconômico (Escolar) de 2019
tmpfile <- tempfile()
download.file("https://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2019/nivel_socioeconomico/INSE_2019_MUNICIPIOS.xlsx",
              tmpfile,
              mode = "wb"
)




# Ler o arquivo de Nível Socioeconômico e corrigir nomes
inse <- readxl::read_excel(tmpfile,
                           skip = 2
) %>%
  rename_with(~ gsub("\\*", "", .)) # Remover o asterisco dos nomes com problema

# Downlaod municípios do Brasil
municipios <- geobr::read_municipality(code_muni = "all", 2019)
estados <- geobr::read_state(code_state = "all", year = 2019)


# Tratamento dos Dados ----------------------------------------------------
inse_selecionado <- inse %>%
  mutate(REDE = case_when(
    TP_TIPO_REDE == 6 ~ "Total Pública",
    TP_TIPO_REDE == 1 ~ "Federal",
    TP_TIPO_REDE == 2 ~ "Estadual",
    TP_TIPO_REDE == 3 ~ "Municipal"
  )) %>%
  # Filtrar apenas informações com Rede e não diferenciar urbana e rural
  filter(
    TP_LOCALIZACAO == 0,
    !is.na(REDE)
  ) %>%
  select(CO_MUNICIPIO, MEDIA_INSE, REDE, TP_TIPO_REDE)


# Unificar banco de dados
municipios_tratado <- municipios %>%
  left_join(inse_selecionado, by = c("code_muni" = "CO_MUNICIPIO"))



# Gerar Mapas -------------------------------------------------------------

# Ajustes visuais no mapa
theme_map <- function(...) {
  theme_minimal(base_size = 15) +
    theme(
      text = element_text(color = "#000000"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#E4E4E4", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(color = "gray20"),
      panel.background = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      # "#E4E4E4"
      panel.border = element_blank(),
      legend.title = element_text(size = 12, face = "bold"),
      legend.position = c(0.17, 0.22),
      legend.margin = margin(r = 5, l = 6, t = 5, b = 5),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = 0.25),
      plot.caption = element_text(hjust = 0),
      ...
    )
}
theme_set(theme_map())



mapa_total <- municipios_tratado %>%
  filter(TP_TIPO_REDE == 6) %>%
  ggplot() +
  geom_sf(aes(fill = MEDIA_INSE), color = NA, size = .15) +
  geom_sf(data = estados, fill = NA) +
  scale_fill_gradientn(
    colours = rev(magma(6)),
    name = "Nível Socioeconômico\nEscolar (INSE)",
    na.value = "gray20"
  ) +
  theme_map() +  
  labs(
    title = "Nível socieconômico médio dos estudantes de\nescola pública por município",
    caption = "Fonte: Daniel Castro - https://dlcastro.com\nDados: Indicador de Nível Socioeconômico (INSE) 2019 - INEP/MEC", size = 8
  )


#Estou tendo problemas com ggsave, utilizarei dev.off como alternativa para exportação
png("Result/public_inequality.png", units="cm", width=18, height=18, res=300)
mapa_total

dev.off()



# Cenários de Escola Municipal, Estadual, Federal e total --------------------------
mapa_grid <- municipios_tratado %>%
  na.omit %>% 
  ggplot() +
  geom_sf(aes(fill = MEDIA_INSE), color = NA, size = .15) +
  geom_sf(data = estados, fill = NA) +
  facet_wrap(REDE~.)+
  scale_fill_gradientn(
    colours = rev(magma(6)),
    name = "Nível Socioeconômico\nEscolar (INSE)",
    na.value = "gray20"
  ) +
  theme_map() + 
  theme(legend.position = "bottom")+
  labs(
    title = "Nível socieconômico médio dos estudantes de\nescola pública por município",
    caption = "Fonte: Daniel Castro - https://dlcastro.com\nDados: Indicador de Nível Socioeconômico (INSE) 2019 - INEP/MEC", size = 8
  )

png("Result/all_public_inequality.png", units="cm", width=21, height=21, res=300)
mapa_grid

dev.off()



