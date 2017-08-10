## Dados externos

## fonte: atlas Brasil 2013 http://www.atlasbrasil.org.br/2013/pt/download/

library(tidyverse)
locale("pt", decimal_mark = ",")
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")

}

#devtools::install_github("sfirke/janitor")
library(janitor)

transform_city_name <- function (city_column, uf_column) 
{
  city <- city_column
  city <- city %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>% 
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>%
    tolower(.) %>% 
    gsub('á', 'a', .) %>%
    gsub('à', 'a', .) %>%
    gsub('â', 'a', .) %>%
    gsub('ã', 'a', .) %>%
    gsub('é', 'e', .) %>%
    gsub('ê', 'e', .) %>%
    gsub('í', 'i', .) %>%
    gsub('ó', 'o', .) %>%
    gsub('ô', 'o', .) %>%
    gsub('õ', 'o', .) %>%
    gsub('ú', 'u', .) %>%
    gsub('ç', 'c', .) %>%
    gsub("`", '', .) 
  
  uf <- uf_column %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>% 
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>%
    tolower(.) %>% 
    gsub('á', 'a', .) %>%
    gsub('à', 'a', .) %>%
    gsub('â', 'a', .) %>%
    gsub('ã', 'a', .) %>%
    gsub('é', 'e', .) %>%
    gsub('ê', 'e', .) %>%
    gsub('í', 'i', .) %>%
    gsub('ó', 'o', .) %>%
    gsub('ô', 'o', .) %>%
    gsub('õ', 'o', .) %>%
    gsub('ú', 'u', .) %>%
    gsub('ç', 'c', .) %>%
    gsub("`", '', .) 
  
  city_uf <- city %>%
    paste(uf, sep="_")

}

locale("pt",  decimal_mark = ",")

setwd("C:\\Users\\mgaldino\\2017\\Google\\Ta de Pe\\Arquivos")

atlas <- read_delim("dados_atlas_munic.txt", delim="\t")

atlas <- atlas %>%
  clean_names()

simec <- read.table(file="SIMEC_transparenciadeobras_24-07-2017_072402.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")

load("atrasadas_pro_manoel.RData")
simec_simples <- atrasadas_pro_manoel %>%
  clean_names()


simec <- simec %>%
  clean_names()

codigo_uf <- read_delim("codigo_uf.txt", delim="\t")
  
codigo_uf <- codigo_uf %>%
  clean_names()

simec1 <- simec %>%
  select(id, municipio, uf) %>%
  mutate( municipio = case_when(municipio == "Balneário Rincão" ~ "Içara",
                                TRUE ~ municipio),
          cidade_uf = transform_city_name(municipio, uf))


atlas1 <- atlas %>%
  select(ano, uf, codmun6, codmun7, municipio, mort5, e_anosestudo,
         t_analf11a14, t_atraso_0_basico, t_atraso_0_fund, t_fbbas,
         t_fbfund, t_fbpre, t_freq0a3, t_freq4a6, t_freq6a14,
         gini, pind, pindcri, pmpob, pmpobcri, rdpc, homem0a4,
         homem5a9, mulh0a4, mulh5a9, pesotot, i_freq_prop, idhm, idhm_e) %>%
  filter(ano == 2010) %>%
  mutate(municipio = case_when(municipio == "EMBU" ~ "Embu das Artes",
                               municipio == "SANTARÉM" & uf == 25 ~ "JOCA CLAUDINO",
                               municipio == "PAU D'ARCO" & uf == 17 ~ "PAU DARCO",
                               municipio == "PAU D'ARCO" & uf == 15 ~ "PAU D ARCO",
                               municipio == "MOJI MIRIM" ~ "MOGI MIRIM",
                               municipio == "MACHADINHO D'OESTE" ~ "MACHADINHO D OESTE",
                               TRUE ~ municipio)) %>%
  inner_join(codigo_uf, by="uf") %>%
  mutate(cidade_uf = transform_city_name(municipio, sigla))

#      pescaria brava_sc ## faltando
#   paraiso das aguas_ms ## faltando

simec_atlas <- simec1 %>%
  full_join(select(atlas1, -(uf:municipio)), by = "cidade_uf") %>%
  left_join(simec_simples, by='id')
  
idh <- simec_atlas %>%
  mutate(contador = ifelse(is.na(id), 0, 1),
         idhm = as.numeric(gsub(",", "\\.", idhm)),
         t_fbpre = as.numeric(gsub(",", "\\.", t_fbpre))) %>%
  filter(!is.na(idhm)) %>%
  group_by(cidade_uf, situacao) %>%
  summarise(num_obras = sum(contador),
            idhm = max(idhm),
            rdpc = max(rdpc),
            t_fbpre = max(t_fbpre),
            tempo_de_atraso = mean(as.numeric(tempo_de_atraso)),
            pagamento_cte_jun17 = mean(pagamento_cte_jun17)) %>%
  filter(!is.na(tempo_de_atraso)) #3 olhar pq está precisando disso

head(idh)
with(idh, cor(num_obras, t_fbpre))
with(idh, plot(num_obras ~ idhm))

summary(idh)
library(ggplot2)
idh %>%
  filter(num_obras < 100 & num_obras > 0) %>%
ggplot( aes(y=num_obras, x=t_fbpre)) + geom_point() +
  geom_smooth(method="lm") + facet_wrap(~ situacao)


idh %>%
  filter(num_obras < 100 & num_obras > 0) %>%
  ggplot( aes(y=tempo_de_atraso, x=num_obras)) + geom_point() +
  geom_smooth(method="lm") + facet_wrap(~ situacao)

idh %>%
  filter(num_obras < 100 & num_obras > 0 & pagamento_cte_jun17 < 10000000) %>%
  ggplot( aes(y=tempo_de_atraso, x=pagamento_cte_jun17)) + geom_point() +
  geom_smooth(method="lm") + facet_wrap(~ situacao)
