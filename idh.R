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

head(atlas)


simec_atraso <- obras_situacao_tb %>%
  clean_names()

codigo_uf <- read_delim("codigo_uf.txt", delim="\t")

codigo_uf <- codigo_uf %>%
  clean_names()

simec1 <- simec_atraso %>%
  select(id, municipio, uf, paralisada_tb, atrasada, obra_a_ser_entregue) %>%
  mutate( municipio = case_when(municipio == "Balneário Rincão" ~ "Içara",
                                TRUE ~ municipio),
          cidade_uf = transform_city_name(municipio, uf))


atlas1 <- atlas %>%
  select(ano, uf,  municipio, idhm, idhm_e, idhm_l, idhm_r, pop) %>%
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
  full_join(select(atlas1, -(uf:municipio)), by = "cidade_uf") 

my_idh_mean <- function(rpc, educ, long, pop) {
  rpc_aux <- mean(rpc)
  educ_aux <- sum(educ*pop)/sum(pop)
  long_aux <- sum(long*pop)/sum(pop)
  idh <- (rpc_aux*educ_aux*long_aux)^(1/3)
}


idh <- simec_atlas %>%
  mutate(contador = ifelse(is.na(id), 0, 1),
         idhm = as.numeric(gsub(",", "\\.", idhm)),
         idhm_e = as.numeric(gsub(",", "\\.", idhm_e)),
         idhm_l = as.numeric(gsub(",", "\\.", idhm_l)),
         idhm_r = as.numeric(gsub(",", "\\.", idhm_r))) %>%
  filter(!is.na(idhm)) %>%
  group_by(uf) %>%
  summarise(num_obras = sum(contador),
            num_paralisadas= sum(paralisada_tb == "paralisada"),
            idh_uf = mean(idhm),
            idh_uf_correto = my_idh_mean(educ=idhm_e, long=idhm_l, rpc=idhm_r, pop),
            perc = num_paralisadas/num_obras) 

idh %>%
  filter(!is.na(perc) & !is.na(idh_uf)) %>%
  summarise(cor(perc, idh_uf_correto))


head(idh)
idh_uf_chart <- ggplot(idh, aes(idh_uf_correto, perc, label = uf)) + 
  geom_smooth(method="lm", se=F) +
geom_text(aes(label=uf, size=2),hjust=0, vjust=0) + theme_bw() + 
  theme(legend.position = "none") +
  ylab("percentual de obras paralisadas") + xlab("IDH") +
  scale_y_continuous(label=percent, lim=c(0, .5)) + 
  scale_x_continuous(label=percent, lim=c(.6, .9)) +  guides(fill=FALSE)

