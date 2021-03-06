# Rela��o entere IDH e percentual de obras atrasadas por UF

# instalar vers�o do github (usando devtools)
library(janitor)

# fun��o que criei
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
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'e', .) %>%
    gsub('�', 'e', .) %>%
    gsub('�', 'i', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'u', .) %>%
    gsub('�', 'c', .) %>%
    gsub("`", '', .) 
  
  uf <- uf_column %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>% 
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>%
    tolower(.) %>% 
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'a', .) %>%
    gsub('�', 'e', .) %>%
    gsub('�', 'e', .) %>%
    gsub('�', 'i', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'o', .) %>%
    gsub('�', 'u', .) %>%
    gsub('�', 'c', .) %>%
    gsub("`", '', .) 
  
  city_uf <- city %>%
    paste(uf, sep="_")
  
}

# seta locale
locale("pt",  decimal_mark = ",")

setwd("C:\\Users\\mgaldino\\2017\\Google\\Ta de Pe\\Arquivos")

# carrega dados municipais
atlas <- read_delim("dados_atlas_munic.txt", delim="\t")

# limpa nomes das colunas
atlas <- atlas %>%
  clean_names()

# cria um df com nomes das colunas
simec_atraso <- obras_situacao_tb %>%
  clean_names()

# importa tabela com siglas das ufs, para fazer join
codigo_uf <- read_delim("codigo_uf.txt", delim="\t")

# limpa nome das colunas
codigo_uf <- codigo_uf %>%
  clean_names()

# corrige nomes de munic�pios no simec, para fazer o join correto
simec1 <- simec_atraso %>%
  select(id, municipio, uf, paralisada_tb, atrasada, obra_a_ser_entregue) %>%
  mutate( municipio = case_when(municipio == "Balne�rio Rinc�o" ~ "I�ara",
                                TRUE ~ municipio),
          cidade_uf = transform_city_name(municipio, uf))

# corrige nome dos munic�pios do atlas para fazer o join
# faz o join por enquanto s� com codigo_uf, para ter sigla
atlas1 <- atlas %>%
  select(ano, uf,  municipio, idhm, idhm_e, idhm_l, idhm_r, pop) %>%
  filter(ano == 2010) %>%
  mutate(municipio = case_when(municipio == "EMBU" ~ "Embu das Artes",
                               municipio == "SANTAR�M" & uf == 25 ~ "JOCA CLAUDINO",
                               municipio == "PAU D'ARCO" & uf == 17 ~ "PAU DARCO",
                               municipio == "PAU D'ARCO" & uf == 15 ~ "PAU D ARCO",
                               municipio == "MOJI MIRIM" ~ "MOGI MIRIM",
                               municipio == "MACHADINHO D'OESTE" ~ "MACHADINHO D OESTE",
                               TRUE ~ municipio)) %>%
  inner_join(codigo_uf, by="uf") %>%
  mutate(cidade_uf = transform_city_name(municipio, sigla))

# cidades exclu�das do join
#      pescaria brava_sc ## faltando
#   paraiso das aguas_ms ## faltando

# join com simec
simec_atlas <- simec1 %>%
  full_join(select(atlas1, -(uf:municipio)), by = "cidade_uf") 

# cria fun��o que gera meu idh com pondera��o
my_idh_mean <- function(rpc, educ, long, pop) {
  rpc_aux <- mean(rpc)
  educ_aux <- sum(educ*pop)/sum(pop)
  long_aux <- sum(long*pop)/sum(pop)
  idh <- (rpc_aux*educ_aux*long_aux)^(1/3)
}

#3 cria tabela com idh por uf
idh <- simec_atlas %>%
  mutate(contador = ifelse(is.na(id), 0, 1),
         idhm = as.numeric(gsub(",", "\\.", idhm)),
         idhm_e = as.numeric(gsub(",", "\\.", idhm_e)),
         idhm_l = as.numeric(gsub(",", "\\.", idhm_l)),
         idhm_r = as.numeric(gsub(",", "\\.", idhm_r))) %>%
  filter(!is.na(idhm)) %>%
  group_by(uf) %>%
  summarise(num_obras = sum(obra_a_ser_entregue == "sim"),
            num_paralisadas= sum(paralisada_tb == "paralisada"),
            num_atrasadas= sum(atrasada == "sim"),
            idh_uf = mean(idhm),
            idh_uf_correto = my_idh_mean(educ=idhm_e, long=idhm_l, rpc=idhm_r, pop),
            perc = num_paralisadas/num_obras,
            perc_atrasadas = num_atrasadas/num_obras) 

# calcula correla��o
# filtro NA pq entrou um na a� (acho que cidades q n�o deram join)
idh %>%
  filter(!is.na(perc) & !is.na(idh_uf)) %>%
  summarise(cor(perc, idh_uf_correto))


# chart scatterplot idh e percentual de obra paralisada
idh_uf_chart <- ggplot(idh, aes(idh_uf_correto, perc, label = uf)) + 
  geom_smooth(method="lm", se=F) +
geom_text(aes(label=uf,  alpha=.8), size=3,hjust=0, vjust=0) + theme_bw() + 
  theme(legend.position = "none") +
  ylab("percentual de obras paralisadas") + xlab("IDH") +
  scale_y_continuous(label=percent, lim=c(0, .5)) + 
  scale_x_continuous(lim=c(.6, .85)) +  guides(fill=FALSE)

ggsave(idh_uf_chart, file="idh_e_obras_paralisadas.png", 
       height = 8, width= 14, scale=.5)


## Idh e atraso
idh %>%
  filter(!is.na(perc) & !is.na(idh_uf)) %>%
  summarise(cor(perc_atrasadas, idh_uf_correto))


idh_uf_chart_atraso <- ggplot(idh, aes(idh_uf_correto, perc_atrasadas, label = uf)) + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=uf,  alpha=.8), size=3,hjust=0, vjust=0) + theme_bw() + 
  theme(legend.position = "none") +
  ylab("percentual de obras paralisadas") + xlab("IDH") +
  scale_y_continuous(label=percent, lim=c(.2, .7)) + 
  scale_x_continuous(lim=c(.6, .85)) +  guides(fill=FALSE)

ggsave(idh_uf_chart_atraso, file="idh_e_obras_atrasadas.png", 
       height = 8, width= 14, scale=.5)
