
## Script de raspagem de dados financeiros do simec e breve análise dos dados

library(tidyverse)
library(rvest)
library(stringr)

locale(date_names = "pt", date_format = "%AD", time_format = "%AT",
       decimal_mark = ",", grouping_mark = ".")

setwd("C:\\Users\\mgaldino\\2017\\Google\\Tá de Pé\\Arquivos")

## importa planilha e pega os ids das urls
simec <- read.table(file="SIMEC_transparenciadeobras_24-07-2017_072402.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")



url <- "http://simec.mec.gov.br/painelObras/recurso.php?obra="

simec <-  simec %>%
  mutate(url_base = url,
         url_final = paste(url_base, ID, sep="")) %>%
  filter(Tipo.da.Obra == "Construção" & !grepl("QUADRA", Tipo.do.Projeto) &
           Rede.de.Ensino.Público == "Municipal" & !grepl("Ampliação", Tipo.do.Projeto),
         !grepl("Reforma", Tipo.do.Projeto))

## raspagem dos dados
obras <- list()
infos <- list()
n <- nrow(simec)
start <- 1 # se houver qualquer erro, basta continaur de onde parou, inicializando star com o último valor de i no loop

for ( i in start:n) {
  download.file(simec$url_final[i], destfile = "scrapedpage.html", quiet=TRUE)
  obras[[i]] <- read_html("scrapedpage.html", encoding = "latin1")
  infos[[i]] <- obras[[i]] %>% 
    html_nodes("table") %>%
    html_table(fill=T)
  Sys.sleep(.05)
  if ( i %% 1000 == 0 ) print(i)
}

vec <- numeric()
for ( i in 1:length(infos)) {
  infos[[i]]$id <- simec$ID[i]
  infos[[i]][grepl("Nenhum resultado encontrado", infos[[i]])] <- NULL
  infos[[i]][grepl("CNPJ", infos[[i]])] <- NULL
  infos[[i]][grepl("Nome da Obra", infos[[i]])] <- NULL
  infos[[i]] <- as.data.frame(infos[[i]])
  vec[i] <- ncol(infos[[i]])
}

simec_fin1 <- bind_rows(infos[vec==4])
simec_fin2 <- bind_rows(infos[vec==5])

lista_objetos <- list(infos, infos_backup, simec_fin1, simec_fin2)


## alternativa de fontes de dados, a investigar.

# http://dados.gov.br/dataset/transferencias-de-recursos-do-poder-executivo-federal-publicadas-no-portal-da-transparencia
# http://www.portaldatransparencia.gov.br/convenios/ConveniosLista.asp?UF=ce&CodMunicipio=1359&CodOrgao=26000&TipoConsulta=1&Periodo=
# http://www.portaldatransparencia.gov.br/convenios/DetalhaConvenio.asp?CodConvenio=667904&TipoConsulta=1&UF=ce&CodMunicipio=1359&CodOrgao=26000&Pagina=&Periodo=
# http://www.portaldatransparencia.gov.br/convenios/DetalhaConvenio.asp?CodConvenio=664113&TipoConsulta=1&UF=ce&CodMunicipio=1359&CodOrgao=26000&Pagina=&Periodo=


