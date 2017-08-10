### Script para webscrapgin do que estava faltando 

library(tidyverse)
library(rvest)
library(stringr)

locale(date_names = "pt", date_format = "%AD", time_format = "%AT",
       decimal_mark = ",", grouping_mark = ".")

setwd("C:\\Users\\mgaldino\\2017\\Google\\Tá de Pé\\Arquivos")

## importa planilha e pega os ids das urls
simec <- read.table(file="obras28072017.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")


url <- "http://simec.mec.gov.br/painelObras/recurso.php?obra="

simec <-  simec %>%
  mutate(url_base = url,
         url_final = paste(url_base, ID, sep="")) %>%
  filter(Tipo.da.Obra == "Construção" & !grepl("QUADRA", Tipo.do.Projeto) &
           Rede.de.Ensino.Público == "Municipal" & !grepl("Ampliação", Tipo.do.Projeto),
         !grepl("Reforma", Tipo.do.Projeto))

Tipo.do.Projeto1 <- c("Escola de Educação Infantil Tipo B",
                      "Escola de Educação Infantil Tipo C",
                      "MI - Escola de Educação Infantil Tipo B",
                      "MI - Escola de Educação Infantil Tipo C",
                      "Espaço Educativo - 12 Salas",
                      "Espaço Educativo - 01 Sala",
                      "Espaço Educativo - 02 Salas",
                      "Espaço Educativo - 04 Salas",
                      "Espaço Educativo - 06 Salas",
                      "Projeto 1 Convencional",
                      "Projeto 2 Convencional")

valid <- simec %>%
  filter(!is.na(Situação), 
         Tipo.da.Obra == "Construção", 
         Tipo.do.Projeto %in% Tipo.do.Projeto1) %>%
  left_join(pagamento_simec, by = c("ID" = "id")) %>%
  filter(is.na(primeira_data))
         

## raspagem dos dados
obras <- list()
infos <- list()
n <- nrow(valid)
start <- 1 # se houver qualquer erro, basta continaur de onde parou, inicializando star com o último valor de i no loop

for ( i in start:n) {
  download.file(valid$url_final[i], destfile = "scrapedpage.html", quiet=TRUE)
  obras[[i]] <- read_html("scrapedpage.html", encoding = "latin1")
  infos[[i]] <- obras[[i]] %>% 
    html_nodes("table") %>%
    html_table(fill=T)
  Sys.sleep(.001)
  if ( i %% 500 == 0 ) print(i)
}

vec <- numeric()
for ( i in 1:length(infos)) {
  infos[[i]]$id <- valid$ID[i]
  infos[[i]][grepl("Nenhum resultado encontrado", infos[[i]])] <- NULL
  infos[[i]][grepl("CNPJ", infos[[i]])] <- NULL
  infos[[i]][grepl("Nome da Obra", infos[[i]])] <- NULL
  infos[[i]] <- as.data.frame(infos[[i]])
  vec[i] <- ncol(infos[[i]])
}

simec_fin1 <- bind_rows(infos[vec==4])
simec_fin2 <- bind_rows(infos[vec==5])

pagamentos_simec1 <- simec_fin1 %>%
  mutate(Valor.do.Pagamento = str_trim(Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("R\\$ ", "", Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("\\.", "", Valor.do.Pagamento),
         Valor.do.Pagamento = as.numeric(gsub(",", "\\.", Valor.do.Pagamento)),
         Data.de.Pagamento = as.Date(Data.de.Pagamento, "%d/%m/%Y")) %>%
  group_by(id) %>%
  summarise(pagamento = sum(Valor.do.Pagamento),
            primeira_data = min(Data.de.Pagamento),
            ultima_data = max(Data.de.Pagamento))

pagamentos_simec2 <- simec_fin2 %>%
  mutate(Valor.Repassado = str_trim(Valor.Repassado),
         Valor.Repassado = gsub("R\\$ ", "", Valor.Repassado),
         Valor.Repassado = gsub("\\.", "", Valor.Repassado),
         Valor.Repassado = as.numeric(gsub(",", "\\.", Valor.Repassado)),
         Data.do.Repasse = as.Date(Data.do.Repasse, "%d/%m/%Y")) %>%
  group_by(id) %>%
  summarise(pagamento = sum(Valor.Repassado),
            primeira_data = min(Data.do.Repasse),
            ultima_data = max(Data.do.Repasse))

# juntando as duas tabelas
pagamento_simec_complemento <- bind_rows(pagamentos_simec1, pagamentos_simec2)

save(pagamento_simec_complemento, file="pagamento_simec_complemento.RData")


lista_objetos <- list(infos, infos_backup, simec_fin1, simec_fin2)

## validação

length(unique(valid$url_final)) # 3189
teste <- bind_rows(infos)
head(teste)
head(simec_fin1)

head(infos[lengths(infos)>1])

length(unique(pagamento_simec_complemento$id))
