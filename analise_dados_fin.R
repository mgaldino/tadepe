## Análise dos dados pós-raspagem

library(tidyverse)
library(stringr)


setwd("C:\\Users\\mgaldino\\2017\\Google\\Tá de Pé\\Arquivos")
load("arquivos_simec_fin_v2.RData")

simec_fin1 <- lista_objetos[[3]]
simec_fin2 <- lista_objetos[[4]]

simec <- read.table(file="SIMEC_transparenciadeobras_24-07-2017_072402.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")

## números rápidos
# fonte dos dados do ipca http://dadosabertos.bcb.gov.br/dataset/4447-indice-nacional-de-precos-ao-consumidor-amplo-ipca---comercializaveis

ipca <- read_csv2("bcdata.sgs.4447.csv")
ipca <- bind_rows(ipca, data.frame(data="01/07/2017", valor=0))
ipca <- ipca %>%
  mutate(data= as.Date(data, "%d/%m/%Y"),
         mes_ano = format(data, "%m/%Y")) %>%
  filter(data > as.Date("2007-01-01")) %>%
  mutate(indice = cumprod(1+valor/100),
         indice_max = last(indice),
         indice = indice/indice_max)
           


# criando indice para atualizar para junho de 2017

# ajustando formato dos dados (gasto e data)
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
pagamento_simec <- bind_rows(pagamentos_simec1, pagamentos_simec2)

pagamento_simec <- pagamento_simec %>%
  mutate(mes_ano = format(primeira_data, "%m/%Y"))

# descontando inflação
pagamento_simec_inflacao <- pagamento_simec %>%
  left_join(ipca, by="mes_ano") %>%
  mutate(pagamento_cte_jun17 = pagamento/indice)


pagamento_simec_inflacao %>%
  summarise(gasto_jun17 = sum(pagamento_cte_jun17),
            gasto_burro = sum(pagamento))

# total gasto por ano (valores correntes)
pagamento_simec_inflacao %>%
  mutate(ano = format(primeira_data, "%Y")) %>%
  group_by(ano) %>%
  summarise(x=sum(pagamento_cte_jun17),
            y=sum(pagamento)) %>%
  summarise(sum(x), sum(y))

# juntando com tabela original do simec
simec_gastos <- simec %>%
  inner_join(simec_fin_simples1, by = c("ID" = "id")) 

# total de gasto por situação, com soma burra (sem considerar inflação)
simec_gastos %>%
  group_by(Situação) %>%
  summarise(sum(pagamento_cte_jun17))