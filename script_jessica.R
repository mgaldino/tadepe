## Análise dos dados pós-raspagem

library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(raster) # Para baixar o polígono 426
library(rvest) # Para importar a base de dados
library(viridis) # Para selecionar uma bonita paleta de cores
library(tmap) # Para plotar o mapa
library(tmaptools)

#Endereço para achar as obras: http://simec.mec.gov.br/painelObras/dadosobra.php?obra=
setwd("C:\\Users\\mgaldino\\2017\\Google\\Ta de Pe\\relatorio\\tadepe")
load("arquivos_simec_fin_v2.RData")
load("pagamento_simec_complemento.RData")

simec_fin1 <- lista_objetos[[3]]
simec_fin2 <- lista_objetos[[4]]

simec <- read.table(file="obras28072017.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")

## números rápidos
# fonte dos dados do ipca http://dadosabertos.bcb.gov.br/dataset/4447-indice-nacional-de-precos-ao-consumidor-amplo-ipca---comercializaveis

# criando indice para atualizar para junho de 2017
ipca <- read_csv2("bcdata.sgs.4447.csv")
ipca <- bind_rows(ipca, data.frame(data="01/07/2017", valor=0))
ipca <- ipca %>%
  mutate(data= as.Date(data, "%d/%m/%Y"),
         mes_ano = format(data, "%m/%Y")) %>%
  filter(data > as.Date("2007-01-01")) %>%
  mutate(indice = cumprod(1+valor/100),
         indice_max = last(indice),
         indice = indice/indice_max)

simec <- simec %>%
  mutate(Data.de.Assinatura.do.Contrato = as.Date(Data.de.Assinatura.do.Contrato),
         mes_ano_assinatura_contrato = format(Data.de.Assinatura.do.Contrato, "%m/%Y"))


head(simec)


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
            segunda_data = rev(sort(Data.de.Pagamento)[2]),
            ultima_data = max(Data.de.Pagamento))

pagamentos_simec2 <- simec_fin2 %>%
  mutate(Valor.Repassado = str_trim(Valor.Repassado),
         Valor.Repassado = gsub("R\\$ ", "", Valor.Repassado),
         Valor.Repassado = gsub("\\.", "", Valor.Repassado),
         Valor.Repassado = as.numeric(gsub(",", "\\.", Valor.Repassado)),
         Data.do.Repasse = as.Date(Data.do.Repasse, "%d/%m/%Y")) %>%
  group_by(id) %>%
  summarise(pagamento = sum(Valor.Repassado),
            primeira_data = min(Data.do.Repasse),      #primeira data registrada do repasse
            ultima_data = max(Data.do.Repasse))        #ultima data registrada do repasse

# juntando as duas tabelas
pagamento_simec <- bind_rows(pagamentos_simec1, pagamentos_simec2, pagamento_simec_complemento)

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
  left_join(pagamento_simec_inflacao, by = c("ID" = "id")) %>%
  mutate(mes_ano_assinatura_contrato = ifelse(is.na(mes_ano_assinatura_contrato), 
                                              primeira_data, mes_ano_assinatura_contrato)) %>%
  full_join(ipca, by=c("mes_ano_assinatura_contrato" = "mes_ano")) %>%
  mutate(valor_pactuado_fnde_cte_jun17 = Valor.Pactuado.com.o.FNDE/indice.y)

## filtrando para projetos que entram na análise


Tipo.do.Projeto <- c("Escola de Educação Infantil Tipo B",
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

tempo_exe_meses <- c(9,6,6,4,13,5,5,7,7,11,9)

execucao <- data.frame(Tipo.do.Projeto, tempo_exe_meses)

# criando df filtrado por tipo de projeto, construção (contém algumas obras sem dado de gasto, pois webscraping n achou nada)
simec_atraso <- simec_gastos %>%   #2
  filter(Tipo.da.Obra == "Construção") %>%
  inner_join(execucao)

simec_atraso$Data.Prevista.de.Conclusão.da.Obra <- as.Date(simec_atraso$Data.Prevista.de.Conclusão.da.Obra , "%d/%m/%Y")
simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município <- as.Date(simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município , "%Y-%m-%d")
simec_atraso$primeira_data <- as.Date(simec_atraso$primeira_data ,  "%Y-%m-%d")
simec_atraso$Data.de.Assinatura.do.Contrato <- as.Date(simec_atraso$Data.de.Assinatura.do.Contrato, "%Y-%m-%d")

simec_atraso <- simec_atraso %>%
  mutate(tempo_exe_dias = tempo_exe_meses*30,
         data_concluida = as.Date("01-01-2017"),
         data_concluida = case_when(is.na(Data.Prevista.de.Conclusão.da.Obra) ~
                                      Data.da.Última.Vistoria.do.Estado.ou.Município,
                                    TRUE ~ Data.Prevista.de.Conclusão.da.Obra),
         ano_concluida = format(data_concluida, "%Y"),
         tempo_exec_real = as.numeric(data_concluida - Data.de.Assinatura.do.Contrato))

mean(simec_atraso$tempo_exec_real[simec_atraso$Situação == "Concluída"], na.rm=TRUE)
#Tempo de execução médio das obras já concluídas foi de 860 dias ou mais de 2 anos

# total de gasto por situação

simec_gastos_tb <- simec_atraso %>%
  group_by(Situação) %>%
  summarise(gasto = sum(pagamento_cte_jun17, na.rm=T),
            num_obras = n(),
            pactuado = sum(valor_pactuado_fnde_cte_jun17, na.rm=T),
            medio_pactuado = mean(valor_pactuado_fnde_cte_jun17, na.rm=T)) %>%
  bind_rows(data.frame(Situação = "total", 
                       gasto = sum(simec_atraso$pagamento_cte_jun17, na.rm=T),
                       num_obras = length(simec_atraso$ID),
                       pactuado = sum(simec_atraso$valor_pactuado_fnde_cte_jun17, na.rm=T))) %>%
  ungroup() %>%
  mutate(perc_gasto_realizado = round(gasto/max(gasto),2),
         perc_obras = round(num_obras/max(num_obras),2),
         perc_gasto_pactuado = round(pactuado/max(pactuado),2))


write.table(simec_gastos_tb, file="simec_gastos_tb.csv", sep=";", row.names = FALSE,
            dec = ",")

######################################################################################################
#Jessica começou a partir daqui 

#1 Tabela de obras do proinfância e situação de cada uma das obras



#Existem 4728 obras a serem entregues pelo proinfância

#2. Calculando tempo de duração das obras

#teste se podemos usar tanto Data.Prevista.de.Conclusão.da.Obra quanto Data.da.Última.Vistoria.do.Estado.ou.Município

#Observação : teremos como data de término oficial da obra Data.Prevista.de.Conclusão.da.Obra |
# Data.da.Última.Vistoria.do.Estado.ou.Município

#3. Atraso das obras concluidas

simec_atraso$Data.Prevista.de.Conclusão.da.Obra <- as.Date(simec_atraso$Data.Prevista.de.Conclusão.da.Obra)

simec_atraso_concluidas <- simec_atraso %>%
  filter(Situação == "Concluída") %>%
  mutate(data_ideal = Data.de.Assinatura.do.Contrato + tempo_exe_dias,
         data_final_gov = Data.Prevista.de.Conclusão.da.Obra,
         data_final_gov = case_when(
           is.na(Data.Prevista.de.Conclusão.da.Obra) ~ Data.da.Última.Vistoria.do.Estado.ou.Município,
           TRUE ~ Data.Prevista.de.Conclusão.da.Obra))

simec_atraso_concluidas$data_ideal <- as.Date(simec_atraso_concluidas$data_ideal , "%Y-%m-%d")
simec_atraso_concluidas$data_final_gov <- as.Date(simec_atraso_concluidas$data_final_gov , "%Y-%m-%d")

simec_atraso_concluidas <- simec_atraso_concluidas %>%
  mutate(atraso = data_final_gov - data_ideal,
         entregue_atrasada = ifelse(atraso > 0, "sim", "não"))

simec_atraso_concluidas %>%
  group_by(entregue_atrasada) %>%
  summarise(atraso = mean(atraso))

##4. Total gasto até hoje (nas concluídas) e total pactuado até hoje

simec_atraso_concluidas_pagto <- simec_atraso_concluidas %>%
  group_by(Tipo.do.Projeto) %>%
  summarise(valor_total_pactuado = sum(valor_pactuado_fnde_cte_jun17, na.rm=T),
            valor_total_gasto = sum(pagamento_cte_jun17, na.rm=T),
            obras = n()) %>%
  mutate(dif_per = (valor_total_gasto - valor_total_pactuado)/valor_total_pactuado)

simec_atraso_concluidas_pagto

total_pagto_concluidas <- sum(simec_atraso_concluidas_pagto$valor_total_gasto) 
total_pactuado_concluidas <- sum(simec_atraso_concluidas_pagto$valor_total_pactuado) 
total_pagto_concluidas/total_pactuado_concluidas - 1 #foi gasto 4% a mais do que o pactuado
total_pagto_concluidas - total_pactuado_concluidas   #259.056.016 ou cerca de 259 mil reais

# 5. Gasto por ano (efeito da eleição)

locale("pt", decimal_mark = ",")

pagamentos_ano_simec1 <- simec_fin1 %>%
  mutate(Valor.do.Pagamento = str_trim(Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("R\\$ ", "", Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("\\.", "", Valor.do.Pagamento),
         Valor.do.Pagamento = as.numeric(gsub(",", "\\.", Valor.do.Pagamento)),
         Data.de.Pagamento = as.Date(Data.de.Pagamento, "%d/%m/%Y")) %>%
  rename(data_pagamento_ou_repasse = Data.de.Pagamento,
         valor_pagamento_ou_repasse = Valor.do.Pagamento) %>%
  select(id, data_pagamento_ou_repasse, valor_pagamento_ou_repasse)

pagamentos_ano_simec2 <- simec_fin2 %>%
  mutate(Valor.Repassado = str_trim(Valor.Repassado),
         Valor.Repassado = gsub("R\\$ ", "", Valor.Repassado),
         Valor.Repassado = gsub("\\.", "", Valor.Repassado),
         Valor.Repassado = as.numeric(gsub(",", "\\.", Valor.Repassado)),
         Data.do.Repasse = as.Date(Data.do.Repasse, "%d/%m/%Y")) %>%
  rename(data_pagamento_ou_repasse = Data.do.Repasse,
         valor_pagamento_ou_repasse = Valor.Repassado) %>%
  select(id, data_pagamento_ou_repasse, valor_pagamento_ou_repasse)

pagamento_ano_simec <- bind_rows(pagamentos_ano_simec1, pagamentos_ano_simec2) %>%
  mutate(mes_ano = format(data_pagamento_ou_repasse, "%m/%Y")) %>%
  left_join(ipca, by="mes_ano") %>%
  mutate(pagto_repasse_cte_jun17 = valor_pagamento_ou_repasse/indice,
         ano = format(data_pagamento_ou_repasse, "%Y",
                      ano_eleitoral = ifelse(ano == 2002 |
                                               ano == 2006 | 
                                               ano == 2010 | 
                                               ano == 2014 , 
                                             "sim", "não"))) 
graf_pagto_ano <- pagamento_ano_simec %>%
  group_by(ano) %>%
  summarise(total_pagto_repasse_cte_jun17 = sum(pagto_repasse_cte_jun17)) %>%
  mutate(ano = as.numeric(ano),
         total_pagto = total_pagto_repasse_cte_jun17 / 1000000000) %>%
  filter(ano != 2017)

graf_pagto_ano %>%
  ggplot(aes(x=ano, y=total_pagto)) +
  labs(title="Repasses Proinfância", 
       subtitle="Repasses efetuados pelo Governo Federal às prefeituras", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil", 
       y="Repasse") +
  geom_line() + xlab("") + ylab("") + scale_y_continuous(labels = dollar_format(suffix = " bi", prefix = "R$ ",
                                                                                decimal.mark = ",",
                                                                                big.mark = " ")) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw() +
  theme(panel.grid.minor = element_blank())

#Repasses feitos para obras que não foram concluídas:

graf_pagto_ano_naoconl <- pagamento_ano_simec %>%
  group_by(ano) %>%
  summarise(repasses_concluidas = sum(pagto_repasse_cte_jun17
                                      
  )) %>%
  mutate(ano = as.numeric(ano),
         total_pagto = repasses_concluidas / 1000000000)

graf_pagto_ano %>%
  ggplot(aes(x=ano, y=total_pagto)) +
  labs(title="Repasses Proinfância", 
       subtitle="Repasses efetuados pelo Governo Federal às prefeituras", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil", 
       y="Repasse") +
  geom_line() + xlab("") + ylab("") + scale_y_continuous(labels = dollar_format(suffix = " bi", prefix = "R$ ",
                                                                                decimal.mark = ",",
                                                                                big.mark = " ")) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw() +
  theme(panel.grid.minor = element_blank())


dia_final <- as.Date("2017-07-27")

#6. obras em execução e iniciadas
#todas as obras que têm Data de assinatura do contrato foram consideradas como iniciadas

obras_iniciadas <- simec_atraso %>%
  filter(!is.na(Data.de.Assinatura.do.Contrato),
         Situação != "Concluída") %>%
  mutate(data_estimada_de_entrega = Data.Prevista.de.Conclusão.da.Obra,
         data_estimada_de_entrega = case_when(!is.na(Data.Prevista.de.Conclusão.da.Obra) ~ data_estimada_de_entrega,
                                              TRUE ~ Data.de.Assinatura.do.Contrato + tempo_exe_dias),
         dia_final = dia_final,
         ja_devia_estar_concluida = ifelse(data_estimada_de_entrega <= dia_final ,
                                           "sim", "não"),
         tempo_de_atraso = dia_final,
         tempo_de_atraso = dia_final - data_estimada_de_entrega) %>%
  select(ID, Situação, ja_devia_estar_concluida) %>%
  mutate(situacao_tb = ifelse(!Situação %in% c("Obra Cancelada","Execução","Contratação"), 
                              "paralisada", "não-paralisada")) %>%
  select(-Situação)

## da tb
obras_iniciadas %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(obras_a_serem_entregues = sum(total),
         perc = round(total/obras_a_serem_entregues, 2))

obras_situacao_tb <- obras_iniciadas %>%
  right_join(simec_atraso, by="ID") %>%
  mutate(paralisada_tb = ifelse(Situação %in% c("Paralisada", "Inacabada"), "paralisada",
                                ifelse(!is.na(situacao_tb) & situacao_tb == "paralisada", 
                                       "paralisada", "não-paralisada")),
         atrasada = ifelse(is.na(ja_devia_estar_concluida), "não", ja_devia_estar_concluida),
         obra_a_ser_entregue = ifelse(Situação %in% c("Obra Cancelada", "Concluída"), "não", "sim"))


obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(paralisada_tb) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(obras_a_serem_entregues = sum(total),
         perc = round(total/obras_a_serem_entregues, 2))

obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(atrasada) %>%
  summarise(total=n()) %>%
  ungroup() %>%
  mutate(obras_a_serem_entregues = sum(total),
         perc = round(total/obras_a_serem_entregues, 2))

obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(paralisada_tb, atrasada) %>%
  summarise(total=n(), 
            custo = sum(pagamento_cte_jun17, na.rm=T),
            num_obras_custo = sum(!is.na(pagamento_cte_jun17))) %>%
  ungroup() %>%
  mutate(obras_a_serem_entregues = sum(total),
         perc = round(total/obras_a_serem_entregues, 2))

obras_situacao_tb %>%
  summarise(total=n(), 
            custo = sum(pagamento_cte_jun17, na.rm=T),
            num_obras_custo = sum(!is.na(pagamento_cte_jun17))) %>%
  mutate(obras_a_serem_entregues = sum(total),
         perc = round(total/obras_a_serem_entregues, 2))


## paralisada por UF
chart_uf <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(UF, paralisada_tb) %>%
  summarise(total=n()) %>%
  group_by(UF) %>%
  mutate(obras_a_serem_entregues_uf = sum(total),
         perc = round(total/obras_a_serem_entregues_uf, 2)) %>%
  filter(paralisada_tb == "paralisada")

chart_uf$UF <- factor(chart_uf$UF, levels = chart_uf$UF[order(-chart_uf$perc)]) 


chart_uf_g <- chart_uf %>%
  ggplot(aes(y=perc, x=UF)) + 
  geom_bar(stat= "identity") + coord_flip() +
  scale_y_continuous(labels = scales::percent) + theme_bw() +
  xlab("") + ylab("Percentual de obras paralisadas")

ggsave(chart_uf_g, file="chart_uf_g.png", height = 10, width=8)



## Atrasadas por UF
chart_uf_atrasadas <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(UF, atrasada) %>%
  summarise(total=n()) %>%
  group_by(UF) %>%
  mutate(obras_a_serem_entregues_uf = sum(total),
         perc = round(total/obras_a_serem_entregues_uf, 2)) %>%
  filter(atrasada == "sim")

chart_uf_atrasadas$UF <- factor(chart_uf_atrasadas$UF, levels = chart_uf_atrasadas$UF[order(-chart_uf_atrasadas$perc)]) 


chart_uf__atrasado_g <- chart_uf_atrasadas %>%
  ggplot(aes(y=perc, x=UF)) + 
  geom_bar(stat= "identity") + coord_flip() +
  scale_y_continuous(labels = scales::percent, lim = c(0 ,.6)) + theme_bw() +
  xlab("") + ylab("Percentual de obras atrasadas")

ggsave(chart_uf__atrasado_g, file="chart_uf_g_atrasadas.png", height = 10, width=8)

# correlação entre idh e obras paralisadas

idh <- read.table("idh_estados.txt", sep="\t", dec=",", header=T,
                  as.is=T)

idh <- idh %>%
  mutate(idh_2010 = as.numeric(gsub(",", ".", idh_2010)))

idh_cor <- chart_uf %>%
  ungroup() %>%
  mutate(sigla = as.character(UF)) %>%
  inner_join(idh, by= "sigla") 

cor(idh_cor$perc, idh_cor$idh_2010)
idh_cor %>%
  ggplot(aes(x=idh_2010, y=perc)) + geom_point() +
  geom_smooth(method="lm")

### Tabela e mapa de obras a serem entregues pro estado
#importar o polígono contendo o mapa do Brasil
br <- getData('GADM', country='BRA', level=1) # 

# criando paleta
pal27 <- viridis(27,option="C")

obras_uf <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim") %>%
  group_by(UF) %>%
  summarise(obras_a_entregar = n()) %>%
  ungroup() %>%
  mutate(total = sum(obras_a_entregar),
         perc = round(obras_a_entregar/total, 2))

# importando tabela que converge sigla em extenso
codigo_uf <- read_delim("codigo_uf.txt", delim="\t")

# checando que nome dos estados bate
codigo_uf$UFN %in% br$NAME_1

obras_uf <- obras_uf %>%
  inner_join(codigo_uf[,-1], by=c("UF"="Sigla")) %>%
  rename(uf = UFN)


br <- append_data(br, obras_uf, key.shp="NAME_1",key.data="uf", ignore.na = F)

br$sigla <- str_replace(br$HASC_1,"BR.","")

## cores

region_col$mycolors[region_col$iso_a3 == "GBR"] <- "#00ff00"
region_col$mycolors[region_col$iso_a3 == "FRA"] <- "#ff0000"
region_col$mycolors[region_col$iso_a3 == "DEU"] <- "#ffff00"
region_col$mycolors[region_col$iso_a3 == "ESP"] <- "#0000ff"

#Import color selection from region_col dataframe into Europe SpatialPolygonDataFrame
Europe$color <- region_col$mycolors


## plotando o mapa
perc_mapa_entregar <- tm_shape(br) + 
  tm_fill(col="obras_a_entregar",
          labels=c("De 0 a 200","De 200 a 400","De 400 a 600","De 600 a 800", "> 800"),
          #palette= c(),
          title="",
          convert2density=F,
          n=4) +
  tm_borders(col="white",alpha=.8) +
  tm_text("sigla",size=.8,legend.size.show=F) + # retirar commentário coloca sigla
  # tm_compass(position=c("RIGHT","TOP"),type="4star")  +
  tm_legend(position=c("left","bottom"), scale=1.2,
            legend.title.size = 1.5, legend.text.size = 1.5) +
  # tm_scale_bar() +
  tm_layout(title="",title.size=1.3,scale=1.6)
  # Obs. Se demorar muito para plotar, retire a última camada +tm_layout(...). Ela atrasa a plotagem, mas não apresenta problemas para salvar.

perc_mapa_entregar

save_tmap(perc_mapa_entregar,
          "mapa_obras_a_entregar_com_sigla_v4.png", 
          width = 10,height=10, dpi=300)

write.table(obras_uf, "obras_uf.csv", sep=";", row.names=T)


#obras canceladas 
#numero de obras iniciadas exceto canceladas e concluidas
3083 +1189 +510 +1414 #temos informação de gastos de apenas 6.196

#quantas obras já deveriam ter sido concluídas de fato foram?

#7. Obras iniciadas e atrasadas:

#para obras que ainda não foram concluidas
#e que não foram canceladas
#e que possuem Data de assinatura de contrato ou constam como "em execução
#e cujos projetos tem tempo de execução conhecida

#OBS: A data da última vistoria como procy da entrega vale só para as obras já concluídas

execucao_e_atrasos <- simec_atraso %>%
  filter(Situação != "Concluída",           
         Situação!= "Obra Cancelada",
         !is.na(Data.de.Assinatura.do.Contrato),    
         !is.na(tempo_exe_dias) | !is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%  
  mutate(data_estimada_de_entrega = Data.Prevista.de.Conclusão.da.Obra,
         data_estimada_de_entrega = case_when(!is.na(Data.Prevista.de.Conclusão.da.Obra) ~ data_estimada_de_entrega,
                                              TRUE ~ Data.de.Assinatura.do.Contrato + tempo_exe_dias),
         dia_final = dia_final,
         ja_devia_estar_concluida = ifelse(data_estimada_de_entrega <= dia_final ,
                                           "sim", "não"),
         tempo_de_atraso = dia_final,
         tempo_de_atraso = dia_final - data_estimada_de_entrega)

# 7 Quantidade de obras atrasadas -

obras_atrasadas <- execucao_e_atrasos %>%
  filter(Situação == "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(obras = n())

obras_atrasadas

#Quantas obras já deviam estar concluídas e qual é a situação de cada uma delas:

obras_atrasadas_sit <- execucao_e_atrasos %>%
  filter(Situação == "Execução") %>%
  group_by(Situação) %>%
  summarise(Obras = n(), Custo = sum(pagamento_cte_jun17)) %>%
  mutate(Custo = round(Custo/1000000, 2)) %>%
  arrange(desc(Custo)) %>%
  mutate(Custo = as.character(Custo))


obras_atrasadas_sit$Custo <- paste(obras_atrasadas_sit$Custo, "mi")
obras_atrasadas_sit$Custo <- gsub("[.]", ",", obras_atrasadas_sit$Custo)

obras_atrasadas_sit
write.table(obras_atrasadas_sit, file="obras_atrasadas_sit.csv", row.names = F, sep=";")

### Qual é o atraso médio das obras iniciadas?

atraso_medio_execucao <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação == "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 
atraso_medio_execucao 

atraso_medio_iniciadas <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação != "Contratação") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 

atraso_medio_iniciadas

atraso_medio_paralisadas <- custo_paralisadas %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação != "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 

atraso_medio_paralisadas

# 9. Obras entregues por ano

ano_conclusao <- simec_atraso %>%
  filter(Situação == "Concluída") %>%
  mutate(ano_assinatura = format(Data.de.Assinatura.do.Contrato, "%Y"))

graf_ano_conclusao <- ano_conclusao %>%
  group_by(ano_concluida) %>%
  summarise(obras = n(), 
            tempo_medio = mean(tempo_exec_real, na.rm=T)) %>%
  filter(ano_concluida != "<NA>",
         ano_concluida != "2018") %>%
  mutate(ano_concluida = as.numeric(ano_concluida))

graf_ano_conclusao %>%
  ggplot(aes(x=ano_concluida, y=obras, group=1)) +
  labs(title="Obras Proinfância entregues", 
       subtitle="Obras concluídas por ano", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil") +
  geom_line() + xlab("") + ylab("") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw()



graf_ano_conclusao %>%
  ggplot(aes(x=ano_concluida, y=tempo_medio, group=1)) +
  labs(title="Obras Proinfância entregues", 
       subtitle="Obras concluídas por ano", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil") +
  geom_line() + xlab("") + ylab("") + facet_wfrap(~ano_assinatura) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw()

graf_ano_conclusao1 <- ano_conclusao %>%
  group_by(ano_concluida, ano_assinatura) %>%
  summarise(obras = n(), 
            tempo_medio = mean(tempo_exec_real, na.rm=T)) %>%
  ungroup() %>%
  filter(ano_concluida != "<NA>",
         ano_concluida != "2018") %>%
  mutate(ano_concluida = as.numeric(ano_concluida))

graf_ano_conclusao1 %>%
  ggplot(aes(x=ano_concluida, y=tempo_medio, group=1)) +
  labs(title="Obras Proinfância entregues", 
       subtitle="Obras concluídas por ano", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil") +
  geom_line() + xlab("") + ylab("") + facet_wrap(~ ano_assinatura) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw()

#Dados inconsistentes e inexistentes
# base para cálculo da falta de informações: as obras 
names(simec_atraso)

inex <- simec_atraso %>%           #página 4 base de todos os 
  filter(is.na(Município)|
           is.na(UF)|
           is.na(CEP)|
           is.na(Situação)|
           is.na(Logradouro)|
           is.na(Termo.Convênio)|
           is.na(Fim.da.Vigência.Termo.Convênio)|
           is.na(Tipo.do.Projeto)|
           is.na(Valor.Pactuado.pelo.FNDE)|
           is.na(Email)|
           is.na(Total.Pago)) %>%
  mutate(Município = ifelse(is.na(Município), 1 ,0),
         UF = ifelse(is.na(UF), 1 ,0),
         CEP = ifelse(is.na(CEP), 1 ,0),
         Logradouro = ifelse(is.na(Logradouro), 1 ,0),
         Termo.Convênio = ifelse(is.na(Termo.Convênio), 1 ,0),
         Fim.da.Vigência.Termo.Convênio = ifelse(is.na(Fim.da.Vigência.Termo.Convênio), 1 ,0),
         Tipo.do.Projeto = ifelse(is.na(Tipo.do.Projeto), 1 ,0),
         Valor.Pactuado.pelo.FNDE = ifelse(is.na(Valor.Pactuado.pelo.FNDE), 1 ,0),
         Email = ifelse(is.na(Email), 1 ,0),
         Total.Pago = ifelse(is.na(Total.Pago), 1 ,0),
         Situação = ifelse(is.na(Situação),1,0)) %>%
  select(Município, UF, CEP, Logradouro, Termo.Convênio, Fim.da.Vigência.Termo.Convênio,
         Tipo.do.Projeto, Valor.Pactuado.pelo.FNDE, Email, Total.Pago, Situação)

Município <- sum(inex$Município)
UF <- sum(inex$UF) 
CEP <- sum(inex$CEP)
Logradouro <- sum(inex$Logradouro)
Termo.Convênio <- sum(inex$Termo.Convênio)
Fim.da.Vigência.Termo.Convênio <- sum(inex$Fim.da.Vigência.Termo.Convênio)
Tipo.do.Projeto <- sum(inex$Tipo.do.Projeto)
Valor.Pactuado.pelo.FNDE <- sum(inex$Valor.Pactuado.pelo.FNDE)
Email <- sum(inex$Email)
Total.Pago <- sum(inex$Total.Pago)
Situação <- sum(inex$Situação)

inexistentes <- data.frame(Município, UF, CEP, Logradouro, Termo.Convênio,
                           Fim.da.Vigência.Termo.Convênio, Tipo.do.Projeto,
                           Valor.Pactuado.pelo.FNDE, Email, 
                           Total.Pago, Situação)
inexistentes

write.table(inexistentes, file="inexistentes.csv", row.names = F, sep=";")

ww <- simec_atraso %>%
  filter(Situação != "Concluída",
         Situação != "Obra Cancelada") %>%
  filter(is.na(Total.Pago)) %>%
  select(Fim.da.Vigência.Termo.Convênio, Total.Pago)  #Essas duas colunas são ausentes nos mesmos casos

sum(is.na(ww$Fim.da.Vigência.Termo.Convênio)) 
sum(is.na(ww$Total.Pago)) 


#inconsistências:
#a. Obras em execução sem data de assinatura de contrato: #16

simec %>%
  filter(Situação == "Execução" & is.na(Data.de.Assinatura.do.Contrato)) %>%
  summarise(n())   #16

#b. Obras em execução que não possuem data prevista de entrega 279

simec %>%
  filter(Situação == "Execução" & is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%
  summarise(n()) #279

#c. Obras concluídas que constam que a última data de vistoria do município foi em 2018: 5

simec %>%
  mutate(dia_final = dia_final) %>%
  mutate(Data.da.Última.Vistoria.do.Estado.ou.Município = 
           as.Date(Data.da.Última.Vistoria.do.Estado.ou.Município)) %>%
  filter(Situação == "Concluída",
         Data.da.Última.Vistoria.do.Estado.ou.Município > dia_final) %>%
  summarise(n()) #5

#d. Obras em licitação que tem percentual de execução da obra > 0 #227

simec %>%
  filter(Situação == "Licitação" & !is.na(Percentual.de.Execução) & Percentual.de.Execução >= 0.01) %>%
  summarise( casos = n()) #227

#e. Obras em contratação que já tem data de assinatura do contrato #387

simec %>%
  filter(Situação == "Contratação",
         !is.na(Data.de.Assinatura.do.Contrato)) %>%
  summarise( casos = n())

#página 3 - % de escolas sem número

simec_atraso %>%
  filter(Situação != "Obra Cancelada",
         is.na(Logradouro)) %>%
  group_by(Situação)%>%
  summarise(n())    #929 escolas sem endereço ou 10%


simec_atraso %>%
  filter(Situação != "Obra Cancelada") %>%
  mutate(end_sem_num = as.numeric(grepl("[0-9]", Logradouro))) %>%
  group_by(end_sem_num) %>%
  summarise(n())

# end_sem_num `n()`
# <dbl> <int>
# 1           0  8099   escolas sem número ou sem endereço
# 2           1   948   escolas com número

8099 - 929 # = 7170 ou 76% das escolas

#Quando foram pactuadas as obras?

convenios_pactuados <- simec_atraso %>%
  mutate(ano_pacto = str_sub(Termo.Convênio, start = -4)) %>%
  mutate(ano_pacto = as.Date(ano_pacto, "%Y"))

convenios_pactuados$ano_pacto <- lubridate::year(convenios_pactuados$ano_pacto)

convenios_pactuados <- convenios_pactuados%>%
  group_by(ano_pacto) %>%
  summarise(obras = n()) %>%
  filter(!is.na(ano_pacto))

convenios_pactuados  
sum(convenios_pactuados$obras)

convenios_pactuados %>%
  ggplot(aes(x=ano_pacto, y=obras)) + geom_line() +
  labs(title="Obras pactuadas por ano", 
       subtitle="Convênios com prefeituras pactuados por ano", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil", 
       y="", x="") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw() +
  theme(panel.grid.minor = element_blank())

#(2255-752)/752 = aumento de 1.99867 entre 2010 ew 2014
#Queda de 98% entre 2014 e 2015 ((2255-33)/2255 )


obras_pactuadas <- simec_atraso %>%
  mutate(ano_pacto = str_sub(Termo.Convênio, start = -4)) %>%
  mutate(ano_pacto = as.Date(ano_pacto, "%Y"))

obras_pactuadas$ano_pacto <- lubridate::year(obras_pactuadas$ano_pacto)

obras_pactuadas <- obras_pactuadas%>%
  group_by(Termo.Convênio) %>%
  summarise(obras = n()) %>%
  filter(!is.na(Termo.Convênio))

#Quando foram pactuadas as obras que ainda precisam ser entregues?

x <- simec_atraso %>%
  filter(Situação != "Canceladas",
         Situação != "Concluída") %>%
  group_by(Município, UF) %>%
  summarise(obras = n()) 

#2211 municípios


pacto_concluidas_andamento <- simec_atraso %>%
  mutate(ano_pacto = str_sub(Termo.Convênio, start = -4)) %>%
  mutate(ano_pacto = as.Date(ano_pacto, "%Y"))

pacto_concluidas_andamento$ano_pacto <- lubridate::year(pacto_concluidas_andamento$ano_pacto)

pacto_concluidas_andamento <- pacto_concluidas_andamento %>%
  filter(Situação != "Obra Cancelada") %>%
  mutate(count_concluida = ifelse(Situação == "Concluída", 1,0),
         count_andamento = ifelse(Situação != "Concluída", 1,0)) %>%
  group_by(ano_pacto) %>%
  summarise(obras_concluidas = sum(count_concluida),
            obras_andamento = sum(count_andamento)) %>%
  filter(!is.na(ano_pacto)) %>%
  gather(situacao_obra, obras, obras_concluidas, obras_andamento )

pacto_concluidas_andamento

pacto_concluidas_andamento %>%
  ggplot(aes(x=ano_pacto, y=obras, colour = situacao_obra)) + geom_line() +
  labs(title="Proporção descumprimento da entrega", 
       subtitle="Proporção de obras entregues de acordo com o ano que foram pactuadas", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil", 
       y="", x="") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw() +
  theme(panel.grid.minor = element_blank())


#####
# Dados do pedido do Manoel

library(readr)
pedido_supervisao_in_loco <- read_delim("~/tadepe/fantastico/rel_fant/pedido_supervisao_in_loco.csv", 
                                        ";", escape_double = FALSE, col_types = cols(`Ano Termo/Convjnio` = col_date(format = "%Y"), 
                                                                                     ID = col_character(), `N: Processo` = col_character(), 
                                                                                     `Termo/N: Convjnio` = col_character(), 
                                                                                     perc_executado_empresa = col_number(), 
                                                                                     perc_informado_munic = col_number()), 
                                        locale = locale(encoding = "ASCII"), 
                                        trim_ws = TRUE)

#Vou verificar a diferença entre o que foi atestado percentualmente pelos engenheiros e 
#o que a verificação in loco demonstrou ser o verdadeiro percentual #

dif_execucao_ver_in_loco <- pedido_supervisao_in_loco %>%
  mutate(dif_vistoria = perc_informado_munic - perc_executado_empresa)

mean(dif_execucao_ver_in_loco$dif_vistoria)  #20.93389
median(dif_execucao_ver_in_loco$dif_vistoria) #12.23

dif_execucao_ver_in_loco %>%
  select(ID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  summarise(n())    #15529 - número de obras que tiveram ao menos uma vistoria

base_vist <- simec %>%
  mutate(ano_pacto = str_sub(Termo.Convênio, start = -4)) %>%
  filter(ano_pacto <= 2016) %>%
  group_by(ano_pacto) %>%
  summarise(obras = n())

sum(base_vist$obras)  #27266

15529/27266 #0.5695372 - estimativa percentual de obras vistoriadas pelo CGIMP

dif_execucao_ver_in_loco %>%
  filter(dif_vistoria >= 90) %>%
  summarise(n())

#Quanto dinheiro ainda precisa para terminar as obras em execução

dinheiro_falta_exe <- simec_atraso %>%
  filter(Situação == "Execução") %>%
  mutate(falta = valor_pactuado_fnde_cte_jun17 - pagamento_cte_jun17) 

sum(dinheiro_falta_exe$falta, na.rm=TRUE) # 1,280.999.379 ainda precisam ser investidos nas obras em execução
sum(is.na(dinheiro_falta_exe$falta)) #16 obras não temos informações

dinheiro_falta_todas <- simec_atraso %>%
  filter(Situação != "Concluída",
         Situação != "Obra Cancelada") %>%
  mutate(falta = valor_pactuado_fnde_cte_jun17 - pagamento_cte_jun17) 

sum(dinheiro_falta_todas$falta, na.rm = TRUE) #2,535.293.840
sum(is.na(dinheiro_falta_todas$Valor.Pactuado.pelo.FNDE)) #9 obras

dinheiro_falta_todas %>%
  filter(Valor.Pactuado.pelo.FNDE = 0) %>%
  summarise(n())

dinheiro_falta_todas %>%
  filter(is.na(falta))

graf_pagto_ano # 521.445.932 foram pagos em 2016 


# Anexo I - Obras por estado 
# (Obras total, em execução, atrasadas, paralisadas e dinheiro investido nessas obras)

anexo1_atrasadas <- execucao_e_atrasos %>%
  filter(Situação == "Execução",
         ja_devia_estar_concluida == "sim") %>%
  group_by(UF) %>%
  summarise(obras_atrasadas = n(),
            gasto_atrasadas = sum(pagamento_cte_jun17),
            gasto_atrasadas_mi = round(gasto_atrasadas/1000000, 2))

anexo1_paralisadas <- obras_iniciadas %>%
  filter(Situação != "Obra Cancelada",
         Situação != "Execução",
         Situação != "Contratação") %>%
  group_by(UF) %>%
  summarise(obras_paralisadas = n(),
            gasto_paralisadas = sum(pagamento_cte_jun17),
            gasto_paralisadas_mi = round(gasto_paralisadas/1000000, 2))

anexo1 <- simec_atraso %>%
  filter(Situação != "Concluída",
         Situação != "Obra Cancelada") %>%
  group_by(UF) %>%
  summarise(total_obras = n(),
            gasto_total = sum(pagamento_cte_jun17),
            gasto_total_mi = round(gasto_total /1000000,2)) %>%
  left_join(anexo1_atrasadas) %>%
  left_join(anexo1_paralisadas) %>%
  mutate(perc_atrasada = round(obras_atrasadas / total_obras, 2),
         perc_paralisada = round(obras_paralisadas / total_obras, 2)) %>%
  select(UF, total_obras, gasto_total_mi, obras_atrasadas,  perc_atrasada, gasto_atrasadas_mi,
         obras_paralisadas, perc_paralisada, gasto_paralisadas_mi) %>%
  mutate(obras_atrasadas = ifelse(is.na(obras_atrasadas), 0, obras_atrasadas),
         perc_atrasada = ifelse(is.na(perc_atrasada), 0, perc_atrasada))

View(anexo1)  

write.table(anexo1, file="anexo1.csv", sep=";", dec=",", row.names = FALSE)


## Escolas Rio de Janeiro UF
## Nome , endereço, % de execução, Calssificação (atrasada , paralisada) 

escolas_rj_atraso <- execucao_e_atrasos %>%
  filter(UF == "RJ",
         Situação == "Execução",
         ja_devia_estar_concluida == "sim") %>%
  mutate(classificação_tb = "atrasada") %>%
  select(ID, Nome, Logradouro, Município, Percentual.de.Execução, classificação_tb,
         pagamento_cte_jun17)


escolas_rj_paralisadas <- obras_iniciadas %>%
  filter( UF == "RJ",
          Situação != "Obra Cancelada",
          Situação != "Execução",
          Situação != "Contratação") %>%
  mutate(classificação_tb = "paralisada") %>%
  select(ID, Nome, Logradouro, Município, Percentual.de.Execução, classificação_tb,
         pagamento_cte_jun17)

escolas_rj <- escolas_rj_atraso %>%
  bind_rows(escolas_rj_paralisadas) %>%
  rename(total_repassado =  pagamento_cte_jun17)

View(escolas_rj)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\tadepe\\fantastico\\rel_fant")
write.table(escolas_rj, file="escolas_rj.csv", sep=";", dec=",", row.names = FALSE)

## Anexo 2 - obras por munic

anexo2_atrasadas <- execucao_e_atrasos %>%
  filter(Situação == "Execução",
         ja_devia_estar_concluida == "sim") %>%
  group_by(Município, UF) %>%
  summarise(obras_atrasadas = n(),
            gasto_atrasadas = sum(pagamento_cte_jun17),
            gasto_atrasadas_mi = round(gasto_atrasadas/1000000, 2))

anexo2_paralisadas <- obras_iniciadas %>%
  filter(Situação != "Obra Cancelada",
         Situação != "Execução",
         Situação != "Contratação") %>%
  group_by(Município, UF) %>%
  summarise(obras_paralisadas = n(),
            gasto_paralisadas = sum(pagamento_cte_jun17),
            gasto_paralisadas_mi = round(gasto_paralisadas/1000000, 2))

anexo2 <- simec_atraso %>%
  filter(Situação != "Concluída",
         Situação != "Obra Cancelada") %>%
  group_by(Município, UF) %>%
  summarise(total_obras = n(),
            gasto_total = sum(pagamento_cte_jun17),
            gasto_total_mi = round(gasto_total /1000000,2)) %>%
  left_join(anexo2_atrasadas) %>%
  left_join(anexo2_paralisadas) %>%
  mutate(perc_atrasada = round(obras_atrasadas / total_obras, 2),
         perc_paralisada = round(obras_paralisadas / total_obras, 2)) %>%
  select(UF, total_obras, gasto_total_mi, obras_atrasadas,  perc_atrasada, gasto_atrasadas_mi,
         obras_paralisadas, perc_paralisada, gasto_paralisadas_mi) %>%
  mutate(obras_atrasadas = ifelse(is.na(obras_atrasadas), 0, obras_atrasadas),
         perc_atrasada = ifelse(is.na(perc_atrasada), 0, perc_atrasada))

View(anexo2)  

write.table(anexo2, file="anexo2.csv", sep=";", dec=",", row.names = FALSE)