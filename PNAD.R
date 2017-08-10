## pnad

devtools::install_github("lucasmation/microdadosBrasil")
library('microdadosBrasil')


download_sourceData("PNAD", 2015, unzip = T)
d  <- read_PNAD("domicilios", 2015)
d2 <- read_PNAD("pessoas", 2015)

pnad_dic <- get_import_dictionary(dataset = "PNAD",i = 2015, ft = "pessoas")
pnad_dic2 <- get_import_dictionary(dataset = "PNAD",i = 2015, ft = "domicilios")
library(xtable)



# V0101 - Ano de Referência (da realização da pesquisa)
# V3033 - Ano de Nascimento
# V3032 - Mês de Nascimento (recorte de idade com referência em março)
# V0602 - Frequenta escola ou creche (2-sim 4-não)
# UF - Unidade da Federação (estados)
# Exemplo de Comandos:
  idaderef=3 if ((V0101-V3033) < 4 ) | ((V0101-V3033) == 4 & V3032 > 3)
idaderef=4 if (V0101-V3033) == 4 & V3032 <= 3
idaderef=4 if (V0101-V3033) > 4 & (V0101-V3033) < 6
idaderef=4 if ((V0101-V3033) == 6 & V3032 > 3)
  tab UF idaderef if V0101==2013 [fw=V4729]
tab UF idaderef if V0101==2013 & V0602==2 [fw=V4729]
tab UF idaderef if V0101==2014 [fw=V4729]
tab UF idaderef if V0101==2014 & V0602==2 [fw=V4729]

