# Exemplo de utilização da API do Tá de Pé no R


## bibliotecas
library(httr)
library(jsonlite) 
library(dplyr)

# token
token_meu <- "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiYXBpLXRlc3QifQ.vwzSUQKsig7JFQ2w-o85JgnVv-VytT2NsZYwRELTiaY"

# query de exemplo. Aqui, pega uma obra com id = 1018526
query <- "http://tadepe.transparencia.org.br/api/projects/content?id=1018526&status=paralyzed"

# requisição GET. Usando header accept_json e Authorization  
req <- GET(query, accept_json(),
           add_headers(Authorization = token_meu))

# checa status do request. Se der certo, é invisível (aka não retorna nada)
stop_for_status(req)

# for fromJSON(...)
df <- fromJSON(content(req,type="text"))

df1 <- df[[1]] %>%
  as.data.frame

View(df1)

## paralisadas
url <- "http://tadepe.transparencia.org.br/api/projects/content?"
modifier0 <- 'status=paralyzed&type_of_work=construction&funded_by=city'
modifier1 <- "&type_of_project="
modifier2 <- "&status="

type_project <- c("educational_space_1_room","educational_space_2_room",
                  "educational_space_4_room", "educational_space_6_room",
                  "educational_space_8_room","educational_space_10_room",
                  "educational_space_1_room", "conventional_design_1",
                  "conventional_design_2")

status <- c("execution", "paralyzed","bidding", "hiring",
            "in_recasting", "planning_by_the_proponent",
            "unfinished")

req <- list()
aux_j <- list()
aux_i <- list()
n <- length(type_project)
k <- length(status)

for (i in 1:n) {
  for (j in 1:k) {
    url1 <- paste0(url, modifier0, modifier1, type_project[i],
                   modifier2, status[j])
   req <- GET(url1, accept_json(),
               add_headers(Authorization = token_meu))
   paralisadas <- fromJSON(content(req,type="text"))

   paralisadas1 <- paralisadas[[1]] %>%
     as.data.frame
   aux_j[[j]] <- paralisadas1
    print(paste(paste("i é", i), " e ", paste("j é", j)))
  }
  aux_i[[i]] <- aux_j
}


# checa status do request. Se der certo, é invisível (aka não retorna nada)
stop_for_status(req)

# for fromJSON(...)
paralisadas <- fromJSON(content(req,type="text"))

paralisadas1 <- paralisadas[[1]] %>%
  as.data.frame

View(paralisadas1)
