# Exemplo de utilização da API do Tá de Pé no R


## bibliotecas
library(httr)
library(jsonlite) 
library(dplyr)
library(stringr)

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

## gerando função
## paralisadas

req <- list()
aux_j <- list()
aux_i <- list()
n <- length(type_project)
k <- length(status)


gera_urls_app <- function() {
  lista_urls <- vector("character", n*k)
  
  type_project <- c("educational_space_1_room","educational_space_2_room",
                    "educational_space_4_room", "educational_space_6_room",
                    "educational_space_8_room","educational_space_10_room",
                    "educational_space_1_room", "conventional_design_1",
                    "conventional_design_2")
  
  status <- c("execution", "paralyzed","bidding", "hiring",
              "in_recasting", "planning_by_the_proponent",
              "unfinished")
  n <- length(type_project)
  k <- length(status)
  url <- "http://tadepe.transparencia.org.br/api/projects/content?"
 
   modifier0 <- 'status=paralyzed&type_of_work=construction&funded_by=city'
  modifier1 <- "&type_of_project="
  modifier2 <- "&status="
  
  n <- length(type_project)
  k <- length(status)
  
  for ( i in 1:n) {
    for ( j in 1:k) {
      lista_urls[j+(k*(i-1))] <- paste0(url, modifier0, modifier1, type_project[i],
                        modifier2, status[j])
    }
    
  }
  return(lista_urls)

}

get_json <- function(sleep_time = .05) {
  
  stopifnot(require(jsonlite))
  
  url <- gera_urls_app()
  n <- length(url)
  
  obras <- vector("list", n)
  paginas <- vector("list", n)
  
  for ( i in 1:n) {
    req <- GET(url[i], accept_json(),
               add_headers(Authorization = token_meu))
    
    obras[[i]] <- fromJSON(content(req,type="text"))
    
    paginas[[i]] <- paralisadas$links
    print(i)
    Sys.sleep(sleep_time)
  }
  
  return(list(obras, paginas))
}


lista_df <- vector("list", n)
paginas <- list()
i <- 1

while(finished != T) {
    req <- GET(url[i], accept_json(),
               add_headers(Authorization = token_meu))

   paralisadas <- fromJSON(content(req,type="text"))
   
   paginas[[i]] <- paralisadas$links
   
  if(length(paralisadas[[1]])!=0) {
       paralisadas1 <- paralisadas[[1]] %>%
       as.data.frame
     
       lista_obras[[i]] <- paralisadas1[[3]]
  }

   bol_paginacao <- length(paginas[[i]]) != 0 # condição se tem paginação no resultado.
   
   if(bol_paginacao) {
     j <- 2
     string_tmp <- str_extract(paginas[[i]]$last, "page%5Bnumber%5D=[0-9]+")
     last_page <- gsub("=", "", str_extract(string_tmp, "=[0-9]+"))
     
     while(j <= last_page) {
       req <- GET(paste0(url[i], "&page=", j), accept_json(),
                  add_headers(Authorization = token_meu))
       
       paralisadas <- fromJSON(content(req,type="text"))
       paginas[[i]] <- paralisadas$links
       j <- j+1
       print(j)
     }
   }
   
    print(paste(paste("i é", i), " e ", paste("j é", j)))
    contador <- contador + 1
}

df_final <- bind_rows(lista_df)
dim(df_final)

for( i in 1:63) {
  print(paginas[[i]])
  print(i)
} 
length(paginas[[57]])
length(paginas[[63]])

# checa status do request. Se der certo, é invisível (aka não retorna nada)
stop_for_status(req)

# for fromJSON(...)
paralisadas <- fromJSON(content(req,type="text"))

paralisadas1 <- paralisadas[[1]] %>%
  as.data.frame

View(paralisadas1)
