#3 Exemplo de utilização da API do Tá de Pé no R


## bibliotecas
library(httr)
library(jsonlite) 
library(dplyr)

# token
token_meu <- "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiYXBpLXRlc3QifQ.vwzSUQKsig7JFQ2w-o85JgnVv-VytT2NsZYwRELTiaY"

# query de exemplo. Aqui, pega uma obra com id = 1018526
query <- "http://tadepe.transparencia.org.br/api/projects/content?id=1018526"

# requisiÃ§Ã£o GET. Usando header accept_json e Authorization  
req <- GET(query, accept_json(),
           add_headers(Authorization = token_meu))

# checa status do request. Se der certo, Ã© invisÃ­vel (aka nÃ£o retorna nada)
stop_for_status(req)

# for fromJSON(...)
df <- fromJSON(content(req,type="text"))

df1 <- df[[1]] %>%
  as.data.frame

View(df1)