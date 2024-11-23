# Aprendizagem Estatistica de Maquina II 
# Aula 8
# Inteligencia Artificial Generativa

library(httr)
library(stringr)

# Definicao de variaveis de ambiente --------------------------------------

# link para gerar chave da OpenAI API (requer assinatura/adição de crédito): 
# https://platform.openai.com/api-keys
# Sys.setenv(`OPENAI_API_KEY`= "SUA CHAVE AQUI")


# Usando OpenAI -----------------------------------------------------------

chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  
  if(status_code(response)>200) {
    stop(content(response)$error$message)
  }
  
  str_trim(content(response)$choices[[1]]$message$content)
}

chatgpt("porque o ceu é azul?")

