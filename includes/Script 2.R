
pegar_fisico <- function(id) {
  tryCatch({
    res <- nba_commonplayerinfo(player_id = id)
    df <- res$CommonPlayerInfo %>% dplyr::select(PERSON_ID, HEIGHT, WEIGHT)
    # Adiciona uma pausa para não sobrecarregar a API
    Sys.sleep(0.5) 
    return(df)
  }, error = function(e) {
    cat(sprintf("Erro ou dados não encontrados para o ID: %s\n", id))
    return(NULL) # Retorna NULL para IDs sem dados
  })
}


# 2. Roda para todos os 605 jogadores (isso vai levar alguns minutos)
df_todos_fisicos <- map_dfr(dados_basicos$PLAYER_ID, pegar_fisico, .progress = TRUE)


dados_basicos <- left_join(dados_basicos, df_todos_fisicos, by = c("PLAYER_ID" = "PERSON_ID"))

# Função para converter altura (6-8 -> metros)
converter_altura <- function(valor) {
  partes <- as.numeric(unlist(strsplit(valor, "-")))
  # (Pés * 30.48) + (Polegadas * 2.54)
  cm <- (partes[1] * 30.48) + (partes[2] * 2.54)
  return(cm / 100) 
}

# Função para converter peso (lbs -> kg)
converter_peso <- function(lbs) {
  return(lbs * 0.453592)
}


dados_analise <- dados_analise |>
  rowwise() |>
  mutate(
    Altura = converter_altura(HEIGHT.x),
    Peso = converter_peso(WEIGHT.x)
  )


dados_analise <- dados_analise |> dplyr::select(-WEIGHT.x, -HEIGHT.x)
