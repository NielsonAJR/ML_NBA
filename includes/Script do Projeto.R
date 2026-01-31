

# Pacotes e Tema ----------------------------------------------------------

library(hoopR) # Banco de Dados
library(tidyverse) # Framework do tidyverse
library(tidymodels) # Framework de modelagem
library(skimr) # Estatística descritiva rápida
library(DataExplorer) # Exploração do conjunto de dados
library(corrplot) # Gráfico de correlação
library(GGally) # Gráficos adicionais com estrutura ggplot2
library(stringr) # Para lidar com strings
library(glmnet) # LASSO, Ridge e Rede Elástica
library(MASS) # Discriminante Linear (LDA) e Quadrático (RL)
library(recipes) # Pré-processamento dos dados
library(class) #knn
library(themis) # Balanceamento de dados
library(discrim) # lda, qda
library(kknn) # (Kernel) K-NN
library(finetune) # Otimização fina de hiperparâmetros
library(gt)
library(naivebayes)
library(corrplot)

ggplot2::theme_set(theme_minimal()) # Ajustando Tema



# Coleta e Manipulação de Dados ---------------------------------------------------------


dados_basicos <- nba_leaguedashplayerstats(season = "2021-22", 
                                           per_mode = "Per36", 
                                           measure_type = "Base")$LeagueDashPlayerStats 

# A função puxa as informações de Altura e Peso pelo PERSON_ID/PLAYER_ID de cada Jogador

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

# Roda para todos os 605 jogadores (isso vai levar alguns minutos)
df_todos_fisicos <- map_dfr(dados_basicos$PLAYER_ID, pegar_fisico, .progress = TRUE)

# Combina os dois bancos criados pelo PLAYER_ID E PERSON_ID
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

dados_basicos <- dados_basicos |>
  rowwise() |>
  mutate(
    Altura = converter_altura(HEIGHT),
    Peso = converter_peso(as.numeric(WEIGHT))
  )


dados_analise <- dados_basicos |>
  dplyr::select(PLAYER_NAME, TEAM_ABBREVIATION, AGE, PTS, AST, REB, OREB, DREB, TOV, STL, 
                BLK, FG_PCT, FG3_PCT, FT_PCT, Peso, Altura) |>
  dplyr::mutate(
    Position = case_when(
      PLAYER_NAME %in% c("Trae Young", "Luka Dončić", "Stephen Curry", "Ja Morant", 
                         "LaMelo Ball", "Darius Garland", "Russell Westbrook", 
                         "Dejounte Murray", "James Harden", "Shai Gilgeous-Alexander", 
                         "CJ McCollum", "De'Aaron Fox", "Fred VanVleet", 
                         "Tyrese Maxey", "Jrue Holiday", "Tyrese Haliburton", 
                         "D'Angelo Russell", "Cole Anthony", "Mike Conley", "Chris Paul", 
                         "Kevin Porter Jr.", "Monte Morris", "Spencer Dinwiddie", 
                         "Devonte' Graham", "Immanuel Quickley", "Dennis Schröder", 
                         "Davion Mitchell", "Marcus Smart", "Kyle Lowry", "Kyrie Irving", 
                         "Coby White", "Damian Lillard", "Malcolm Brogdon", "Tyus Jones",
                         "Cameron Payne", "Tre Mann", "Gabe Vincent", "Jalen Suggs", 
                         "Frank Jackson", "Eric Bledsoe", "Patrick Beverley", 
                         "Raul Neto", "Cory Joseph", "Duane Washington Jr.", 
                         "Lonzo Ball", "Killian Hayes", "Ricky Rubio", "Payton Pritchard", 
                         "Kemba Walker", "Tre Jones", "Ish Smith", "Aaron Holiday", 
                         "Theo Maledon", "Lou Williams", "George Hill", "Facundo Campazzo", 
                         "Jose Alvarado", "Derrick Rose", "Brandon Williams", "D.J. Augustin", 
                         "Jevon Carter", "Jordan McLaughlin", "T.J. McConnell", "Trey Burke", 
                         "Saben Lee", "Dennis Smith Jr.", "Dalano Banton", "Trent Forrest", 
                         "Markelle Fultz", "Malachi Flynn", "Rajon Rondo", "Isaiah Thomas", 
                         "Keifer Sykes", "Brandon Goodwin", "Goran Dragic", "Elfrid Payton", 
                         "Kira Lewis Jr.", "Kris Dunn", "David Duke Jr.", "Miles McBride", 
                         "Brad Wanamaker", "Lindell Wigginton", "Justin Robinson", 
                         "Mychal Mulder", "Chris Chiozza", "Javonte Smart", 
                         "Devin Cannady", "Hassani Gravett", "Zavier Simpson",
                         "Kevin Pangos", "Tim Frazier", "Jared Harper", 
                         "Brandon Knight", "Devon Dotson", "Derrick Walton Jr.", 
                         "Jeff Dowtin Jr.", "Tyler Johnson", "Ryan Arcidiacono", 
                         "Cassius Winston", "Myles Powell", "Tremont Waters", 
                         "Malik Newman", "Sharife Cooper", "McKinley Wright IV", 
                         "Darren Collison", "Carlik Jones", "Emmanuel Mudiay", "Tyrell Terry", 
                         "Cat Barber", "JaQuori McLaughlin", "Jaysean Paige") ~ "PG",
      PLAYER_NAME %in% c("Joel Embiid", "Nikola Jokić", "Karl-Anthony Towns", "LeBron James", 
                         "Jonas Valančiūnas", "Nikola Vučević", "Christian Wood", "Bam Adebayo", 
                         "Bobby Portis", "Rudy Gobert", "Kevin Love", "Deandre Ayton", 
                         "Montrezl Harrell", "Anthony Davis", "Jakob Poeltl", "Jarrett Allen", 
                         "Jusuf Nurkić", "Clint Capela", "Ivica Zubac", "Mo Bamba", 
                         "Dwight Powell", "Al Horford", "Alperen Sengun", "JaVale McGee", 
                         "Daniel Gafford", "P.J. Washington", "Precious Achiuwa", "Jaxson Hayes", 
                         "Naz Reid", "Mitchell Robinson", "LaMarcus Aldridge", "Robert Williams III",   
                         "Isaiah Stewart", "Andre Drummond", "Moritz Wagner", "Isaiah Hartenstein", 
                         "Drew Eubanks", "Myles Turner", "Chimezie Metu", "Hassan Whiteside", 
                         "Steven Adams", "Kevon Looney", "Mason Plumlee", "Richaun Holmes", 
                         "Damian Jones", "Willy Hernangomez", "Nemanja Bjelica", "DeMarcus Cousins", 
                         "Dewayne Dedmon", "Nic Claxton", "Onyeka Okongwu", "Daniel Theis", 
                         "Dwight Howard", "Jeremiah Robinson-Earl", "Kelly Olynyk", "Serge Ibaka", 
                         "Blake Griffin", "Goga Bitadze", "Mike Muscala", "Tristan Thompson", 
                         "Omer Yurtseven", "Isaiah Jackson", "Jock Landale", "Robin Lopez", 
                         "Khem Birch", "Alex Len", "Taj Gibson", "Zach Collins", 
                         "Bismack Biyombo", "Derrick Favors", "DeAndre Jordan", "Thomas Bryant", 
                         "Luka Garza", "Moses Brown", "Tony Bradley", "Brook Lopez", 
                         "Gorgui Dieng", "Olivier Sarr", "Nick Richards", "Cody Zeller", 
                         "Enes Freedom", "Killian Tillie", "Paul Reed", "Paul Millsap", 
                         "Boban Marjanovic", "Frank Kaminsky", "Bruno Fernando", "Nerlens Noel", 
                         "Udoka Azubuike", "Greg Monroe", "Charles Bassey", "Neemias Queta", 
                         "Willie Cauley-Stein", "Udonis Haslem", "Luke Kornet", "Ed Davis", 
                         "Kai Jones", "Vernon Carey Jr.", "Marko Simonovic", "Tacko Fall", 
                         "Micah Potter", "Cheick Diallo", "Daniel Oturu", "Norvel Pelle", 
                         "Jordan Bell", "Javin DeLaurier", "Jaime Echenique", "Jay Huff", 
                         "Jon Teske") ~ "C",
      PLAYER_NAME %in% c("DeMar DeRozan", "Giannis Antetokounmpo", "Kevin Durant", "Miles Bridges", 
                         "Pascal Siakam", "Julius Randle", "Jaren Jackson Jr.", "Harrison Barnes", 
                         "Tobias Harris", "Bojan Bogdanovic", "Domantas Sabonis", "Scottie Barnes", 
                         "Kyle Kuzma", "Aaron Gordon", "Evan Mobley", "Kristaps Porziņģis", 
                         "Wendell Carter Jr.", "Carmelo Anthony", "Jae'Sean Tate", "Jerami Grant", 
                         "Dorian Finney-Smith", "John Collins", "Marcus Morris Sr.", "Cameron Johnson", 
                         "Trey Lyles", "Jeff Green", "Danilo Gallinari", "Paul George", 
                         "Darius Bazley", "Herbert Jones", "Georges Niang", "Brandon Clarke", 
                         "Obi Toppin", "Jaden McDaniels", "Jae Crowder", "Chuma Okeke", 
                         "Robert Covington", "Grant Williams", "Doug McDermott", "Marvin Bagley III", 
                         "P.J. Tucker", "Kyle Anderson", "Otto Porter Jr.", "Jarred Vanderbilt", 
                         "Taurean Prince", "Nicolas Batum", "Rui Hachimura", "Jalen Smith", 
                         "Aleksej Pokusevski", "Isaiah Roby", "Rudy Gay", "JaMychal Green", 
                         "Maxi Kleber", "Lamar Stevens", "Draymond Green", "James Johnson", 
                         "Eric Paschall", "Thaddeus Young", "Larry Nance Jr.", "Stanley Johnson", 
                         "Terry Taylor", "Davis Bertans", "Derrick Jones Jr.", "Justise Winslow", 
                         "Zeke Nnaji", "Dean Wade", "Xavier Tillman", "Day'Ron Sharpe", 
                         "Anthony Gill", "Marquese Chriss", "Sandro Mamukelashvili", "Patrick Williams", 
                         "Wenyen Gabriel", "Nathan Knight", "Santi Aldama", "Juancho Hernangomez", 
                         "Markieff Morris", "Ish Wainright", "Gary Clark", "Semi Ojeleye", 
                         "Reggie Perry", "Jericho Sims", "KZ Okpala", "Tyler Cook", 
                         "JT Thor", "Vlatko Čančar", "Mamadi Diakite", "Jabari Parker", 
                         "Jalen Johnson", "Jamorko Pickett", "Usman Garuba", "Devontae Cacok", 
                         "Alize Johnson",  "Bol Bol", "D.J. Wilson", "Chris Silva", 
                         "Freddie Gillespie", "Malik Fitts", "Isaiah Todd", "Gabriel Deck", 
                         "Petr Cornelie", "Sekou Doumbouya", "Juwan Morgan", "Moses Wright", 
                         "Sam Dekker", "Emanuel Terry") ~ "PF",
      PLAYER_NAME %in% c("Jayson Tatum", "Jaylen Brown", "RJ Barrett", "Khris Middleton", 
                         "Saddiq Bey", "Keldon Johnson", "Andrew Wiggins", "Brandon Ingram", 
                         "Jimmy Butler III", "Franz Wagner", "Mikal Bridges", "Kelly Oubre Jr.", 
                         "Lauri Markkanen", "Kevin Huerter", "Terance Mann", "Devin Vassell", 
                         "Norman Powell", "OG Anunoby", "Gordon Hayward", "Chris Boucher", 
                         "Justin Holiday", "Max Strus", "De'Andre Hunter", "Cedi Osman", 
                         "KJ Martin", "Deni Avdija", "Josh Giddey", "Jonathan Kuminga", 
                         "Bruce Brown", "Pat Connaughton", "Corey Kispert", "Oshae Brissett", 
                         "Dillon Brooks", "Isaac Okoro", "Reggie Bullock Jr.", "Royce O'Neale", 
                         "Caleb Martin", "Cody Martin", "Torrey Craig", "Ziaire Williams", 
                         "Cam Reddish", "Jordan Nwora", "Javonte Green", "Nassir Little", 
                         "Jeremy Lamb", "Danny Green", "Trendon Watford", "Kenrich Williams", 
                         "Jalen McDaniels", "CJ Elleby", "Keita Bates-Diop", "Trey Murphy III", 
                         "Josh Jackson", "Joe Ingles", "Naji Marshall", "Juan Toscano-Anderson", 
                         "Joshua Primo", "Kessler Edwards", "DeAndre' Bembry", "Troy Brown Jr.", 
                         "Svi Mykhailiuk", "Danuel House Jr.", "David Nwaba", "Timothe Luwawu-Cabarrot", 
                         "Greg Brown III", "Maurice Harkless", "Ignas Brazdeikis", "Aaron Nesmith", 
                         "Thanasis Antetokounmpo", "Kelan Martin", "Yuta Watanabe", "Rodney Hood", 
                         "Joe Harris", "Admiral Schofield", "Kent Bazemore", "Andre Iguodala", 
                         "Isaiah Livers", "Dylan Windler", "Jaylen Hoard", "Justin Anderson", 
                         "Trevor Ariza", "Keljin Blevins", "Kevin Knox II", "Michael Porter Jr.", 
                         "Georgios Kalaitzakis", "Justin Champagnie", "Jake Layman", "Braxton Key", 
                         "Sam Hauser", "Alfonzo McKinnie", "Leandro Bolmaro", "Theo Pinson", 
                         "Louis King", "Haywood Highsmith", "Didi Louzada", "Abdel Nader", 
                         "Paul Watson", "Chaundee Brown Jr.", "James Ennis III", "BJ Johnson", 
                         "Cameron Oliver", "Wes Iwundu", "Justin Jackson", "Aleem Ford", 
                         "Cameron McGriff", "Yves Pons", "Isaac Bonga", "Solomon Hill", 
                         "Eugene Omoruyi", "Robert Woodard II", "Paris Bass", "Trevon Scott", 
                         "Xavier Sneed", "Chandler Hutchison", "Jemerrio Jones", "Matt Ryan", 
                         "Aaron Henry", "George King", "Zylan Cheatham", "Feron Hunt", 
                         "Arnoldas Kulboka", "Anthony Lamb") ~ "SF",
      PLAYER_NAME %in% c("Devin Booker","Donovan Mitchell", "Zach LaVine", "Anthony Edwards", 
                         "Terry Rozier", "Jordan Poole", "Desmond Bane", "Tyler Herro", 
                         "Jalen Brunson", "Gary Trent Jr.", "Jordan Clarkson", "Buddy Hield", 
                         "Reggie Jackson", "Jalen Green", "Evan Fournier", "Cade Cunningham", 
                         "Malik Monk", "Will Barton", "Kentavious Caldwell-Pope", "Derrick White", 
                         "Anfernee Simons", "Caris LeVert", "Seth Curry", "Malik Beasley", 
                         "Bogdan Bogdanović", "Alec Burks", "Bradley Beal", "Patty Mills", 
                         "Luguentz Dort", "Duncan Robinson", "Lonnie Walker IV", "Luke Kennard", 
                         "Josh Hart", "De'Anthony Melton", "Eric Gordon", "Grayson Allen", 
                         "Chris Duarte", "Bones Hyland", "Nickeil Alexander-Walker", "Ayo Dosunmu", 
                         "Gary Harris", "Josh Richardson", "Bryn Forbes", "Ben McLemore", 
                         "Klay Thompson", "Garrison Mathews", "Hamidou Diallo", "Terrence Ross", 
                         "Amir Coffey", "Tim Hardaway Jr.", "Talen Horton-Tucker", "Josh Christopher", 
                         "Landry Shamet", "Cam Thomas", "Jaylen Nowell", "Furkan Korkmaz", 
                         "Gary Payton II", "R.J. Hampton", "Damion Lee", "Shake Milton", 
                         "Austin Reaves", "Aaron Wiggins", "Austin Rivers", "Avery Bradley", 
                         "Lance Stephenson", "Donte DiVincenzo", "Matisse Thybulle", "John Konchar", 
                         "Brandon Boston", "Delon Wright", "Ty Jerome", "Josh Green", 
                         "Terence Davis", "Garrett Temple", "Alex Caruso", "Armoni Brooks", 
                         "Wayne Ellington", "Quentin Grimes", "Rodney McGruder", "Keon Johnson", 
                         "Wesley Matthews", "Frank Ntilikina", "Moses Moody", "Romeo Langford", 
                         "Davon Reed", "Lindy Waters III", "Isaiah Joe", "Tomas Satoransky", 
                         "Vít Krejčí", "Tony Snell", "Collin Sexton", "Jared Butler", 
                         "Matt Thomas", "Sterling Brown", "James Bouknight", "Josh Okogie", 
                         "Jarrett Culver", "Markus Howard", "Elijah Hughes", "Victor Oladipo", 
                         "PJ Dozier", "Skylar Mays", "Daishen Nix", "Kyle Guy", 
                         "Malcolm Hill", "Denzel Valentine", "Joe Wieskamp", "Jahmi'us Ramsey", 
                         "Xavier Moon", "Cassius Stanley", "Jay Scrubb", "Trevelin Queen", 
                         "Melvin Frazier Jr.", "Quinndary Weatherspoon", "Charlie Brown Jr.", 
                         "Mason Jones", "Sam Merrill", "Carsen Edwards", "Brodric Thomas",
                         "Rayjon Tucker", "Nik Stauskas", "Tyrone Wallace", "Ruben Nembhard Jr.", 
                         "Langston Galloway", "Marcus Garrett", "Gabriel Lundberg", "Gabe York", 
                         "Mac McClung", "Dakota Mathias", "Craig Sword", "Miye Oni",
                         "Jordan Schakel", "Wayne Selden", "Damyean Dotson", "Shaquille Harrison", 
                         "Scotty Hopson", "Rob Edwards", "Deividas Sirvydis", "Joel Ayayi", 
                         "Shaq Buchanan", "Ahmad Caver", "Jarron Cumberland", "DaQuan Jeffries",
                         "Joe Johnson", "Jaylen Morris", "Jaden Springer", "Scottie Lewis", 
                         "Jordan Goodwin", "Tyler Hall", "Nate Hinton", "DeJon Jarreau", 
                         "David Johnson", "CJ Miles", "Matt Mooney", "Ade Murkey", "Trayvon Palmer", 
                         "MJ Walker") ~ "SG",
      TRUE ~ "To_Map" 
    ),
    across(c(PTS, AGE, AST, REB, OREB, DREB ,TOV, BLK, FG_PCT, FG3_PCT, FT_PCT), as.numeric),
    Position = as.factor(Position)
    )


# Facilitar carregamentos futuros
dados_analise |> write.csv2("includes/nba.csv", row.names = FALSE)


# Position - Armador (PG), Ala-armador (SG), Ala (SF), Ala-pivô (PF) e Pivô (C)



# Análise Exploratória de Dados -------------------------------------------


dados_analise |>
  dplyr::select(AGE, PTS, AST, REB, OREB, DREB, TOV, STL, BLK , Peso, Altura) |> 
  summarise(across(
    everything(),
    list(
      Média   = ~mean(.x, na.rm = TRUE),
      Mediana = ~median(.x, na.rm = TRUE),
      SD      = ~sd(.x, na.rm = TRUE),
      Mín     = ~min(.x, na.rm = TRUE),
      Q1      = ~quantile(.x, 0.25, na.rm = TRUE),
      Q3      = ~quantile(.x, 0.75, na.rm = TRUE),
      Máx     = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Variável", "Estatística"),
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = Variável,
    values_from = value
  ) |>
  kable(digits = 2, caption = "Resumo Estatístico dos Dados (per36)")


table(dados_analise$Position) |>
  kable(col.names = c("Posição", "Frequência"))


ggplot(dados_analise, aes(x = AGE)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "Distribuição da Idade dos Jogadores",
       x = "Idade", y = "Frequência")


ggplot(dados_analise, aes(x = Position)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribuição das Posições dos Jogadores",
    x = "Posição",
    y = "Quantidade de Jogadores"
  )


ggplot(dados_analise, aes(x = Position, y = PTS, fill = Position)) +
  geom_boxplot() +
  labs(title = "Distribuição de Pontos por Posição",
       x = "Posição", y = "Pontos")


ggplot(dados_analise, aes(x = AST, y = REB, color = Position)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Separabilidade entre Posições (Assistências vs Rebotes)",
    x = "Assistências",
    y = "Rebotes"
  )


ggplot(dados_analise, aes(x = FG_PCT)) +
  geom_histogram(bins = 15, fill = "darkgreen", color = "black") +
  labs(title = "Distribuição do FG%",
       x = "Field Goal %", y = "Frequência")



corrplot(
  cor(dados_analise %>% dplyr::select(AGE, PTS, AST, REB, OREB, DREB, TOV, STL, BLK, FG_PCT, FG3_PCT, FT_PCT, Altura, Peso), use = "complete.obs"),
  method = "color",
  tl.col = "black"
)



# Modelagem I -------------------------------------------------------------

dados_analise <- read.csv2("includes/nba.csv", sep = ";") 

dados_analise <- dados_analise |>
  mutate(
    across(c(PTS, AGE, AST, REB, OREB, DREB ,TOV, BLK, FG_PCT, FG3_PCT, FT_PCT), as.numeric),
    Position = as.factor(Position)
  ) |>
  dplyr::select(-TEAM_ABBREVIATION, -PLAYER_NAME) # Removendo variável irrelevantes


set.seed(16723)

dados_analise_split <- initial_split(dados_analise, prop = .7, strata = Position)
train_data <- training(dados_analise_split)
test_data <- testing(dados_analise_split)



dados_analise_rec <- recipe(Position ~ ., data = train_data) |>
  
  step_mutate(
    AST_TOV_RATIO = AST / (TOV + 0.1),
    REB_HEIGHT_INTER = REB * Altura,
    PLAYMAKER_SCORE = AST - TOV,
    RIM_PROTECT = REB + BLK,
    PTS_EFF = PTS * FG_PCT) |>  
  step_YeoJohnson(all_numeric_predictors()) |> # Transformação Yeo-Johnson
  step_normalize(all_numeric_predictors()) |> # normaliza variáveis numéricas para terem média 0 e variância 1
  step_corr(all_numeric_predictors(), threshold = 0.8,
            method = "spearman"
  ) # remove preditores que tenham alta correlação com algum outro preditor


prepped_data <- dados_analise_rec |> # usa a receita
  prep() |> # aplica a receita no conjunto de treinamento
  juice()# extrai apenas o dataframe preprocessado


cv_folds <- vfold_cv(train_data, v = 10, strata = Position)


knn_spec <- nearest_neighbor(neighbors = tune()) %>% # K-NN
  set_engine("kknn")%>%
  set_mode("classification")

nbayes_spec <- naive_Bayes() %>% # Naive Bayes
  set_engine("naivebayes") %>%
  set_mode("classification")


multinom_spec <- multinom_reg() %>% # Reg Multinominal
  set_engine("nnet") %>% 
  set_mode("classification")

lda_spec <- discrim_linear() %>% # Linear discriminant analysis
  set_engine("MASS") %>%
  set_mode("classification")

qda_spec <- discrim_quad() %>% # Quadratic discriminant analysis
  set_engine("MASS") %>%
  set_mode("classification")


wf = workflow_set(
  preproc = list(dados_analise_rec),
  models = list(
    knn_fit = knn_spec,
    nbayes_fit = nbayes_spec,
    linear_fit = multinom_spec,
    lda_fit = lda_spec,
    qda_fit = qda_spec
  )
) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))


grid_ctrl = control_grid(
  save_pred = TRUE,
  parallel_over = "resamples",
  save_workflow = TRUE
)
grid_results = wf %>%
  workflow_map(
    seed = 16723,
    resamples = cv_folds,
    grid = 10,
    control = grid_ctrl
  )


autoplot(grid_results, metric = "accuracy")

autoplot(grid_results, select_best = TRUE ,metric = "accuracy")

autoplot(grid_results)

results_acc = workflowsets::rank_results(grid_results,
                                         select_best = TRUE,
                                         rank_metric = "accuracy") %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(wflow_id, mean, std_err, model, rank)
results_acc %>% gt()

best_set_linear = grid_results %>% 
  extract_workflow_set_result("linear_fit") %>% 
  select_best(metric = "accuracy")
best_set_knn = grid_results %>% 
  extract_workflow_set_result("knn_fit") %>% 
  select_best(metric = "accuracy")
best_set_nbayes = grid_results %>%
  extract_workflow_set_result("nbayes_fit") %>% 
  select_best(metric = "accuracy")
best_set_lda = grid_results %>% 
  extract_workflow_set_result("lda_fit") %>% 
  select_best(metric = "accuracy")
best_set_qda = grid_results %>% 
  extract_workflow_set_result("qda_fit") %>% 
  select_best(metric = "accuracy")

test_results <- function(rc_rslts, fit_obj, par_set, split_obj) {
  res <- rc_rslts %>%
    extract_workflow(fit_obj) %>%
    finalize_workflow(par_set) %>%
    last_fit(split = split_obj,
             metrics = metric_set(
               accuracy,roc_auc,
               f_meas,precision,
               recall,spec,kap))
  res
}

test_results_linear = test_results(grid_results,"linear_fit",best_set_linear,dados_analise_split)
test_results_knn = test_results(grid_results,"knn_fit",best_set_knn,dados_analise_split)
test_results_nbayes = test_results(grid_results,"nbayes_fit",best_set_nbayes,dados_analise_split)
test_results_lda = test_results(grid_results,"lda_fit",best_set_lda,dados_analise_split)
test_results_qda = test_results(grid_results,"qda_fit",best_set_qda,dados_analise_split)

metrics_table = rbind(
  collect_metrics(test_results_linear)$.estimate,
  collect_metrics(test_results_knn)$.estimate,
  collect_metrics(test_results_nbayes)$.estimate,
  collect_metrics(test_results_lda)$.estimate,
  collect_metrics(test_results_qda)$.estimate
)

metrics_table <- round(metrics_table, 4)
rnms = c("multi_reg","k_nn","naive_bayes", "lin_discr","quad_discr")
metrics_table <- cbind(rnms, metrics_table)
metrics_table <- metrics_table %>% dplyr::as_tibble()

colnames(metrics_table) = c("method","acc","roc_auc","f_meas",
                            "precision","recall","spec","kappa")


metrics_table %>%
  arrange(desc(acc),desc(roc_auc),desc(f_meas),desc(kappa)) %>%
  gt()


# Modelagem Alternativa ---------------------------------------------------


dados_analise <- dados_analise %>%
  mutate(
    Court = factor(case_when(
      Position %in% c("PG", "SG") ~ "Backcourt",
      Position %in% c("SF", "PF", "C") ~ "Frontcourt",
    )
    )) |>
  dplyr::select(-Position)


set.seed(16723)

dados_analise_split <- initial_split(dados_analise, prop = .7, strata = Court)
train_data <- training(dados_analise_split)
test_data <- testing(dados_analise_split)

dados_analise_rec <- recipe(Court ~ ., data = train_data) |>
  
  step_mutate(
    AST_TOV_RATIO = AST / (TOV + 0.1),
    REB_HEIGHT_INTER = REB * Altura,
    PLAYMAKER_SCORE = AST - TOV,
    RIM_PROTECT = REB + BLK,
    PTS_EFF = PTS * FG_PCT) |>  
  step_YeoJohnson(all_numeric_predictors()) |> # Transformação Yeo-Johnson
  step_normalize(all_numeric_predictors()) |> # normaliza variáveis numéricas para terem média 0 e variância 1
  step_corr(all_numeric_predictors(), threshold = 0.8,
            method = "spearman"
  ) # remove preditores que tenham alta correlação com algum outro preditor


prepped_data <- dados_analise_rec |> # usa a receita
  prep() |> # aplica a receita no conjunto de treinamento
  juice()# extrai apenas o dataframe preprocessado


cv_folds <- vfold_cv(train_data, v = 10, strata = Court)

knn_spec <- nearest_neighbor(neighbors = tune()) %>% # K-NN
  set_engine("kknn")%>%
  set_mode("classification")

nbayes_spec <- naive_Bayes() %>% # Naive Bayes
  set_engine("naivebayes") %>%
  set_mode("classification")


logistic_spec <- logistic_reg() %>% # RL
  set_engine(engine = "glm") %>%
  set_mode("classification")

lda_spec <- discrim_linear() %>% # Linear discriminant analysis
  set_engine("MASS") %>%
  set_mode("classification")

qda_spec <- discrim_quad() %>% # Quadratic discriminant analysis
  set_engine("MASS") %>%
  set_mode("classification")


wf = workflow_set(
  preproc = list(dados_analise_rec),
  models = list(
    knn_fit = knn_spec,
    nbayes_fit = nbayes_spec,
    linear_fit = logistic_spec,
    lda_fit = lda_spec,
    qda_fit = qda_spec
  )
) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))


grid_ctrl = control_grid(
  save_pred = TRUE,
  parallel_over = "resamples",
  save_workflow = TRUE
)
grid_results = wf %>%
  workflow_map(
    seed = 16723,
    resamples = cv_folds,
    grid = 10,
    control = grid_ctrl
  )


autoplot(grid_results, metric = "accuracy")

autoplot(grid_results, select_best = TRUE ,metric = "accuracy")

autoplot(grid_results)

results_acc = workflowsets::rank_results(grid_results,
                                         select_best = TRUE,
                                         rank_metric = "accuracy") %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(wflow_id, mean, std_err, model, rank)
results_acc %>% gt()

best_set_linear = grid_results %>% 
  extract_workflow_set_result("linear_fit") %>% 
  select_best(metric = "accuracy")
best_set_knn = grid_results %>% 
  extract_workflow_set_result("knn_fit") %>% 
  select_best(metric = "accuracy")
best_set_nbayes = grid_results %>%
  extract_workflow_set_result("nbayes_fit") %>% 
  select_best(metric = "accuracy")
best_set_lda = grid_results %>% 
  extract_workflow_set_result("lda_fit") %>% 
  select_best(metric = "accuracy")
best_set_qda = grid_results %>% 
  extract_workflow_set_result("qda_fit") %>% 
  select_best(metric = "accuracy")


test_results <- function(rc_rslts, fit_obj, par_set, split_obj) {
  res <- rc_rslts %>%
    extract_workflow(fit_obj) %>%
    finalize_workflow(par_set) %>%
    last_fit(split = split_obj,
             metrics = metric_set(
               accuracy,roc_auc,
               f_meas,precision,
               recall,spec,kap))
  res
}


test_results_linear = test_results(grid_results,"linear_fit",best_set_linear,dados_analise_split)
test_results_knn = test_results(grid_results,"knn_fit",best_set_knn,dados_analise_split)
test_results_nbayes = test_results(grid_results,"nbayes_fit",best_set_nbayes,dados_analise_split)
test_results_lda = test_results(grid_results,"lda_fit",best_set_lda,dados_analise_split)
test_results_qda = test_results(grid_results,"qda_fit",best_set_qda,dados_analise_split)


metrics_table = rbind(
  collect_metrics(test_results_linear)$.estimate,
  collect_metrics(test_results_knn)$.estimate,
  collect_metrics(test_results_nbayes)$.estimate,
  collect_metrics(test_results_lda)$.estimate,
  collect_metrics(test_results_qda)$.estimate
)

metrics_table <- round(metrics_table, 4)
rnms = c("logistic_reg","k_nn","naive_bayes", "lin_discr","quad_discr")
metrics_table <- cbind(rnms, metrics_table)
metrics_table <- metrics_table %>% dplyr::as_tibble()

colnames(metrics_table) = c("method","acc","roc_auc","f_meas",
                            "precision","recall","spec","kappa")

metrics_table %>%
  arrange(desc(acc),desc(roc_auc),desc(f_meas),desc(kappa)) %>%
  gt()









