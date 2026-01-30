
dados_analise <- read.csv2("nba.csv", sep = ";") 

dados_analise <- dados_analise |>
  dplyr::select(-TEAM_ABBREVIATION, -PLAYER_NAME, -MIN) |>
  dplyr::mutate(
    Court = case_when(
      Position %in% c("PG", "SG") ~ "Backcourt",
      Position %in% c("SF", "PF", "C") ~ "Frontcourt"
    ),
    Court = factor(Court)
  )



str(dados_analise)
dados_analise |> skim()

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
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.8,
            method = "spearman"
  ) 
  

prepped_data <- dados_analise_rec |> prep() |> juice()

cv_folds <- vfold_cv(train_data, v = 10, strata = Position)


knn_spec <- nearest_neighbor(neighbors = tune()) %>% # K-NN
  set_engine("kknn")%>%
  set_mode("classification")

nbayes_spec <- naive_Bayes() %>% # Naive Bayes
  set_engine("naivebayes") %>%
  set_mode("classification")


multinom_spec <- multinom_reg() %>% 
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
autoplot(grid_results, select_best = TRUE, metric = "accuracy")


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
rnms = c("el_net","k_nn","naive_bayes", "lin_discr","quad_discr")
metrics_table <- cbind(rnms, metrics_table)
metrics_table <- metrics_table %>% dplyr::as_tibble()

colnames(metrics_table) = c("method","acc","roc_auc","f_meas",
                            "precision","recall","spec","kappa")

metrics_table %>% gt()


metrics_table %>%
  arrange(desc(acc),desc(roc_auc),desc(f_meas),desc(kappa)) %>%
  gt()

