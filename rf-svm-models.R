library(tidyverse)
library(readxl)
data_raw <- read_excel(file.choose())
data_raw1 <- na.omit(data_raw)
data_raw1
data_raw1 %>% view()
data_raw1 %>% distinct(WHO_CODE)

data_raw1 %>% count(WHO_CODE,sort = TRUE)

library(tidytext)

data_raw1 %>% filter(!is.na(TEXT)) %>%
  mutate(YEAR = fct_inorder(YEAR),
         WHO_CODE = fct_lump_n(WHO_CODE,10)) %>%
  count(YEAR,WHO_CODE) %>%
  mutate(WHO_CODE = reorder_within(WHO_CODE,n,YEAR)) %>%
  ggplot(aes(n,WHO_CODE,fill=YEAR)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~YEAR,scales = "free") + 
  scale_y_reordered() +
  labs(y=NULL)




data <- data_raw1 %>% 
  filter(!is.na(TEXT)) %>%
  mutate(digestive = if_else(WHO_CODE == "11 Diseases of the digestive system","11 Diseases of the digestive system","09 Diseases of the circulatory system")) %>%
  select(digestive, YEAR, text = TEXT)

data


data %>% 
  filter(digestive=="11 Diseases of the digestive system") %>% 
  sample_n(10) %>% 
  pull(text)



library(tidylo)

data_lo <- data %>% 
  unnest_tokens(word,text) %>% 
  count(digestive,word) %>% 
  bind_log_odds(digestive,word,n) %>% 
  arrange(-log_odds_weighted)


data_lo


data_lo  %>% 
  group_by(digestive) %>% 
  slice_max(log_odds_weighted,n=15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,log_odds_weighted)) %>% 
  ggplot(aes(log_odds_weighted,word,fill = digestive)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~digestive, scales = "free") + labs(y = NULL) 



library(textfeatures)

tf <- textfeatures(data,sentiment = FALSE, word_dims = 0,normalize = FALSE)

tf


tf %>%  
  bind_cols(data)  %>% 
  group_by(digestive) %>% 
  summarise(across(starts_with("n_"),mean)) %>% 
  pivot_longer(starts_with("n_"),names_to = "text_feature") %>% 
  filter(value > 0.01) %>% 
  mutate(text_feature = fct_reorder(text_feature, -value)) %>% 
  ggplot(aes(digestive,value,fill = digestive)) +
  geom_col(position = "dodge",alpha = 0.8, show.legend = FALSE) + 
  facet_wrap(~text_feature,scales = "free",ncol = 6) +
  labs(x = NULL,y = "Mean text features per spoken line")



## Build Two Models


library(tidymodels)

set.seed(123)
data_split <- initial_split(data, strata = digestive)
data_train <- training(data_split)
data_test <- testing(data_split)


set.seed(234)
data_folds <- vfold_cv(data_train, strata = digestive)
data_folds 


## data preprocessing
library(themis)
library(textrecipes)

data_rec <- recipe(digestive ~ text, data = data_train) %>% 
  step_downsample(digestive) %>% 
  step_textfeature(text) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

data_rec


data_prep <- prep(data_rec)
juice(data_prep)


## Random Forest Model

rf_spec <- rand_forest(trees = 1000)  %>% 
  set_engine("ranger")  %>% 
  set_mode("regression")
rf_spec

svm_spec <- svm_rbf(cost = 0.5)  %>% 
  set_engine("kernlab")  %>% 
  set_mode("classification")
svm_spec


data_wf <- workflow() %>% 
  add_recipe(data_rec)
data_wf

doParallel::registerDoParallel()

set.seed(1234)
rf_rs <- data_wf %>% 
  add_model(rf_spec) %>% 
  fit_resamples(
    resamples = data_folds,
    metrics = metric_set(roc_auc,accuracy,sens,spec,precision,recall),
    control = control_resamples(save_pred = TRUE)
  )

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)


set.seed(2345)
svm_rs <- data_wf %>% 
  add_model(svm_spec) %>% 
  fit_resamples(
    resamples = data_folds,
    metrics = metric_set(roc_auc,accuracy,sens,spec,precision,recall),
    control = control_resamples(save_pred = TRUE)
  )

collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)


## Evaluaate model

svm_rs %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(digestive, `.pred_11 Diseases of the digestive system`) %>% 
  ggplot(aes(1-specificity,sensitivity,color = id)) +
  geom_abline(lty = 2, color = "brown", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()




library(vip)

set.seed(345)
data_imp <- data_wf %>%
  add_model(svm_spec) %>%
  fit(data_train) %>%
  extract_fit_parsnip() %>% 
  vi(
    method = "permute", nsim = 10,
    target = "digestive", metric = "auc", reference_class = "09 Diseases of the circulatory system",
    pred_wrapper = kernlab::predict, train = juice(data_prep)
  )

data_imp %>%
  slice_max(Importance, n = 8) %>%
  mutate(
    Variable = str_remove(Variable, "textfeature_text_n_"),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(Importance, Variable, color = Variable)) +
  geom_errorbar(aes(xmin = Importance - StDev, xmax = Importance + StDev),
                alpha = 0.5, size = 1.3
  ) +
  geom_point(size = 3) +
  theme(legend.position = "none") +
  labs(y = NULL)


data_final <- data_wf %>%
  add_model(svm_spec) %>%
  last_fit(data_split)

data_final %>%
  collect_metrics()


data_final %>%
  collect_predictions() %>%
  conf_mat(digestive, .pred_class)














