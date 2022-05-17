install.custom("NeuralNetTools")
require(devtools)
source_gist('6206737')


log_reg_df <- readRDS("/Users/leonardseok/Documents/University of Sydney/Computational Statistics STAT5003/Assignment/Stage 2/ROC & variable importance/logistic_reg_df.rds")
log_reg_model <- readRDS("/Users/leonardseok/Documents/University of Sydney/Computational Statistics STAT5003/Assignment/Stage 2/ROC & variable importance/logistic_reg_fit.rds")

vi <- log_reg_model$fit %>% varImp() 


vi_df <- data.frame(var = rownames(vi), Importance = vi$Overall)
write_csv(vi_df, "/Users/leonardseok/Documents/University of Sydney/Computational Statistics STAT5003/Assignment/Stage 2/ROC & variable importance/logreg_varimp.csv")

vi_df %>% arrange(desc(Importance)) %>% ggplot(aes(x=var, y = Importance)) + geom_col()

probs = predict(log_reg_model, new_data=test_raw, type="prob")

log_reg_df_2 <- data.frame(obs = test_raw$readmitted, preds = predict(log_reg_model, new_data=test_raw))

df <- log_reg_df_2 %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(`<30` = probs$`.pred_<30`
         , `>30` = probs$`.pred_>30`
         , NO = probs$.pred_NO) 

df$obs <- factor( df$obs , ordered = FALSE )
levels(df$obs) <- c("<30",">30","NO")

df %>% 
  roc_curve(obs, `<30`:NO) %>%
  autoplot()

log_reg_df %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(`<30` = probs$`.pred_<30`
         , `>30` = probs$`.pred_>30`
         , NO = probs$.pred_NO) %>% 
  roc_auc(obs, `<30`:NO) 

confusionMatrix(predict(log_reg_model, new_data=test_raw)$.pred_class, test_raw$readmitted, mode = "everything")

