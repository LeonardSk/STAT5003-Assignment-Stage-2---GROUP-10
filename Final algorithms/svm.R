svm_model <- readRDS("/Users/leonardseok/Documents/University of Sydney/Computational Statistics STAT5003/Assignment/Stage 2/Final algorithms/model_smote_data_SVM.rds")

install.custom("e1071")
preds_SVM = predict(svm_model, test_raw)

#vi(svm_model)
#varImp(svm_model)
test_raw_unord <- factor( test_raw$readmitted , ordered = FALSE )
levels(test_raw_unord) <- c("<30",">30","NO")

confusionMatrix(preds_SVM, test_raw_unord, mode = "everything")
