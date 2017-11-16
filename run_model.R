build_model <- function(x_train,y_train,x_test,y_test){
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 100, input_shape = ncol(x_train)) %>% 
    layer_activation(activation = 'relu') %>% 
    layer_dense(units=1)%>%
    layer_activation(activation = 'linear')
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam',#try also sgd 
    metrics = c('mean_squared_error')
  )
  
  history <- model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 2,
    validation_split = 0.3,callbacks=list(callback_tensorboard())
  )
  
  score <- model %>% evaluate(
    x_test, y_test,
    batch_size = batch_size,
    verbose = 1
  )
  
  print(paste0("RMSE on test set ",sqrt(score$mean_squared_error)))
  return(list(model,history,score))
}
