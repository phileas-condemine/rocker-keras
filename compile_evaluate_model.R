compile_evaluate_model <- function(model,x_train,y_train,x_test,y_test){

  
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
    validation_split = 0.3,callbacks=list(callback_tensorboard(log_dir = "keras_demo/logs/",write_graph = TRUE,write_images = TRUE,write_grads = TRUE,),
                                          callback_early_stopping(monitor = "val_loss",patience=100,mode = "min"),
                                          callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.8,patience=10,verbose=1))
  )
  
  score <- model %>% evaluate(
    x_test, y_test,
    batch_size = batch_size,
    verbose = 1
  )
  
  print(paste0("RMSE on test set ",sqrt(score$mean_squared_error)))
  return(list(model,history,score))
}
