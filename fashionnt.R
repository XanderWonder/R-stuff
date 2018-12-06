## Installation

# install latest miniconda
# install latest RTools.exe
# install latest R version

install.packages("devtools")
library(devtools)
install.packages("keras")
library(keras)

# pip install tensorflow
# pip install keras

## Data Wrangling
# Load the data from the website
data <- dataset_cifar100()

# Split the raw data into suitable groupings
# x = Input (pixel values), y = Output (number categories)
x_train <- data$train$x
y_train <- data$train$y
x_test <- data$test$x
y_test <- data$test$y

# Reshape into matrices
x_train <- array_reshape(x_train, c(nrow(x_train), 3072))
x_test <- array_reshape(x_test, c(nrow(x_test), 3072))

# Scale to turn them into range 0-1
x_train <- x_train / 255
x_test <- x_test / 255

# Turns the integers into classes suitable for Keras' processing
y_train <- to_categorical(y_train, 100)
y_test <- to_categorical(y_test, 100)

## Build the Neural Network

# Base network creation - used as scaffolding to attach other layers
neural_network <- keras_model_sequential()

# Add layers with the pipe (%>%) operator
neural_network %>% 
  layer_dense(units = 3072, activation = 'relu', input_shape = c(3072)) %>% 
  #layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 1536, activation = 'relu') %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 768, activation = 'relu') %>%
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 876, activation = 'relu') %>%
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 364, activation = 'relu') %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 192, activation = 'relu') %>%
  #layer_dropout(rate = 0.2) %>%
  #layer_dense(units = 96, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 48, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 24, activation = 'relu') %>%
  layer_dense(units = 100, activation = 'softmax')

# Compile the model

neural_network %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Train the model, recording performance in a variable
history <- neural_network %>% fit(
  x_train, y_train, 
  epochs = 5, batch_size = 384, 
  validation_split = 0.2,
  lr = 0.9
)

gen_image<-image_data_generator(featurewise_center = TRUE,
                     featurewise_std_normalization = TRUE,
                     rotation_range = 20,
                     width_shift_range = 0.30,
                     height_shift_range = 0.30,
                     horizontal_flip = TRUE  )
gen_image %>% fit_image_data_generator(x_train)

# View the history
# Only needed if Keras interactive graph doesn't load

plot(history)

# Evaluate performance

neural_network %>% evaluate(x_test, y_test)

# Generate predictions

neural_network %>% predict_classes(x_test)
