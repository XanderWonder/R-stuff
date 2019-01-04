install.packages("devtools")
library(devtools)
install.packages("keras")
library(keras)
install.packages("jpeg")
library(jpeg)

source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library(EBImage)

setwd("C:\\Users\\Admin\\Documents\\chest-xray-pneumonia\\chest_xray\\chest_xray\\test\\NORMAL")
Nfiles <- list.files( pattern = "*.jpeg")
NImageList <-(lapply(Nfiles,readImage))

setwd("C:\\Users\\Admin\\Documents\\chest-xray-pneumonia\\chest_xray\\chest_xray\\test\\PNEUMONIA")
Pfiles <- list.files( pattern = "*.jpeg")
PImageList <- (lapply(Pfiles,readImage))


#newImageList <- array()
#for (i in 1:length(ImageList)){
 # img <- resize(ImageList[i],w =800, h=600)
 # newImageList[i] <- img
#}
#NewImageList <-lapply(ImageList, resize(ImageList,w = 800, h = 600))
#imgMatrix <- do.call('cbind',)
str(ImageList[1])
#for (i in 1:length(Nfiles)){
 # assign(Nfiles[i],
    #     img <- readImage(paste(folder,Nfiles[i], sep = "")),
      #analyze
     # B <- matrix(im2,nrow=1000,ncol=1000, byrow=FALSE, dimnames = NULL)
      #Haarimtest <- TOS2D(B, smooth = FALSE, nsamples = 1000)
     # summary(Haarimtest)

## Data Wrangling
# Load the data from the website


# Split the raw data into suitable groupings
# x = Input (pixel values), y = Output (number categories)
x_train <- data$train$x
y_train <- data$train$y
x_test <- data$test$x
y_test <- data$test$y

# Reshape into matrices
#for (file in Nfiles){
  
myjpg <- readJPEG(Nfiles[1])
dim(myjpg)

imagedata(myjpg)


  #img <- read.pnm(Nfiles[1])
  #str(img)
  #mat <- matrix(NA, img@size[1],img@size[2])
  #mat <- img@all
#}
abaa <- array_reshape(Nfiles, c(nrow(Nfiles), 1341))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Scale to turn them into range 0-1
x_train <- x_train / 255
x_test <- x_test / 255

# Turns the integers into classes suitable for Keras' processing
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

## Build the Neural Network

# Base network creation - used as scaffolding to attach other layers
neural_network <- keras_model_sequential()

# Add layers with the pipe (%>%) operator
neural_network %>%
  layer_dropout(rate =  0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model

neural_network %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#Make vector long as training data -> 0 if = Normal index, 1 in Pneumonia index
Trainvector <- vector(length=(length(NImageList)+length(PImageList)))
Trainvector[1:length(NImageList)] <- FALSE
Trainvector[(length(NImageList)+1):(length(NImageList)+length(PImageList))] <- TRUE
summary(Trainvector)
# Train the model, recording performance in a variable
Mnimagelist = as.matrix(NImageList)
history <- neural_network %>% fit(
  NImageList, Trainvector, 
  epochs = 30, batch_size = 12, 
  validation_split = 0.2
)

history <- neural_network %>% fit(
  PImageList, Trainvector, 
  epochs = 30, batch_size = 12, 
  validation_split = 0.2
)

# View the history
# Only needed if Keras interactive graph doesn't load

plot(history)

# Evaluate performance

neural_network %>% evaluate(x_test, y_test)

# Generate predictions

neural_network %>% predict_classes(x_test)
