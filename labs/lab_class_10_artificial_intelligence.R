#--------------------------------------------
# Setup
#--------------------------------------------
rm(list = ls())
# note, run install_keras() after you have 
# installed the package 'keras'
library('keras')

# this deletes any existing keras objects
k_clear_session()

#--------------------------------------------
# Load MNIST dataset
#--------------------------------------------
# load the MNIST dataset using this keras function
mnist <- dataset_mnist()
summary(mnist)
dim(mnist$train$x)


#--------------------------------------------
# Clean and transform data
#--------------------------------------------
# let's see what one of them looks like
mnist$train$x[2, ,]

# create test and training labels and images
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# transform data so that it is all 28x28 pixels, 
# and each pixel has a value between 0 and 1
train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

# encode labels to categorical such that labels are factors (0,1,...,9)
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)


#--------------------------------------------
# Define feed-forward network architecture 
#--------------------------------------------
# first we'll start with a simple feed-forward deep neural network 

# network has 28*28=784 inputs 
# 512 hidden units 
# followed by a relu activation function 
# finally we apply a "softmax" activation to predict one of 
# ten lables (digits 0,1,.,,,9)
network_base <- keras_model_sequential() %>%
  layer_dense(units = 512, 
              activation = "relu", 
              input_shape = c(28 * 28)) %>%
  layer_dense(units = 10, activation = "softmax")


# print network to check network architecture
print(network_base)
# parameters dense_1 = inputs * neurons + bias = 28 * 28 * 512 + 512
# parameters dense_2 = inputs * neurons + bias = 512 * 10 + 10


#--------------------------------------------
# Define model compilation stage 
#--------------------------------------------
# here we set details that will be used when we train the model
# specifically what we do we want to optimize over? 
# how do we want to optimize it? 
network_base %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)


#--------------------------------------------
# Train or estimate model parameters
#--------------------------------------------
# finally we are ready to train and we use the "fit" function 
# to start the training process
# epochs = 5 means we do 5 passes of the data
# batch_size = 128 means we do this 128 images at a time
network_base %>% fit(train_images, train_labels, epochs = 5, batch_size = 128,
                     verbose = 2)

metrics_base <- network_base %>% evaluate(test_images, test_labels, verbose = 0)
metrics_base




#--------------------------------------------
# ***CNN Model***
#--------------------------------------------
# next let's try a convolutional neural network model 
# setup
k_clear_session()

# let's create some parameters
batch_size <- 128
num_classes <- 10
epochs <- 2

# input image dimensions
img_rows <- 28
img_cols <- 28

# load MNIST digit data
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape input 
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

# Convert class vectors to binary class matrices
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

#--------------------------------------------
# Define CNN model 
#--------------------------------------------
# here we build our CNN "architecture"
# we start with a convolutional layer with 32 filters
# followed by another CNN layer with 64 filters
# we apply dropout (20%) 
# then flatten the network and predict one of 10 classes
CNNmodel <- keras_model_sequential()
CNNmodel %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                input_shape = input_shape) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# print to see model 
print(CNNmodel)

# compile model
CNNmodel %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)


# load tensorboard
tensorboard("logs/run_d")

#--------------------------------------------
# Train and Evaluate Model 
#--------------------------------------------
history <- CNNmodel %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_data = list(x_test, y_test),
  validation_split = 0.2,
  callbacks = callback_tensorboard("logs/run_c")
)

plot(history)

scores <- CNNmodel %>% evaluate(
  x_test, y_test, verbose = 0
)



