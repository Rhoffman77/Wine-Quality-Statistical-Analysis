# Stats Project 
getwd()
setwd("/Users/rosiehoffman")
install.packages("MASS")
library(MASS)
data <- WineQT
install.packages("VGAM")
library(VGAM)

#split data training and test
library(caret)
set.seed(123)
train_index <- createDataPartition(data$quality, p = 0.60, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
dim(train_data) 
dim(test_data) 

data$quality <- factor(data$quality, ordered=TRUE, levels=3:8)
train_data$quality <- factor(train_data$quality, ordered = TRUE, levels = 3:8)
test_data$quality <- factor(test_data$quality, ordered = TRUE, levels = 3:8)




model <- vglm(as.ordered(quality) ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + `chlorides` + 
                `free sulfur dioxide` + `total sulfur dioxide` + `density` + `pH` + `sulphates` + `alcohol`, 
              data = train_data, family = cumulative(parallel = TRUE))
summary(model)
pred_probs <- predict(model,newdata=test_data, type = "response")
pred_classes <- apply(pred_probs, 1, function(x) which.max(x))
actual_classes <- as.numeric(as.ordered(test_data$quality))
accuracy <- mean(pred_classes == actual_classes)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
#59.34%

#confusion matrix model 1
pred_probs <- predict(model, newdata = test_data, type = "response")

# Step 2: Convert predicted probabilities to predicted classes
levels_order <- 3:8  # Levels for the ordered factor (quality)
pred_classes <- apply(pred_probs, 1, function(x) which.max(x))+2
pred_classes_factor <- factor(pred_classes, levels = 3:8, labels = levels_order)

# Step 3: Convert actual values (from test_data) to numeric for easier comparison
actual_classes <- as.numeric(as.ordered(test_data$quality))

# Step 4: Create confusion matrix
conf_matrix1 <- table(Predicted = pred_classes_factor, Actual = test_data$quality)
print(conf_matrix1)
# Print confusion matrix
print(conf_matrix1)
conf_df <- as.data.frame(as.table(conf_matrix1))
ggplot(conf_df, aes(x=Predicted, y=Actual, fill=Freq))+
  geom_tile(color="white")+
  geom_text(aes(label=Freq),color="black",size=5)+
  scale_fill_gradient(low="lightblue",high="darkblue")+
  labs(
    title="Confusion Matrix Full Model",
    x="Predicted Quality",
    y="Actual Quality",
    fill="Count"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)+
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),  # Increase title size
        axis.title.x = element_text(size = 14),  # Increase x-axis title size
        axis.title.y = element_text(size = 14),  # Increase y-axis title size
        plot.title.position = "plot"
  ))







#variable selection with StepAIC
step_model <- step4vglm(model, direction = "backward")
summary(step_model)

model2 <- vglm(as.ordered(quality)~`fixed acidity`+`volatile acidity`+`chlorides`+`free sulfur dioxide`+`total sulfur dioxide`+density+sulphates+alcohol, data=train_data, family=cumulative(parallel=TRUE))
summary(model2)
test_data$quality <- factor(test_data$quality, ordered = TRUE, levels = 3:8)
pred_probs <- predict(model2, newdata = test_data, type = "response")
levels_order <- 3:8
pred_classes <- apply(pred_probs, 1, function(x) which.max(x))
pred_classes_factor <- factor(pred_classes, levels = 3:8, labels = levels_order)
# Convert the actual `quality` values to numeric for easier comparison
actual_classes <- as.numeric(as.ordered(test_data$quality))
accuracy <- mean(pred_classes == actual_classes)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
#59.12%


pred_probs <- predict(model2, newdata = test_data, type = "response")

# Step 2: Convert predicted probabilities to predicted classes
levels_order <- 3:8  # Levels for the ordered factor (quality)
pred_classes <- apply(pred_probs, 1, function(x) which.max(x))+2
pred_classes_factor <- factor(pred_classes, levels = 3:8, labels = levels_order)

# Step 3: Convert actual values (from test_data) to numeric for easier comparison
actual_classes <- as.numeric(as.ordered(test_data$quality))

# Step 4: Create confusion matrix
conf_matrix <- table(Predicted = pred_classes_factor, Actual = test_data$quality)

# Print confusion matrix
print(conf_matrix)
conf_df <- as.data.frame(as.table(conf_matrix))
ggplot(conf_df, aes(x=Predicted, y=Actual, fill=Freq))+
  geom_tile(color="white")+
  geom_text(aes(label=Freq),color="black",size=5)+
  scale_fill_gradient(low="lightblue",high="darkblue")+
  labs(
    title="Confusion Matrix Fitted Model",
    x="Predicted Quality",
    y="Actual Quality",
    fill="Count"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)  # This centers the title
  )
  

#color confusion matrix 





#look at stats notes for explanation! 
ggplot(train_data, aes(x = alcohol, y = as.ordered(quality))) +
  geom_jitter(aes(color = `volatile acidity`), alpha = 0.6) +  # Scatter plot
  labs(
    title = "Scatter Plot of Alcohol vs Wine Quality",
    x = "Alcohol Content",
    y = "Wine Quality",
    color = "Volatile Acidity"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)  # Centers the title
  )


ggplot(train_data, aes(x = `alcohol`, y = as.ordered(quality))) +
  geom_jitter(aes(color = sulphates), alpha = 0.6) +  # Scatter plot
  labs(
    title = "Alcohol vs Wine Quality",
    x = "Alcohol",
    y = "Wine Quality",
    color = "Sulphates"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centers the title
  )+
  scale_color_viridis(option = "C") +  # Apply viridis color scale
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5)  # Centers the legend title
  )
# Centers the title
  )
#shows relationship betweeen varibales. Change legend name. Can say higher alcohol usuallly means higher quality if the graph shows that, etc. 
#based on which variables most important (highest slopes) 
# Scatter Plot of Free Sulfur Dioxide vs Wine Quality
ggplot(train_data, aes(x = `volatile acidity`, y = as.ordered(quality))) +
  geom_jitter(aes(color = alcohol), alpha = 0.6) +  # Scatter plot
  labs(
    title = "Free Sulfur Dioxide vs Wine Quality",
    x = "Volatile Acidity",
    y = "Wine Quality",
    color = "alcohol"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centers the title
  )




install.packages("viridis")
library(viridis)
ggplot(train_data, aes(x = alcohol, y = as.ordered(quality))) +
  geom_jitter(aes(color = `volatile acidity`), alpha = 0.6) +  # Scatter plot
  labs(
    title = "Alcohol vs Wine Quality",
    x = "Alcohol Content",
    y = "Wine Quality",
    color = "Volatile Acidity"
  ) +
  scale_color_viridis(option = "C") +  # Apply viridis color scale
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centers the title
  legend.title = element_text(hjust = 0.5)  # Centers the legend title
)


install.packages("viridis")
library(viridis)
ggplot(train_data, aes(x = alcohol, y = as.ordered(quality))) +
  geom_jitter(aes(color = `sulphates`), alpha = 0.6) +  # Scatter plot
  labs(
    title = "Alcohol vs Wine Quality",
    x = "Alcohol Content",
    y = "Wine Quality",
    color = "Sulphates"
  ) +
  scale_color_viridis(option = "C") +  # Apply viridis color scale
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centers the title
    legend.title = element_text(hjust = 0.5)  # Centers the legend title
  )
