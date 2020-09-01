Iris <- read.csv("ThinIris.csv") ## Read in the file
Iris.df <- as.data.frame(Iris) ## Convert to a dataframe for easier handling
Iris.df$actual_class<-ifelse(Iris.df$class == 'Iris-setosa', 'setosa', 'not setosa') ## We'll use the binary setosa vs. not setosa for this exercise
Iris.df <- Iris.df[,names(Iris.df) %in% c("sepalwidth","actual_class")] # Keep the only the sepalwidth and our new actual_class column

stripchart(Iris.df$sepalwidth ~ Iris.df$actual_class, col = "red", pch = 18, main="Sepal Widths for Setosa and Non-Setosa Irises", xlab = "Sepal Width (cm)") # Explore our data
abline(v=3.0, col="blue", lwd=3, lty=2) # Draw a vertical line. This will be one of our test cases
abline(v=3.4, col="blue", lwd=3, lty=2) # Draw a vertical line. Spoilers: This is best according to all classification methods
abline(v=3.6, col="blue", lwd=3, lty=2) # Draw a vertical line. This will be one of our test cases

width_tests <- c(3.0, 3.4, 3.6)
### Classification Error

ce<-c() # initialize an array to store the classification error
classifiction_error <-0 # initialize the classification_error variable 
sepal_width_min_ce <-1 # initialize a variable to store the min score


for (i in width_tests) {
  Iris.df$predicted_class_class <- ifelse(Iris.df$sepalwidth >= i, 'setosa','not setosa') # This is our classification rule. If evaluates to TRUE send to left (setosa predicted), if false, send to right (not-setosa predicted)
  
  ## left side
  left_branch.df <- subset(Iris.df, Iris.df$predicted_class_class == 'setosa') # left branch is predicted setosa
  left_rate <- nrow(left_branch.df)/nrow(Iris.df) # number of samples that went to the left branch divided by total in parent node.
  left_min_rate <- min(
    nrow(left_branch.df[left_branch.df$actual_class == 'setosa',]),
    nrow(left_branch.df[left_branch.df$actual_class != 'setosa',])
    )/nrow(left_branch.df) # Find the portion that is the lesser actual class (i.e. not predicted). If the lesser class has 0 instances then we know it is completely pure and will return a classification error  of 0.
  
  ## right side
  right_branch.df <- subset(Iris.df, Iris.df$predicted_class_class != 'setosa') # right branch is predicted setosa
  right_rate <- nrow(right_branch.df)/nrow(Iris.df) # Calculate rate of not predicted setosa vs total in the sample.
  right_min_rate <- min(
    nrow(right_branch.df[right_branch.df$actual_class == 'setosa',]),
    nrow(right_branch.df[right_branch.df$actual_class != 'setosa',])
  )/nrow(right_branch.df) # Minimum instances by class in the node.
  
  classifiction_error<- left_rate*left_min_rate + right_rate*right_min_rate # classification error is the weighted average of the child nodes.
  classifiction_error<- ifelse(is.na(classifiction_error),1,classifiction_error) # delete this when moving to predefined tests
  ce<- c(ce,classifiction_error) # store our classication error
  sepal_width_min_ce <- ifelse(classifiction_error == min(ce), i, sepal_width_min_ce) # save off the best performing width
}

plot(width_tests, ce, pch=19, ylab = "Classification Error", xlab= "Sepal Width Rule" , main = "Classification Error by Test Widths") # visualize our results. We see 3.4 cm has the lowest classification error and is therefore the best performer.
Iris.df$predicted_class <- ifelse(Iris.df$sepalwidth >= sepal_width_min_ce, 'setosa','not setosa') # Let's rerun with the best performer ...
ce.tbl <- table(Iris.df$actual_class,Iris.df$predicted_class_class) # ... and save off the confusion matrix


#### Gini
g<-c() # initialize an array to store the gini score
gini <- 0# initialize the classification_error variable 
sepal_width_min_g <- 1
for (i in width_tests) {
  Iris.df$predicted_class <- ifelse(Iris.df$sepalwidth >= i, 'setosa','Not setosa') # This is our classification rule.
  
  ## left side
  left_branch.df <- subset(Iris.df, Iris.df$predicted_class == 'setosa') # left branch is predicted setosa
  left_p_setosa <- nrow(left_branch.df[left_branch.df$actual_class == "setosa",])/nrow(left_branch.df) # find what portion of the setosa samples when to the left branch
  
  ## right side
  right_branch.df <- subset(Iris.df, Iris.df$predicted_class != 'setosa') # left branch is predicted setosa
  right_p_setosa <- nrow(right_branch.df[right_branch.df$actual_class == "setosa",])/nrow(right_branch.df) # find what portion of the setosa samples went to the right branch
  
  gini_left <- 1 - left_p_setosa^2 - (1-left_p_setosa)^2 # 'left_p_setosa' is the portion of setosas in the left node. '(1-left_p_setosa)' is the portion of non-setosas in the left branch. 
                                                         #  So, if there is a low purity, these values will be similar and their difference will be close to 0. The '1 minus' means the Gini value will end up being close to 1.
  gini_right <- 1 - right_p_setosa^2 - (1-right_p_setosa)^2 # Calculate Gini score for right node
  
  gini <- gini_left*(nrow(left_branch.df)/nrow(Iris.df)) + gini_right*(nrow(right_branch.df)/nrow(Iris.df)) # Multiply the Gini scores by the portion of the parent node that went to each child. This is the weighted average.
  g<- c(g,Gini) # store our gini scores
  sepal_width_min_g <- ifelse(Gini == min(g), i, sepal_width_min_g) # save off the best performing width.
}

plot(width_tests, g, pch=19, ylab = "Gini Score", xlab= "Sepal Width Rule" ,main = "Gini Score by Test Widths") # visualize our results. We see 3.4 cm has the lowest classification error and is therefore the best performer.
Iris.df$predicted_class <- ifelse(Iris.df$sepalwidth >= sepal_width_min_g, 'setosa','not setosa') # Let's rerun with the best performer ...
g.tbl <- table(Iris.df$actual_class,Iris.df$predicted_class) # ... and save off the confusion matrix


#### Entropy
e<-c() # initialize an array to store the entropy scores
entropy <- 1 # initialize a variable to store entropy
sepal_width_min_e <- 1 # initialize a variable to store our best performing width

for (i in width_tests) {
  #i<-3.7
  Iris.df$predicted_class <- ifelse(Iris.df$sepalwidth >= i, 'setosa','Not setosa') # This is our classification rule. If evaluates to TRUE send to left
  ## left side
  left_branch.df <- subset(Iris.df, Iris.df$predicted_class == 'setosa') # left branch is predicted setosa
  left_p_setosa <- nrow(left_branch.df[left_branch.df$actual_class == "setosa",])/nrow(left_branch.df) # Find the rate that samples go towards the left branch
  
  ## right side
  right_branch.df <- subset(Iris.df, Iris.df$predicted_class != 'setosa') # right branch is predicted not setosa
  right_p_setosa <- nrow(right_branch.df[right_branch.df$actual_class == "setosa",])/nrow(right_branch.df) 
  
  entropy_left <- -1*left_p_setosa*log(left_p_setosa,2) - (1-left_p_setosa)*log(1-left_p_setosa,2) # branch rates are always between 0 and 1. So, log2 of the rates is negative. multiply by -1 to make positive.
  entropy_right <- -1*right_p_setosa*log(right_p_setosa,2) - (1-right_p_setosa)*log(1-right_p_setosa,2) # calculate right node entropy
  
  entropy <- entropy_left*(nrow(left_branch.df)/nrow(Iris.df)) + entropy_right*(nrow(right_branch.df)/nrow(Iris.df)) # calculate the weighted average of the child nodes
  e<- c(e,entropy) # store the entopy scores
  sepal_width_min_e <- ifelse(entropy == min(e), i, sepal_width_min_e) # save off the best performing width.
}

plot(width_tests, e, pch=19, ylab = "Entropy Score", xlab= "Sepal Width Rule" ,main = "Entropy Score by Test Widths") # visualize our results. We see 3.4 cm has the lowest classification error and is therefore the best performer.
Iris.df$predicted_class <- ifelse(Iris.df$sepalwidth >= sepal_width_min_e, 'setosa','not setosa') # Let's rerun with the best performer ...
e.tbl <- table(Iris.df$actual_class,Iris.df$predicted_class) # ... and save off the confusion matrix