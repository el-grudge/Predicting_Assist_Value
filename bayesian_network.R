# Bayesian networks
# Aaron Smith
# 5/30/2020
# Graphical models treat all variables as predictors and as target variables.
# The bnlearn package has structure-learning algorithms. This is the strongest aspect of the package.
# bnlearn supports data sets with all factor variables very well. 
# bnlearn has features to support models for data sets with all numeric variables. 
# bnlearn has the fewest features for mixed data types (better to convert to discrete).

## The hailfinder data set
options(digits = 1)
require(bnlearn)

## Loading required package: bnlearn
## Attaching package: 'bnlearn'
## The following object is masked from 'package:stats':
## 
##     sigma

# 2- loading dataset
data(hailfinder,package = "bnlearn") ## all factors
summary(hailfinder)

# 3- A quick example of a graphical model
# Let’s look a an example of a graphical model
model_hailfinder <- bnlearn::hc(hailfinder)
graphviz.plot(model_hailfinder)

# Strength plots
# Strength plots visually shows the strength between variables.
# There are three types of errors.
# • Includeing an arc/edge when there should not be one
# • Missing an arc/edge when there should be one
# • Arc pointed in the wrong direction

arc.strength_hailfinder <- arc.strength(
  x = model_hailfinder,
  data = hailfinder
)

strength.plot(
  x = model_hailfinder,
  strength = arc.strength_hailfinder
)

# Other plots
# Unfortunately, the bnlearn package has multiple object classes that fill similar purposes. 
# To be able to use all of the features of the package, we need to be able to convert object classes.
# These plots only work for discrete data sets.

bn_hailfinder <- bn.fit(
  x = model_hailfinder,
  data = hailfinder
)

graphviz.chart(
  x = bn_hailfinder,
  type = "barchart"
)

graphviz.chart(
  x = bn_hailfinder,
  type = "dotplot"
)

graphviz.chart(
  x = bn_hailfinder,
  type = "barprob"
)

# Model fitting
# In machine learning, we fit multiple models. Measure the quality of each model and select the best one.
# With graphical models, all variables are predictors and target variables.
# bnlearn supports using cross-validation well. In this example, we will use repeated two-fold cross-validation.

v_models <- c(
  "pc.stable","gs","iamb","fast.iamb","inter.iamb","iamb.fdr",
  "hc", "tabu",
  "mmhc","rsmax2","h2pc",
  "mmpc","si.hiton.pc","hpc", 
  "chow.liu","aracne"
)

list_cv <- list()
for(j in v_models) try({ 
  list_cv[[j]] <- bn.cv(
    data = hailfinder,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_cv

list_mean <- list()
for(j in names(list_cv)){
  for(k in 1:length(list_cv[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_cv[[j]][[k]]))
    for(l in 1:length(list_cv[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_cv[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))


##############################################################################################################################

# Case study: recommendation system with bnlearn
# Aaron Smith
# 6/20/2020
# Let’s build a recommendation system using R’s bnlearn package. 
# Our goal is to create a model that identifies what a customer would be prone to adding to their shopping basket. 
# At all times we want to get the customer to add one more item to their basket; 
# once they add an item, we update our recommendation to get them to add another item.
# Conditional probability models
# We can use the arules package in R to build rules for this task. 
# Here we want to use a graphical model. There are advantages and disadvantages either way. 
# My opinion is that graphical models have a more natural interface, and it is easier to identify meaningful rules.
# We will use the Groceries data set from the arules package.

options(digits = 1)
data(Groceries,package = "arules")
Groceries

# The data is stored in a sparse matrix format. We will need to convert it to a data.frame() 
# before passing it to the bnlearn functions.

M_Groceries <- as.data.frame(as(
  object = Groceries,
  Class = "matrix"
))

colnames(M_Groceries) <- make.names(colnames(M_Groceries))
M_Groceries[1:6,1:6]

M_Groceries <- M_Groceries[
  order(rowSums(M_Groceries),decreasing = TRUE),
  order(colSums(M_Groceries),decreasing = TRUE)
  ]

# Reduce the number of items in the model
# • Conditional probability models take a long time to compute.
# • Recommendations for rarely purchased items probably will be unsuccessful.
# • Recommendations based on rarely purchased items will rarely get used.
# Let’s trim down the items in our model to the ones that account for 80% of the distinct items in a basket.

v_sums <- colSums(M_Groceries)
mean(cumsum(v_sums)/sum(v_sums) <= 0.8)

sum(cumsum(v_sums)/sum(v_sums) <= 0.8)

v_names <- names(v_sums[cumsum(v_sums)/sum(v_sums) <= 0.8])
M_Groceries <- M_Groceries[,v_names]

# To make output more compact, I converted TRUE/FALSE to “1”/“0” factors. 
# Notice that zero and one are categorical instead of numeric. 
# The bnlearn package would have build multiple linear regression models (numeric target variables); 
# we want binary target variables.

for(j in 1:ncol(M_Groceries)){
  M_Groceries[,j] <- factor(as.numeric(M_Groceries[,j]))
}

# Building the model
# Here are the possible algorithms in bnlearn that we can use to build the model. 
# They are grouped by their method of identifying relationships.
# Before writing this, I benchmarked each algorithm on a subset of the data. Each group is ordered by computation time.
# • iamb.fdr() was the fastest constraint-based algorithm.
# • hc() was the fastest score-based algorithm.
# • h2pc() was the fastest hybrid algorithm.
# • aracne() was the fastest local discovery algorithm (I have never had success with the local discovery algorithms).
# My experience is that the constraint-based and score-based algorithms are the best ones for my tasks at work.

v_algorithms <- c(
  "iamb.fdr","fast.iamb","inter.iamb","gs","iamb","hpc","si.hiton.pc","mmpc","pc.stable",
  "hc","tabu",
  "h2pc","mmhc","rsmax2",
  "aracne","chow.liu"
)
require(bnlearn)

# For this presentation, I did not run this code chunk. It took too long to run. 
# I would run this overnight if I needed the best model possible. 
# Then use BIC(), AIC(), logLik(), or another network score to determine which model to use.

## list_bnlearn <- list()
## for(j in v_algorithms) try({
##   list_bnlearn[[j]] <- do.call(
##     what = j,
##     args = list(x = M_Groceries[,v_names])
##   )
## },silent = TRUE)

# My experience is that hc() (hill-climber) gives solid results, and is fast. For this case study, let’s use hc() as our model.

list_bnlearn <- list()
for(j in "hc") try({
  list_bnlearn[[j]] <- do.call(
    what = j,
    args = list(x = M_Groceries[,v_names])
  )
},silent = TRUE)
graphviz.plot(list_bnlearn[["hc"]])

# Unfortunately, the bnlearn package has multiple model structures. 
# To fully take advantage of the model, we need to convert the model using bn.fit()

bn_Groceries <- bn.fit(
  x = list_bnlearn[["hc"]],
  data = M_Groceries[,v_names]
)


##########################################
# Scoring the models

M_score <- matrix(
  data = NA,
  nrow = length(v_algorithms),
  ncol = length(list_M),
)
rownames(M_score) <- v_algorithms
colnames(M_score) <- names(list_M)

for(j in v_algorithms) for(k in names(list_M)) try({
  M_score[j,k] <- score(
    x = list_bnlearn[[j]][[k]],
    data = list_M[[k]],
    type = "bic"
  )
})
for(j in rownames(M_score)) M_score <- M_score[,order(M_score[j,])]
for(j in colnames(M_score)) M_score <- M_score[order(M_score[,j]),]
M_score

#################################################
# Preparing data for bnlearn models

options(digits = 1)
require(bnlearn)

data(gaussian.test)
head(gaussian.test)

summary(gaussian.test)

# Discretize the data
# Let’s use the three discretize methods on the data set, and store the prepared data in a list.

M <- gaussian.test

list_M <- lapply(
  X = c("interval","quantile","hartemink"),
  FUN = function(method) discretize(
    data = M,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)
names(list_M) <- c("interval","quantile","hartemink")
lapply(X = list_M,FUN = summary)

# Build the Bayesian networks
# Now let’s build the models. Some of the algorithms generate partially ordered networks. 
# To make the models more consistent, let’s use bnlearn::choose.direction() to order all graphs.

v_algorithms <- c(
  "pc.stable","gs","iamb","fast.iamb","inter.iamb","iamb.fdr","mmpc","si.hiton.pc","hpc",
  "hc","tabu",
  "rsmax2","mmhc","h2pc",
  "aracne","chow.liu"
)
list_bnlearn <- list()

for(j in v_algorithms) for(k in names(list_M)) try({
  list_bnlearn[[j]][[k]] <- do.call(
    what = j,
    args = list(x = list_M[[k]])
  )
  M_arcs <- arcs(list_bnlearn[[j]][[k]])
  for(l in 1:nrow(M_arcs)){
    list_bnlearn[[j]][[k]] <- set.arc(
      x = list_bnlearn[[j]][[k]],
      from = M_arcs[l,1],
      to = M_arcs[l,2],
      check.cycles = FALSE,
      check.illegal = FALSE
    )
    list_bnlearn[[j]][[k]] <- choose.direction(
      x = list_bnlearn[[j]][[k]],
      arc = M_arcs[l,],
      data = list_M[[k]]
    )
  }
},silent = TRUE)
