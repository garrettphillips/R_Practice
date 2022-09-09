# Set working directory and load necessary packages
setwd("C:/Users/Garrett/Documents/Spring 21/MKT 4480")
library(lme4) 

# Load data and assign to a variable
phone.data <- read.csv("Phone Conjoint Data.csv", stringsAsFactors=TRUE)


# Understanding the design of the study
## Number of participants 
max(phone.data$participant.id) #100

## Number of levels each factor has
length(unique(phone.data$camera)) #3
length(unique(phone.data$pinch.to.zoom)) #2
length(unique(phone.data$size)) #3
length(unique(phone.data$rubberbanding)) #2
length(unique(phone.data$price)) #4

 ## Levels of each attribute
list(unique(phone.data$camera)) # 12mp, 3mp, 8mp
list(unique(phone.data$pinch.to.zoom)) # No, Yes
list(unique(phone.data$size)) # 5.8in, 6.1in, 6.4in
list(unique(phone.data$rubberbanding)) # No, Yes
list(unique(phone.data$price)) # $199, $359, $499, $659


# Estimating a statistical model for a conjoint analysis
conjoint.model<- lmer(rating ~ camera + pinch.to.zoom + rubberbanding + price + size + (1|participant.id), data=phone.data)
summary(conjoint.model)



# Finding the utility of the standard Samsung Galaxy Phone
get.conjoint.utility.table(conjoint.model)
phone.conjoint.results<- get.conjoint.utility.table(conjoint.model)

## Getting the utility for each factor
contrasts(phone.data$camera)
contrasts(phone.data$pinch.to.zoom)
contrasts(phone.data$size)
contrasts(phone.data$rubberbanding)
contrasts(phone.data$price)


## The utility of the standard Samsung Galaxy
get.utility(list(
  price="$499",
  camera="8mp",
  pinch.to.zoom="Yes",
  rubberbanding="Yes",
  size="6.1in"
), utilities = phone.conjoint.results
)  # The utility is 1.88


# 5 phones of equal utility
## 1
get.utility(list(
  price="$359",
  camera="12mp",
  pinch.to.zoom="No",
  rubberbanding="Yes",
  size="6.1in"
), utilities = phone.conjoint.results
) # Utility of 1.79

## 2
get.utility(list(
  price="$359",
  camera="8mp",
  pinch.to.zoom="Yes",
  rubberbanding="No",
  size="6.1in"
), utilities = phone.conjoint.results
) # Utility of 1.73

## 3
get.utility(list(
  price="$499",
  camera="12mp",
  pinch.to.zoom="Yes",
  rubberbanding="Yes",
  size="5.8in"
), utilities = phone.conjoint.results
)  # Utility of 1.56

## 4
get.utility(list(
  price="$359",
  camera="12mp",
  pinch.to.zoom="Yes",
  rubberbanding="No",
  size="6.1in"
), utilities = phone.conjoint.results
) # Utility of 2.17

## 5
get.utility(list(
  price="$199",
  camera="3mp",
  pinch.to.zoom="No",
  rubberbanding="Yes",
  size="6.1in"
), utilities = phone.conjoint.results
) # Utility of 1.86


# Finding difference in utility of phone with pinch to zoom and rubberbanding toone without
phone.with.both <- get.utility(list(
  price="$499",
  camera="12mp",
  pinch.to.zoom="Yes",
  rubberbanding="Yes",
  size="6.1in"
), utilities = phone.conjoint.results
)

phone.with.niether <- get.utility(list(
  price="$499",
  camera="12mp",
  pinch.to.zoom="No",
  rubberbanding="No",
  size="6.1in"
), utilities = phone.conjoint.results
)

diff.in.utility <- (phone.with.both - phone.with.niether)
diff.in.utility


# 5 equivalent models
## 1
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    size = "5.8in"
  ),
  attribute.to.compute ="price"
) #Price of $399.69

## 2
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    camera = "12mp"
  ),
  attribute.to.compute ="price"
) #Price of $535.6

## 3
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    size = "6.4in"
  ),
  attribute.to.compute ="price"
) #Price of $385.33

## 4
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    pinch.to.zoom = "No"
  ),
  attribute.to.compute ="price"
) #Price of 332.93

## 5
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    rubberbanding = "No"
  ),
  attribute.to.compute ="price"
) #Price of 351


# Attribute importance
conjoint.attribute.importance(conjoint.model)

# Price of Galaxy without the two software features
conjoint.tradeoff(
  utility.table= phone.conjoint.results,
  option.1= list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  new.attribute.levels = list(
    rubberbanding = "No",
    pinch.to.zoom = "No"
  ),
  attribute.to.compute ="price"
)


# Market Share change
conjoint.market.share(
  list(
    price="$499",
    camera="12mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
),
list(
  price="$499",
  camera="8mp",
  pinch.to.zoom="Yes",
  rubberbanding="Yes",
  size="6.1in"
),
utilities = phone.conjoint.results,
product.names = c("Apple", "Samsung")
)

# Market Share with no software in Samsung
conjoint.market.share(
  list(
    price="$499",
    camera="12mp",
    pinch.to.zoom="Yes",
    rubberbanding="Yes",
    size="6.1in"
  ),
  list(
    price="$499",
    camera="8mp",
    pinch.to.zoom="No",
    rubberbanding="No",
    size="6.1in"
  ),
  utilities = phone.conjoint.results,
  product.names = c("Apple", "Samsung")
)
