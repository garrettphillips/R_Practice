## Install and load necessary packages
install.packages(lme4)
library(lme4)      

## Load data
setwd()
car.conjoint<- read.csv("Car Conjoint Data.csv", stringsAsFactors = TRUE)

## Estimate conjoint model

## Check codings (dummy coded)
contrasts(car.conjoint$price) # default looks good

# Set levels to be same as case
contrasts(car.conjoint$brand)
car.conjoint$brand<- factor(car.conjoint$brand, levels=c("Toyota","Volkswagen","Saturn","Kia"))

contrasts(car.conjoint$horsepower)

contrasts(car.conjoint$upholstery)

## Set levels Y/N
contrasts(car.conjoint$sunroof)
car.conjoint$sunroof<- factor(car.conjoint$sunroof, levels=c("Yes", "No"))

## Estimate model (additive model, accounting for repeated measurements)
conjoint.model<- lmer(rating ~ price + brand + horsepower + upholstery + sunroof + (1|participant.id), data=car.conjoint)

## Save summary to variable
conjoint.summary<- summary(conjoint.model)
conjoint.summary

## Get price utilities & t statistics

### Get the coefficients from the model, with comparision price's utility set to 0
price.utilities<- c( "price$23,000"=0, conjoint.summary$coef[,1][startsWith(names(fixef(conjoint.model)), "price")])

### Normalize so that utilities sum to 0 
price.utilities<- price.utilities - mean(price.utilities)
price.utilities

### Compute t-statistics after centering around 0
price.t<- price.utilities / conjoint.summary$coef[,2][startsWith(names(fixef(conjoint.model)), "price")][1]
price.t

## Get brand utilities & t statistics

### Get utilities and center around 0
brand.utilities<- c( "brandToyota"=0, conjoint.summary$coef[,1][startsWith(names(fixef(conjoint.model)), "brand")])
brand.utilities<- brand.utilities - mean(brand.utilities)
brand.utilities

### Compute t-statistics
brand.t<- brand.utilities / conjoint.summary$coef[,2][startsWith(names(fixef(conjoint.model)), "brand")][1]
brand.t

## Get horsepower utilities and t statistics

hp.utilities<- c( "horsepower220 HP"=0, conjoint.summary$coef[,1][startsWith(names(fixef(conjoint.model)), "horsepower")])
hp.utilities<- hp.utilities - mean(hp.utilities)
hp.utilities

hp.t<- hp.utilities / conjoint.summary$coef[,2][startsWith(names(fixef(conjoint.model)), "horsepower")][1]
hp.t

## Get upholstery utilities and t statistics

upholstery.utilities<- c( "upholsteryCloth"=0, conjoint.summary$coef[,1][startsWith(names(fixef(conjoint.model)), "upholstery")])
upholstery.utilities<- upholstery.utilities - mean(upholstery.utilities)
upholstery.utilities

upholstery.t<- upholstery.utilities / conjoint.summary$coef[,2][startsWith(names(fixef(conjoint.model)), "upholstery")][1]
upholstery.t

## Get sunroof utilities and t statistics

sunroof.utilities<- c( "sunroofYes"=0, conjoint.summary$coef[,1][startsWith(names(fixef(conjoint.model)), "sunroof")])
sunroof.utilities<- sunroof.utilities - mean(sunroof.utilities)
sunroof.utilities

sunroof.t<- sunroof.utilities / conjoint.summary$coef[,2][startsWith(names(fixef(conjoint.model)), "sunroof")][1]
sunroof.t

## Combine utilities and t stats in a table, like in the case document

conjoint.result<- data.frame(
  Utility = c(price.utilities, brand.utilities, hp.utilities, upholstery.utilities, sunroof.utilities), 
  t.statistic = c(price.t, brand.t, hp.t, upholstery.t, sunroof.t)
)
round(conjoint.result, 2)


# Tradeoff Analysis

## Function that takes in features of a product and feature utilities from a conjoint model and outputs the product's overall utility

get.utility<- function(features, utilities, utility.column=1){
  # features must be a list or named vector with names like the columns of the original data set and values that correspond to possible levels
  # utilities must be a data frame with row names formatted like those in the 'conjoint.results' data frame above
  
  # Paste each name and value in 'features' together, to get the corresponding look up value in the 'utilities' vector, e.g., price = "$23,000" will become "price$23,000"
  lookup.names<- paste(names(features), features, sep="")
  
  # Initialize utility to 0
  utility<- 0
  
  # Loop over features to sum up the component utilities
  for(feature in lookup.names){
    # All we need to do is add the feature utility to the overall utility

    # The commented code below shows the iterative process in action. Remove the # symbols before running the function to see it. It's not needed to get the right output, though.
    # message("Current utility: ", round(utility, 2))
    # message("Current feature: ", feature)
    # message("Feature utility: ", round(utilities[feature, utility.column],2))
    # message("Updated utility: ", round(utility + utilities[feature, utility.column], 2), "\n\r")

    utility<- utility + utilities[feature, utility.column]
  }
  
  # Return (i.e., output) the overall utility
  return(utility)
}

# Get utility for a Toyota with 280 HP, leather interior, no sunroof, that costs $23,000

## Put features in a list (you could make this work as a vector or data frame, also)
car.1<- list(
  price="$23,000",
  brand="Toyota",
  horsepower="280 HP",
  upholstery="Leather",
  sunroof="No"
)

## Add utility to the list
car.1$utility<- get.utility(car.1, conjoint.result)
car.1$utility

# What price could you charge for the same car with a sunroof

## Utility difference (utility of improved car - utility of original)

util.diff<- get.utility(list(
              price="$23,000",
              brand="Toyota",
              horsepower="280 HP",
              upholstery="Leather",
              sunroof="Yes"
            ), conjoint.result) - car.1$utility

## Compute target price utility (current price utility minus excess utility from adding sunroof)

target.price.utility<- conjoint.result[paste("price", car.1$price, sep=""), 1] - util.diff # .87 which is between $25,000 and $27,000

## Compute price to equate utilities (assuming linear relationship between utility and price within the $25,000 to $27,000 interval)

25000 + # lower price
  (conjoint.result["price$25,000", 1] - target.price.utility) / # Together, this and the next line compute what percentage of the distance between $25,000 and $27,000 the price is at.
                                                                # That is equal to the distance of the new price from the lower bound (numerator)
  (conjoint.result["price$25,000", 1] - conjoint.result["price$27,000", 1]) * # divided by the distance from the lower bound to the upper bound (denominator).
  2000 # The difference in price between the lower bound and the upper bound (i.e., $27,000 - $25,000)

#
#
#
#
#
#
#

conjoint.tradeoff.preprocessor<- function(utility.table, option.1, new.attribute.levels, attribute.to.compute){
  
  # Create the second option from the parameters
  ## Option 2 will have all the same features as option 1, except for those in the 'new.attribute.levels' parameter.
  ## So, step 1 is to just copy all the attributes.
  option.2<- option.1
  
  ## Step 2 is to loop over the attribute names in the 'new.attribute.levels' list and change option 2's attributes as specified.
  for(attribute in names(new.attribute.levels)){
    option.2[attribute]<- new.attribute.levels[[attribute]]
  }
  
  # Next, compute the difference in utilities
  utility.difference<- get.utility(option.2, utility.table) - get.utility(option.1, utility.table)
  
  # Then, compute the utility the target attribute should have (the utility of the attribute for option 1 minus the change in utility by changing the attributes as specified)
  target.attribute.utility<- utility.table[paste(attribute.to.compute, option.1[attribute.to.compute], sep=""), 1] - utility.difference
  
  # Finally, find the levels the target utility is between
  ## Copy the utilty table, keeping only the rows for the target attribute
  target.attribute.utility.table<- utility.table[startsWith(rownames(utility.table), attribute.to.compute),]
  
  ## Find the closest utility value less than the target utility
  ### Compute the row #
  lower.row<- which( # The which function tells you the index number(s) that match the arguments given (i.e., the index numbers of TRUE values)
    target.attribute.utility.table[, 1] == # Check all utility values in the target attribute utilities table
      max(
        target.attribute.utility.table[target.attribute.utility.table[, 1] <= target.attribute.utility, 1] # Select only the elements less than the target attribute
      ) # The maximum of the elements less than a value is the same as closest element less than a value (e.g., if your target # is 4 and your vector is 1, 2, 3, 5, 6, the values less than 4 are 1, 2, and 3 and the maximum is 3)
    )
  
  ## Find the closest utility value greater than the target utility (the minimum of the values greater than the target)
  upper.row<- which(
    target.attribute.utility.table[, 1] == 
      min(
        target.attribute.utility.table[target.attribute.utility.table[, 1] >= target.attribute.utility, 1] 
      ) 
  )
  
  
  message(
    "Target attribute: ", attribute.to.compute, "\n\r",
    "Target attribute utility: ",round(target.attribute.utility, 2), "\n\r",
    "Target attribute value is between:\n\r",
    rownames(target.attribute.utility.table)[upper.row], "(utility: ", round(target.attribute.utility.table[upper.row, 1], 2), ")\n\r",
    rownames(target.attribute.utility.table)[lower.row], "(utility: ", round(target.attribute.utility.table[lower.row, 1], 2), ")"
  )
  
  
  invisible(list(
    target.attribute = attribute.to.compute, 
    target.attribute.utility = target.attribute.utility, 
    lower.utility.attribute.level = target.attribute.utility.table[lower.row, , drop=FALSE],
    upper.utility.attribute.level = target.attribute.utility.table[upper.row, , drop=FALSE]))
}

conjoint.tradeoff.preprocessor(utility.table = conjoint.result,
                  option.1 = list(
                    price="$23,000",
                    brand="Toyota",
                    horsepower="280 HP",
                    upholstery="Leather",
                    sunroof="No"),
                  new.attribute.levels = list(
                    sunroof = "Yes"),
                  attribute.to.compute = "price"
)

conjoint.tradeoff<- function(target.utility, lower.level, lower.level.utility, upper.level, upper.level.utility){
  
}

conjoint.tradeoff(.87, 25000, 1.11, 27000, -1.52)
