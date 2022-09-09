###############################
# Conjoint Analysis Functions #
###############################

# The functions in this file are as followed:
# 
# get.conjoint.utility.table
# 
#    Description: Creates a data.frame of attribute utilities and t-statistics for those 
#                 utilities, given a statistical conjoint analysis model (either the
#                 result of an 'lm' function or 'lmer' function from the 'lme4' package). 
#                 Prints the result to the console. Can also output the result as in a list
#                 format.
# 
#    Parameters:
#       model: A statistical model. Either the result of an `lm` or an `lmer` function
#
#       centered: (Default: TRUE) Whether or not to center attribute level utilities around
#                 the attribute's mean utility
# 
#       return.data: (Default: "data.frame") Whether to return the utility information as a
#                    table (i.e., a data.frame) or as a list. Data.frame is usually what is
#                    needed, but some analyses, such as the internal part of the attribute
#                    importance function use the list output.
# 
#       verbose: (Default: TRUE) Whether or not to print the table to console when the
#                function runs, as opposed to solely storing it to a variable.
#
#
# get.utility
# 
#    Description: Computes the utility of a given product (provided as a list of 
#                 'attributeName = attributeLevel' pairs) for a given conjoint analysis
#                 model provided as a data.frame with utilities in one column and attribute
#                 levels as rownames with format 'attributeNameAttributeLevel'.
# 
#    Parameters:
#       features: A list containing 'attributeName = attributeLevel' pairs. The attributeName
#                 should exactly match a column from your data. The attributeValue should
#                 exactly match a possible value in that column.
# 
#       utilities: A table of attribute level utilities. Typically, the result of the
#                  get.conjoint.utility.table function.
# 
#       utility.column: (Default: 1) The column in the table provided to the `utilities`
#                       parameter that contains utilities.
# 
# 
# conjoint.tradeoff
# 
#    Description: Computes a new attribute value that equates the overall utility of
#                 a product to a given initial product. You can provide either a list of
#                 attributes that have changed from the original or an overall utility
#                 value you want the new product to have.
# 
#    Parameters:
#       utility.table: A table of utilities. Typically the result of `get.conjoint.utility.table`.
# 
#       option.1: A list of product features for a full product. Same as the `features` parameter
#                 of get.utility.
# 
#       new.attribute.levels: A list of product features. It should only contain the features that
#                             are different from option.1. Do not provide an argument if you are
#                             using the `option.2.utility` parameter.
# 
#       attribute.to.compute: The attribute name you want to compute a new value for.
# 
#       option.2.utility: The overall utility value you want to the product to have after
#                         changing the target feature. Do not provide an argument if you are
#                         using the `new.attribute.levels` parameter.
#  
# 
# conjoint.market.share
# 
#    Description: Computes the market share of a set of products, assuming a logit function
#                 linking utilities to choice. The output provides the market share for each 
#                 product provided, as well as the product's overall utility.
# 
#    Parameters:
#       ...: Any number of products, separated by commas. Products are lists of features. See the
#            `features` parameter in the `get.utility` function.
# 
#       utilities: A table of utilities. Typically the result of `get.conjoint.utility.table`.
# 
#       utility.column: (Default: 1) The column in the table provided to the `utilities` parameter
#                       that contains utilities.
# 
#       product.names: (Default: NULL) Labels for the products in the ellipsis parameter. If NULL,
#                      product1, product2, etc. are used.
# 
#
# conjoint.attribute.importance
# 
#    Description: Computes one definition of importance of each attribute within a conjoint
#                 analysis model. Importance is defined as the range of an attribute's utility
#                 values divided by the sum of the ranges of all attribute utility values.
#                 Note that this definition normalizes an effect size with other effect sizes.
#                 Thus, it is assuming the causal events are of similar size. For instance,
#                 horsepower of a car might not seem important if the levels used in the study
#                 aren't very far apart (e.g., 220 HP and 225 HP).
# 
#    Parameters:
#       model: A statistical model. Either the result of an `lm` or an `lmer` function
# 
# 
# best.product
# 
#    Description: Two usages - If you provide products to the ellipsis parameter, then
#                 the function computes the the product with the highest overall utility,
#                 according to the utility table provided. If you do not provide any
#                 products, then the function computes what the best possible product
#                 for the person is, according to the utility table provided.
# 
#    Parameters:
#       model: A statistical model. Either the result of an `lm` or an `lmer` function
# 
#       ...: Any number of products, separated by commas. Products are lists of features. See the
#            `features` parameter in the `get.utility` function. This parameter is optional. If
#            no arguments are provided, then the functions computes the best possible product
#            from the utility table.
# 
#       n: (Default: NULL) The function will return the top `n` products from the ellipsis
#          parameter.
# 
#       product.names: (Default: NULL) Labels for the products in the ellipsis parameter. If NULL,
#                      product1, product2, etc. are used.





get.conjoint.utility.table<- function(model, centered = TRUE, return.data = "data.frame", verbose = TRUE){

  
  # Get x variable names. Must use S3 methods for lm objects and S4 methods for lmerMod objects
  # Also get x variable levels
  if(class(model) == "lm"){
    var.names<- attr(model$terms, "term.labels")
    xlevels<- model$xlevels
  } else if (class(model) == "lmerMod"){
    var.names<- attr(attr(model@frame, "terms"),"term.labels")
    xlevels<- list()
    for(var in var.names){
      if(class(model@frame[,var]) == "factor"){
        xlevels[[var]]<- levels(model@frame[,var])
      }
    }
  }
  
  # Run the summary of the model
  model.summary<- summary(model)
  
  # Get utilities & standard errors
  conjoint.data<- list() # initialize list to store utilities
  
  for(var in var.names){
    # Loop over the variable names in the model
    # Test if the variable name appears in the coefficients table (random effect variables will not)
    # If yes, get coefficients and one of the standard errors for all rows starting with variable name
    
    if(sum(startsWith(rownames(model.summary$coefficients), var))){
      # Get utilities
      conjoint.data[[var]][["utilities"]]<- c(0, model.summary$coefficients[,1][startsWith(rownames(model.summary$coefficients), var)])
      names(conjoint.data[[var]][["utilities"]])<- c(paste0(var,xlevels[[var]][[1]]), names(conjoint.data[[var]][["utilities"]])[2:length(conjoint.data[[var]][["utilities"]])])
      
      if(centered == TRUE){
        # Mean-center utilities (they will add to 0)
        conjoint.data[[var]][["utilities"]]<- conjoint.data[[var]][["utilities"]] - mean(conjoint.data[[var]][["utilities"]])
      }
      
      # Get standard errors
      conjoint.data[[var]][["SE"]]<- model.summary$coefficients[startsWith(rownames(model.summary$coefficients), var), 2][1]
      
      # Compute new t-statistics
      conjoint.data[[var]][["t"]]<- conjoint.data[[var]][["utilities"]] / conjoint.data[[var]][["SE"]]
    }
  }
  
  # Print table
  table.utilities<- c()
  table.t<- c()
  
  for(var in names(conjoint.data)){
    table.utilities<- c(table.utilities, conjoint.data[[var]][["utilities"]])
    table.t<- c(table.t, conjoint.data[[var]][["t"]])
  }
  table<- data.frame(Utilities=table.utilities, t.stat=table.t)
  if(verbose == TRUE) print(round(table,2))
  
  # Return the list or data.frame version of the results
  if(return.data == "list"){
    invisible(conjoint.data)
  } else if (return.data == "table" | return.data == "df" | return.data == "data.frame" | return.data == "dataframe" | return.data == "data frame"){
    invisible(table)
  }
}





get.utility<- function(features, utilities, utility.column=1){
  # features must be a list or named vector with names like the columns of the original data set and values that correspond to possible levels (e.g., price = "$23,000")
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





conjoint.tradeoff<- function(utility.table, option.1, new.attribute.levels, attribute.to.compute, option.2.utility){
  if(!missing(new.attribute.levels) & !missing(option.2.utility)){
    stop("Parameters new.attribute.levels and option.2.utility are both set, but only one is needed.")
  } else if (missing(new.attribute.levels)& missing(option.2.utility)){
    stop("You must provide either new.attribute.levels or option.2.utility.")
  }
  
  if(!missing(new.attribute.levels)){
    # Create the second option from the parameters
    ## Option 2 will have all the same features as option 1, except for those in the 'new.attribute.levels' parameter.
    ## So, step 1 is to just copy all the attributes.
    option.2<- option.1
    
    ## Step 2 is to loop over the attribute names in the 'new.attribute.levels' list and change option 2's attributes as specified.
    for(attribute in names(new.attribute.levels)){
      option.2[attribute]<- new.attribute.levels[[attribute]]
    }
    
    # Compute the utility for option 2
    option.2.utility<- get.utility(option.2, utility.table)
    
    # Next, compute the difference in utilities between products
    utility.difference<- option.2.utility - get.utility(option.1, utility.table)
  } else {
    # Next, compute the difference in utilities between products
    # **** The way the math works out, when using the function this way, you are changing
    #      option 1 to fit option 2, rather than changing option 2 to fit option 1.
    #      consider changing this in the future, since it's kinda confusing that it works
    #      for one of the usage scenarios.
    utility.difference<- get.utility(option.1, utility.table) - option.2.utility 
  }
  
  
  
  # Then, compute the utility the target attribute should have (the utility of the attribute for option 1 minus the change in utility by changing the attributes as specified)
  target.attribute.utility<- utility.table[paste(attribute.to.compute, option.1[attribute.to.compute], sep=""), 1] - utility.difference
  
  # Finally, find the levels the target utility is between
  ## Copy the utilty table, keeping only the rows for the target attribute
  target.attribute.utility.table<- utility.table[startsWith(rownames(utility.table), attribute.to.compute),]
  
  # If target attribute utility is greater than the max or less than the min, issue warning and compute extrapolation
  if(target.attribute.utility > max(target.attribute.utility.table[, 1]) | target.attribute.utility < min(target.attribute.utility.table[, 1])){
    stop(paste0("The target attribute utility (",
                   round(target.attribute.utility, 2),
                   ") is outside the range of measured utility values (min = ",
                   round(min(target.attribute.utility.table[, 1]), 2),
                   ", max = ",
                   round(max(target.attribute.utility.table[, 1]), 2),
                   ")."))
  }
  
  ## Find the closest utility value less than the target utility
  ### Compute the row #
  lower.row<- which( # The which function tells you the index number(s) that match the arguments given (i.e., the index numbers of TRUE values)
    target.attribute.utility.table[, 1] == # Check all utility values in the target attribute utilities table
      max(
        target.attribute.utility.table[target.attribute.utility.table[, 1] <= target.attribute.utility, 1] # Select only the elements less than the target attribute
        ) # The maximum of the elements less than a value is the same as closest element less than a value (e.g., if your target # is 4 and your vector is 1, 2, 3, 5, 6, the values less than 4 are 1, 2, and 3 and the maximum is 3
    )
  
  ## Find the closest utility value greater than the target utility (the minimum of the values greater than the target)
  upper.row<- which(
    target.attribute.utility.table[, 1] == 
      min(
        target.attribute.utility.table[target.attribute.utility.table[, 1] >= target.attribute.utility, 1] 
        )
    )
  
  bounds<- c(as.numeric(gsub("[^0-9.-]", "", rownames(target.attribute.utility.table)[lower.row])),
             as.numeric(gsub("[^0-9.-]", "", rownames(target.attribute.utility.table)[upper.row])))
  
  bounds.utilities<- c(target.attribute.utility.table[lower.row, 1],
                       target.attribute.utility.table[upper.row, 1])
  
  if(bounds[1] < bounds[2]){
    names(bounds)<- c("lower", "upper")
    names(bounds.utilities)<- c("lower", "upper")
  } else {
    names(bounds)<- c("upper", "lower")
    names(bounds.utilities)<- c("upper", "lower")
  }
  
  tradeoff.value<- bounds["lower"] + abs((bounds.utilities["lower"] - target.attribute.utility) / (bounds.utilities["lower"] - bounds.utilities["upper"])) * (bounds["upper"] - bounds["lower"])
  
  message(
    "Target attribute: ", attribute.to.compute, "\n\r",
    "Target attribute utility: ",round(target.attribute.utility, 2), "\n\r",
    "Target attribute value is between:\n\r",
    bounds["lower"], " (utility: ", round(bounds.utilities["lower"], 2), ")\n\r",
    bounds["upper"], " (utility: ", round(bounds.utilities["upper"], 2), ")\n\r",
    "New target attribute value: ", round(tradeoff.value ,2)
  )
  
  
  invisible(tradeoff.value)
}





conjoint.market.share<- function(..., utilities, utility.column=1, product.names=NULL){
  products<- list(...)
  if(is.null(product.names)){
    product.names<- paste0("product", 1:length(products))
  } else {
    if(length(product.names) != length(products)){
      stop("Products (", length(products), ") and product.names (", length(product.names), ") have different lengths.")
    }
  }
  utility<- c()
  
  for(product in products){
    utility[length(utility) + 1]<- get.utility(product, utilities, utility.column)
  }
  names(utility)<- product.names
  
  market.share<- exp(utility) / sum(exp(utility))
  names(market.share)<- product.names
  
  return(list(utility=utility, market.share=market.share))
}




conjoint.attribute.importance<- function(model){
  # Get utilities table as a list
  utilities<- get.conjoint.utility.table(model, return.data = "list", verbose = FALSE)
  
  lows<- numeric(length(utilities))
  highs<- numeric(length(utilities))
  # Loop over the attributes in the list, finding the highest and lowest utilities
  for(i in 1:length(utilities)){
    lows[i]<- min(utilities[[i]][["utilities"]])
    highs[i]<- max(utilities[[i]][["utilities"]])
  }
  # Add attribute names
  names(lows)<- names(utilities)
  names(highs)<- names(utilities)
  
  # Return the attribute importance proportions
  return((highs - lows) / sum((highs - lows)))
}





best.product<- function(model, ..., n=NULL, product.names=NULL){
  # Convert ellipsis parameter to list
  products<- list(...)
  
  # If n isn't set, set it to return all products
  if(is.null(n)){
    n<- length(products)
  }
  print(length(products))
  # If products parameter isn't set, get the ideal product -- the maximum utility for all attributes
  # If it is set, then find the highest utility product in the list.
  
  if(length(products) == 0){
    # Get the utilities in list format
    utilities<- get.conjoint.utility.table(model, return.data="list", verbose=FALSE)
    
    # Initialize value and name vectors for loop
    best.product<- numeric(length(utilities))
    best.product.names<- character(length(best.product))
    
    # Loop through the attributes, finding the level with the maximum utility
    for(i in 1:length(best.product)){
      best.level<- utilities[[i]][["utilities"]][utilities[[i]][["utilities"]]==max(utilities[[i]][["utilities"]])]
      best.product[i]<- best.level
      best.product.names[i]<- names(best.level)
    }
    # Assign names
    names(best.product)<- best.product.names
  } else {
    # Get the attribute utilities in data.frame format
    utilities<- get.conjoint.utility.table(model, verbose=FALSE)
    
    # Get product utilities and probabilities of choice
    best.product<- conjoint.market.share(..., utilities = utilities, utility.column = 1, product.names = product.names)
    
    # Sort the best.product results
    descending.order<- order(-best.product[["utility"]])
    
    best.product[["utility"]]<- best.product[["utility"]][descending.order]
    best.product[["market.share"]]<- best.product[["market.share"]][descending.order]
    
    # Remove products in best.product after n
    best.product[["utility"]]<- best.product[["utility"]][1:n]
    best.product[["market.share"]]<- best.product[["market.share"]][1:n]
    
    # Rename the market.share variable, since this is supposed to be for one person
    names(best.product)<- c("utility", "purchase.likelihood")
  }
  
  return(best.product)
}