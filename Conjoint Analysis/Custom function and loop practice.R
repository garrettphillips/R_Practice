# Count # of Even numbers in a vector using a for loop
set.seed(1234)
my.numbers<- round(runif(n=500, min=0, max=1000), 0)

num.evens<- 0 # Create a variable to store the answer

# Loop over the numbers in the input vector
for(i in my.numbers){
  
  # Test if current number is even
  if(i %% 2 == 0){
    # If yes, icrement the answer variable
    num.evens<- num.evens + 1
  }
  # If yes, icrement the answer variable
  # If no, do nothing
  
}

num.evens

# Same thing using vectorized operations
sum(my.numbers %% 2 ==0)

# Compute the first N Fibonnaci Numbers

n<- 10# Create a variable with the number of numbers to compute
fibonacci<- numeric(n) # Create a variable to store the result

# Loop of the idices of the result vector
for(i in 1:length(fibonacci)){
 if(i ==1){
   #If i == 1, then define the value as 0
   fibonacci[i]<- 0
 } else if (i == 2){
   #If i == 2, then define the value as 1
   fibonacci[i]<- 1
 } else if (i > 2){
   # If i > 2, then fibonacci = the sum of the previous 2 values
     fibonacci[i]<- fibonacci[i-2] +fibonacci[i-1]
 }
}

  
###########################################################
# Add up all the numbers in a vector

my.numbers<- c(3, 8, 13, 54, 24)

sum.vector<- function(x){
  value<- 0
  for(i in x){
    value<- value + i
  }
  return(value)
}

sum.vector(my.numbers)

sum.vector(1:100)
sum.vector(-10:10)
sum.vector