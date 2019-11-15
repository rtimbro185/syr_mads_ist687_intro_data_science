# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")


#R Code - unexecuted
# ---------- HW1: Intro -----------

# create a vector "height" containing numbers
height <- c(59,60,61,58,67,72,70)   
# create a vector "weight" containing numbers
weight <- c(150,140,180,220,160,140,130) 
# define a variable a (a = 150)
a <- 150  

# ---------------------------------
# Step 1: Calculating means
# compute, using R, the average(mean) height
mean(height)

# compute, using R, the average(mean) weight 
mean(weight)

# calculate the length of the vector height (the number of elements inside the vector)
length(height)

# calculate the length of the vector weight (the number of elements inside the vector)
length(weight)

# calculate the sum of the heights
sum(height)

# compute the average height by dividing the sum by the length of the vector
sum(height)/length(height)

# compute the average weight by dividing the sum by the length of the vector
sum(weight)/length(weight)


# ---------------------------------
# Step 2: Using max/min functions
# compute the max height, store the result in maxH
maxH <- max(height)
maxH

# compute the min weight, store the results in minW
minW <- min(weight)
minW

# ---------------------------------
# Step 3: Vector Math
# create a new vector, which is the weight + 5 (every person gained 5 pounds)
weightPlusFive <- weight + 5
weightPlusFive

# compute the pounds/inch for each person, using the new weight just created
poundsPerInch <- weightPlusFive / height
poundsPerInch

# ---------------------------------
# Step 4: Using Conditional if statements
# test if max height is greater than 60 (output yes or no)
if(max(height)>60) print("yes") else print("no")

# test if min weight is greater than the variable ‘a’ (output yes or no)
if(min(weight)>a) print("yes") else print("no")

