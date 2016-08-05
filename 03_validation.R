### Please use pacman for loading packages
### see https://cran.r-project.org/web/packages/pacman/vignettes/Introduction_to_pacman.html
library(pacman)
p_load(dplyr, readxl)
### VALIDATION ###

## DO NOT CHANGE THIS SECTION OF THE CODE
## It's preparing data for validation
# Note that we have provided an xlsx with a small data set (10 persons), to demonstrate how this will work.
THRESHOLD = 1200
validationSet = read_excel("validation.xlsx")
validationScores = validationSet[["Score"]]

create_validation_set = function()
{
  valDuration     = validationSet %>% select(matches("Du[0-9][0-9]"))
  valDuration     = apply(valDuration, 1, cumsum) %>% t
  valLastQuestion = apply(valDuration > THRESHOLD, 1, function(x) which(x)[1])
  valLastQuestion[is.na(valLastQuestion)] = ncol(valDuration)
  
  for(i in seq_along(valLastQuestion))
  {
    valLQ = valLastQuestion[i]
    if(valLQ == ncol(valDuration)) next; # skip when someone solved all questions
    valLQ = sprintf("An%02i",valLQ + 1)
    valLQ = which(colnames(validationSet) == valLQ):ncol(validationSet)
    validationSet[i,valLQ] = NA
    print(i)
  }
  validationSet
}

validationSet = create_validation_set()

### VALIDATE MODEL (THIS PART IS WHERE YOU CAN CHANGE!)

# This is Super Naive R Model: If they do not finish within 1200 seconds, they
# will not score more. This ignores all the other possible parameters of a real
# model, such as error analysis, time for an item, probability of future results
# based on earlier scores and times, etc.

validationSet[is.na(validationSet)] = 0 # Replace this naive model with your own model

# THIS IS THE PART WHICH CALCULATES IF YOU ARE THE WINNER
# Calculate predicted score:
predictedScores = rowSums(validationSet %>% select(matches("Sc[0-9][0-9]")))

SSE = sum((predictedScores - validationScores)^2)
print(SSE) # THE SMALL THIS NUMBER IS, THE MORE CHANCE OF WINNING.