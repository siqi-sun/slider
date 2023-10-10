# Slider Simulation Study
# Siqi Sun
# Edit Oct 9, 2022

#This function should take our study design parameters and generate our datasets
#We have 2 types of data we need to generate: continuous sliders and ordinal likert
#There are a couple of different design parameters we need to consider:
#1. Number of items - we are going to keep this the same for both the sliders and the likert
n_items <- 
#2. Measurement error in the sliders - We need to keep this separate from the measurement error in the likert
#                                      because we want to see where the performances cross
err_slid <-
#3. Measurement error in the Likert - We are going to operationalize this as the alpha parameter
err_lik <-
#4. Number of likert Response categories
n_lik_resp <- 
#5. Sample size 
n_sample <-
#6. Number of iterations - This is a standard control parameter.


gen_sliders = function(latent_scores, n_items, err_slid){
  #this function should generate the sliding scales as follows:
  #1. For each of the n_items, first find the expected value of the sliding scale
  #   which is going to be the percentile of the latent score from the pdf of the standard normal
  #   i.e. use qnorm
  #2. Convert this to be between 0 and 100 (the percentile comes out as a decimal, so multiply by 100 and round)
  expected_values <- round(100 * qnorm(latent_scores)) 
  #3. Generate noise using rnorm with a mean of 0 and an SD of err_slid. Round to the nearest integer value
  #   make sure you generate the correct number of samples (i.e. length(latent_scores))    
  noise <- round(rnorm(length(latent_scores), mean = 0, sd = err_slid))
  #4. Add your noise to your expected values from step 1, then check to see if the scores are above 100 or below 0
  #   if so, correct those.
  slider_scores <- pmax(0, pmin(100, expected_values + noise))
  #This function should return a dataframe that consists of n_item columns, with length(latent_scores) rows.
  return(data.frame(matrix(slider_scores, ncol = n_items)))
  }

gen_likert = function(latent_scores, n_items, err_lik, n_lik_resp){
  #This function should generate the items as follows:
  #1. Using the function gen_likert_item (which is below and you will have to program),
  #   generate the correct number of likert items
  #This function should return a dataframe that consists of n_item columns, with length(latent_scores) rows.
  likert_scores <- matrix(0, nrow = length(latent_scores), ncol = n_items)
  for (i in 1:length(latent_scores)) {
    for (j in 1:n_items) {
      likert_scores[i, j] <- gen_likert_item(latent_scores[i], err_lik, n_lik_resp)
    }
  }
  return(data.frame(likert_scores)) 
  }

gen_likert_item = function(latent_score, err_lik, n_lik_resp){
  #First, you will need to generate some beta values. To keep it simple, we are going
  #   to generate beta values that evenly subdivide the region between -2 and 2. The code
  #   below does this. 
  beta <- seq(-2,2, 4/(n_lik_resp))[c(-1,(-n_lik_resp-1))]
  
  #Then, you will need to generate the appropriate probability distribution.
  #These are found in equations 7 and 8 of the ordinal state space paper
  #   (you will have to modify the equations slightly), also, remember that err_lik is alpha
  #This will be a challenging task, please feel free to ask me questions about how to translate the math.
 
  # Equation (7): Probability that yit > j given xt
  probability_yit_greater_than_j <- 1 / (1 + exp(-err_lik * (latent_score - beta)))
  
  # Equation (8):
  # Initialize probabilities for response options
  probabilities <- numeric(n_lik_resp)
  
  # Calculate probability for response option 1
  probabilities[1] <- 1 
  
  # Calculate probabilities for response options 2 to n_lik_resp
  for (j in 2:n_lik_resp) {
    probabilities[j] <- probability_yit_greater_than_j[j] -
      probability_yit_greater_than_j[j - 1]
  }
  
  #Once you have those probabilities, use the sample() function to get the observed score.
  observed_score <- sample(1:n_lik_resp, 1, prob = probabilities) 
  #Return the score
  return(observed_score)
}

#Use this function definition (i.e. don't change the arguments)
generate_data <- function(n_items, n_sample, err_slid, err_lik, n_lik_resp){
  #First, generate a n_sample size sample from a univariate normal distribution
  # with mean 0 and sd of 1
  latent_scores <- rnorm(n_sample, mean = 0, sd = 1) 
  #Next, use the gen_sliders function (which you need to define below), to generate the sliding data
  slider_data <- gen_sliders(latent_scores, n_items, err_slid) 
  #Then use the gen_likert function to generate the likert data
  likert_data <- gen_likert(latent_scores, n_items, err_lik, n_lik_resp)
  #Then construct a data frame with the latent scores, the slider data and the likert data, and return that.
  #   Make sure that the columns are appropriately named.
  data <- data.frame(latent_scores, slider_data, likert_data) 
  colnames(data) <- c("latent_scores", paste0("slider_", 1:n_items), paste0("likert_", 1:n_items))
  return(data)
}

fit_model <- function(data) {
  #This is a bit more complicated. I'll update these directions when we get done with the generation code.
}
