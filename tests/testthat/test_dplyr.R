library(tidyposterior)
library(testthat)
library(dplyr)

###################################################################

set.seed(144422)
obj <- tibble(model = rep(c(letters[1:3]), 100),
              posterior = runif(300))
class(obj) <- c("posterior", class(obj))

###################################################################

test_that('object types', {
  expect_true(tidyposterior:::is_posterior(obj))
  expect_false(tidyposterior:::is_posterior(obj[, 1]))  
})

###################################################################

test_that('dplyr ops', {
  expect_true(
    tidyposterior:::is_posterior(
      obj %>% filter(posterior > .5)
    )
  )
  expect_true(  
    tidyposterior:::is_posterior(
      obj %>% mutate(new_col = posterior^2)
    )    
  )
  expect_true(  
    tidyposterior:::is_posterior(
      obj %>% select(model, posterior)
    )    
  ) 
  expect_true(  
    tidyposterior:::is_posterior(
      obj %>% arrange(posterior)
    )    
  )   
  expect_true(  
    tidyposterior:::is_posterior(
      obj %>% mutate(new_col = posterior^2) %>% rename(newer = new_col)
    )    
  )  
  expect_true(  
    tidyposterior:::is_posterior(
      obj %>% slice(1L)
    )    
  )    
})

