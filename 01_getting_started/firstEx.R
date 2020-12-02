D <- 1000
K <- 5
h <- 0.25
die <-1:6
Q=sqrt((2*D*K)/h)
car <- mtcars
Q
K
h
die
history()
mean(die)
round(mean(die), digits = 2)
dice <- sample(die, size = 2, replace = TRUE)
dice
## 3 4

sum(dice)
## 7
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
roll()
roll2 <- function(faces = 1:6) {
  dice <- sample(faces, size = 2, replace = TRUE)
  sum(dice)
}

roll2()
## 9
roll2(faces = 1:6)
## 7

roll2(faces = 1:10)
## 13
roll3 <- function(faces = 1:6, num_of_dice = 2) {
  dice <- sample(faces, size = num_of_dice, replace = TRUE)
  sum(dice)
}

roll3(faces = 1:6, num_of_dice = 3)
tibble(
  x = 1:50,
  y = runif(50), 
  z = x + y^2,
  outcome = rnorm(50)
)

class(cars)
## "data.frame"

cars_tbl <- as_tibble(cars)
class(cars_tbl)
## "tbl_df"     "tbl"        "data.frame"

