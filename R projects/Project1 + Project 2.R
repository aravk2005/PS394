#Project 1 by Arav Kansal

#1st roll function
roll <- function() {
  
  die <- 1:6
  
  dice <- sample(die, size = 2, replace = TRUE)
  
  sum(dice)
  
}
 
roll()


#2nd roll function with bones being passed as a parameter 
roll2 <- function(bones = 1:6) {
  
  dice <- sample(bones, size = 2, replace = TRUE)
  
  sum(dice)
  
}

roll2() 


install.packages("ggplot2")
library("ggplot2")


#creating scatter plot with vectors x and y
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x

y <- x^3
y

qplot(x, y)


#creating single vector histograms
x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1)

x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)

x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
qplot(x3, binwidth = 1)

replicate(3, 1 + 1)
replicate(10, roll())


rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)


#roll function re-written to account for weighted probability 
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE, 
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)



##Project 2 by Arav Kansal

hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades", 
  "spades", "spades", "spades")

matrix(hand1, nrow = 5)


hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", 
           "spades", "ten", "spades")

matrix(hand2, nrow = 5, byrow = TRUE)


card <- c("ace","hearts", "1")
card

card <- list("ace", "hearts", "1")
card


df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df

#viewing the first 6 rows of the deck of cards dataset
head(deck)

#viewing the last 6 rows of the deck of cards dataset
tail(deck)


write.csv(deck, file = "cards.csv", row.names = FALSE)


#deal function
deal <- function(cards) {
  
  cards[1, ]
}

deal(deck)


#shuffle function
shuffle <- function(cards) {
  
  random <- sample(1:52, size = 52)
  
  cards[random, ]
}

shuffle(deck)

deck2 <- shuffle(deck)
deal(deck2)

mean(deck$value)
median(deck$value)

lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst

lst[1]


deck2 <- deck
deck2$new <- NULL
head(deck2)
sum(deck2$face == "ace")

deck3 <- shuffle(deck2)
deck3$value[deck3$face == "ace"] <- 14
head(deck3)


deck4 <- deck
deck4$value <- 0
head(deck4, 13)

deck4$suit == "hearts"
deck4$value[deck4$suit == "hearts"] 
head(deck4)

deck4[deck4$face == "queen", ]
deck4[deck4$suit == "spades", ]

queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"
deck4[queenOfSpades, ]
deck4$value[queenOfSpades]

deck4$value[queenOfSpades] <- 13

deck4[queenOfSpades, ]


deck5 <- deck

head(deck5, 13)

facecard <- deck5$face %in% c("king", "queen", "jack")

deck5[facecard, ]

deck5$value[facecard] <- 10

head(deck5, 13)

deck5$value[deck5$face == "ace"] <- NA

head(deck5, 13)



setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle


rm(deck)

shuffle()

deal()

deal()
