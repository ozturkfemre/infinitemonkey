

# At first, lets start deciding the first passage. Let the first passage be `monkey`
passage1 <- ("monkey")


# Lets create our monkey that randomly hitting keys on a typewriter
monkey_the_author <- function(passage) {
  chars <- c(letters, " ")
  paste0(sample(chars, length(passage1[1]), replace = TRUE), collapse = "")
}


# seconly, what do we need? we need the experiment environment.

count <- 0 # I want to know how many hits the monkey needed to write the passage.
set.seed(123)
repeat { # I will use repeat loop because the loop has to continue until the condition I want is met.
  count <- count + 1
  test_passage <- rep("", length(passage1)) # vector to store the generated text.
  for (i in 1:length(passage1)) { # iterate through each line 
    for (j in 1:nchar(passage1[[i]])) { # iterate through each character
      test_char <- monkey_the_author(passage1) # monkey hits
      # for each character, the loop generates random strings until a character that matches the corresponding character in the target text is found.
      while (test_char != substr(passage1[[i]], j, j)) { # it uses the substr function to extract the character from the target text and compare it to the generated character.
        test_char <- monkey_the_author(passage1) # if the generated character does not match the target character, the loop generates another random string and continues until a match is found
        count <- count + 1 # to keep track of the number of attempts required
      }
      test_passage[i] <- paste0(test_passage[i], test_char) # Once a match is found, the loop concatenates the matched character to the current line
    }
  }
  if (identical(test_passage, passage1)) { # If the entire test_passage vector matches the passage vector
    break # the loop breaks
  }
}

# lets see how many attempts required to generate the target passage
cat("It took", count, "attempts to generate the target passage.")

# lets check whether the entire test_passage matches the original passage.
test_passage

# lets create more complicated passage and test it again:
passage2 <- ("monkey wrote this passage")

# again, the same processes

count2 <- 0
set.seed(122)
repeat {
  count2 <- count2 + 1
  test_passage2 <- rep("", length(passage2))
  for (i in 1:length(passage2)) {
    for (j in 1:nchar(passage2[[i]])) {
      test_char2 <- monkey_the_author(passage2)
      while (test_char2 != substr(passage2[[i]], j, j)) {
        test_char2 <- monkey_the_author(passage2)
        count2 <- count2 + 1
      }
      test_passage2[i] <- paste0(test_passage2[i], test_char2)
    }
  }
  if (identical(test_passage2, passage2)) {
    break
  }
}


# lets see how many attempts required to generate the target passage
cat("It took", count2, "attempts to generate the target passage.")

# lets check whether the entire test_passage matches the original passage.
test_passage2

# As the number of words in the passage that the monkeys are asked to write increases, the number of iterations in the loop, i.e. the number of attemps required, will also increase. 
# This inevitable relationship is actually the basis of the theorem. 
