library(stringr)

# Read the file.
con <- file('input.txt', 'r')
cards <- readLines(con)
close(con)

# Go through each card.
matching <- lapply(seq(cards), function(i) {
  s <- cards[i]
  
  # Remove the "Card X:" part and extra whitespaces
  s <- gsub("^Card\\s+\\d+:\\s+", "", s)
  s <- gsub("\\s+", " ", s)
  
  # Split the string at the pipe symbol
  split_s <- strsplit(trimws(s), " \\| ")[[1]]
  
  # Use a regular expression to extract the numbers
  winning <- as.numeric(unlist(strsplit(trimws(split_s[1]), " ")))
  numbers <- as.numeric(unlist(strsplit(trimws(split_s[2]), " ")))
  
  matches <- numbers[numbers %in% winning]
  
  list(winning = winning, numbers = numbers, matches = matches)
})

# Calculate the score.
scores <- lapply(matching, function(match) {
  count <- length(unlist(match['matches']))
  
  # Calculate the points
  scoring <- 2^(0:(count-1))
  score <- scoring[[length(scoring)]]
  
  list(winning = match['winning'][[1]], numbers = match['numbers'][[1]], matches = match['matches'][[1]], score = as.integer(score))
})

print(sum(unlist(lapply(scores, function(score) { score['score'] }))))

# 21821