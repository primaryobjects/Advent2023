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

# Print the sum of all scores.
print(sum(unlist(lapply(scores, function(score) { score['score'] }))))

# 21821

# Create a hash for counts of length "scores" and initialize each count to 1 (for the original card).
occurrences <- vector("list", length(scores))
for (i in seq_along(occurrences)) {
  occurrences[[i]] <- 1
}

lapply(seq(matching), function(i) {
  # Get the current card data.
  match <- matching[i][[1]]

  # Count winning numbers.
  count <- length(match$matches)

  # If there is at least one winning number, increment the counts for the subsequent cards.
  if (count > 0) {
    # Set the first and last card to increment the counts for.
    first_card <- min(i + 1, length(scores))
    last_card <- min(i + 1 + count - 1, length(scores))
    
    # Update the count for each subsequent card.
    for (card_number in seq(first_card, last_card)) {
      # Increment the occurrences for this card by the count of the parent card.
      #print(paste('Card', i, 'Matches', count, 'Occurrences', occurrences[[i]], 'Card', card_number, 'Occurrences', occurrences[[card_number]], 'Add', occurrences[[i]]))
      occurrences[card_number] <<- occurrences[[card_number]] + occurrences[[i]]
    }
  }
})

# Print the sum of all the occurrences of cards.
print(sum(unlist(occurrences)))

# 5539496