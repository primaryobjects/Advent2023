library(stringr)
library(foreach)
library(doParallel)

#library(doSNOW)
#cl <- makeCluster(1, type = "SOCK")
#registerDoSNOW(cl)

# Read the file.
con <- file('input.txt', 'r')
data <- readLines(con)
close(con)

# Get the list of seeds.
seeds <- as.numeric(strsplit(data[1], ' ')[[1]][-1])

mappings <- list()
destination_values <<- list()
source_values <<- list()
name <- ''

# Read each data map.
lapply(seq(data)[-1], function(i) {
  line <- data[i]
  
  if (nchar(line) > 0) {
    parts <- strsplit(line, ' ')[[1]]
    
    if (grepl('map:', line)) {
      if (length(name) > 0) {
        # Store current mapping.
        mappings <<- c(mappings, list(list(name=name, data=c(destination=destination_values, source=source_values))))
        destination_values <<- list()
        source_values <<- list()
      }
      
      # Begin a new mapping.
      name <<- parts[1]
      print(paste('Found mapping', name))
    }
    else {
      destination_value_start <- as.numeric(parts[1])
      source_value_start <- as.numeric(parts[2])
      range <- as.numeric(parts[3])

      # Expand the source and destination values by the range.
      destination_values <<- c(destination_values, list(destination_value_start:(destination_value_start + range - 1)))
      source_values <<- c(source_values, list(source_value_start:(source_value_start + range - 1)))
    }
  }
})

# Append final mapping.
mappings <<- c(mappings, list(list(name=name, data=c(destination=destination_values, source=source_values))))[-1]

# We now have a map:
# name=seed-to-soil
# data=c(source1, source2, source3, ..., destination1, destination2, destination3, ...)

find_location_values <- function(seeds, verbose = T, print_count = 1000) {
  # To find the lowest location number for any of the seeds we have to parse through each map.
  current_i <- 1
  start_time <- Sys.time()
  
  # Pre-allocate the results list.
  results <- vector("list", length(seeds))
  
  min_target <- -1
  
  # Use foreach instead of lapply for parallel execution.
  results <- foreach(seed = seeds, .combine = c, .packages = "foreach") %dopar% {
    if (verbose || current_i %% print_count == 0) {
      print(paste('Seed', seed, current_i, '/', length(seeds)))
    }

    # Set the starting point to the seed.
    target <- seed
    
    # Iterate across each map.
    for(map in mappings) {
      # Find the source range that the current target lies within.
      # See if the index is in source1, source2, source3, ... and then get the same destination index as the next target.
      for(i in seq(length(map$data)/2)) {
        if (target == seed) {
          # Get the values for source.
          source <- map$data[paste0('source', i)][[1]]
          # Get the values for destination.
          destination <- map$data[paste0('destination', i)][[1]]
          
          # Find the index in source N so we can get the matching destination.
          if (target >= source[1] && target <= source[length(source)]) {
            # target is within this mapping range. We calculate the index as:
            # target = 15
            # source = 10,11,12,13,14,15,...20
            # index = 15 - 10 + 1 = 6
            index <- target - source[1] + 1
            
            # Get the destination and update our next target.
            target <- destination[index]
            if (verbose) {
              print(paste(map$name, 'destination', target))
            }
          }
        }
      }
      
      # Update our step in the mapping to the next map.
      seed <- target
    }
    
    if (verbose || current_i %% print_count == 0) {
      current_time <- Sys.time()
      
      # Calculate the elapsed time and the average time per iteration
      elapsed_time <- as.numeric(difftime(current_time, start_time, units = "secs"))
      avg_time_per_iteration <- elapsed_time / current_i
      
      # Estimate the remaining time
      remaining_iterations <- length(seeds) - current_i
      estimated_time_remaining <- remaining_iterations * avg_time_per_iteration
      
      # Convert the estimated time remaining to hours, minutes, and seconds
      hours <- floor(estimated_time_remaining / 3600)
      minutes <- floor((estimated_time_remaining %% 3600) / 60)
      seconds <- round(estimated_time_remaining %% 60)
      
      # Print the estimated time remaining
      print(paste("Min", min_target, "Estimated time remaining:", hours, "hours,", minutes, "minutes,", seconds, "seconds"))
    }
    
    current_i <<- current_i + 1

    if (min_target == -1 || target < min_target) {
      min_target <<- target
    }
    
    target
  }
  
  results
}

results <- find_location_values(seeds)
print(min(unlist(results)))

# 403695602

# Part 2 - reset the seed numbers so that we read in pairs with the first being the starting seed number and the second being the range.
# Initialize an empty list to store the expanded ranges.
#expanded_ranges <- list()

# Loop through the list and increment the index by 2 each time.
#for(i in seq(1, length(seeds), by = 2)) {
  # Expand the range for each pair and store it in the list
#  expanded_ranges[[i]] <- seq(from = seeds[[i]], to = seeds[[i]] + seeds[[i+1]] - 1)
#}

# Register the parallel backend.
#registerDoParallel(cores = (detectCores() / 4))

# "Estimated time remaining: 609 hours, 39 minutes, 18 seconds" !!!
#seeds2 <- unlist(expanded_ranges)
#results2 <- find_location_values(seeds2, F)
#print(min(unlist(results2)))

results2 <- list()
minimum <- -1
start_time <- Sys.time()
for (i in seq(1, length(seeds), by = 2)) {
  #if (i <= 7)
  #if (i <= 9)
  #  next
  
  start <- seeds[[i]]
  end <- seeds[[i]] + seeds[[i+1]] - 1
  
  count <- 1
  for (seed2 in rev(start:end)) {
    result <- find_location_values(seed2, F)
    results2 <- c(results2, result)
    
    if (minimum == -1 || result < minimum) {
      minimum <- result
      print(paste('New minimum', minimum))
    }

    if (count %% 100 == 0) {
      current_time <- Sys.time()
      
      # Calculate the elapsed time and the average time per iteration
      elapsed_time <- as.numeric(difftime(current_time, start_time, units = "secs"))
      avg_time_per_iteration <- elapsed_time / count
      
      # Estimate the remaining time
      remaining_iterations <- (end - start) - count
      estimated_time_remaining <- remaining_iterations * avg_time_per_iteration
      
      # Convert the estimated time remaining to hours, minutes, and seconds
      hours <- floor(estimated_time_remaining / 3600)
      minutes <- floor((estimated_time_remaining %% 3600) / 60)
      seconds <- round(estimated_time_remaining %% 60)
    
      print(paste('minimum', minimum, 'pair', i, ',', i+1, '/', length(seeds), ', seed', seed2, '/', end, ', index', count, '/', (end - start), round((count / (end-start)) * 100, 5), '%', "Estimated time remaining:", hours, "hours,", minutes, "minutes,", seconds, "seconds"))
    }

    count <- count + 1
    #if (count > 100) {
    #  break
    #}
  }
  
  break
}

print(min(unlist(results2)))

#stopCluster(cl)
