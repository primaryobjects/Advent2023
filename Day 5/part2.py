import re
import time
from concurrent.futures import ThreadPoolExecutor

# Read the file.
with open('C:/Users/kory/Desktop/Advent2023/Day 5/input.txt', 'r') as f:
    data = f.readlines()

# Get the list of seeds.
seeds = list(map(int, re.split(' ', data[0].strip())[1:]))
exit

mappings = []
destination_values = []
source_values = []
name = ''

# Read each data map.
for line in data[1:]:
    line = line.strip()

    if len(line) > 0:
        parts = re.split(' ', line)

        if 'map:' in line:
            if len(name) > 0:
                # Store current mapping.
                mappings.append({'name': name, 'data': {'destination': destination_values, 'source': source_values}})
                destination_values = []
                source_values = []

            # Begin a new mapping.
            name = parts[0]
            print(f'Found mapping {name}')
        else:
            destination_value_start = int(parts[0])
            source_value_start = int(parts[1])
            range_ = int(parts[2])

            # Expand the source and destination values by the range.
            destination_values.append(list(range(destination_value_start, destination_value_start + range_)))
            source_values.append(list(range(source_value_start, source_value_start + range_)))

# Append final mapping.
mappings.append({'name': name, 'data': {'destination': destination_values, 'source': source_values}})

def find_location_values(seeds, verbose=True, print_count=1000):
    # To find the lowest location number for any of the seeds we have to parse through each map.
    current_i = 1
    start_time = time.time()

    # Pre-allocate the results list.
    results = [None] * len(seeds)

    min_target = -1

    # Use ThreadPoolExecutor for parallel execution.
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(find_location_value, seed, verbose, print_count, current_i): seed for seed in seeds}
        for future in futures:
            results[current_i - 1] = future.result()
            current_i += 1

    return results

def find_location_value(seed, verbose, print_count, current_i):
    # Set the starting point to the seed.
    target = seed
    global min_target

    # Iterate across each map.
    for map_ in mappings:
        # Find the source range that the current target lies within.
        # See if the index is in source1, source2, source3, ... and then get the same destination index as the next target.
        for i in range(len(map_['data']['source'])):
            if target == seed:
                # Get the values for source.
                source = map_['data']['source'][i]
                # Get the values for destination.
                destination = map_['data']['destination'][i]

                # Find the index in source N so we can get the matching destination.
                if source[0] <= target <= source[-1]:
                    # target is within this mapping range. We calculate the index as:
                    # target = 15
                    # source = 10,11,12,13,14,15,...20
                    # index = 15 - 10 + 1 = 6
                    index = target - source[0]

                    # Get the destination and update our next target.
                    target = destination[index]
                    if verbose:
                        print(f"{map_['name']} destination {target}")

        # Update our step in the mapping to the next map.
        seed = target

    if verbose or current_i % print_count == 0:
        current_time = time.time()

        # Calculate the elapsed time and the average time per iteration
        elapsed_time = current_time - start_time
        avg_time_per_iteration = elapsed_time / current_i

        # Estimate the remaining time
        remaining_iterations = len(seeds) - current_i
        estimated_time_remaining = remaining_iterations * avg_time_per_iteration

        # Convert the estimated time remaining to hours, minutes, and seconds
        hours = estimated_time_remaining // 3600
        minutes = (estimated_time_remaining % 3600) // 60
        seconds = round(estimated_time_remaining % 60)

        # Print the estimated time remaining
        print(f"Min {min_target} Estimated time remaining: {hours} hours, {minutes} minutes, {seconds} seconds")

    if min_target == -1 or target < min_target:
        min_target = target

    return target

results = find_location_values(seeds)
print(min([item for sublist in results for item in sublist]))
