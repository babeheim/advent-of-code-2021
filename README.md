
# Links

https://adventofcode.com/
https://carbon.now.sh/ to show off code

## Other Solvers

- using [{R6} objects](https://github.com/karawoo/adventofcode2021/blob/main/R/day02.R#L98-L150)
- [Hvitfeldt's solution](https://emilhvitfeldt.github.io/rstats-adventofcode/2021.html?panelset=day-2)


# Day 8: Seven Segment Search

https://adventofcode.com/2021/day/8

The seven segments of a seven-segment display are labelled a thru g

 aaaa
b    c
b    c
 dddd
e    f
e    f
 gggg

They can be used to display any digit from 0 to 9:

   0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

1:   c  f
7: a c  f
4:  bcd f

2: a cde g
3: a cd fg
5: ab d fg

0: abc efg
6: ab defg
9: abcd fg

8: abcdefg

However, the signal inputs, also labelled a to g, have been randomized for each 4-digit display.

For each 4-digit panel, you watch it cycle through numbers until you see all 10 unique combinations of signals, and record those 10 patterns. For each panel, we also have a 4-digit number to decode, represented as:

  the ten unique signal patterns | the four digit output to decode

You don't know which signals correspond to which segments, but can identify which number the signal group is trying to represent by the *number* of signals involved. First, we focus on the *easy* cases: "1", "7", "4", and "8". Our first task is to take the 4-digit output and ask, how many times do digits 1, 4, 7, or 8 appear?

(Obviously the next step will be to decode all seven signals and thus the numbers themselves)

```r

count_simple_numbers <- function(path) {
  x <- readLines(path)
  targets <- rep(NA, length(x))
  for (i in 1:length(x)) {
    targets[i] <- strsplit(x[i], " \\| ")[[1]][2]
  }
  # how many words in targets have 2, 3, 4 or 7 characters?
  targets <- strsplit(targets, split = "\\s")
  counts <- unlist(lapply(targets, function(z) sum(nchar(z) %in% c(2, 3, 4, 7))))
  sum(counts)
}

# easy game bap bap
count_simple_numbers("day8_input_test.txt") == 26
count_simple_numbers("day8_input.txt") == 412

decode_targets <- function(x, mapping) {
  out <- x
  for (i in 1:10) {
    decoded <- gsub(letters[i], substr(mapping, i, i), x)
  }

  if (decoded == "cf") out[i] <- 1
  if (decoded == "acf") out[i] <- 7
  if (decoded == "bcdf") out[i] <- 4
  if (decoded == "acdeg") out[i] <- 2
  if (decoded == "acdfg") out[i] <- 3 # oh that's a nice contrast, 2 vs 3
  if (decoded == "abdfg") out[i] <- 5 # oh that's a nice contrast, 3 vs 5
  if (decoded == "abcefg") out[i] <- 0
  if (decoded == "abdefg") out[i] <- 6
  if (decoded == "abcdfg") out[i] <- 9
  if (decoded == "abcdefg") out[i] <- 8
}

sum_decodings <- function(path) {
  x <- readLines(path)
  patterns <- rep(NA, length(x))
  targets <- rep(NA, length(x))
  for (i in 1:length(x)) {
    patterns[i] <- strsplit(x[i], " \\| ")[[1]][1]
    targets[i] <- strsplit(x[i], " \\| ")[[1]][2]
  }
  patterns <- strsplit(patterns, split = "\\s")
  targets <- strsplit(targets, split = "\\s")
  decoded_target <- vector("list", length(targets))
  for (i in 1:length(patterns)) {
    mapping <- decode_pattern(patterns[[i]])
    targets[[i]] <- decode_target(targets[[i]], mapping)
  }
  sum(unlist(lapply(targets, sum)))
}

# how to decode the first one?

# looking at one by itself, we know the two possible mappings for segments "c" and "f"
encoded_one <- "be"
# b -> c and e -> f
# e -> c and f -> f

# comparing one to seven, you can figure out the mapping to segment 'a' exactly
# its the one that is new!
encoded_seven <- "bde"
# d -> a

# comparing one to four, you can figure out the two possibilities for mapping "b" and "d"
encoded_four <- "bceg"
# c -> b or g -> b
# g -> d or c -> d

# i think we can identify five then, it contains signal for "a", which we know exactly, and the signals for b and d which must both be present, and also f, which we have narrowed down to one of two possibilities
# by process of elimination, the mapping to g should be known exactly then
# -> g



```




# Day 7: The Treachery of Whales

https://adventofcode.com/2021/day/7

> consider the following horizontal positions:

16,1,2,0,4,2,7,1,2,14

whatever position they have to move to, the cost is a function of the distance they move.

In part 1, 1 unit of fuel for every unit of movement. In part 2, each unit of movement costs one more unit of fuel than the last one.

sum(1:11)

```r

total_cost <- function(path, value = NULL) {
  x <- as.numeric(strsplit(readLines(path), ",")[[1]])
  if (is.null(value)) value <- median(x)
  abs_dev <- abs(x - value)
  total_absolute_dev <- sum(abs_dev)
  print(paste("position", value, "has total fuel cost", total_absolute_dev))
  return(total_absolute_dev)
}

total_cost_cumulative <- function(path, value = NULL) {
  if (length(value) < 2) {
    x <- as.numeric(strsplit(readLines(path), ",")[[1]])
    if (is.null(value)) value <- round(mean(x))
    abs_dev <- abs(x - value)
    total_cumulative_abs_dev <- sum(abs_dev * (abs_dev + 1) / 2)
    print(paste("position", value, "has total cumulative fuel cost", total_cumulative_abs_dev))
    return(total_cumulative_abs_dev)
  } else {
    sapply(value, function(z) total_cost_cumulative(path, z))
  }
}

total_cost("day7_input_test.txt", 1) == 41
total_cost("day7_input_test.txt", 3) == 39
total_cost("day7_input_test.txt", 10) == 71
total_cost("day7_input_test.txt") == 37
total_cost("day7_input.txt") == 340987

total_cost_cumulative("day7_input_test.txt") == 168
# "position 5 has total cumulative fuel cost 168"
plot(1:16, total_cost_cumulative("day7_input_test.txt", 1:16))
points(5, 168, pch = 20)

total_cost_cumulative("day7_input.txt") == 96987919
# "position 479 has total cumulative fuel cost 96987919"
total_cost_cumulative("day7_input.txt", 478)
# "position 478 has total cumulative fuel cost 96987874"
total_cost_cumulative("day7_input.txt", 477)
# "position 477 has total cumulative fuel cost 96988829"

x <- 450:500
plot(x, total_cost_cumulative("day7_input.txt", x))
points(479, 96987919, pch = 20)

# ah hah, so the arithmetic mean is not perfect, but it is *very very close*
# due to rounding perhaps? or the difference between n^2 and n(n-1)
# local grid search finds the answer immediately

```

```r
# observe the squared deviation is *almost exactly* the same as the crab fuel function
par(mfrow = c(1, 2))
n <- 100
plot((1:n)^2, type = "l")
points((1:n) * (2:(n + 1)), type = "l", col = "red")
plot((1:n)^2, (1:n) * (2:(n + 1))); abline(0, 1)

# that would imply that the arithmetic mean, which minimizes the squared deviations, probably also minimizes this cumulative deviation function

```


# Day 6: Lanternfish

https://adventofcode.com/2021/day/6

Exponentially-growing lanternfish, doubles every 7 days. each fish is summarizes by an internal timer representing *number of days until it creates a new lanternfish*. When a fish first appears it needs 2 days to then start the 7-day doubling cycle.

We can represent the internal timer as counting the *number of complete days including today* until the day a doubling occurs. On the days doublings occur, this is zero. The day after a doubling, it starts at 6 because there are 6 days between doubling-days for the full 7-day cycle.

So, the internal timer of one fish and all its offspring over 25 days would look like this:

  3210654321065432106543210
      876543210654321065432
             87654321065432
               876543210654
                    8765432
                      87654
                      87654
                        876
                  
Given an initial population of fish with known timers, we can simulate forwards as new fish appear with their own timers.

How many lanternfish would there be after 80 days? In the training example, there are 5 fish, and after 18 days, there are a total of 26 fish. After 80 days, there would be a total of 5934. In the validation set there are 300 fish.

alternative strategy: given a single fish on a particular day I can calculate the number of descendants mathematically, then times that by the number of fish with that internal clock on that day...then just add over all the days 6 to 0

In part 2, we ask how many will be there in 256 days!

```r

sim_lanternfish <- function(path, n_days = 80) {
  init <- as.numeric(strsplit(readLines(path), split = ",")[[1]])
  n_alive <- length(init)
  n_doublings <- n_days %/% 7 + 1
  pop <- rep(8, length.out = n_alive * 2^(n_doublings))
  # initialize living pop
  pop[1:n_alive] <- init
  print(paste("before day 1 there are", n_alive, "lanternfish"))
  # experience each day
  for (i in 1:n_days) {
    pop[1:n_alive] <- pop[1:n_alive] - 1
    n_alive <- n_alive + sum(pop[1:n_alive] == (-1))
    pop[pop == (-1)] <- 6
    if (i %% 10 == 0) print(paste("after day", i, "there are", n_alive, "lanternfish"))
    if (length(pop) < n_alive) stop("vector is too short!")
  }
  print(paste("after day", i, "there are", n_alive, "lanternfish"))
  return(n_alive)
}

sim_lanternfish_v2 <- function(path, n_days = 80) {
  # initialize counter vector
  n <- rep(0, 9) # number in state 0 is n[0 + 1], in state 8 is n[8 + 1]
  # load initial population
  init <- as.numeric(strsplit(readLines(path), split = ",")[[1]])
  for (i in (0:8 + 1)) {
    n[i] <- sum(init == (i - 1))
  }
  print(paste("before day 1 there are", sum(n), "lanternfish"))
  # experience each day
  for (i in 1:n_days) {
    n[7 + 1] <- n[7 + 1] + n[0 + 1] # parents switch from state 0 to state 7
    n <- c(n[1:8 + 1], n[0 + 1]) # all decrease 1 state; n[0 + 1] new offspring appear in state 8
    if (i %% 10 == 0) print(paste("after day", i, "there are", sum(n), "lanternfish"))
  }
  print(paste("after day", i, "there are", sum(n), "lanternfish"))
  return(sum(n))
}

options(scipen = 999)

sim_lanternfish("day6_input_test.txt", n_days = 18) == 26
sim_lanternfish("day6_input_test.txt") == 5934
sim_lanternfish("day6_input.txt") == 385391

# in part 2, we run for 256 days
# but it is too big, we cant even run the test version
# gotta go simpler...

# the key is to realize that it's a *stage model* - simply advance each individual thru each stage and voila
sim_lanternfish_v2("day6_input_test.txt", n_days = 18) == 26
sim_lanternfish_v2("day6_input_test.txt") == 5934
sim_lanternfish_v2("day6_input.txt") == 385391

# in part 2, we run for 256 days
sim_lanternfish_v2("day6_input_test.txt", n_days = 256) == 26984457539
sim_lanternfish_v2("day6_input.txt", n_days = 256) == 1728611055389


```

now lets show off

```r

n <- rep(0, 9)
init <- as.numeric(strsplit(readLines("day6_input.txt"), split = ",")[[1]])
for (i in 1:9) n[i] <- sum(init == (i - 1))
for (i in 1:256) {
  n[8] <- n[8] + n[1] # parents switch from state 0 to state 7
  n <- c(n[2:9], n[1]) # all decrease 1 state; offspring to state 8
}
sum(n) # 0.043 seconds

```



# Day 5: Hydrothermal Venture

We have the start- and stop-coordinates for lines of hydrothermal vents in discrete x-y space, in the format of

```
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
```

Our task is to count up the number of cells in which 2 or more such lines intersect as a sum. In Part One, we consider only horizonal and vertical lines. In Part Two, we consider diagonal lines as well.

```r

map_danger <- function(start, stop, include_diag = FALSE) {
  x_max <- max(c(start[,1], stop[,1]))
  y_max <- max(c(start[,2], stop[,2]))
  vent_map <- matrix(0, ncol = x_max, nrow = y_max)

  for (i in 1:nrow(start)) {
    line_x <- integer()
    line_y <- integer()
    # same x (vertical lines)
    if (start[i,1] == stop[i,1]) {
      line_y <- start[i,2]:stop[i,2]
      line_x <- rep(start[i,1], length(line_y))
    }
    # same y (horizontal lines)
    if (start[i,2] == stop[i,2]) {
      line_x <- start[i,1]:stop[i,1]
      line_y <- rep(start[i,2], length(line_x))
    }
    # diagonal lines
    if (include_diag) {
      if (abs(stop[i,2] - start[i,2]) == abs(stop[i,1] - start[i,1])) {
        line_x <- start[i,1]:stop[i,1]
        line_y <- start[i,2]:stop[i,2]
      }
    }
    if (length(line_y) != length(line_x)) stop("line_x and line_y must have same length!")
    if (length(line_x) > 0 & length(line_y) == length(line_x)) {
      for (j in 1:length(line_y)) {
        vent_map[line_y[j], line_x[j]] <- vent_map[line_y[j], line_x[j]] + 1
      }
    }
  }
  image(vent_map >= 2)
  return(sum(vent_map >= 2))
}

read_coords <- function(path) {
  raw <- readLines(path)
  start <- matrix(NA, nrow = length(raw), ncol = 2)
  stop <- matrix(NA, nrow = length(raw), ncol = 2)

  for (i in 1:length(raw)) {
    coord_strings <- strsplit(raw[i], split = " -> ")[[1]]
    start[i, ] <- as.numeric(strsplit(coord_strings[1], split = ",")[[1]]) + 1
    stop[i, ] <- as.numeric(strsplit(coord_strings[2], split = ",")[[1]]) + 1
  }
  out <- list(start = start, stop = stop)
  return(out)
}

# for part 1, only include horizonal or vertical lines

dat <- read_coords("day5_input_test.txt")
map_danger(dat$start, dat$stop) == 5

dat <- read_coords("day5_input.txt")
map_danger(dat$start, dat$stop) == 7674

# for part 2, include diagonal lines

dat <- read_coords("day5_input_test.txt")
map_danger(dat$start, dat$stop, include_diag = TRUE) == 12

dat <- read_coords("day5_input.txt")
map_danger(dat$start, dat$stop, include_diag = TRUE) == 20898

```



# Day 4: Bingo with a Giant Squid

For a given set of 5x5 bingo boards, find the board that will have the first "bingo" as you proceed through a given sequence of bingo draws. Also find the board that will have the *last* bingo through these draws, a guaranteed loser.

```r

score_board <- function(board, draws) {
  unmarked_sum <- sum(as.numeric(board[!(board %in% draws)]))
  unmarked_sum * draws[length(draws)]
}

read_bingo <- function(path) {
  raw <- readLines(path)
  out <- list()
  out$draws <- as.numeric(strsplit(raw[1], split = ",")[[1]])
  board_starts <- which(raw == "") + 1
  board_starts <- setdiff(board_starts, length(raw) + 1) # just in case it was picking up last line
  out$board_array <- array(NA, dim = c(5, 5, length(board_starts)))
  for (i in 1:length(board_starts)) {
    for (j in 1:5) {
      raw_line <- raw[board_starts[i] + (j - 1)]
      raw_line <- gsub("^\\s+", "", raw_line)
      out$board_array[j,,i] <- as.numeric(strsplit(raw_line, split = "\\s+")[[1]])
    }
  }
  return(out)
}

find_bingos <- function(bingo_dat) {
  past_winners <- integer()
  for (i in 1:length(bingo_dat$draws)) {
    hit_array <- array(bingo_dat$board_array %in% bingo_dat$draws[1:i], dim = dim(bingo_dat$board_array))
    row_sums <- apply(hit_array, c(1, 3), sum) # hit sum for row i, board j
    col_sums <- apply(hit_array, c(2, 3), sum) # hit sum for column i, board j
    winner <- integer()
    if (any(row_sums == 5)) {
      winner <- c(winner, which(apply(row_sums == 5, 2, sum) > 0))
    }
    if (any(col_sums == 5)) {
      winner <- c(winner, which(apply(col_sums == 5, 2, sum) > 0))
    }
    winner <- sort(unique(winner))
    winner <- setdiff(winner, past_winners)
    past_winners <- c(past_winners, winner)
    if (length(winner) == 1) {
      print(paste("board", winner, "is a winner after draw", i))
    }
    if (length(winner) > 1) {
      winner_string <- paste(winner, collapse = ", ")
      print(paste("boards", winner_string, "are winners after draw", i))
    }
    if (length(winner) == dim(bingo_dat$board_array)[3]) break()
    if (length(past_winners) == 0 & i == length(bingo_dat$draws)) {
      print("no winner found after all draws!")
    }
  }
}

dat <- read_bingo("day4_input_test.txt")
find_bingos(dat) # "board 3 is a winner after draw 12"
score_board(dat$board_array[,,3], dat$draws[1:12]) == 4512

dat <- read_bingo("day4_input.txt")
find_bingos(dat) # "board 17 is a winner after draw 22"
score_board(dat$board_array[,,17], dat$draws[1:22]) == 33462

# part 2 - find the last board that wins

dat <- read_bingo("day4_input_test.txt")
find_bingos(dat) # "board 2 is a winner after draw 15"
score_board(dat$board_array[,,2], dat$draws[1:15]) == 1924

dat <- read_bingo("day4_input.txt")
find_bingos(dat) # "board 92 is a winner after draw 83"
score_board(dat$board_array[,,92], dat$draws[1:83]) == 30070

```



# Day 3: Binary Diagnostics

Here we work with a list of binary numbers, all the same length. In Part One, we calculate two new numbers, `gamma` and `epsilon`. Each bit in `gamma` is the most-common bit in the corresponding position of the list. Each bit in `epsilon` is the least-common bit. For a given list, the product of gamma and epsilon is our target. In Part Two, we must find the unique number in the list (the "oxygen consumption score") that contains the most-common bit in the first position, in the second position, and so forth, and find the unique number in the list (the "CO2 scrubber score") that contains the least-common bit in the first position, in the second position, and so forth. Our target in Part Two is again the product of these two numbers.

```r

bin_to_dec <- function(x) {
  sum(rev(x) * 2^((1:length(x)) - 1))
}

most_common_bit <- function(x) mean(x == 1) >= 0.5
least_common_bit <- function(x) !(mean(x == 1) >= 0.5)

load_binary <- function(path) {
  x <- readLines(path)
  stopifnot(all(nchar(x) == nchar(x)[1]))
  bits <- matrix(NA, nrow = length(x), ncol = nchar(x))
  for (i in 1:length(x)) {
    bits[i,] <- as.numeric(strsplit(x, split = "")[[i]])
  }
  return(bits)
}

calc_gamma_epsilon <- function(path) {
  bits <- load_binary(path)
  gamma <- bin_to_dec(apply(bits, 2, most_common_bit))
  epsilon <- bin_to_dec(apply(bits, 2, least_common_bit))
  out <- gamma * epsilon
  return(out)
}

calc_oxygen_co2 <- function(path) {
  bits <- load_binary(path)
  oxy_candidates <- 1:nrow(bits)
  for (i in 1:ncol(bits)) {
    keep <- which(bits[oxy_candidates, i] == most_common_bit(bits[oxy_candidates, i]))
    oxy_candidates <- oxy_candidates[keep]
    if (length(oxy_candidates) == 1) break()
  }
  co2_candidates <- 1:nrow(bits)
  for (i in 1:ncol(bits)) {
    keep <- which(bits[co2_candidates, i] == least_common_bit(bits[co2_candidates, i]))
    co2_candidates <- co2_candidates[keep]
    if (length(co2_candidates) == 1) break()
  }
  oxy_rating <- bin_to_dec(bits[oxy_candidates, ])
  co2_rating <- bin_to_dec(bits[co2_candidates, ])
  out <- oxy_rating * co2_rating
  return(out)
}

# part 1
calc_gamma_epsilon("day3_input_test.txt") == 198
calc_gamma_epsilon("day3_input.txt") == 3374136

# part 2
calc_oxygen_co2("day3_input_test.txt") == 230
calc_oxygen_co2("day3_input.txt") == 4432698

```




# Day 2: Dive!

We calculate our horizonal and vertical position after applying a list of text commands. In Part One, the command `forward 8` just means add 8 to our horizonal displacement, `up 8` means a decrease in depth by 8, and `down 2` an increase in depth of 2. In Part Two, we modify this interpretation: now `up 8` means decrease a quantity called *aim* by 8, and `down 2` means increase aim by 2. When we apply a `forward` command, we increase our horizonal displacement by the number given, and change our vertical displacement by the horizonal change times the aim.

```r

text_to_vectors <- function(path) {
  x <- readLines(path)
  sub_direction <- gsub("\\s.*$", "", x)
  stopifnot(all(sub_direction %in% c("forward", "down", "up")))
  sub_magnitude <- as.numeric(gsub("^.*\\s", "", x))
  stopifnot(all(!is.na(sub_magnitude)))
  deltas <- matrix(0, ncol = 2, nrow = length(x)) # forward deltas, vertical deltas
  for (i in 1:nrow(deltas)) {
    if (sub_direction[i] == "forward") deltas[i, 1] <- sub_magnitude[i]
    # here 'up' and 'down' refer to vertical deltas
    if (sub_direction[i] == "up") deltas[i, 2] <- (-1) * sub_magnitude[i]
    if (sub_direction[i] == "down") deltas[i, 2] <- sub_magnitude[i]
  }
  positions <- matrix(NA, ncol = 2, nrow = (nrow(deltas) + 1)) # horizonal position, vertical position
  positions[1,] <- c(0, 0)
  # horizonal position as cumulative horizonal deltas
  positions[2:nrow(positions), 1] <- cumsum(deltas[, 1])
  # vertical position as cumulative vertical deltas
  positions[2:nrow(positions), 2] <- cumsum(deltas[, 2])
  return(positions)
}

text_to_vectors_with_aim <- function(path) {
  x <- readLines(path)
  sub_direction <- gsub("\\s.*$", "", x)
  stopifnot(all(sub_direction %in% c("forward", "down", "up")))
  sub_magnitude <- as.numeric(gsub("^.*\\s", "", x))
  stopifnot(all(!is.na(sub_magnitude)))
  deltas <- matrix(0, ncol = 3, nrow = length(x)) # forward deltas, vertical deltas, aim deltas
  for (i in 1:nrow(deltas)) {
    if (sub_direction[i] == "forward") deltas[i, 1] <- sub_magnitude[i]
    # here 'up' and 'down' refer to aim deltas, not vertical deltas
    if (sub_direction[i] == "up") deltas[i, 3] <- (-1) * sub_magnitude[i]
    if (sub_direction[i] == "down") deltas[i, 3] <- sub_magnitude[i]
  }
  positions <- matrix(NA, ncol = 3, nrow = (nrow(deltas) + 1)) # horizonal position, vertical position, cumulative aim
  positions[1,] <- c(0, 0, 0)
  # horizonal position as cumulative horizonal deltas
  positions[2:nrow(positions), 1] <- cumsum(deltas[, 1])
  # aim as cumulative aim deltas
  positions[2:nrow(positions), 3] <- cumsum(deltas[, 3])
  # vertical delta = horizontal delta * current (cumulative) aim
  deltas[, 2] <- deltas[, 1] * positions[2:nrow(positions), 3]
  # vertical position as cumulative vertical deltas
  positions[2:nrow(positions), 2] <- cumsum(deltas[, 2])
  return(positions)
}

# --- Part One ---
vectors <- text_to_vectors("day2_input_test.txt")
prod(vectors[nrow(vectors),]) == 150 # should be 150

vectors <- text_to_vectors("day2_input.txt")
prod(vectors[nrow(vectors),]) # 1762050
plot(vectors, type = "l", ylim = c(max(vectors[,2]), 0), ylab = "depth", xlab = "x displacement")

# --- Part Two ---
vectors <- text_to_vectors_with_aim("day2_input_test.txt")
prod(vectors[nrow(vectors),1:2]) == 900 # should be 900

vectors <- text_to_vectors_with_aim("day2_input.txt")
prod(vectors[nrow(vectors),1:2]) # 1855892637

# plot to compare
par(mfrow = c(1, 2))
vectors_bad <- text_to_vectors("day2_input.txt")
plot(vectors_bad, type = "l", ylim = c(max(vectors_bad[,2]), 0), ylab = "depth", xlab = "x displacement", col = "red", las = 1)
vectors <- text_to_vectors_with_aim("day2_input.txt")
plot(vectors, type = "l", ylim = c(max(vectors[,2]), 0), ylab = "depth", xlab = "x displacement", las = 1)
points(vectors_bad, type = "l", col = "red")

```




# Day 1: Sonar Sweep

For a list of measurements, how many are larger than the previous measurement in the list? For Part One, we are given the list of measurements. For Part Two, we construct the list from a longer list, as the rolling sum of every group of three measures.

```r

read_sum_diff <- function(path) {
  x <- as.numeric(readLines(path))
  sum(diff(x) > 0)
}

read_sum_diff_three <- function(path) {
  x <- as.numeric(readLines(path))
  x_sum <- rep(NA, (length(x) - 2))
  for (i in 1:(length(x) - 2)) x_sum[i] <- x[i] + x[i + 1] + x[i + 2]
  sum(diff(x_sum) > 0)
}

# part 1
read_sum_diff("day1_input_test.txt") == 7
read_sum_diff("day1_input.txt") == 1316

# part 2
read_sum_diff_three("day1_input_test.txt") == 5
read_sum_diff_three("day1_input.txt") == 1344

```
