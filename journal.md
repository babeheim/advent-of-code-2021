
https://adventofcode.com/


# 2021-12-08 - Advent of Code Day 8


# 2021-12-07 - Advent of Code Day 7


# 2021-12-06 - Advent of Code Day 6: Lanternfish

https://adventofcode.com/2021/day/6

Exponentially-growing lanternfish, doubles every 7 days. each fish is summarizes by an internal timer representing *number of days until it creates a new lanternfish*. When a fish first appears it needs 2 days to then start the 7-day doubling cycle.

We can represent the internal timer as counting the *number of complete days including today* until the day a doubling occurs. On the days doublings occur, this is zero. The day after a doubling, it starts at 6 because there are 6 days between doubling-days for the full 7-day cycle.

So, the internal timer of one fish and all its offspring over 25 days would look like this:

  3210654321065432106543210 
     8765432106543210654321
            876543210654321
             87654321065432
                   87654321
                    8765432
                    8765432
                     876543
                          8

Given an initial population of fish with known timers, we can simulate forwards as new fish appear with their own timers.

How many lanternfish would there be after 80 days? In the training example, there are 5 fish, and after 18 days, there are a total of 26 fish. After 80 days, there would be a total of 5934. In the validation set there are 300 fish.

```r



```




# 2021-12-05 - Advent of Code Day 5: Hydrothermal Venture

> We have the start and stop coordinates for lines of hydrothermal vents in discrete x-y space, in the format of

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

> Our task is to count up the number of cells in which 2 or more such lines intersect as a sum. In the first part we consider only horizonal and vertical lines. In the second part, we consider diagonal lines as well.

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



# 2021-12-04 - Advent of Code Day 4: Giant Squid

> For a given set of 5x5 matricies (bingo boards), find the board that will have the first "bingo" as you proceed through a sequence of bingo draws. The "score" for that board is the set of un-marked values times the final drawn value that produced the "bingo".

> --- Part Two ---

> Now we have to find the bingo board that wins *last*, and score that one.

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



# 2021-12-03 - Advent of Code Day 3: Binary Diagnostic

> For a given set of input binary numbers, find two new binary numbers, the gamma rate and the epsilon rate.
> Each bit in the gamma rate is the most common bit in the corresponding position of all input numbers.
> Each bit in the epsilon rate is the least common bit in the corresponding position of all input numbers.
> The product of the gamma rate and epsilon rate is the power consumption.

> --- Part Two ---

> life support ratin = oxygen generator rating * co2 scrubber rating

> for the oxygen generator rating, find the most common bit in that position, discard numers that do not have it; if 0 and 1 equally common, favor 1
> for the co2 scrrubber rating, find the least common bit in each position, discarding numbrs that do not have it; if 0 or 1 equally common, favo 0

> keep doing this bit by bit, starting at the left and going right, until one number is left

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





# 2021-12-02 - Advent of Code Day 2: Dive!

> It seems like the submarine can take a series of commands like `forward 1`, `down 2`, or `up 3`:

> `forward X` increases the horizontal position by X units.
> `down X` increases the depth by X units.
> `up X` decreases the depth by X units.

> Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

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

vectors <- text_to_vectors("day2_input_test.txt")
prod(vectors[nrow(vectors),]) == 150 # should be 150

vectors <- text_to_vectors("day2_input.txt")
prod(vectors[nrow(vectors),]) # 1762050
plot(vectors, type = "l", ylim = c(max(vectors[,2]), 0), ylab = "depth", xlab = "x displacement")

```

> --- Part Two ---

> In addition to horizontal position and depth, you'll also need to track a third value, aim, which also starts at 0. The commands also mean something entirely different than you first thought:

> `down X` increases your aim by X units.
> `up X` decreases your aim by X units.
> `forward X` does two things: It increases your horizontal position by X units and it increases your depth by your aim multiplied by X.


```r

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

vectors <- text_to_vectors_with_aim("day2_input_test.txt")
prod(vectors[nrow(vectors),1:2]) == 900 # should be 900

vectors <- text_to_vectors_with_aim("day2_input.txt")
prod(vectors[nrow(vectors),1:2]) # 1855892637

par(mfrow = c(1, 2))
vectors_bad <- text_to_vectors("day2_input.txt")
plot(vectors_bad, type = "l", ylim = c(max(vectors_bad[,2]), 0), ylab = "depth", xlab = "x displacement", col = "red", las = 1)
vectors <- text_to_vectors_with_aim("day2_input.txt")
plot(vectors, type = "l", ylim = c(max(vectors[,2]), 0), ylab = "depth", xlab = "x displacement", las = 1)
points(vectors_bad, type = "l", col = "red")

```

## Other Solutions

- using [{R6} objects](https://github.com/karawoo/adventofcode2021/blob/main/R/day02.R#L98-L150)
- [Hvitfeldt's solution](https://emilhvitfeldt.github.io/rstats-adventofcode/2021.html?panelset=day-2)



# 2021-12-01 - Advent of Code Day 1: Sonar Sweep

> How many measurements are larger than the previous measurement?

> Start by comparing the first and second three-measurement windows. The measurements in the first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second window is larger than the sum of the first, so this first comparison increased.

199  A      
200  A B    
208  A B C  
210    B C D
200  E   C D
207  E F   D
240  E F G  
269    F G H
260      G H
263        H

> Your goal now is to count the number of times the sum of measurements in this sliding window increases from the previous sum. So, compare A with B, then compare B with C, then C with D, and so on. Stop when there aren't enough measurements left to create a new three-measurement sum.

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
read_sum_diff_three("day1_input_test.txt") == 5 # supposed to be 5
read_sum_diff_three("day1_input.txt") == 1344

```
