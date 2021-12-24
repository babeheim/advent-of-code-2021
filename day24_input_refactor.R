w <- ins[1]
x1 <- as.integer(!((z %% 26L) + 13L == w))
z <- z + x1 * (z * 25L + w + 13L)

w <- ins[2]
x2 <- as.integer(!((z %% 26L) + 11L == w))
z <- z + x2 * (z * 25L + w + 10L)

w <- ins[3]
x3 <- as.integer(!((z %% 26L) + 15L == w))
z <- z + x3 * (z * 25L + w + 5L)

w <- ins[4]
x4 <- as.integer(!((z %% 26L) - 11L == w))
z <- (z %/% 26L) + x4 * ((z %/% 26L) * 25L * x4 + w + 14L)

w <- ins[5]
x5 <- as.integer(!((z %% 26L) + 14L == w))
z <- z + x5 * (z * 25L * x5 + w + 5L)

w <- ins[6]
x6 <- as.integer(!((z %% 26L) + 0L == w))
z <- (z %/% 26L) + x6 * ((z %/% 26L) * 25L + w + 15L)

w <- ins[7]
x7 <- as.integer(!((z %% 26L) + 12L == w))
z <- z + x7 * (z * 25L + w + 4L)

w <- ins[8]
x8 <- as.integer(!((z %% 26L) + 12L == w))
z <- z + x8 * (z * 25L + w + 11L)

w <- ins[9]
x9 <- as.integer(!((z %% 26L) + 14L == w))
z <- z + x9 * (z * 25L + w + 1L)

w <- ins[10]
x10 <- as.integer(!((z %% 26L) - 6L == w))
z <- (z %/% 26L) + x10 * ((z %/% 26L) * 25L + w + 15L)

w <- ins[11]
x11 <- as.integer(!((z %% 26L) - 10L == w))
z <- (z %/% 26L) + x11 * ((z %/% 26L) * 25L + w + 12L)

w <- ins[12]
x12 <- as.integer(!((z %% 26L) - 12L == w))
z <- (z %/% 26L) + x12 * ((z %/% 26L) * 25L + w + 8L)

w <- ins[13]
x13 <- as.integer(!((z %% 26L) - 3L == w))
z <- (z %/% 26L) + x13 * ((z %/% 26L) * 25L + w + 14L)

w <- ins[14]
x14 <- as.integer(!((z %% 26L) - 5L == w))
z <- (z %/% 26L) + x14 * ((z %/% 26L) * 25L + w + 9L)

