z0 <- as.numeric(ins[1] + 13L) * 26L + (ins[2] + 10L)
# ins[1] sets the initial count for 26s (14:22)
# ins[2] controls initial remainder (11:19)
g1 <- as.integer(!(ins[3] - 6L == ins[4])) # g1=1 if ins[3] != ins[4] + 6
stopifnot(g1 == 0)
g2 <- as.integer(!(ins[5] + 5L == ins[6])) # g2=1 if ins[5] != ins[6] - 5
stopifnot(g2 == 0)

z6 <- z0 +
  g1 * z0 * 25L + g1 * (ins[4] + 14L) +
  g2 * z0 * 25L + g2 * (ins[6] + 15L) +
  g1 * g2 * (25L * 25L * z0 + 25L * (ins[4] + 14L))

z7 <- z6 * 26L + (ins[7] + 4L) # ins[7] controls remainder for z7, 5:13
z8 <- z7 * 26L + (ins[8] + 11L) # ins[8] controls remainder for z8, 12:20

g3 <- as.integer(!(ins[9] - 5L == ins[10])) # g3=1 if ins[9] != ins[10] + 5
stopifnot(g3 == 0)
z10 <- z8 + g3 * (z8 * 25L + (ins[10] + 15L))
x11 <- as.integer(!((z10 %% 26L) - 10L == ins[11])) # two possibilities here; if g3 is 1, this could be 0 or 1. if g3 is 0, this must be 1
stopifnot(x11 == 0)
z11 <- (z10 %/% 26L) + x11 * ((z10 %/% 26L) * 25L + ins[11] + 12L) # (z10 %/% 26L) could be z8 or z7
x12 <- as.integer(!((z11 %% 26L) - 12L == ins[12])) # how to close this?? ins[7] to 9, ins[12] to 1
stopifnot(x12 == 0)
z12 <- (z11 %/% 26L) + x12 * ((z11 %/% 26L) * 25L + ins[12] + 8L)

x13 <- as.integer(!((z12 %% 26L) - 3L == ins[13])) # ins[13] and ins[2], would be nice if ins[13] could be 9...
stopifnot(x13 == 0)
z13 <- (z12 %/% 26L) + x13 * ((z12 %/% 26L) * 25L + ins[13] + 14L)
x14 <- as.integer(!((z13 %% 26L) - 5L == ins[14]))
stopifnot(x14 == 0)
z <- (z13 %/% 26L) + x14 * ((z13 %/% 26L) * 25L + ins[14] + 9L)
