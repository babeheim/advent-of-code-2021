w <- ins[1]
x <- as.integer(!((z %% 26L) + 13L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 13L) * x)

w <- ins[2]
x <- as.integer(!((z %% 26L) + 11L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 10L) * x)

w <- ins[3]
x <- as.integer(!((z %% 26L) + 15L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 5L) * x)

w <- ins[4]
x <- as.integer(!((z %% 26L) - 11L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 14L) * x)

w <- ins[5]
x <- as.integer(!((z %% 26L) + 14L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 5L) * x)

w <- ins[6]
x <- as.integer(!((z %% 26L) + 0L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 15L) * x)

w <- ins[7]
x <- as.integer(!((z %% 26L) + 12L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 4L) * x)

w <- ins[8]
x <- as.integer(!((z %% 26L) + 12L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 11L) * x)

w <- ins[9]
x <- as.integer(!((z %% 26L) + 14L == w))
z <- ((z %/% 1L) * (25L * x + 1L)) + ((w + 1L) * x)

w <- ins[10]
x <- as.integer(!((z %% 26L) - 6L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 15L) * x)

w <- ins[11]
x <- as.integer(!((z %% 26L) - 10L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 12L) * x)

w <- ins[12]
x <- as.integer(!((z %% 26L) - 12L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 8L) * x)

w <- ins[13]
x <- as.integer(!((z %% 26L) - 3L == w))
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 14L) * x)

w <- ins[14]
x <- as.integer(!((z %% 26L) - 5L == w)) # is ins[14] NOT equal to (five less than the remainder of z/26), which is inside [-5, 20].
z <- ((z %/% 26L) * (25L * x + 1L)) + ((w + 9L) * x) # VALID if y = z = 0, OR y = -z
