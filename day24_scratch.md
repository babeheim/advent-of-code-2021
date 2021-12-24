
here we code the ALU for holding the 

```r

rm(list = ls())

inp <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- b
    b_arg <- paste0("in=", b)
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("writing", b_arg, "to register", a)
  return(reg_list)
}

add <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] + b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] + reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("adding", a_arg, "and", b_arg)
  return(reg_list)
}

mul <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] * b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] * reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("multiplying", a_arg, "and", b_arg)
  return(reg_list)
}

div <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  if (b == 0L) stop("cannot divide by zero!")
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] %/% b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] %/% reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("dividing", a_arg, "by", b_arg)
  return(reg_list)
}

mod <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  if (a < 0L) stop("cannot modulo a negative dividend!")
  if (b == 0L) stop("cannot modulo by zero!")
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- reg_list[[a]] %% b
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- reg_list[[a]] %% reg_list[[b]]
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("modulo", a_arg, "by", b_arg)
  return(reg_list)
}

eql <- function(a, b, reg_list) {
  stopifnot(a %in% names(reg_list))
  a_arg <- paste0(a, "=", reg_list[[a]])
  if (is.integer(b)) {
    reg_list[[a]] <- as.integer(reg_list[[a]] == b)
    b_arg <- paste0("in=", b)
  } else if (b %in% names(reg_list)) {
    reg_list[[a]] <- as.integer(reg_list[[a]] == reg_list[[b]])
    b_arg <- paste0(b, "=", reg_list[[b]])
  } else {
    stop("b input is not valid")
  }
  reg_list$msg <- paste("testing equality between", a_arg, "and", b_arg)
  return(reg_list)
}


alu <- list(inp = inp, add = add, mul = mul, div = div, mod = mod, eql = eql)

run_line <- function(line, reg_list, input = NULL) { 
  l <- as.list(strsplit(line, " ")[[1]])
  if (l[[1]] == "inp") {
    if (!is.null(input)) {
      l[[3]] <- input
    } else {
      stop("inp input missing!")
    }
  }
  if (!(l[[3]] %in% c("w", "x", "y", "z"))) {
    l[[3]] <- as.integer(l[[3]])
    stopifnot(!is.na(l[[3]]))
  }
  out <- alu[[l[[1]]]](l[[2]], l[[3]], reg_list)
  return(out)
}

regs <- list(w = 0, x = 0, y = 0, z = 0)
eql("w", 3L, regs)
eql("w", "x", regs)

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- inp("w", 5L, regs)
regs <- inp("x", 5L, regs)
regs <- mul("w", "x", regs)
regs$w == 25L

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- inp("w", 5L, regs)
regs <- mul("w", -1L, regs)
regs$w == -5L

regs <- list(w = 0, x = 0, y = 0, z = 0)
regs <- run_line("inp w", regs, 15L)
regs <- run_line("add z w", regs)
regs <- run_line("mod z 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("add y w", regs)
regs <- run_line("mod y 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("add x w", regs)
regs <- run_line("mod x 2", regs)
regs <- run_line("div w 2", regs)
regs <- run_line("mod w 2", regs)

run_program <- function(path, inputs) {
  lines <- readLines(path)
  input_list <- vector("list", length(lines))
  inp_lines <- grep("inp", lines)
  stopifnot(length(inputs) == length(inp_lines))
  for (i in seq_along(inp_lines)) {
    input_list[[inp_lines[i]]] <- inputs[i]
  }
  regs <- list(w = 0, x = 0, y = 0, z = 0)
  log <- vector("list", length(lines))
  log[[1]] <- regs
  for (l in seq_along(lines)) {
    regs <- run_line(lines[l], regs, input_list[[l]])
    log[[l]] <- regs
  }
  log <- rbind(log)
  log <- dplyr::bind_rows(log)
  log <- dplyr::select(log, msg, w, x, y, z)
  log <- as.data.frame(log)
  return(log)
}

asm_program <- function(path, outpath) {
  lines <- readLines(path)
  inp_lines <- grep("inp", lines)
  inp_vars <- gsub("inp ", "", lines[inp_lines])
  inp_ins <- paste0("ins[", seq_along(inp_lines), "]")
  to_var_list <- strsplit(lines, " ")
  to_var <- rep(NA, length(lines))
  for (line in seq_along(to_var_list)) {
    to_var[line] <- to_var_list[[line]][2]
  }
  lines <- gsub("add ", "sum(", lines)
  lines <- gsub("mul ", "prod(", lines)
  lines <- gsub("mod ", "`%%`(", lines)
  lines <- gsub("div ", "`%/%`(", lines)
  lines <- gsub("eql ", "`==`(", lines)
  lines <- paste0(lines, ")")
  lines <- gsub(" ", ", ", lines)
  lines <- paste(to_var, "<-", lines)
  lines[inp_lines] <- paste(inp_vars, "<-", inp_ins)
  writeLines(lines, outpath)
}

# asm_program("day24_input.txt", "day24_input.R")

run_program("day24_binary_converter.txt", 1L)
# sweet, but i want a full report!

run_monad <- function(path, input) {
  # have to specify input as a 14-digit number, no zeros!'
  monad_input <- as.integer(strsplit(as.character(input), "")[[1]])
  stopifnot(all(monad_input %in% 1:9))
  stopifnot(length(monad_input) == 14L)
  out <- run_program(path, monad_input)
  return(out)
}

run_monadR <- function(path, input) {
  ins <- as.integer(strsplit(as.character(input), "")[[1]])
  w <- x <- y <- z <- 0
  source(path, local = TRUE)
  out <- list(w = w, x = x, y = y, z = z)
  return(out)
}

```

cool, now lets check the annotations...

```r

run_monad("day24_input.txt", 13579246899998)$z[252] == 172660765
run_monadR("day24_input.R", 13579246899998)$z == 172660765
run_monadR("day24_input.R", 13579246899998)$z == 172660765

x <- run_monad("day24_input.txt", 11111111111111)$z[252]
run_monadR("day24_input.R", 11111111111111)$z == x
run_monadR("day24_input.R", 11111111111111)$z == x

# the biggest integer the machine can represent is 2147483647, only 10 digits!
for (i in 1:1000) {
  seed <- as.numeric(paste(sample(1:9, 14, replace = TRUE), collapse = ""))
  x <- run_monad("day24_input.txt", seed)$z[252]
  stopifnot(run_monadR("day24_input_refactor.R", seed)$z == x)
  if (i %% 100 == 0) print(i)
}

# how the heck did i get away with treatig z as numeric??

```

With the code refactored, we can finally see the structure of the MONAD program. We start with z = 0. Each digit of input serves as a logical gate. The first three gates, x1 to x3, MUST be 1. The fourth could be 1 or 0