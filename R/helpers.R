remove_dups <- \(x) {
  x <- as.character(x)
  x[x == lag(x)] <- ""
  x
}
