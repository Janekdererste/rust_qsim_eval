parse_duration <- function(values) {
  sapply(values, function(value) {

    i <- index_of_last_numeric(value)
    unit_size <- nchar(value) - i


    unit <- substr(value, nchar(value) - unit_size + 1, nchar(value))
    number <- as.numeric(substr(value, 1, nchar(value) - unit_size))

    switch(unit,
           "s" =dseconds(number),
           "ms" = dmilliseconds(number),
           "µs" =  dmicroseconds(number),
           "ns" = dnanoseconds(number),
           "ps" = dpicoseconds(number),
    )
  })
}

index_of_last_numeric <- function(value) {
  matches <- gregexpr("\\d", value)
  sapply(matches, function(match) {
    max(match)
  })
}