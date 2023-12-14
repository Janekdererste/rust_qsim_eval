parse_duration <- function(values) {
  sapply(values, function(value) {
    unit <- substr(value, nchar(value) - 1, nchar(value))
    number <- as.numeric(substr(value, 1, nchar(value) - 2))

    switch(unit,
           "s" =dseconds(number),
           "ms" = dmilliseconds(number),
           "µs" =  dmicroseconds(number),
           "ns" = dnanoseconds(number),
           "ps" = dpicoseconds(number),
    )
  })
}