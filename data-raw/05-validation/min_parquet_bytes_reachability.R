# Is MIN_PARQUET_BYTES (100) ever reachable by a VALID parquet file?
#
# The divergence between the two load paths only bites on a file that is under
# the floor AND that arrow can read. If the smallest valid parquet is larger
# than 100 bytes, the floor only ever rejects corrupt files -- which the other
# path also rejects, via an arrow exception -- and the two agree on every real
# input.
suppressMessages(library(arrow))

cases <- list(
  "zero rows, one int column"   = data.frame(a = integer(0)),
  "zero rows, one chr column"   = data.frame(a = character(0)),
  "one row, one int column"     = data.frame(a = 1L),
  "one row, one dbl column"     = data.frame(a = 1.5),
  "typical torpdata shape (0 rows, 10 cols)" =
    as.data.frame(stats::setNames(rep(list(numeric(0)), 10), paste0("c", 1:10)))
)

cat(sprintf("%-42s %8s  %s\n", "case", "bytes", "readable?"))
for (nm in names(cases)) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(cases[[nm]], tmp)
  sz <- file.size(tmp)
  ok <- tryCatch({ arrow::read_parquet(tmp); TRUE }, error = function(e) FALSE)
  cat(sprintf("%-42s %8d  %s%s\n", nm, sz, ok,
              if (sz < 100) "   <-- UNDER THE FLOOR" else ""))
}

# And the other half: what does arrow do with a sub-100-byte non-parquet?
truncated <- tempfile(fileext = ".parquet")
writeBin(as.raw(c(0x50, 0x41, 0x52, 0x31, rep(0, 20))), truncated)
cat(sprintf("\ntruncated 24-byte file: size %d, readable = %s\n",
            file.size(truncated),
            tryCatch({ arrow::read_parquet(truncated); TRUE },
                     error = function(e) FALSE)))
invisible(NULL)
