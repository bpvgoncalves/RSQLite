test_that("can extract column info", {
  db <- memory_db()
  on.exit(dbDisconnect(db))

  df <- data.frame(
    a = 1L, b = 2, c = "three", d = I(list(raw(4))),
    stringsAsFactors = FALSE
  )
  dbWriteTable(db, "test", df)

  res <- dbSendQuery(db, "SELECT * FROM test")
  info <- dbColumnInfo(res)
  dbClearResult(res)

  expect_equal(
    info,
    data.frame(
      name = names(df),
      type = vapply(df, typeof, character(1)),
      .declared_type = c("INTEGER", "REAL", "TEXT", "BLOB"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )
})


test_that("results without any declared type return 'NA'", {
  db <- memory_db()
  on.exit(dbDisconnect(db))

  res <- dbSendQuery(db, "SELECT 1 as one")
  info <- dbColumnInfo(res)
  dbClearResult(res)

  expect_equal(
    info,
    data.frame(
      name = "one",
      type = "integer",
      .declared_type = "NA",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )
})


test_that("results with mixed declared/undeclared and existing/non-existent column type", {
  db <- dbConnect(SQLite(), extended_types = TRUE)
  on.exit(dbDisconnect(db))

  dbExecute(db, "CREATE TABLE tmp (col_int integer,
                                   col_real real,
                                   col_text text,
                                   col_date date,
                                   col_none,
                                   col_dummy foo,
                                   col_test bar)")
  dbExecute(db, "INSERT INTO tmp VALUES (1, 8.8, 'a', '2025-05-23', 'b', 3.5, TRUE)")

  res <- dbSendQuery(db, "SELECT * FROM tmp")
  info <- dbColumnInfo(res)
  dbClearResult(res)

  expect_equal(
    info,
    data.frame(
      name = c("col_int", "col_real", "col_text", "col_date", "col_none", "col_dummy", "col_test"),
      type = c("integer", "double", "character", "Date", "character", "double", "integer"),
      .declared_type = c("INTEGER", "REAL", "TEXT", "date", "NA", "foo", "bar"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )
})
