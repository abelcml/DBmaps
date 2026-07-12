library(data.table)

# helper: does the registry's discovered relationships include a child->parent
# link for the given columns? (checked through the public API: the registry's
# grouping variables and identifier columns as consumed by map_join_paths)
has_join <- function(join_map, from, to) {
  if (nrow(join_map) == 0) return(FALSE)
  any(join_map$table_from == from & join_map$table_to == to |
        join_map$table_from == to & join_map$table_to == from)
}

test_that("discover_metadata validates its inputs", {
  expect_error(discover_metadata(list(data.table(x = 1))), "named list")
  expect_error(discover_metadata(list(a = data.frame(x = 1))), "data.tables")
  expect_error(
    discover_metadata(list(a = data.table(x = 1)), tau = 0),
    "'tau'")
  expect_error(
    discover_metadata(list(a = data.table(x = 1)), min_card = 0),
    "'min_card'")
})

test_that("a real FK is found and a same-named non-key column is rejected", {
  customers <- data.table(customer_id = 1:5, name = c("a", "b", "c", "d", "e"))
  orders <- data.table(order_id = 101:106,
                       customer_id = c(1, 2, 2, 3, 5, 5),
                       name = c("p", "q", "q", "r", "s", "s"))
  reg <- discover_metadata(list(customers = customers, orders = orders))
  jm <- map_join_paths(reg)

  expect_true(has_join(jm, "orders", "customers"))
  # 'name' is shared by both tables but is not a key: it must not create a
  # relationship, and no grouping variable should be based on it.
  expect_false(any(vapply(reg$grouping_variable,
                          function(g) "name" %in% g, logical(1))))
})

test_that("surrogate-key coincidences are rejected; self-references are kept", {
  # album_id (1:5) is fully value-contained in track_id (1:20): a surrogate
  # coincidence. The real FK is track.album_id -> album.album_id.
  album <- data.table(album_id = 1:5, title = letters[1:5])
  track <- data.table(track_id = 1:20, album_id = rep(1:5, 4),
                      name = letters[1:20])
  # self-referential FK: category.parent_category_id -> category.category_id
  category <- data.table(category_id = 1:6,
                         parent_category_id = c(NA, 1, 1, 2, 2, 3),
                         label = letters[1:6])
  reg <- discover_metadata(list(album = album, track = track,
                                category = category))

  track_groups <- unlist(reg$grouping_variable[reg$table_name == "track"])
  expect_true("album_id" %in% track_groups)
  # album must not be linked to track's key: album's grouping variables stay
  # its own identifier, not track_id.
  album_groups <- unlist(reg$grouping_variable[reg$table_name == "album"])
  expect_false("track_id" %in% album_groups)
  # the self-reference is discovered
  cat_groups <- unlist(reg$grouping_variable[reg$table_name == "category"])
  expect_true("parent_category_id" %in% cat_groups)
})

test_that("a shared key name binds to the right parent table", {
  # Both 'store' and 'staff' hold a unique store_id; customer.store_id must
  # bind to the 'store' table (table-name affinity), not 'staff'.
  store <- data.table(store_id = 1:2, mgr = c("x", "y"))
  staff <- data.table(staff_id = 1:2, store_id = c(1, 2))
  customer <- data.table(customer_id = 1:6, store_id = c(1, 1, 2, 2, 1, 2))
  reg <- discover_metadata(list(store = store, staff = staff,
                                customer = customer))
  jm <- map_join_paths(reg)
  expect_true(has_join(jm, "customer", "store"))
})

test_that("list-columns (BLOBs) are ignored instead of failing", {
  # DBI backends return BLOB columns as blob/vctrs list-columns, on which
  # unique()/anyDuplicated() error with "Unsupported type raw" (issue #1).
  parent <- data.table(parent_id = 1:3,
                       photo = list(as.raw(1:4), as.raw(5:8), as.raw(9:12)))
  child <- data.table(child_id = 1:6, parent_id = c(1, 2, 3, 1, 2, 3))
  expect_no_error(reg <- discover_metadata(list(parent = parent,
                                                child = child)))
  jm <- map_join_paths(reg)
  expect_true(has_join(jm, "child", "parent"))
  # the blob column takes part in nothing
  expect_false(any(vapply(reg$grouping_variable,
                          function(g) "photo" %in% g, logical(1))))
  expect_false(any(vapply(reg$identifier_columns,
                          function(g) "photo" %in% g, logical(1))))
})

test_that("keyless tables get their foreign keys as a composite grain", {
  customers <- data.table(customer_id = 1:4, region = c("N", "S", "E", "W"))
  products <- data.table(product_id = 1:3, price = c(10, 20, 30))
  sales <- data.table(customer_id = c(1, 2, 3, 4, 1),
                      product_id = c(1, 1, 2, 3, 2),
                      amount = c(5, 6, 7, 8, 9))          # no key of its own
  reg <- discover_metadata(list(customers = customers, products = products,
                                sales = sales))
  ids <- reg$identifier_columns[reg$table_name == "sales"][[1]]
  expect_setequal(ids, c("customer_id", "product_id"))
  jm <- map_join_paths(reg)
  expect_true(has_join(jm, "sales", "customers"))
  expect_true(has_join(jm, "sales", "products"))
})

test_that("tables with no identifier and no FKs are skipped with a message", {
  lonely <- data.table(text = c("a", "b", "b"))
  keyed <- data.table(thing_id = 1:3, v = 1:3)
  expect_message(
    reg <- discover_metadata(list(lonely = lonely, keyed = keyed)),
    "lonely")
  expect_false("lonely" %in% reg$table_name)
  expect_true("keyed" %in% reg$table_name)
})

test_that("the registry works end-to-end on the bundled example data", {
  reg <- discover_metadata(list(customers = customers, products = products,
                                transactions = transactions))
  expect_s3_class(reg, "data.table")
  expect_true(all(c("customers", "products", "transactions") %in%
                    reg$table_name))
  jm <- map_join_paths(reg)
  # product_id is fully contained in products (containment 1.0) -> discovered.
  expect_true(has_join(jm, "transactions", "products"))
  # customer_id is NOT discovered: only ~30% of transactions$customer_id
  # values exist in customers$customer_id in the bundled data, far below any
  # referential-integrity threshold. The discoverer treating that as
  # non-joinable is intended behavior, not a miss.
  expect_false(has_join(jm, "transactions", "customers"))
})
