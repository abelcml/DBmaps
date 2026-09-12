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

test_that("short parent keys link via an underscore boundary, not a bare suffix", {
  users <- data.table(uid = 1:5, uname = letters[1:5])
  # 'liquid' carries the same values as a real FK would, and its name happens
  # to end in "uid": only the underscore boundary separates it from user_uid.
  posts <- data.table(post_id = 1:8,
                      user_uid = c(1, 2, 3, 4, 5, 1, 2, 3),
                      liquid   = c(1, 2, 3, 4, 5, 1, 2, 3))
  reg <- discover_metadata(list(users = users, posts = posts))
  groups <- unlist(reg$grouping_variable[reg$table_name == "posts"])
  expect_true("user_uid" %in% groups)
  expect_false("liquid" %in% groups)
})

test_that("id detection covers prefix conventions without matching ordinary words", {
  # suffix forms (unchanged)
  expect_true(.dm_looks_like_id("id"))
  expect_true(.dm_looks_like_id("user_id"))
  expect_true(.dm_looks_like_id("userId"))
  # prefix forms: underscore, and camelCase on the original string
  expect_true(.dm_looks_like_id("id_user"))
  expect_true(.dm_looks_like_id("ID_USER"))
  expect_true(.dm_looks_like_id("idCustomer"))
  expect_true(.dm_looks_like_id("IdUser"))
  expect_true(.dm_looks_like_id("IDNumber"))
  # ordinary words beginning with "id" must not qualify
  expect_false(.dm_looks_like_id("identity"))
  expect_false(.dm_looks_like_id("Identity"))
  expect_false(.dm_looks_like_id("IDENTITY"))
  expect_false(.dm_looks_like_id("ideal"))
  expect_false(.dm_looks_like_id("idle"))
  expect_false(.dm_looks_like_id("idea"))
})

test_that("a table keyed with a prefix id is not skipped", {
  users <- data.table(id_user = 1:5, username = letters[1:5],
                      signup_date = as.Date("2026-01-01") + 1:5)
  reg <- discover_metadata(list(Users = users))
  expect_true("Users" %in% reg$table_name)
  expect_identical(reg$identifier_columns[reg$table_name == "Users"][[1]],
                   "id_user")
})

test_that("prefix-id foreign keys link across tables", {
  users <- data.table(id_user = 1:5, username = letters[1:5])
  posts <- data.table(id_post = 1:6, id_user = c(1, 2, 3, 1, 2, 3))
  reg <- discover_metadata(list(Users = users, Posts = posts))
  jm <- map_join_paths(reg)
  expect_true(has_join(jm, "Posts", "Users"))
})

test_that("irregular plural table names singularize correctly", {
  expect_equal(.dm_singularize("categories"), "category")
  expect_equal(.dm_singularize("countries"), "country")
  expect_equal(.dm_singularize("addresses"), "address")
  expect_equal(.dm_singularize("boxes"), "box")
  expect_equal(.dm_singularize("customers"), "customer")
  expect_equal(.dm_singularize("houses"), "house")
})

test_that("category_id binds to 'categories' even against a rival key", {
  # Both parents hold the same id values; only table-name affinity (through
  # correct singularization of 'categories') picks the right one.
  categories <- data.table(id = 1:4, label = letters[1:4])
  stores     <- data.table(id = 1:4, sname = letters[1:4])
  items <- data.table(item_id = 1:8, category_id = rep(1:4, 2))
  profiles <- lapply(list(categories = categories, stores = stores,
                          items = items), .dm_profile_table)
  joins <- .dm_discover_join_pairs(profiles, tau = 0.95, min_card = 2,
                                   alias_map = list())
  hit <- joins[joins$table_from == "items" & joins$col_from == "category_id", ]
  expect_identical(hit$table_to, "categories")
})

test_that("alias_map opens the name gate for semantic FK names", {
  # CamelCase column names, as real databases (Chinook, Northwind) have them:
  # alias matching must be case-insensitive like every other name comparison.
  employees <- data.table(EmployeeId = 1:5, ename = letters[1:5],
                          ReportsTo = c(NA, 1, 1, 2, 2))
  shippers <- data.table(ShipperId = 1:3, sname = c("a", "b", "c"))
  orders <- data.table(order_id = 1:6, ShipVia = c(1, 2, 3, 1, 2, 3))
  dl <- list(employees = employees, shippers = shippers, orders = orders)

  # without aliases these semantic names are (correctly) not linked
  reg0 <- discover_metadata(dl)
  expect_false("ShipVia" %in%
                 unlist(reg0$grouping_variable[reg0$table_name == "orders"]))

  # aliases map the semantic name to the actual parent key column name
  reg1 <- discover_metadata(dl, alias_map = list(reportsto = "employeeid",
                                                 shipvia = "shipperid"))
  expect_true("ShipVia" %in%
                unlist(reg1$grouping_variable[reg1$table_name == "orders"]))
  # includes the self-referential case
  expect_true("ReportsTo" %in%
                unlist(reg1$grouping_variable[reg1$table_name == "employees"]))
})

test_that("alias_map cannot force a join without value evidence", {
  things <- data.table(thing_id = 1:5, v = letters[1:5])
  other <- data.table(other_code = 100:105)   # values disjoint from thing_id
  reg <- suppressMessages(
    discover_metadata(list(things = things, other = other),
                      alias_map = list(other_code = "thing_id")))
  # the alias opens the name gate but containment still fails -> no link,
  # and 'other' (no identifier, no FK) is skipped entirely
  expect_false("other" %in% reg$table_name)
})

test_that("alias_map is validated", {
  dl <- list(a = data.table(a_id = 1:3))
  expect_error(discover_metadata(dl, alias_map = list("employeeid")),
               "alias_map")
  expect_error(discover_metadata(dl, alias_map = list(x = c("a", "b"))),
               "alias_map")
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
test_that("a standalone detail table gets its composite key detected", {
  order_details <- data.table(
    order_id   = c(1, 1, 2, 2, 3),
    product_id = c(10, 20, 10, 30, 20),
    quantity   = c(2, 5, 1, 1, 10)
  )
  reg <- discover_metadata(list(OrderDetails = order_details))
  expect_true("OrderDetails" %in% reg$table_name)
  expect_setequal(reg$identifier_columns[[1]], c("order_id", "product_id"))
})

test_that("a column that is unique on its own never forms a composite key", {
  # user_id already identifies a row, so (user_id, group_id) is a single key
  # with a passenger, not a composite key.
  users <- data.table(user_id = 1:5, group_id = c(1, 1, 2, 2, 3))
  reg <- discover_metadata(list(Users = users))
  expect_identical(reg$identifier_columns[[1]], "user_id")
})

test_that("a three-column key is found when no pair is unique", {
  d <- data.table(
    a_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    b_id = c(1, 1, 2, 2, 1, 1, 2, 2),
    c_id = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  reg <- discover_metadata(list(Cube = d))
  expect_setequal(reg$identifier_columns[[1]], c("a_id", "b_id", "c_id"))
})

test_that("the key returned is minimal, and max_key_cols caps the search", {
  d <- data.table(
    a_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    b_id = c(1, 1, 2, 2, 1, 1, 2, 2),
    c_id = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  # capped at pairs, no pair is unique, so nothing is found and the table is skipped
  expect_message(reg <- discover_metadata(list(Cube = d), max_key_cols = 2),
                 "Cube")
  expect_false("Cube" %in% reg$table_name)
})