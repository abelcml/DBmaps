#' Automatically Discover Table Metadata from Data
#'
#' This function acts as an automated "scaffolding" step for the metadata
#' workflow. It inspects a named list of data.tables, detects each table's
#' identifier column(s) and the foreign-key relationships between tables from
#' the data itself, and returns a populated metadata registry. The result is
#' consumable by [map_join_paths()] unchanged, so users can start joining
#' immediately and then layer their analytical `key_outcome_specs` on top of
#' the generated scaffold with [table_info()] / [add_table()].
#'
#' @details
#' Detection combines two signals. Candidate parent keys are columns that are
#' unique, id-named (suffix forms `id`, `_id`, `<entity>Id`, or prefix forms
#' `id_<entity>` and `id<Entity>`), and have at least `min_card`
#' distinct values. A child column is linked to a parent key only if (a) its
#' name points at that parent (equal to the parent column, ending with it, or
#' matching the parent table's entity name, e.g. `customer_id` for table
#' `customers`) and (b) at least `tau` of its distinct non-missing values are
#' contained in the parent key's values. Names decide direction and
#' disambiguate between candidate parents; values decide joinability. Relying
#' on value containment alone is unreliable, because any dense surrogate key
#' `1..k` is fully contained in any larger surrogate key `1..m`.
#'
#' Tables whose primary key cannot be detected still receive a registry entry
#' when they contain discovered foreign-key columns: those columns are used as
#' the table's composite grain (mirroring the convention of describing a
#' transactions-style table by `c("customer_id", "product_id", ...)`). Tables
#' with neither a detectable identifier nor foreign keys are skipped with a
#' message. List-columns (e.g. BLOBs read from a database, which arrive as
#' `blob` class columns) are ignored during detection.
#'
#' The generated `key_outcome_specs` are intentionally minimal: one
#' `record_count` outcome (`ValueExpression = 1`) aggregated by each discovered
#' foreign-key column, so that [map_join_paths()] can recover the discovered
#' relationships from the registry alone. They are placeholders for the user's
#' real analytical definitions, not a substitute for them.
#'
#' @param data_list A named list of data.tables, one per table.
#' @param tau A number in (0, 1]; the minimum fraction of a child column's
#'   distinct values that must be contained in a parent key for the pair to be
#'   linked. Defaults to `0.95`, which tolerates a small share of orphan rows.
#' @param min_card A positive integer; the minimum number of distinct values a
#'   column needs to qualify as a parent key. Defaults to `2`.
#' @param alias_map An optional named list mapping semantically-named child
#'   columns to the parent key column they reference, e.g.
#'   `list(reportsto = "employeeid", shipvia = "shipperid")`. The alias value
#'   must be the parent key's actual column name; matching is case-insensitive
#'   on both sides (a `reportsto` alias covers a `ReportsTo` column). An alias only opens the
#'   name-link gate: value containment is still required, so a wrong alias
#'   cannot force a spurious join. This is the escape hatch for names the
#'   conventions above cannot cover (semantic names, irregular plurals).
#' @return A `MetadataRegistry` object (a data.table), as produced by
#'   [create_metadata_registry()] and [add_table()], with one set of rows per
#'   table for which metadata could be discovered.
#' @importFrom data.table data.table rbindlist as.data.table is.data.table
#' @export
#' @examples
#' # Discover metadata for the bundled example tables. 'transactions' has no
#' # primary key of its own; its discovered foreign keys become its grain.
#' registry <- discover_metadata(list(
#'   customers    = customers,
#'   products     = products,
#'   transactions = transactions
#' ))
#' registry
#'
#' # The scaffold is immediately consumable by map_join_paths(). Note that
#' # transactions -> products is discovered (every product_id exists in
#' # 'products'), while transactions -> customers is intentionally not: only
#' # ~30% of transactions$customer_id values exist in 'customers' in the
#' # bundled data, so joining on it would silently drop most rows.
#' map_join_paths(registry)
discover_metadata <- function(data_list, tau = 0.95, min_card = 2,
                              alias_map = list()) {
  if (!is.list(data_list) || is.null(names(data_list)) ||
      any(!nzchar(names(data_list)))) {
    stop("'data_list' must be a named list of data.tables.")
  }
  if (!all(vapply(data_list, data.table::is.data.table, logical(1)))) {
    stop("All elements in 'data_list' must be data.tables.")
  }
  if (!is.numeric(tau) || length(tau) != 1 || tau <= 0 || tau > 1) {
    stop("'tau' must be a single number in (0, 1].")
  }
  if (!is.numeric(min_card) || length(min_card) != 1 || min_card < 1) {
    stop("'min_card' must be a positive integer.")
  }
  if (!is.list(alias_map) ||
      (length(alias_map) > 0 && (is.null(names(alias_map)) ||
        any(!nzchar(names(alias_map))))) ||
      !all(vapply(alias_map, function(x)
        is.character(x) && length(x) == 1 && nzchar(x), logical(1)))) {
    stop("'alias_map' must be a named list of single character strings, ",
         "e.g. list(reportsto = \"employeeid\").")
  }

  profiles <- lapply(data_list, .dm_profile_table)
  joins <- .dm_discover_join_pairs(profiles, tau = tau, min_card = min_card,
                                   alias_map = alias_map)

  registry <- create_metadata_registry()
  skipped <- character(0)
  for (tbl in names(data_list)) {
    identifier <- .dm_pick_identifier(tbl, profiles[[tbl]])
    fk_cols <- unique(joins$col_from[joins$table_from == tbl])
    if (is.na(identifier)) {
      if (length(fk_cols) == 0) { skipped <- c(skipped, tbl); next }
      identifier <- fk_cols            # composite grain for keyless tables
    }
    grouping <- if (length(fk_cols) > 0) fk_cols else identifier[1]
    aggs <- lapply(grouping, function(g) list(
      AggregatedName = paste0("record_count_by_", g),
      AggregationFunction = "sum",
      GroupingVariables = g
    ))
    spec <- list(list(
      OutcomeName = "record_count",
      ValueExpression = 1,
      AggregationMethods = aggs
    ))
    registry <- add_table(
      registry,
      table_info(tbl, paste0("discovered:", tbl), identifier, spec)
    )
  }
  if (length(skipped) > 0) {
    message("No identifier or foreign keys discovered for: ",
            paste(skipped, collapse = ", "), " (skipped).")
  }
  registry
}

# Does this column name follow a primary/foreign key naming convention?
# Suffix forms: "id", "*_id", "<entity>Id". Prefix forms: "id_<entity>" and
# camelCase "id<Entity>". The prefix forms need a boundary, or ordinary words
# beginning with "id" (identity, ideal, idle) would qualify: the underscore
# supplies it in one case, and in the other the "ID" must be followed by an
# uppercase letter and then a lowercase one, which "IDENTITY" (all caps) and
# "Identity" (title case) both fail.
#' @noRd
.dm_looks_like_id <- function(col) {
  grepl("(^id$)|(_id$)|([a-z0-9]id$)|(^id_)", tolower(col)) ||
    grepl("^[Ii][Dd][A-Z][a-z]", col)
}

# Dictionary-free singularization of a table name, for matching child columns
# like category_id against a table named 'categories'. Handles the regular
# English patterns: ies -> y (categories), sibilant + es (addresses, boxes,
# dishes), plain s (customers, houses). Truly irregular names (people, criteria)
# are out of scope by design -- the alias_map argument is the escape hatch.
#' @noRd
.dm_singularize <- function(x) {
  x <- tolower(x)
  if (endsWith(x, "ies")) return(sub("ies$", "y", x))
  if (grepl("(ss|x|z|ch|sh)es$", x)) return(sub("es$", "", x))
  sub("s$", "", x)
}

# Profile one table: per column, the distinct non-missing values (as
# character), their count, and whether the column is unique. List-columns
# (e.g. BLOBs, which DBI backends return as 'blob' class columns) are marked
# unusable rather than profiled: they cannot serve as keys, and vctrs-backed
# classes error on unique()/anyDuplicated() for raw types.
#' @noRd
.dm_profile_table <- function(dt) {
  lapply(names(dt), function(cn) {
    v <- dt[[cn]]
    if (is.list(v)) {
      return(list(col = cn, set = character(0), n_distinct = 0L,
                  is_unique = FALSE))
    }
    v <- v[!is.na(v)]
    uv <- unique(as.character(v))
    list(col = cn, set = uv, n_distinct = length(uv),
         is_unique = length(uv) == length(v) && length(v) > 0)
  })
}

# Does the child column NAME point at this parent key? This is what separates
# a real foreign key (track.album_id -> album.album_id) from a surrogate-key
# coincidence (album.album_id value-contained in track.track_id). The parent-
# table form ("customerid" ~ table "customers") is only valid across tables:
# within a table it would link the table's own key to its sibling id columns.
#' @noRd
.dm_name_links_to_parent <- function(child_col, parent_col, parent_table,
                                     same_table) {
  cc <- tolower(child_col)
  pc <- tolower(parent_col)
  if (cc == pc) return(TRUE)
  # A bare suffix match needs length to be safe ('liquid' must not hit a key
  # named 'uid'); an underscore boundary makes short keys safe ('user_uid'
  # ends with '_uid', 'liquid' does not).
  if (nchar(pc) >= 4 && endsWith(cc, pc)) return(TRUE)
  if (nchar(pc) >= 2 && endsWith(cc, paste0("_", pc))) return(TRUE)
  if (same_table) return(FALSE)
  ent <- .dm_singularize(parent_table)
  forms <- c(paste0(ent, "id"), paste0(ent, "_id"), paste0("id_", ent))
  if (cc %in% forms) return(TRUE)
  any(nchar(forms) >= 5 & vapply(forms, function(f) endsWith(cc, f), logical(1)))
}

# Discover directed foreign-key pairs across (and within) the profiled tables.
# Returns data.table(table_from, col_from, table_to, col_to); table_from is
# the child (FK side), table_to the parent (key side). One best parent is kept
# per child column, ranked by containment, table-name affinity, and exact
# column-name match.
#' @noRd
.dm_discover_join_pairs <- function(profiles, tau, min_card,
                                    alias_map = list()) {
  empty <- data.table::data.table(
    table_from = character(), col_from = character(),
    table_to = character(), col_to = character())
  tn <- names(profiles)

  pkeys <- list()
  for (t in tn) for (p in profiles[[t]]) {
    if (p$is_unique && p$n_distinct >= min_card && .dm_looks_like_id(p$col)) {
      pkeys[[length(pkeys) + 1]] <- list(table = t, col = p$col, set = p$set)
    }
  }
  if (length(pkeys) == 0) return(empty)

  best <- list()
  for (ct in tn) for (cc in profiles[[ct]]) {
    if (cc$n_distinct < 1) next
    # an alias substitutes the child's *effective* name for the name gate and
    # scoring; value containment below is never bypassed. Lookup is
    # case-insensitive, like every other name comparison here.
    ai <- match(tolower(cc$col), tolower(names(alias_map)))
    eff_col <- if (!is.na(ai)) alias_map[[ai]] else cc$col
    cc_l <- tolower(eff_col)
    for (pk in pkeys) {
      same_tbl <- pk$table == ct
      if (same_tbl && pk$col == cc$col) next
      if (!.dm_name_links_to_parent(eff_col, pk$col, pk$table, same_tbl)) next
      inter <- length(intersect(cc$set, pk$set))
      exact <- identical(cc_l, tolower(pk$col))
      if (inter < (if (exact) 1 else 2)) next
      containment <- inter / cc$n_distinct
      if (containment < tau) next
      ent <- .dm_singularize(pk$table)
      base <- sub("_?id$", "", cc_l)
      affinity <- (base == ent) || (nchar(ent) >= 3 && startsWith(cc_l, ent))
      score <- containment + 0.30 * affinity + 0.05 * exact
      key <- paste(ct, cc$col, sep = "\r")
      if (is.null(best[[key]]) || score > best[[key]]$score) {
        best[[key]] <- list(table_from = ct, col_from = cc$col,
                            table_to = pk$table, col_to = pk$col,
                            score = score)
      }
    }
  }
  if (length(best) == 0) return(empty)
  data.table::rbindlist(lapply(best, function(b) data.table::data.table(
    table_from = b$table_from, col_from = b$col_from,
    table_to = b$table_to, col_to = b$col_to)))
}

# Choose a table's identifier column: a unique, id-named column, preferring
# one named after the table itself (customers -> customer_id / customerid /
# id), breaking remaining ties by cardinality.
#' @noRd
.dm_pick_identifier <- function(tbl, profile) {
  cands <- Filter(function(p) p$is_unique && p$n_distinct >= 1 &&
                    .dm_looks_like_id(p$col), profile)
  if (length(cands) == 0) return(NA_character_)
  ent <- .dm_singularize(tbl)
  score <- function(p) {
    cl <- tolower(p$col)
    3 * (cl == paste0(ent, "id")) + 3 * (cl == paste0(ent, "_id")) +
      3 * (cl == paste0("id_", ent)) +
      2 * (cl == "id") + 1 * startsWith(cl, ent) + 1e-9 * p$n_distinct
  }
  vapply(cands, function(p) p$col, character(1))[
    which.max(vapply(cands, score, numeric(1)))]
}
