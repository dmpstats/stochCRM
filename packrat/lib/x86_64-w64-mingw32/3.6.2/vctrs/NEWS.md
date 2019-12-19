
# vctrs 0.2.0

With the 0.2.0 release, many vctrs functions have been rewritten with
native C code to improve performance. Functions like `vec_c()` and
`vec_rbind()` should now be fast enough to be used in packages. This
is an ongoing effort, for instance the handling of factors and dates
has not been rewritten yet. These classes still slow down vctrs
primitives.

The API in 0.2.0 has been updated, please see a list of breaking
changes below. vctrs has now graduated from experimental to a maturing
package (see the [lifecycle of tidyverse packages](https://www.tidyverse.org/lifecycle/)).
Please note that API changes are still planned for future releases,
for instance `vec_ptype2()` and `vec_cast()` might need to return a
sentinel instead of failing with an error when there is no common type
or possible cast.


## Breaking changes

* Lossy casts now throw errors of type `vctrs_error_cast_lossy`.
  Previously these were warnings. You can suppress these errors
  selectively with `allow_lossy_cast()` to get the partial cast
  results. To implement your own lossy cast operation, call the new
  exported function `maybe_lossy_cast()`.

* `vec_c()` now fails when an input is supplied with a name but has
  internal names or is length > 1:

  ```
  vec_c(foo = c(a = 1))
  #> Error: Can't merge the outer name `foo` with a named vector.
  #> Please supply a `.name_spec` specification.

  vec_c(foo = 1:3)
  #> Error: Can't merge the outer name `foo` with a vector of length > 1.
  #> Please supply a `.name_spec` specification.
  ```

  You can supply a name specification that describes how to combine
  the external name of the input with its internal names or positions:

  ```
  # Name spec as glue string:
  vec_c(foo = c(a = 1), .name_spec = "{outer}_{inner}")

  # Name spec as a function:
  vec_c(foo = c(a = 1), .name_spec = function(outer, inner) paste(outer, inner, sep = "_"))
  vec_c(foo = c(a = 1), .name_spec = ~ paste(.x, .y, sep = "_"))
  ```

* `vec_empty()` has been renamed to `vec_is_empty()`.

* `vec_dim()` and `vec_dims()` are no longer exported.

* `vec_na()` has been renamed to `vec_init()`, as the primary use case
  is to initialize an output container.

* `vec_slice<-` is now type stable (#140). It always returns the same
  type as the LHS. If needed, the RHS is cast to the correct type, but
  only if both inputs are coercible. See examples in `?vec_slice`.

* We have renamed the `type` particle to `ptype`:

  - `vec_type()` => `vec_ptype()`
  - `vec_type2()` => `vec_ptype2()`
  - `vec_type_common()` => `vec_ptype_common()`

  Consequently, `vec_ptype()` was renamed to `vec_ptype_show()`.


## New features

* New `vec_proxy()` generic. This is the main customisation point in
  vctrs along with `vec_restore()`. You should only implement it when
  your type is designed around a non-vector class (atomic vectors,
  bare lists, data frames). In this case, `vec_proxy()` should return
  such a vector class. The vctrs operations will be applied on the
  proxy and `vec_restore()` is called to restore the original
  representation of your type.

  The most common case where you need to implement `vec_proxy()` is
  for S3 lists. In vctrs, S3 lists are treated as scalars by
  default. This way we don't treat objects like model fits as
  vectors. To prevent vctrs from treating your S3 list as a scalar,
  unclass it from the `vec_proxy()` method. For instance here is the
  definition for `list_of`:

  ```
  #' @export
  vec_proxy.vctrs_list_of <- function(x) {
    unclass(x)
  }
  ```

  If you inherit from `vctrs_vctr` or `vctrs_rcrd` you don't need to
  implement `vec_proxy()`.

* `vec_c()`, `vec_rbind()`, and `vec_cbind()` gain a `.name_repair`
  argument (#227, #229).

* `vec_c()`, `vec_rbind()`, `vec_cbind()`, and all functions relying
  on `vec_ptype_common()` now have more informative error messages
  when some of the inputs have nested data frames that are not
  convergent:

  ```
  df1 <- tibble(foo = tibble(bar = tibble(x = 1:3, y = letters[1:3])))
  df2 <- tibble(foo = tibble(bar = tibble(x = 1:3, y = 4:6)))

  vec_rbind(df1, df2)
  #> Error: No common type for `..1$foo$bar$y` <character> and `..2$foo$bar$y` <integer>.
  ```

* `vec_cbind()` now turns named data frames to packed columns.

  ```r
  data <- tibble::tibble(x = 1:3, y = letters[1:3])
  data <- vec_cbind(data, packed = data)
  data
  # A tibble: 3 x 3
        x y     packed$x $y
    <int> <chr>    <int> <chr>
  1     1 a            1 a
  2     2 b            2 b
  3     3 c            3 c
  ```

  Packed data frames are nested in a single column. This makes it
  possible to access it through a single name:

  ```r
  data$packed
  # A tibble: 3 x 2
        x y
    <int> <chr>
  1     1 a
  2     2 b
  3     3 c
  ```

  We are planning to use this syntax more widely in the tidyverse.

* New `vec_is()` function to check whether a vector conforms to a
  prototype and/or a size. Unlike `vec_assert()`, it doesn't throw
  errors but returns `TRUE` or `FALSE` (#79).

  Called without a specific type or size, `vec_assert()` tests whether
  an object is a data vector or a scalar. S3 lists are treated as
  scalars by default. Implement a `vec_is_vector()` for your class to
  override this property (or derive from `vctrs_vctr`).

* New `vec_order()` and `vec_sort()` for ordering and sorting
  generalised vectors.

* New `.names_to` parameter for `vec_rbind()`. If supplied, this
  should be the name of a column where the names of the inputs are
  copied. This is similar to the `.id` parameter of
  `dplyr::bind_rows()`.

* New `vec_seq_along()` and `vec_init_along()` create useful sequences (#189).

* `vec_slice()` now preserves character row names, if present.

* New `vec_split(x, by)` is a generalisation of `split()` that can divide
  a vector into groups formed by the unique values of another vector. Returns
  a two-column data frame containing unique values of `by` aligned with
  matching `x` values (#196).


## Other features and bug fixes

* Using classed errors of class `"vctrs_error_assert"` for failed
  assertions, and of class `"vctrs_error_incompatible"` (with
  subclasses `_type`, `_cast` and `_op`) for errors on incompatible
  types (#184).

* Character indexing is now only supported for named objects, an error
  is raised for unnamed objects (#171).

* Predicate generics now consistently return logical vectors when
  passed a `vctrs_vctr` class. They used to restore the output to
  their input type (#251).

* `list_of()` now has an `as.character()` method. It uses
  `vec_ptype_abbr()` to collapse complex objects into their type
  representation (tidyverse/tidyr#654).

* New `stop_incompatible_size()` to signal a failure due to mismatched sizes.

* New `validate_list_of()` (#193).

* `vec_arith()` is consistent with base R when combining `difftime`
  and `date`, with a warning if casts are lossy (#192).

* `vec_c()` and `vec_rbind()` now handle data.frame columns properly
  (@yutannihilation, #182).

* `vec_cast(x, data.frame())` preserves the number of rows in `x`.

* `vec_equal()` now handles missing values symmetrically (#204).

* `vec_equal_na()` now returns `TRUE` for data frames and records when
  every component is missing, not when _any_ component is missing
  (#201).

* `vec_init()` checks input is a vector.

* `vec_proxy_compare()` gains an experimental `relax` argument, which
  allows data frames to be orderable even if all their columns are not
  (#210).

* `vec_size()` now works with positive short row names. This fixes
  issues with data frames created with jsonlite (#220).

* `vec_slice<-` now has a `vec_assign()` alias. Use `vec_assign()`
  when you don't want to modify the original input.

* `vec_slice()` now calls `vec_restore()` automatically. Unlike the
  default `[` method from base R, attributes are preserved by default.

* `vec_slice()` can correct slice 0-row data frames (#179).

* New `vec_repeat()` for repeating each element of a vector the same number
  of times.

* `vec_type2(x, data.frame())` ensures that the returned object has
  names that are a length-0 character vector.
