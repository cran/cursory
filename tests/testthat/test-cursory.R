#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `cursory.R`')
#line 63 "R/cursory.R"
test_that('cursory_at.tbl', {#@testing
    requireNamespace('RSQLite')
    requireNamespace('DBI')
    requireNamespace('dbplyr')
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    pkgcond::suppress_warnings({
        .tbl <- group_by(copy_to(con, iris, 'iris', overwrite=TRUE), Species)
    }, "partial argument match")
    .vars <- setdiff(tbl_vars(.tbl), group_vars(.tbl))
    .funs <- lst(mean, sum)
    val <- cursory_at( .tbl, .vars, .funs, na.rm=TRUE)
    expect_is(val, 'tbl_sql')

    local.val <- collect(val)
    expect_is(local.val, 'tbl_df')
    expect_equal(dim(local.val), c(12L, 4L))
})
#line 100 "R/cursory.R"
test_that('cursory_at with function passed to .funs', {#@testing cursory_at with function passed to .funs
    val <- cursory_at(iris, 1:2, mean)
    expect_equal(names(val), c('Variable', 'mean'))

    val <- cursory_if(iris, is.numeric, mean)
    expect_equal(names(val), c('Variable', 'mean'))

    val <- cursory_all(select(iris, -Species), mean)
    expect_equal(names(val), c('Variable', 'mean'))
})
#line 120 "R/cursory.R"
test_that('cursory_at.grouped_df', {#@testing
    val <- cursory_all(group_by(iris, Species), lst(mean, sd))
    expect_equal(group_vars(val), 'Species')
    expect_equal(dim(val), c(12L, 4L))
    expect_equal( as.character(tbl_vars(val))
                , c('Species', 'Variable', 'mean', 'sd'))
})
#line 137 "R/cursory.R"
test_that('cursory_all.tbl', {#@testing
    .tbl <- group_by(as_tibble(iris), Species)
    .funs <- lst(Missing = . %>% is.na %>% sum(na.rm = TRUE)
                , mean, sd )

    val <- cursory_all(.tbl, .funs)
    expect_is(val, 'tbl_df')
    expect_identical( as.character(tbl_vars(val))
                    , c('Species', 'Variable'
                       , 'Missing', 'mean', 'sd'))
    expect_identical(dim(val), c(12L, 5L))

    expect_is(val, 'grouped_df')
    expect_equal(group_vars(val), 'Species')
})
#line 159 "R/cursory.R"
test_that('cursory_if.tbl_df', {#@testing
    val <- cursory_if(datasets::iris, is.numeric, lst(mean, sd))

    expect_is(val, 'tbl_df')
    expect_equal(dim(val), c(4L, 3L))
})
#line 166 "R/cursory.R"
test_that('cursory_if.grouped_df', {#@testing cursory_if.grouped_df
    val <- cursory_if(group_by(iris, Species), is.numeric, lst(mean, sd))
    expect_equal(group_vars(val), 'Species')
    expect_equal(dim(val), c(12L, 4L))
    expect_equal( as.character(tbl_vars(val))
                , c('Species', 'Variable', 'mean', 'sd'))
})
