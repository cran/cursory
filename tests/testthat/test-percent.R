#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `percent.R`')
#line 27 "R/percent.R"
test_that('percent', {#@testing
    val <- percent(1/3)
    expect_is(val, 'percent')
    expect_true(is.character(val))
    expect_equal(as.character(val), "33.33%")
    expect_identical(attr(val, 'raw'), 1/3)
})
#line 50 "R/percent.R"
test_that('pct', {#@testing
    val <- pct(1/3, places=3)
    expect_equal(val, "33.333%")

    val <- pct(0.009, places=2)
    expect_equal(val, "< 0.01%")
})
#line 68 "R/percent.R"
test_that('format.percent', {#@testing
    val <- format.percent(1/3)
    expect_identical(val, "33.33%")
})
#line 82 "R/percent.R"
test_that('as.double.percent', {#@testing
    x <- percent(2/3)

    expect_identical(as.numeric(x), 2/3)
    expect_identical(as.double(x), 2/3)
})
