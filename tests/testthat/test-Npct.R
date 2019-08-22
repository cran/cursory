#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `Npct.R`')
#line 22 "R/Npct.R"
test_that('setGeneric("Npct", ...)', {#@testing
    expect_error(Npct("not", "valid"))
})
#line 35 "R/Npct.R"
test_that('Npct,logical,missing-method', {#@testing
    val <- Npct(c(T, F, T))
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
})
#line 50 "R/Npct.R"
test_that('Npct,logical,logical-method', {#@testing
    val <- Npct( c(T,F,T,F,T)
               , c(T,T,T,F,F)
               )
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
})
#line 70 "R/Npct.R"
test_that('Npct,integer,integer-method', {#@testing
    val <- Npct(2L, 3L)
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
})
#line 90 "R/Npct.R"
test_that('Npct,numeric,numeric-method', {#@testing
    val <- Npct( 2, 2/3)
    expect_equal(as.character(val), '2 (66.67%)')
    expect_identical(attr(val, 'count'), 2L)
    expect_identical(attr(val, 'percent'), 2/3)
})
