#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `dontrepeat.R`')
#line 21 "R/dontrepeat.R"
test_that('format.dontrepeat', {#@testing
    x <- dontrepeat(c('a','a', 'b', 'b', 'b'))
    val <- format(x)
    expect_identical(val, c('a', '', 'b', '', ''))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x)
    expect_identical(val, c('a', '.', 'b', '.', '.'))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x, replace.with='-')
    expect_identical(val, c('a', '-', 'b', '-', '-'))

    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    val <- format(x, replace.with='-', width=5, justify='right')
    expect_identical(val, c('    a', '    -', '    b', '    -', '    -'))
})
#line 61 "R/dontrepeat.R"
test_that('dontrepeat in a tbl', {#@testing dontrepeat in a tbl
    x <- tibble::tibble( x = dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
                       , y = 1:5
                       )
    expect_is(x$x, 'dontrepeat')
    expect_is(head(x, 5)$x, 'dontrepeat')


    expect_equal( format(as.data.frame(x))$x
                , I(c('a', '.', 'b', '.', '.')))

    M <- as.matrix(x)
    expect_identical(M, cbind( x=c('a', '.', 'b', '.', '.')
                             , y=1:5))
})
