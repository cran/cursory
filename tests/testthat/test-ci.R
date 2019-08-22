#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `ci.R`')
#line 40 "R/ci.R"
test_that('ci', {#@testing
    # taken from confint example
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)

    val <- ci(coef(fit), bounds[,1], bounds[,2])
    expect_is(val, 'list<confidence-interval>')
    expect_true(is.list(val))
    testextra::expect_all_inherit(val, 'confidence-interval')
})
#line 73 "R/ci.R"
test_that('`format.confidence-interval`', {#@testing
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    x <- ci(coef(fit), bounds[,1], bounds[,2])
    format(x[[1]])

    val <- format(x, digits=2, span=',')

    expect_is(val, 'AsIs')
    expect_match(val, "( |-|)(\\d+(\\.\\d+)?) \\(( |-|)(\\d+\\.\\d+),( |-|)(\\d+\\.\\d+)\\)")


    val <- format(x, width=50, span=',')
    expect_true(all(nchar(val)==50))
})
#line 110 "R/ci.R"
test_that('`c.list<confidence-interval>`', {#@testing
    a <- ci(0, -1, 1)
    b <- ci(0, -2, 2)

    val <-c(a,b)
    expect_is(val, 'list<confidence-interval>')
    expect_length(val, 2)
})
#line 118 "R/ci.R"
test_that('confidence intervals in grouped data frame operations.', {#@testing confidence intervals in grouped data frame operations.
    fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
    bounds <- confint(fit)
    df <- tibble( variable = names(coef(fit))
                , estimate = coef(fit)
                , lower = bounds[,1]
                , upper = bounds[,1]
                )
    expect_silent(val2 <- group_by(df, variable) %>% mutate(ci=ci(estimate, lower, upper)))
    expect_is(val2$ci, 'list<confidence-interval>')
    expect_length(val2$ci, 5)
})
