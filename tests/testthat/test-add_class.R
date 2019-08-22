#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `add_class.R`')
#line 30 "R/add_class.R"
test_that('add_class', {#@testing
    expect_is(add_class(1, 'test'), 'test')

    val <- add_class(add_class(1, 'class1'), 'class2')
    expect_is(val, 'class2')
    expect_is(val, 'class1')
    expect_is_not(val, 'class3')
})
#line 42 "R/add_class.R"
test_that('set_class', {#@testing
    expect_is(set_class(1, 'test'), 'test')

    val <- set_class(set_class(1, 'class1'), 'class2')
    expect_is(val, 'class2')
    expect_is_not(val, 'class1')
    expect_is_not(val, 'class3')
})
#line 54 "R/add_class.R"
test_that('add_comment', {#@testing
    val <- add_comment(list(), "a test comment")
    expect_equal(comment(val), "a test comment")

    val <- add_comment(val, "another comment")
    expect_equal(comment(val), c("a test comment", "another comment"))
})
#line 65 "R/add_class.R"
test_that('set_comment', {#@testing
    val <- set_comment(list(), "a test comment")
    expect_equal(comment(val), "a test comment")

    val <- set_comment(val, "another comment")
    expect_equal(comment(val), "another comment")
})
#line 91 "R/add_class.R"
test_that('carry_forward', {#@testing
    x <- dontrepeat(c('a','a', 'b', 'b', 'b'), '.')
    y <- carry_forward(factor(c('c', 'd', 'd')), x)
    expect_identical(attributes(x), attributes(y))

    z <- carry_forward(factor(c(1L, 2L, 2L)), x)
    expect_identical(attributes(x), attributes(z))
})
