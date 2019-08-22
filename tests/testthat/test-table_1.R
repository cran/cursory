#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `table_1.R`')
#line 53 "R/table_1.R"
test_that('table_1', {#@testing
    val <- table_1( iris, Species
                  , vars('Petal Length'=Petal.Length, Petal.Width)
                  )
    expect_is(val, 'tbl')
    expect_equal(names(val), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))


    val <- table_1( iris, tools::toTitleCase(as.character(Species))
                  , vars(everything()))
    expect_is(val, 'tbl')
    expect_equal(names(val), c('Variable', 'Level', '(All)', 'Setosa', 'Versicolor', 'Virginica'))
    expect_true(all(c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width') %in% val$Variable))
})
#line 88 "R/table_1.R"
test_that('#', {#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    var <- rlang::expr(toupper(Species))
    name <- 'SPECIES'
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, var, 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
})
#line 106 "R/table_1.R"
test_that('#', {#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, as.name('Species'), 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
})
#line 120 "R/table_1.R"
test_that('table_1_dispatcher.character', {#@testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    result <- table_1_dispatcher( .data
                                , 'Species'
                                , 'SPECIES'
                                , rlang::parse_quo('Size', env=globalenv())
                                )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
    expect_equal(unique(result$Variable), 'SPECIES')
})
#line 166 "R/table_1.R"
test_that('#', {#@Testing
    result <- iris %>%
        dplyr::mutate(Big = Sepal.Length > median(Sepal.Length)) %>%
        table_1_summarise.logical( var  = rlang::as_quosure(as.name('Big'), environment())
                                 , name = 'Is Big?'
                                 , rlang::parse_quo('Species', env=globalenv())
                                 )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 1)

    result2 <- iris %>%
        dplyr::mutate(Big = Sepal.Length > median(Sepal.Length)) %>%
        table_1_summarise( var  = rlang::as_quosure(as.name('Big'), environment())
                         , name = 'Is Big?'
                         , rlang::parse_quo('Species', env=globalenv())
                         )

    expect_identical(result, result2)

    result3 <- iris %>%
        dplyr::mutate(Big = Sepal.Length > median(Sepal.Length)) %>%
        table_1_summarise.logical( var  = rlang::as_quosure(as.name('Big'), environment())
                                 , name = 'Is Big?'
                                 , rlang::parse_quo('Species', env=globalenv())
                                 , all.name='A'
                                 , var.name='V'
                                 , level.name='L'
                                 )
    expect_is(result3, 'tbl')
    expect_equal(names(result3), c('V', 'L', 'A', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result3), 1)
})
#line 222 "R/table_1.R"
test_that('#', {#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    result <- table_1_summarise.character( .data
                                         , var = quo(Species)
                                         , name='SPECIES'
                                         , rlang::parse_quo('Size', env=globalenv())
                                         )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(result$Level, factor(c('setosa', 'versicolor', 'virginica')))
    expect_equal(nrow(result), 3)

    result <- table_1_summarise.character( .data
                                         , var = quo(Species)
                                         , name='SPECIES'
                                         , rlang::parse_quo('Size', env=globalenv())
                                         , all.name = 'Total'
                                         , var.name = 'Measure'
                                         , level.name = 'Type'
                                         )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Measure', 'Type', 'Total', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
})
#line 253 "R/table_1.R"
test_that('#', {#@Testing
    .data <- dplyr::mutate(iris, Size = cut(Sepal.Length, c(-Inf, 5, 6.4, Inf), c('Small', 'Medium', 'Large'))
                                    %>% ordered(c('Small', 'Medium', 'Large')))
    expect_true(is.factor(.data$Size))
    result <- table_1_summarise.character( .data
                                         , var = quo(Size)
                                         , name='SPECIES'
                                         , rlang::parse_quo('Species', env=globalenv())
                                         )
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 3)
    expect_equal(result$Level, forcats::fct_inorder(result$Level))
})
#line 318 "R/table_1.R"
test_that('table_1_summarise.numeric', {#@testing
    result <- table_1_summarise.numeric(iris, rlang::quo(Petal.Length), 'Petal Length', rlang::quo(Species))
    expect_is(result , 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 5)
    expect_equal(result$Level, c('Min', 'Median', 'Mean', 'Max', 'SD'))

    result <- table_1_summarise.numeric( iris, rlang::quo(Petal.Length), 'Petal Length', rlang::quo(Species)
                                       , all.name='Total', var.name="Descriptor", level.name='Metric'
                                       , value.handler=as.list
                                       )
    expect_is(result , 'tbl')
    expect_equal(names(result), c('Descriptor', 'Metric', 'Total', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 5)
    expect_is(result$setosa, 'list')
    expect_is(result$versicolor, 'list')
    expect_is(result$virginica, 'list')

    result <- table_1_summarise.numeric( iris, rlang::quo(Petal.Length), 'Petal Length', rlang::quo(Species)
                                       , all.name='ALL', var.name="Measure", level.name='Summary'
                                       , value.handler='format')

    expect_equal(names(result), c('Measure', 'Summary', 'ALL', 'setosa', 'versicolor', 'virginica'))
    expect_equal(nrow(result), 5)
    expect_is(result$setosa, 'character')
    expect_is(result$versicolor, 'character')
    expect_is(result$virginica, 'character')
})
