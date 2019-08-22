#' @importFrom tidymargins with_margins
#' @importFrom pkgcond assert_that pkg_error
#' @importFrom rlang quos
NULL
utils::globalVariables('.')

#' Create a Dataset Summary Table (I.E. Table 1)
#'
#' This helps creates demographics or summary table for a dataset,
#' the eponymous "table 1".
#' Given a data set a key for columns,
#' describe the differences across the provided
#' factor variables between the levels of key.
#'
#' `table_1_summarise` and `table_1_dispatcher` dispatch on the data type of the
#' variable identified by `var` in `.data`
#'
#' @param .data a dataset
#' @param key the comparison variable, such as case/control.
#' @param .vars a lazy list of variables to include in the description.
#' @param ... passed on to other methods.
#'
#' @return
#' The result is a [table][tibble::tibble()] which includes:
#'
#' * A 'Variable' column, can be renamed with the `var.name` argument
#'
#'
#' @examples
#' table_1(iris, Species)
#' table_1(CO2, Plant)
#'
#' @export
table_1 <-
function( .data, key, .vars = vars(everything())
        , ...
        ){
    key <- rlang::enquo(key)
    kv <- group_by_prepare(.data, .dots = list(Key=key))

    vars <- tidyselect::vars_select( tbl_vars(.data), !!!.vars
                                   , .exclude=c("Key", all.names(key))
                                   )
    vv <- group_by_prepare(kv$data, .dots=vars)
    purrr::map2( vv$groups
               , vv$group_names
               , table_1_dispatcher
               , key = kv$groups[[1]], .data = vv$data
               ) %>%
        purrr::reduce(dplyr::union_all) %>%
        mutate_at("Variable", dontrepeat)
}
if(FALSE){#@testing
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
}

#' @rdname table_1
#' @param var  Variable identifier, used to dispatch
#' @param name name of the variable
#' @export
table_1_dispatcher <- function(.data, var, name, key)
    UseMethod("table_1_dispatcher", var)

#' @method table_1_dispatcher quosure
#' @export
table_1_dispatcher.quosure <- function(.data, var, name, key){
    UseMethod("table_1_dispatcher", rlang::quo_squash(var))
}

#' @method table_1_dispatcher call
#' @export
#' @importFrom rlang :=
table_1_dispatcher.call <- function(.data, var, name, ...){
    test <- .data  %>% utils::head() %>% transmute(!!name := !!var) %>% pull(!!name)
    UseMethod("table_1_summarise", test)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    var <- rlang::expr(toupper(Species))
    name <- 'SPECIES'
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, var, 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
}


#' @method table_1_dispatcher name
#' @export
table_1_dispatcher.name <- function(.data, var, name, ...){
    test <- .data  %>% utils::head() %>% pull(!!var)
    UseMethod("table_1_summarise", test)
}
if(FALSE){#@Testing
    .data <- dplyr::mutate(iris, Size = ifelse(Sepal.Length > median(Sepal.Length), 'Big', 'Little'))
    key  <- rlang::parse_quo('Size', env=globalenv())
    result <- table_1_dispatcher(.data, as.name('Species'), 'SPECIES', key)
    expect_is(result, 'tbl')
    expect_equal(names(result), c('Variable', 'Level', '(All)', 'Big', 'Little'))
    expect_equal(nrow(result), 3)
}

#' @method table_1_dispatcher character
#' @export
table_1_dispatcher.character <- function(.data, var, name, ...){
    methods::callGeneric(.data, as.name(var), name=name,...)
}
if(FALSE){#@testing
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
}

#' @rdname table_1
#' @export
table_1_summarise <-
    function(.data, var, name, key)
        UseMethod("table_1_dispatcher", var)

#' @rdname table_1
#' @export
table_1_summarize <- table_1_summarise

#' @method table_1_summarise logical
#' @export
table_1_summarise.logical <-
function(.data, var, name, key
        , all.name = "(All)"
        , var.name = 'Variable'
        , level.name = 'Level'
        , ...
        , logical.true = 'Yes'
        ){
    n <- pct <- NULL
    .data  %>%
        dplyr::group_by(!!key, add=TRUE) %>%
        with_margins(summarise, all.name=all.name)( !!var.name := !!name
                    , !!level.name := !!logical.true
                    , n        = sum(as.integer(!!var))
                    , pct      = mean(as.numeric(!!var))
                    ) %>%
        mutate(VALUE=Npct(n,pct)) %>%
        select(!!var.name, !!level.name, !!key, 'VALUE') %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::select(!!var.name, !!level.name, !!all.name, tidyselect::everything())
}
if(FALSE){#@Testing
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
}

#' @method table_1_summarise character
#' @export
table_1_summarise.character <-
function( .data, var, name, key
        , all.name = "(All)"
        , var.name = 'Variable'
        , level.name = 'Level'
        , ...
        ){
    pct <- rlang::sym('._PERCENT_.')
    .data %>%
        dplyr::group_by(!!key, add=TRUE) %>%
        with_margins(dplyr::count, all.name=all.name)(!!level.name := !!var) %>%
        dplyr::group_by(!!var.name := !!name, !!key, add=FALSE) %>%
        dplyr::mutate( !!pct   := as.numeric(n)/sum(as.numeric(n), na.rm=TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(!!var.name, !!level.name, !!key, 'n', !!pct) %>%
        dplyr::mutate(VALUE = Npct(n, !!pct)) %>%
        dplyr::select(!!var.name, !!level.name, !!key, 'VALUE') %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::select(!!var.name, !!level.name, !!all.name, tidyselect::everything())
}
if(FALSE){#@Testing
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
}

#' @method table_1_summarise factor
#' @export
table_1_summarise.factor <- function(.data, var, ...){
    .data <- mutate_at(.data, dplyr::vars(!!var), as.character)
    table_1_summarise.character(.data, var=var, ...)
}
if(FALSE){#@Testing
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
}


default.numeric.summary.funs <-
    purrr::map(.f = rlang::as_function, env=baseenv(),
            tibble::lst( Min    = ~min(., na.rm=TRUE)
                       , Median = ~dplyr::nth(.,  n() %/% 2L, .)
                       , Mean   = ~mean(., na.rm=TRUE)
                       , Max    = ~max(., na.rm=TRUE)
                       , SD     = ~stats::sd(., na.rm=TRUE)
                       ))

#' @method table_1_summarise numeric
#' @export
table_1_summarise.numeric <-
function(.data, var, name, key
        , all.name = "(All)"
        , var.name = 'Variable'
        , level.name = 'Level'
        , ...
        , value.handler = format
        , numeric.summary.funs
        ){
    if (!missing(value.handler))
        value.handler <- rlang::as_function(value.handler)

    Variable <- Level <- NULL
    if (missing(numeric.summary.funs)) {
        numeric.summary.funs <- default.numeric.summary.funs
    } else {
        assert_that( is.list(numeric.summary.funs)
                   , rlang::is_named(numeric.summary.funs)
                   )
        if (!all(map_lgl(numeric.summary.funs, rlang::is_function)))
            numeric.summary.funs <- map(numeric.summary.funs, rlang::as_function)
    }

    .data  %>%
        dplyr::group_by( !!key) %>%
        with_margins(dplyr::summarise_at, all.name = all.name)(.data = .
            , .vars = dplyr::vars(!!var)
            , .funs = numeric.summary.funs
            ) %>%
        mutate_at(names(numeric.summary.funs), value.handler) %>%
        tidyr::gather( !!level.name, 'VALUE', !!!names(numeric.summary.funs)) %>%
        dplyr::mutate_at(level.name, ordered, levels=names(numeric.summary.funs)) %>%
        dplyr::mutate( !!var.name := !!name) %>%
        tidyr::spread(!!key, 'VALUE') %>%
        dplyr::select(!!var.name, !!level.name, !!all.name, tidyselect::everything()) %>%
        dplyr::arrange_at(1:2) %>%
        dplyr::mutate_at(level.name, as.character)
}
if(FALSE){#@testing
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
}

