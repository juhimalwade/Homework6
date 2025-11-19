#---------------------------------------------------------------------
#' Sparse Numeric Vector (S4 Class)
#'
#' An S4 class for representing sparse numeric vectors, storing only
#' non-zero values and their positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of 1-based indices of non-zero values.
#' @slot length Integer giving the full length of the represented vector.
#'
#' @name sparse_numeric-class
#' @docType class
#'
#' @importFrom methods setClass setMethod setGeneric new as
NULL

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' @describeIn sparse_numeric-class Validity check
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos))
    return("Lengths mismatch")
  if (any(object@pos <= 0))
    return("Positions must be >= 1")
  if (any(duplicated(object@pos)))
    return("Duplicate positions not allowed")
  if (max(object@pos, 0) > object@length)
    return("Position exceeds vector length")
  TRUE
})

#---------------------------------------------------------------------
# COERCION

#' Coercion to/from sparse_numeric
#'
#' @param from Object to convert.
#' @return A `sparse_numeric` object or numeric vector.
#'
#' @name sparse_numeric-coerce
#' @aliases coerce,numeric,sparse_numeric-method
#' @aliases coerce,sparse_numeric,numeric-method
NULL

#' @rdname sparse_numeric-coerce
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = from[nz],
      pos = as.integer(nz),
      length = as.integer(length(from)))
})

#' @rdname sparse_numeric-coerce
setAs("sparse_numeric", "numeric", function(from) {
  x <- numeric(from@length)
  if (length(from@pos)) x[from@pos] <- from@value
  x
})

#---------------------------------------------------------------------
# as.numeric METHOD

#' Numeric Coercion for sparse_numeric
#'
#' @param x A `sparse_numeric` object.
#' @param ... Ignored.
#'
#' @name as.numeric-sparse
#' @aliases as.numeric,sparse_numeric-method
NULL

#' @rdname as.numeric-sparse
setMethod("as.numeric", "sparse_numeric",
          function(x, ...) as(x, "numeric"))

#---------------------------------------------------------------------
# GENERICS FOR SPARSE OPS

#' Sparse Numeric Operations
#'
#' Arithmetic and utility operations for `sparse_numeric` objects.
#'
#' @param x A `sparse_numeric` operand.
#' @param y A `sparse_numeric` operand.
#' @param ... Additional arguments (unused).
#'
#' @name sparse_ops
NULL

#' @rdname sparse_ops
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_ops
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_ops
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_ops
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_ops
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#---------------------------------------------------------------------
# ADD

#' Add Two Sparse Numeric Vectors
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Additional arguments (unused).
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_add-method
#' @aliases sparse_add,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_add-method
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must be same length.")
            pos_all <- sort(unique(c(x@pos, y@pos)))
            vals <- numeric(length(pos_all))
            if (length(x@value))
              vals[match(x@pos, pos_all)] <- vals[match(x@pos, pos_all)] + x@value
            if (length(y@value))
              vals[match(y@pos, pos_all)] <- vals[match(y@pos, pos_all)] + y@value
            keep <- vals != 0
            new("sparse_numeric",
                value = vals[keep],
                pos = as.integer(pos_all[keep]),
                length = x@length)
          })

#' Addition operator for sparse_numeric
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object.
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_numeric_add_op
#' @aliases +,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_numeric_add_op
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#---------------------------------------------------------------------
# SUBTRACT

#' Subtract Two Sparse Numeric Vectors
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Additional arguments (unused).
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_sub-method
#' @aliases sparse_sub,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_sub-method
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must be same length.")
            pos_all <- sort(unique(c(x@pos, y@pos)))
            vals <- numeric(length(pos_all))
            if (length(x@value))
              vals[match(x@pos, pos_all)] <- vals[match(x@pos, pos_all)] + x@value
            if (length(y@value))
              vals[match(y@pos, pos_all)] <- vals[match(y@pos, pos_all)] - y@value
            keep <- vals != 0
            new("sparse_numeric",
                value = vals[keep],
                pos = as.integer(pos_all[keep]),
                length = x@length)
          })

#' Subtraction operator for sparse_numeric
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object.
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_numeric_sub_op
#' @aliases -,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_numeric_sub_op
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#---------------------------------------------------------------------
# MULTIPLY

#' Multiply Two Sparse Numeric Vectors
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Additional arguments (unused).
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_mult-method
#' @aliases sparse_mult,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_mult-method
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must be same length.")
            same <- intersect(x@pos, y@pos)
            if (!length(same))
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            xv <- x@value[match(same, x@pos)]
            yv <- y@value[match(same, y@pos)]
            prod <- xv * yv
            keep <- prod != 0
            new("sparse_numeric",
                value = prod[keep],
                pos = as.integer(same[keep]),
                length = x@length)
          })

#' Multiplication operator for sparse_numeric
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object.
#'
#' @return A new `sparse_numeric` vector.
#'
#' @name sparse_numeric_mult_op
#' @aliases *,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_numeric_mult_op
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#---------------------------------------------------------------------
# CROSSPRODUCT

#' Dot Product of Two Sparse Vectors
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Additional arguments (unused).
#'
#' @return A single numeric value.
#'
#' @name sparse_crossprod-method
#' @aliases sparse_crossprod,sparse_numeric,sparse_numeric-method
NULL

#' @rdname sparse_crossprod-method
setMethod("sparse_crossprod",
          c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must be same length.")
            same <- intersect(x@pos, y@pos)
            if (!length(same)) return(0)
            sum(x@value[match(same, x@pos)] *
                  y@value[match(same, y@pos)])
          })

#---------------------------------------------------------------------
# MEAN

#' Mean of a sparse_numeric vector
#'
#' @param x A `sparse_numeric` object.
#' @param ... Ignored.
#'
#' @return A numeric mean.
#'
#' @name mean-sparse
#' @aliases mean,sparse_numeric-method
NULL

#' @rdname mean-sparse
setMethod("mean", "sparse_numeric",
          function(x, ...) sum(x@value) / x@length)
#---------------------------------------------------------------------
# NORM

#' Euclidean Norm of a sparse_numeric vector
#'
#' @param x A `sparse_numeric` object.
#' @param type Ignored (kept for compatibility).
#' @param ... Ignored.
#'
#' @return Numeric norm.
#'
#' @name norm-sparse
#' @aliases norm,sparse_numeric,ANY-method
NULL

#' @rdname norm-sparse
setMethod("norm", c("sparse_numeric", "ANY"),
          function(x, type = "2", ...) sqrt(sum(x@value^2)))

#---------------------------------------------------------------------
# STANDARDIZE

#' Standardize a Sparse Numeric Vector
#'
#' @param x A sparse_numeric vector.
#' @param ... Additional arguments (unused).
#'
#' @return A standardized sparse_numeric object.
#'
#' @name standardize-method
#' @aliases standardize,sparse_numeric-method
NULL

#' @rdname standardize-method
setMethod("standardize", "sparse_numeric",
          function(x, ...) {
            n <- x@length
            mu <- mean(x)
            centered_vals <- x@value - mu
            centered <- new("sparse_numeric",
                            value = centered_vals[centered_vals != 0],
                            pos = x@pos[centered_vals != 0],
                            length = n)
            sd_x <- norm(centered) / sqrt(n)
            if (sd_x == 0) stop("Standard deviation is zero.")
            scaled_vals <- centered@value / sd_x
            new("sparse_numeric",
                value = scaled_vals,
                pos = centered@pos,
                length = n)
          })

#---------------------------------------------------------------------
# SHOW

#' Show Method for sparse_numeric
#'
#' @param object A `sparse_numeric` object.
#'
#' @name show-sparse
#' @aliases show,sparse_numeric-method
NULL

#' @rdname show-sparse
setMethod("show", "sparse_numeric",
          function(object) {
            cat("Sparse numeric vector of length", object@length, "\n")
            if (!length(object@pos)) {
              cat("all elements are zero\n")
            } else {
              cat("Positions that are not zero:", object@pos, "\n")
              cat("Values at those positions:", object@value, "\n")
            }
          })

#---------------------------------------------------------------------
# PLOT

#' Plot Two Sparse Vectors
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Passed to `plot()`.
#'
#' @name plot-sparse
#' @aliases plot,sparse_numeric,sparse_numeric-method
#'
#' @importFrom graphics points legend
NULL

#' @rdname plot-sparse
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            plot(x@pos, x@value, col = "blue",
                 xlab = "index", ylab = "value",
                 main = "non-zero elements of sparse vectors")
            points(y@pos, y@value, col = "red")
            legend("topright", legend = c("x", "y"),
                   col = c("blue", "red"), pch = 1)
          })
