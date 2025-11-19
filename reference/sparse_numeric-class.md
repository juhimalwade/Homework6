# Sparse Numeric Vector (S4 Class)

An S4 class for representing sparse numeric vectors, storing only
non-zero values and their positions.

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of 1-based indices of non-zero values.

- `length`:

  Integer giving the full length of the represented vector.
