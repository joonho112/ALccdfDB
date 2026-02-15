# Create a validation check result

Create a validation check result

## Usage

``` r
.make_check(
  check_id,
  description,
  status,
  n_issues = 0L,
  detail = NA_character_
)
```

## Arguments

- check_id:

  Character string check identifier

- description:

  Human-readable description

- status:

  One of "PASS", "ERROR", "WARN", "INFO"

- n_issues:

  Number of issues found

- detail:

  Optional detail string

## Value

A one-row tibble
