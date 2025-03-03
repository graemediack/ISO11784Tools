## UPDATE
v1.2.1

* fix bug - dothex format detected if isodecimal has '.' separator but only 13 characters. Added value new value check
* added test objects for new value check

## RESUBMISSION <enter date when submitting>

New patch 1.2.1

── R CMD check results ────────────────── ISO11784Tools 1.2.0 ────
Duration: 1m 25.2s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded

##############################

## UPDATE
v1.2.0

* new feature - handle different format of isodecimal code where separating characters might be used
* added test functions for new feature

## RESUBMISSION 2025-02-24

New version 1.2.0

── R CMD check results ────────────────── ISO11784Tools 1.2.0 ────
Duration: 1m 25.2s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded

##############################

## RESUBMISSION

Fixed acronymns in DESCRIPTION
R CMD CHECK not rerun

## R CMD check results 2023-02-27
There were no ERRORs or WARNINGs. 

── R CMD check results ───────────────────────────────────────────────────────────────────────────────────────────────────── ISO11784Tools 1.1.3 ────
Duration: 19.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Downstream dependencies

Imports stringr,stringi,dplyr,tibble
