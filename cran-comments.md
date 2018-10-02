This is a re-submission of a new submission.

- Updated examples using `\dontrun{}` to use `\donttest{}` except for the one case that cannot be run. Also checked the package with `--run-donttest` to ensure that all examples will run without issue. We kept most examples wrapped with `\donttest{}` because they are variations of other examples and cumulatively take too long to run in a check. Also, the unit tests already provide good code coverage so there's no need to extend package check time by testing every example that can be run.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.