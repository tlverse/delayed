# delayed 0.5.0

* Changed random seed generation to only generate seed for parallel `future`
  plans. Sequential delayed tasks and tasks under `plan('sequential')` should
  now match seed behavior with code run outside of delayed. Parallel plans
  generate seeds for each delayed subtask prior to running, which should preserve
  reproducibility between parallel runs by generate seeds before race conditions.
  Note that parallel and sequential runs will produce different RNG, which doesn't
  seem to be avoidable.

# delayed 0.3.0

* Initial version for CRAN release.
* Changes to core functions to keep up with `rlang` updates.

# delayed 0.2.1

* Stable release with modifications to keep up with changes in `rlang`.
