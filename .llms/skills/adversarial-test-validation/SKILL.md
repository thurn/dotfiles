---
name: adversarial-test-validation
description: Deliberately inject realistic user-visible bugs to evaluate a project's tests, add regression coverage for surviving mutations, and restore all production code afterward. Use when asked for adversarial test validation, manual mutation testing, or to challenge whether tests catch real product regressions.
---

# Adversarial Test Validation

Probe the test suite with carefully chosen temporary bugs. The durable output is
new tests for meaningful gaps; intentional production-code mutations must never
remain in the finished change.

## Scope

Use the iteration count requested by the user, or **5** when none is given. One
iteration means one valid bug mutation taken through the full verification
cycle. A rejected idea, a mutation that does not exercise the intended behavior,
or a mutation that only causes a syntax/type/build failure does not count.

Unless the user explicitly broadens the definition, a mutation qualifies only
when all of these are true:

- A person using the product normally could observe the failure at a product
  boundary such as the UI, CLI, or other directly consumed behavior.
- The behavior is clearly wrong, rather than merely a different plausible
  product choice or an arbitrary content/data value.
- It is reasonable to expect the project's automated tests to block the
  regression.
- It does not depend on exceptional operating conditions such as exhausted
  memory, full disks, infrastructure failure, or similarly extreme cases.

Incorrect logs, internal-only state differences, harmless implementation
changes, and arbitrary data tuning do not qualify. For example, changing a game
item's price from $5 to $10 is not a qualifying bug without a separate contract
that fixes that price. Apply judgment conservatively: if the user-visible harm
or expected behavior is ambiguous, choose another mutation.

## Prepare

1. Read the repository instructions and honor its isolation, testing, and
   editing conventions. Preserve unrelated and pre-existing changes.
2. Inspect product entry points, normal user journeys, production code, and the
   tests that claim to cover them. Build a shortlist across distinct behaviors,
   favoring important state transitions, input handling, action wiring,
   navigation, persistence, and rendered outcomes.
3. Identify the ordinary project test command that should detect regressions in
   those behaviors. Run it without mutations and require a trustworthy baseline.
   If the baseline is red or materially flaky, do not begin mutation experiments
   unless newly introduced failures can be distinguished with high confidence.
4. Work only in an isolated test/development environment. Do not point mutated
   code at production data or real external side effects.

Prefer behavior-level mutations that remain syntactically and type correct,
such as reversing a meaningful condition, omitting a required state update,
misrouting a normal action, accepting invalid ordinary input, or rendering the
wrong state. Avoid trivial constant changes, compiler errors, test-only code,
and several variants of the same defect.

## Run Each Iteration

Keep exactly one intentional production mutation active at a time.

1. Record the original code and introduce the smallest mutation that produces
   the chosen user-visible failure. Do not weaken, skip, or edit existing tests.
2. Confirm through the closest practical public boundary that the mutation
   actually causes the intended failure. Do not count a mutation based only on
   reading the diff.
3. Run the same ordinary test suite selected at baseline. Do not update
   snapshots or expected output to accommodate the mutation.
4. Classify the result:

   - **Caught:** At least one pre-existing test fails for a reason that
     specifically demonstrates the intended regression. Unrelated or flaky
     failures do not count.
   - **Survived:** The relevant suite passes, or its failures do not demonstrate
     the intended regression.

5. If caught, restore only the intentional mutation and rerun the affected
   passing test or suite.
6. If survived, while the mutation is still active, add the smallest durable
   black-box regression test at the public boundary. Verify that the new test
   fails for the intended behavioral reason. Then restore only the production
   mutation and verify that the new test passes.
7. Check the working diff before continuing. It may contain the regression tests
   added by this workflow and pre-existing user changes, but no intentional
   production mutation. The growing test suite becomes the baseline for later
   iterations.

If a proposed mutation proves unsafe, ambiguous, unreachable in normal use, or
incapable of showing the intended failure, restore it and choose another; it
does not consume an iteration. Never use broad reset or checkout commands that
could erase user work. Track and reverse the exact edits owned by each
experiment, including when a command fails or the run is interrupted.

## Test Quality

New tests should assert the behavior a user or client can observe, not the
temporary mutation or its implementation details. A test must fail against the
mutated behavior and pass against the restored implementation. Avoid brittle
snapshots or exact internal call assertions when a stable product-level
assertion is available. Do not add a test merely to increase coverage when the
mutation fails the qualifying-bug judgment.

Use the broadest ordinary suite that is practical and relevant. If the complete
suite is prohibitively expensive and a standard component suite is used
instead, state that limitation rather than implying whole-project validation.

## Finish

After the requested number of valid iterations:

1. Verify once more that every intentional production mutation has been
   removed.
2. Run the repository's appropriate final checks with only the durable test
   additions present.
3. Report each valid mutation, the observable failure it represented, whether
   existing tests caught it, and which regression test was added when it
   survived. Also report the baseline/final commands, any scoped-suite
   limitation, and any shortfall if too few defensible mutations could be found.

Do not inflate the count with questionable bugs. Fewer high-confidence results
are preferable to claiming that arbitrary or internal changes were meaningful
test gaps.
