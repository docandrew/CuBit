# Approved resource ownership metadata

Run in the Nix development environment:

```sh
nix develop -c bash tests/ccl-resource-policies/run.sh
nix develop -c bash tests/ccl-resource-policies/run.sh --prove
```

These are Linux-hosted tests of the shared SPARK catalog and layout code, not
tests of a new source-language factory or an IPC authority grant.

`CCL.Catalog.Publish_Resource` atomically publishes a nominal resource definition
and its approved ownership/disposition rules. Transition targets are named
types, not numeric ownership tags chosen by a program. Conflicting types or
policies leave the catalog unchanged, including when the root was imported
successfully before a target failed. Target definitions may precede their
policies, allowing mutually dependent descriptions; layout rejects missing
policies before execution.

`CCL.Resource_Policies.Layout` selects the requested resources and their
transition closure, assigns deterministic nonzero ownership tags, and leaves
tag zero unrestricted. It uses bounded iteration rather than recursion.

The suite covers canonical descriptions, invalid/duplicate verbs, unrelated
visible types, weaker-policy substitution, shifted local type numbers,
nominal target conflicts, atomic failure, reverse dependency chains, cycles,
and every ownership-table capacity boundary. There are 1,568 checks.

The focused SPARK run covers two units: `CCL.Resource_Policies` and `CCL.Catalog`.
It discharges 118 checks (flow, initialization, runtime checks, contracts and
termination), including the failure postconditions: publication preserves the
original catalog; unsuccessful layout exposes no partial bindings. There are
no unproved or justified checks. One flow warning identifies an intentionally
unused returned target reference; publication needs its status and updated
candidate catalog, not that local number.

This is not a proof that a publisher is trustworthy, that endpoint authority
was granted, or that an entire bytecode program obeys an approved resource
policy. Source lowering and portable resource linkage must consume this
metadata before portable resource imports can be admitted. They remain rejected
by the current portable contract. Existing native resource tests use trusted
in-memory programs.

Config data is separate: strings, integers and persistable product/sum values
remain ordinary copyable snapshots. These ownership rules apply to live handles,
not to the values returned by Config.
