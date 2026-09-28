# CuBit working rules

- Use the Nix development environment for builds, tests, and SPARK proofs.
- Obsolete experimental interfaces and implementations may be removed when
  replacing them. Do not carry compatibility aliases, dead syscalls, or backup
  implementations just to preserve an undeployed ABI; Git holds the history.
  Keep removals scoped to the work at hand and explain material removals.
- Preserve unrelated working-tree changes. Do not commit or push unless asked.
- Clearly distinguish Linux-hosted demonstrations from live CuBit integration,
  and proved properties from assumptions and regression-tested behavior.
- When sharing this checkout with another agent, follow `coordination/README.md`,
  maintain your own ownership note, and check the other notes before shared
  edits. Native builds/ISO generation/headless tests use the shared build lock.
