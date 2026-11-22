# Selected module lease tests

Run bash tests/aml-module-scopes/run.sh in pinned Nix with TMPDIR set before entering. --validate-only checks source pins without a compiler. Checked/release each run53checks against the actual ledger,covering independent invocation/entry stamps,root lifetime ownership,parent scopes,missing scopes,duplicates,stale leases and capacity. This is hosted testing; it does not authenticate live namespace nodes or frames.
