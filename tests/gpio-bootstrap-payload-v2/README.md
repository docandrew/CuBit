# GPIO bootstrap payload v2

Export TMPDIR=/home/doc/cubit-build-tmp before entering the Nix development environment, then run `tests/gpio-bootstrap-payload-v2/run.sh`. The standalone runner copies and hashes the two actual shared codec files, its fixture and runner into an isolated result directory.

Tests checked and release modes: all span counts, full-width initializer count/digest, complete request equality, independent exact wire bytes, all byte versions/phases and every truncated prefix. Version2 only; phase Before_GPIO_Consumer_Initialization=2; count at90, digest at94, header102. Prior v1 is retired.

The history is diagnostic provenance, not hardware authority or proof of ordering. Native phase owner must enforce the barrier and supply actual initializer history; neither is integrated by this codec-only change. Existing caller/decoder references must migrate atomically. No native/hardware test or new SPARK proof is claimed. Prior private v2 proof attempt timed out and remains recorded separately; tests are not substituted for proof.
