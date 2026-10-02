# Penny browser

Penny is CuBit's native browser, powered by Servo. Commit the sources in this
directory, `assets/penny`, browser tests, and the matching native UI changes.
Do not vendor Servo, Cargo's registry, generated libraries, or disk images.

`UPSTREAM_REVISION` records the Servo revision used for the native validation.
Our `patch_servo.py`, `crate_fixes.py`, and `overlay/` contain the port changes;
`native/` contains the CuBit UI and protected-frame bridge. Patches are applied
to the disposable checkout, not maintained by editing that checkout manually.

## Preparing an upstream checkout

From the CuBit repository root, inside `nix develop`:

```sh
mkdir -p userspace/rust/build/servo-work
git clone https://github.com/servo/servo.git userspace/rust/build/servo-work/servo
git -C userspace/rust/build/servo-work/servo checkout --detach "$(cat userspace/servo/UPSTREAM_REVISION)"
export CARGO_HOME="$PWD/userspace/rust/build/servo-work/cargo-home"
cargo fetch --manifest-path userspace/rust/build/servo-work/servo/Cargo.toml
```

These commands are for a new checkout. Do not overwrite an existing development
checkout. All downloaded source and build output stays under the ignored
`userspace/rust/build/` directory. The upstream checkout retains Servo's licenses
and third-party notices; Penny credits Servo in Help → About Penny.

## Building and checking

From the CuBit repository root:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel servo
flock --exclusive --nonblock coordination/build.lock nix develop -c python3 tests/browser-bookmarks/run-native.py
```

The existing `servo` make target builds the libc prerequisites, invokes
`build-cubitshell.sh`, and stages `cubitshell.app`. The script builds the native
Ada chrome and checks the final ELF's bounded secondary-stack linkage. The
internal executable and Config identity remain unchanged by the Penny rename.
See `docs/servo-port.md` and `tests/servo/README.md` for the broader port and
native regression procedures; native tests require the matching CuBit boot
services already built/staged. Initial builds require network access and many
gigabytes of build space. A pristine, empty-cache rebuild has not been repeated
for this staging pass.

Penny currently depends on the in-development protected Desktop publication
API (`Client_Frame_Pair`, `Client_Input_Budget`, `CuBit.Desktop_Protocol.Publication`)
and matching runtime/Desktop implementation. Those shared platform changes must
be committed together with, or before, the Penny commit. A browser-only commit
on the older Desktop ABI is not a standalone buildable release.

The verified snapshot includes bookmarks/folders and favicon persistence,
native menus/settings, tabs/windows, and the copper globe assets. Session/tab
restoration and the latest requested spacing/icon-only toolbar pass are pending;
do not describe them as implemented in this commit.
