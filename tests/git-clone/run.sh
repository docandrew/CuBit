#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: hosted tests of the clone writer (Git_Clones) over the
#  Linux host's directories: clones of repositories the host's git makes,
#  from the answers its upload-pack gives, judged by the host's git (fsck
#  --strict, log, status, checkout, the files against the tree), and
#  refusals that must leave nothing. Run in the Nix shell:
#    bash tests/git-clone/run.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/git_clone_tests.gpr"
python3 -I "$here/check.py" "$here/build/clone_tool" "$here/build/work" "$here/build/session_tool"
