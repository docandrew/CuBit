# Workbench examples

The USB live ISO includes these originals under `/samples/ccl/` and editable
copies in the live workspace, `@mem:0/work`. The initrd-only laptop image also
includes the workspace copies. No extra filesystem authority is needed.

Open CCL Workbench, press **Ctrl+O**, select a file and press **Enter**:

| Sample | Try it |
| --- | --- |
| `button-clock.ccl` | F5 registers a Refresh button; click it for uptime. Stop closes it. |
| `clock-label.ccl` | F5 updates the label once; F7 watches it periodically. |
| `function-clock-label.ccl` | The same label, using typed function definitions. |
| `monotonic-clock.ccl` | F5 returns elapsed time as `hh:mm:ss`, not calendar time. |

F8 switches between Lisp and BASIC. If you edit a sample, save under a new name
with Ctrl+S before opening another file. Live-workspace changes are lost at reboot.
Workbench currently selects its NVMe workspace first if one is available; the
USB-only live profile uses the RAM workspace. The picker does not yet browse
the CD's read-only originals or switch volumes.
