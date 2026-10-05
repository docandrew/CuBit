# Logs app

`logs.app` shows every service's log records live. It reads through the
log-observer role and names sources through procmgr's process-observer role
(`userspace/apps/logs/manifest.ccl`). Its only write is its own "started"
record.

## Layout

All of it is built from toolkit controls (`userspace/lib/ui`):

- **Search field** (`CuBit.UI.Editor`, `Draw_Text_Edit_Field`): matches the
  message or the source's name, ignoring case.
- **Service combo box**: "All services", then every distinct source name seen.
  The filter is by name, so it still applies after a service restarts with a
  new pid.
- **Time combo box**: all time, or the last minute, 5 minutes, 15 minutes or
  hour. Records leave the view as they age past the window.
- **Level combo box**: the severity floor.
- **Clear** resets every filter. **Pause/Follow** stops or resumes following.
  While following, the newest record stays selected and in view.
- **Table** (`CuBit.UI.Tables` columns API): Time, Level, Source and Message.
  Drag a column's right edge to resize it; the last column takes the rest.
  Click a header to sort by that column; click again to reverse. Ties keep
  arrival order.
- **Selected record**: the whole message, its typed fields, and its pid.
- **Status bar**: the connection state, how many rows are shown, records lost,
  and records that arrived while paused.

Gaps, where logstore could not deliver records to this viewer, show as rows
under every filter.

## Keyboard

| Key | Where | Does |
| --- | --- | --- |
| `/` | anywhere outside the search field | focus the search field |
| `Tab` / `Shift+Tab` | anywhere | next / previous control |
| Up, Down, PgUp, PgDn, Home | table | move the selection; pauses following |
| `End` | table | follow again |
| `1`–`6` | table | severity floor (Trace … Critical) |
| `s` | table | only the selected record's service, or every service again |
| `f` | table | toggle following |
| `Esc` | table | clear every filter |
| `Esc` | search field | clear the text, then return to the table |
| Enter, Space, arrows, letters | combo box | the toolkit's combo box keys |

## Names and reused pids

A source's name comes from procmgr's observer records, with the process's start
time. Process numbers are reused, so a name applies only to records published
at or after that start. Older records from the same number show as `pid N`.
Stamping each record's publisher identity in logstore would remove this
limitation.

## Tests

- `tests/log-viewer/run.sh`: the view on Linux, driven through the same retained
  control dispatch as `CuBit.UI.App.Run`. It covers filters, combo boxes, sorting,
  following, pid reuse and ring capacity, and writes frames to
  `tests/log-viewer/build/*.ppm`.
- `tests/headless/run.sh --test logs`: the native app on a QEMU guest with
  logstore, clock, timesync and the desktop.
