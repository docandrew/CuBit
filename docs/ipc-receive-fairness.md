# IPC receive fairness: desktop/DOOM starvation regression

## Observed failure

Closing NetSurf's native window and then launching DOOM could leave DOOM's
window blank and make desktop input appear frozen, while audio kept playing.
The compositor had removed the surface, but its attempt to kill the client was
denied because it lacked process-write authority. NetSurf continued polling
the destroyed surface without its normal idle backoff.

Debugger inspection found a full desktop mailbox ring containing DOOM's
one-way `Present_Surface` requests, while the compositor continued answering
synchronous input polls. This was starvation, not a halted kernel or a GPU
page-flip deadlock. Rejected requests were also absent from the desktop's
ordinary request counters, obscuring the ongoing work in serial telemetry.

## Shared kernel fix

All request/mixed receive variants now use one mailbox-locked selection helper:
blocking receive, deadline receive, service-request poll, and mixed IPC poll.
The mailbox retains an enum-valued round-robin cursor across calls and across
receiver threads. It rotates between queued messages, waiting synchronous
senders, and persistent IRQ doorbells, skipping unavailable/ineligible lanes.
Service-only polling leaves events and IRQs alone.

A continuously available eligible lane can be preceded by at most two other
successful selections (one in service-only polling). FIFO order within a lane
is retained within each selected traffic class. No allocation, additional IPC,
or bulk-data copy is added. The
cursor resets when a mailbox is initialized/retired. Reply-capability
installation is shared too: only a dequeued synchronous/async request mints
a reply; a one-way request, event, or empty receive does not.

This is a selection bound, not a wall-clock latency guarantee or a proof of
whole-kernel concurrency correctness. Queue admission/backpressure, service
work budgets, scheduler admission, and event-only receive remain separate
concerns. No authority checks were relaxed, no broader kill permission was
granted, and the proposed asynchronous display state machine remains unwired.

## Regression evidence

- Native `async-ipc` exercises all four receive variants with a queued one-way
  message and repeated synchronous polls, requiring queued work to be observed
  by the second poll. It also checks that the one-way receive cannot save a
  reply capability. This extends the existing CI test.
- `desktop-doom` now launches through Apps and checks framebuffer evidence of
  gameplay and a subsequent keyboard-triggered Apps menu. Startup log messages
  alone are not sufficient to pass.
- `CUBIT_DOOM_MULTIAPP=1` additionally opens/closes Workbench and NetSurf before
  DOOM. This reproduced the original failure; the captured pre-fix framebuffer
  fails the visual/liveness checker, and the fixed run passes with gameplay
  and roughly 35 compositor frames/second.

## Follow-up, not masked by this fix

- Replace hard-close-plus-best-effort-kill with a typed close-request/lifecycle
  flow. Applications should shut down when their surface becomes invalid;
  force termination must require separately held, appropriately scoped
  authority. Do not give the compositor blanket kill authority as a shortcut.
- Count rejected IPC requests and expose the rejection reason/caller without
  allowing a log flood. Zero accepted requests does not imply an idle service.
- Add per-endpoint/work admission budgets and measure latency under abusive
  clients; fair dequeue selection alone is not complete denial-of-service
  isolation.
