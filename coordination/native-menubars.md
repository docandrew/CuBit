# Native menubars

2026-10-02: User-requested child of Servo browser agent. Read AGENTS and
coordination README, compositor/networking/filesystem ownership notes.
Own NEW userspace/lib/ui/cubit-ui-menus.ads/.adb, tests/ui-menus/, and
 docs/native-menus.md. No existing UI source, browser, Desktop runtime,
shared build definition, or staging edits. No commits/pushes.

Existing Draw_Menu_Bar/Title and native theme/font/canvas rendering are reused;
new controller registers retained Controls, provides bounded menu models,
popup selection/commands, keyboard and pointer routing. Hosted outputs disjoint.
Parent browser native session77821 freezes existing shared UI sources.

2026-10-02 TERMINAL: additive Menus toolkit/controller and docs implemented;
Nix hosted session43809 exit0, /tmp/cubit-native-menus-host.log. 100 retained
click/redraw cycles, keyboard/disabled/separator/mnemonics, 64-item overflow,
invalid/empty models, clipping sentinels at100%/200%, and five capture-vs-hit
press-drag-release scenarios pass. Real toolkit light/dark rendered preview
/tmp/cubit-native-menus-preview.png inspected. No native boot claim or SPARK
proof. No shared existing file/staging edits or commits. API reserves73IDs,
no allocation, supports one-level popups; hierarchy/wheel scrolling not yet.
Parent review requested title press-drag-release; implemented with separate
menu drag state while App retains native title capture, including disabled,
outside, cancellation and stale-release negative controls. Outside presses
must be intercepted before App to prevent clickthrough; moves/releases still
reach App first to clear capture. Returned commands execute only after input.
