# CuBit Graphical Editor

Status: design proposal

CuBit should ship with a reusable rich graphical text-editor widget suitable
for notes, configuration, CCL programs, source code, logs, and recovery work.
A Notepad-class application and the CCL Workbench are compositions around this
widget rather than independent editor implementations. It is not a terminal or
TUI component. Its editing behavior deliberately carries forward
work from the experimental DAGBuild text field and the earlier Ada TUI editor,
while replacing their hosted allocation, terminal, SDL, and ambient filesystem
assumptions with bounded CuBit types and services.

The initial widget is not a minimal demonstration. Multiple cursors,
selections, syntax highlighting, search, undo, and viewport management are
baseline features. Tabs, file actions, and a capability-scoped folder view are
application composition around it.

## Component boundaries

The design has three layers:

```text
bounded editing core
    document, cursors, selections, commands, undo, highlights
                         |
                         v
rich Text_Editor widget
    viewport, hit-testing, rendering, focus, pointer/key translation
                         |
             +-----------+----------------+
             |                            |
        Notepad app                 CCL Workbench
    tabs/files/folder tree       REPL history/scripts/debugging
```

The editing core has no surface, desktop, SDL, filesystem, or CCL dependency.
The widget has no authority to open or save files: callers provide and receive
document content through bounded operations. This keeps editing reusable in
launch forms, inspectors, application-specific source views, and future tools.

Native CuBit applications instantiate the widget through the shared toolkit.
CCL receives an opaque, surface-scoped editor widget ID through the typed UI
protocol, never a native pointer. The Workbench emulator implements the same
operations with the same editing core. Typed edit/change events are batched so
normal typing does not require a synchronous IPC round trip for every drawing
primitive.

## Indexing and cursor model

Text, lines, columns, cursor collections, and user-facing positions use
one-based indexing. A cursor is an insertion position in the range
`1 .. Length + 1`; `Length + 1` is the legal position after the final
character. Operations must use subtypes and control flow that establish when a
predecessor or successor exists before computing one.

Each cursor contains a current insertion position and a selection anchor:

```text
Cursor = {
  position: Text_Position,
  anchor: Text_Position,
  preferred_column: Display_Column
}
```

Equal position and anchor means no selection. Otherwise the selection is the
ordered range between them. Shift movement changes the position while retaining
the anchor. Ordinary movement collapses or resets the selection according to
the editing command. This avoids storing separately mutable selection start,
end, and direction values.

Rich multiline documents own a bounded collection of cursors. The initial
implementation should support at least 32, with a declared per-document
maximum. Single-line fields always contain exactly one cursor: Ctrl+click has
no multicursor meaning in a text field, search box, launch argument, or REPL
entry. Commands apply
to cursors in document order, and edits that could change later positions are
performed from the end of the document toward the beginning. Overlapping
selections are normalized before mutation. One cursor is designated primary for
viewport following, status, clipboard operations, and commands that inherently
produce one result.

Required initial interactions include:

* click to position the primary cursor;
* drag to select;
* double-click to select a word;
* triple-click to select a logical line;
* Ctrl+click to add or remove a cursor;
* Shift+click and Shift+movement to extend a selection;
* Ctrl+Left and Ctrl+Right word movement;
* matching-delimiter indication plus explicit jump and select-to-match commands;
* Home, End, document start/end, and page movement;
* selection-aware typing, Backspace, and Delete;
* add-next-occurrence and select-all-occurrences with explicit cursor limits;
* rectangular selection when its display-column behavior is specified; and
* horizontal and vertical viewport following without changing cursor state.

Multi-click recognition uses both a bounded time interval and a small
logical-pixel rectangle anchored at the first click. Rapid clicks outside that
rectangle begin a new single-click sequence; platform-provided click counts are
not trusted to define editor gestures.

Word boundaries must be a shared policy, not embedded in SDL key handling or a
renderer. The first policy can be ASCII/CCL-aware, but the document model must
ultimately navigate Unicode grapheme clusters rather than bytes or code points.
The initial policy treats words, whitespace, and punctuation as distinct
classes: moving right from a word may consume its trailing whitespace, but must
not also consume the following punctuation group. Structural navigation is a
separate command rather than a context-dependent interpretation of Ctrl+Arrow.

The initial shared search primitive uses a bounded Knuth-Morris-Pratt prefix
table. Searches therefore have deterministic linear time in the document plus
pattern length rather than a quadratic adversarial case. Patterns are currently
limited to 256 characters. Ctrl+D first selects the identifier at the primary
cursor and then adds the next unselected whole-word occurrence, wrapping at the
end and respecting the 32-cursor limit. Ctrl+F opens a literal, case-sensitive
find field; Enter or F3 advances with wraparound and Escape closes it. Search
options and Unicode-aware matching remain future typed extensions.

## Editing core

The editing core is independent from display, input transport, filesystem IPC,
and syntax highlighting. It accepts typed edit commands and produces a bounded
damage/edit description.

```text
Linux SDL events ----+
                     +--> Edit_Command --> bounded document/cursors
CuBit input IPC -----+                         |
                                               +--> edit result + damage
```

The first reusable component is the editor's bounded inline-document mode for
text fields, search, launch forms, and the CCL REPL. It uses the same cursor,
anchor, word movement, hit-testing, selection, and command types as multiline
documents, but enforces a single cursor. This replaces the current
Workbench-specific append/backspace logic without creating a second text-input
implementation.

The first shared multiline store is a bounded contiguous document ADT. It is
the deliberately simple correctness reference: edits are atomic, positions are
one-based, capacity failure is explicit, and line/column conversion plus
preferred-column vertical movement are SPARK-proved. The multiline store should
later adapt the earlier editor's piece-table design, but
with explicit limits for original text, added text, piece count, line index,
cursor count, and undo storage. Reaching a limit returns a typed result and does
not partially apply an edit. Storage limits are visible in document metadata and
the editor UI.

Undo and redo are bounded resources. Sequential character insertion and
deletion may coalesce, while cursor movement, selection changes, paste, and
structural commands end a coalescing group. Undo restores document and cursor
state atomically. The budget is expressed in bytes and entries rather than an
implicitly growing container.

## Rich editor widget

The rich widget owns bounded interactive state for:

* a document reference or bounded inline document;
* cursors, anchors, and the primary cursor;
* horizontal and vertical viewport position;
* undo/redo budget and coalescing state;
* validated, versioned highlight spans;
* search matches and active match;
* focus, pointer drag, caret blink, and composition state; and
* damage accumulated since the last committed frame.

Its typed interface includes document replacement and snapshots, edit commands,
cursor/selection queries, highlight updates, search, scroll, read-only mode,
and change subscriptions. Clipboard access is injected by an authorized caller;
the widget does not acquire clipboard authority merely because it can select
text.

## Notepad application

The graphical Notepad application uses the shared rich widget and desktop
protocol. Its initial layout contains:

* a tab strip for open documents;
* an optional folder/project tree;
* one rich editor widget for the active document;
* search and replace controls;
* a status area for position, language, encoding, limits, and authority state;
* unsaved-change and external-change indicators; and
* dialogs implemented as typed graphical launch/actions, not terminal prompts.

The Notepad layer owns file handles, load/save coordination, directory handles,
tabs, and unsaved-change policy. The rich widget never infers filesystem
authority from displayed content.

Rendering is viewport-based. Only visible lines, highlights, selections, and
cursors are submitted. Damage is bounded and coalesced. Cursor blinking and
pointer motion must not force complete document rerenders.

The shared viewport tracks both the first visible line and first visible
display column. Horizontal movement follows cursors, while explicit navigation
uses a conventional horizontal scrollbar or Shift+wheel. Pointer hit-testing
is translated through both viewport axes. This column-based implementation is
appropriate for the current monospace editor; tabs and grapheme-aware display
widths will require the planned display-column policy rather than byte offsets.

## Filesystem authority

The editor never receives ambient access to a current working directory or a
global path namespace. A launch context may provide:

* one or more readable file handles;
* a replace/write authority for a particular file;
* an enumerable directory handle for a project tree;
* create-child authority within a particular directory;
* a directory-change subscription; and
* explicit recent-resource handles selected by the user.

Reading a file does not imply authority to overwrite it. Enumerating a directory
does not imply authority to open every child; the filesystem service validates
the directory and requested operation when deriving a child handle. Save As is
an authority-selection interaction mediated by the desktop and filesystem
services, not a raw pathname prompt.

The folder view is lazy and bounded. Directory pages, expanded nodes, visible
rows, watches, and cached names have explicit maxima. Symlink-like aliases or
mount boundaries must not silently widen authority.

File loading and saving use asynchronous filesystem operations. Not all writes
are cancellable after acceptance. Save should prefer atomic replacement when
supported and report committed, rolled-back, returned, or uncertain completion
states honestly.

## Syntax highlighting

Highlight output is a bounded set of styled spans associated with a document
version and line/range. Stale results are discarded. The renderer never trusts
span bounds without validation.

CCL should use its own parser/token information first. Other languages may use
small verified scanners, bounded service-based highlighters, or a contained
parser such as Tree-sitter. A parser does not inherit the editor's filesystem
handles and cannot mutate the document. Parser failure or budget exhaustion
falls back to plain text without affecting editing.

Highlight styles are semantic roles such as keyword, type, string, comment,
diagnostic, and authority rather than terminal color numbers. Themes map roles
to graphical fonts and colors.

The shared canvas editor now accepts an ordered, bounded set of one-based,
inclusive foreground spans. The hosted CCL Workbench supplies these spans from
a bounded presentation-only scanner for comments, forms, literals, booleans,
delimiters, and qualified service operations. Selection remains visually
dominant over highlighting. The scanner has no effect on parsing, diagnostics,
typing, compilation, or execution; failure to classify text therefore cannot
change program meaning. A later parser-token adapter should replace duplicate
CCL lexical knowledge and attach spans to the document version.

## Provenance from earlier projects

The DAGBuild text field supplies the intended single-line interaction behavior:
mouse hit-testing, drag and word selection, modifier navigation, selection-aware
editing, focus, and caret presentation. Its SDL types, unbounded strings, global
selection state, and unchecked one-based arithmetic are not carried forward.

The earlier editor supplies the anchor-based multicursor model, piece-table
document organization, grapheme-aware navigation, viewport behavior, syntax
span concepts, file tree, incremental edit records, and undo coalescing. Its TUI
renderer, ANSI colors, ambient paths, general-purpose containers, VSS dependency,
and hosted file operations remain adapters or design references rather than
CuBit runtime dependencies.

Its behavioral tests should be converted into renderer-independent editing-core
tests before the graphical application is considered feature complete.

## Initial implementation sequence

1. Implement and prove the rich editor's bounded, one-based inline-document
   state and shared edit commands.
2. Translate Linux Workbench SDL and CuBit input IPC into the same edit commands.
3. Replace the CCL REPL field's custom editing with the rich widget in
   single-line mode.
4. Port DAGBuild selection, word movement, and mouse behavior as tests.
5. Adapt the multiline piece-table/document operations with explicit storage
   budgets.
6. Integrate the bounded multicursor collection and normalization rules only
   with the multiline document mode.
7. Port the earlier editor's behavioral tests to the shared core.
8. Complete the reusable rich editor widget, then compose the graphical Notepad
   application with tabs, file actions, search, and an authority-aware folder
   view.
9. Add versioned bounded highlighting, beginning with CCL.
10. Integrate asynchronous load, save, watches, and recovery behavior in CuBit.
