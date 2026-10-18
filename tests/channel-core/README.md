# Channel core (CAP-002 step 3)

Hosted differential tests, proof and mutation checks for the runtime's
channel core (docs/capability-handles.md section 4):

| Unit | What |
|---|---|
| `CuBit.Bell_Sets` | which watched grants were rung (set-associative, whole references) |
| `CuBit.Channel_Admission` | per-client and total channel limits per offer |
| `CuBit.Channel_Schedule` | priority between channels: classes, round robin, once per turn |
| `CuBit.Request_Tags` | a client's outstanding requests: tags never reused, answered once |
| `CuBit.Channel_Declarations` | the `.cubit.channels` manifest section codec |
| `CuBit.Request_Channels` | the standard request/completion duplex over two regions (tested, not proved; its rules are `Submission_Queues`', which is proved) |

Run in the Nix shell, from the repository root:

    tests/channel-core/run.sh
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      tests/channel-core/run.sh --prove
    tests/channel-core/mutations.sh

Proved (level 2, 2026-10-17): 997 checks, 0 unproved, through the test
instances and the production instances in core_instances.ads
(`Process_Bells` = Channel_Bells' 64 x 4 set, `Declared_Schedule` over
`Channel_Limits.Channel_Class`, `Most_Offers` at 8 offers,
`Channel_Tags` at 32). Admission keeps each offer's total equal to the sum
of its clients' counts (ghost sum and lemmas), so Admit never passes a
capped limit (`Server_Limit` caps nothing here) and Release is exact, and
Grow keeps every count; Schedule's Next returns an eligible channel of the
highest eligible class, the first after the class cursor, and Turn_Over
only when nothing eligible is left; Tags never reuse a tag, match at most
the one waiting entry, fail every waiting entry at the end; Bell_Sets marks
and takes exactly the registered reference; the `.cubit.channels` codec
(`Channel_Declarations`) decodes only what it encodes. The wake handshake
model (`wake_handshake_model`) proves the needs-wake set/recheck: a
sleeping consumer with work pending always has a ring coming, and a ring
is sent once per arming. Not proved: `Channel_Servers` and
`Channel_Clients` themselves (heap table, futexes, kernel calls; tested).

Hosted (seeded, 200,000 steps each; about 4.3 million checks): each unit
against a reference model; the request channel with a client and a server
that sleep only after arming and wake only when rung, including the race a
re-check covers (a request landing between the server's last look and its
arming), and a hostile client that writes its indices ahead.

Mutations: 13, all caught (see mutations.sh).
