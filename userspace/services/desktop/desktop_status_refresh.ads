with Interfaces;
with CuBit.Messages;
with CuBit.Clocks;
with CuBit.Audio_Control;
--  The status area's clock and master volume, read and set without waiting
--  (and procmgr's app replacement switch, set without waiting):
--  each request is submitted to its service and its reply arrives as a
--  completion in the event loop (docs: desktop loop never blocks on a peer).
--  One request in flight per service (CuBit.Async_Requests), driven by the
--  Desktop_Status_Machines clock and audio machines: a later volume setting
--  replaces an earlier one not yet sent (only the newest level matters),
--  and a request that fails (not delivered, refused) is asked again at the
--  next due time instead of stopping the item.
package Desktop_Status_Refresh is
   use type Interfaces.Unsigned_64;
   --  Submit a clock read; does nothing while one is in flight.
   procedure Request_Clock (Sequence : in out Interfaces.Unsigned_64);
   --  Submit a volume read, or the pending setting when one is queued.
   procedure Request_Audio (Sequence : in out Interfaces.Unsigned_64);
   procedure Set_Audio (Level : CuBit.Audio_Control.Percent; Muted : Boolean;
                        Sequence : in out Interfaces.Unsigned_64);
   function Clock_Token return Interfaces.Unsigned_64;
   function Audio_Token return Interfaces.Unsigned_64;
   --  The "Allow app replacement" switch procmgr holds (staged launches,
   --  CuBit.Launching.Set_Staged_Launches), driven by the
   --  Desktop_Status_Machines switch machine: Flip it to On.
   procedure Set_Replacement (On : Boolean; Sequence : in out Interfaces.Unsigned_64);
   function Replacement_Token return Interfaces.Unsigned_64;
   --  What the panel shows (the newest flip while one is pending).
   function Replacement_Shown return Boolean;
   --  What procmgr confirmed; Changed: a request ended since the last call
   --  (cleared by it), Answered: procmgr confirmed it (else it refused or
   --  the request was not delivered, and the switch shows Confirmed again).
   procedure Take_Replacement (Confirmed : out Boolean; Changed : out Boolean;
                               Answered : out Boolean);
   function Matches (Token : Interfaces.Unsigned_64) return Boolean is
     (Token /= 0 and then (Token = Clock_Token or else Token = Audio_Token
                           or else Token = Replacement_Token));
   --  Collect a completion that Matches; Sequence submits a queued setting.
   procedure Collect (C : CuBit.Messages.CompletionEntry;
                      Sequence : in out Interfaces.Unsigned_64);
   --  The newest decoded values; Fresh_* is cleared by Take_*.
   procedure Take_Clock (Value : out CuBit.Clocks.Snapshot; Fresh : out Boolean);
   procedure Take_Audio (Value : out CuBit.Audio_Control.State; Fresh : out Boolean);
end Desktop_Status_Refresh;
