with Interfaces;
with CuBit.Messages;
with CuBit.Clocks;
with CuBit.Audio_Control;
--  The status area's clock and master volume, read and set without waiting:
--  each request is submitted to its service and its reply arrives as a
--  completion in the event loop (docs: desktop loop never blocks on a peer).
--  One request in flight per service; a later volume setting replaces an
--  earlier one not yet sent (only the newest level matters).
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
   function Matches (Token : Interfaces.Unsigned_64) return Boolean is
     (Token /= 0 and then (Token = Clock_Token or else Token = Audio_Token));
   --  Collect a completion that Matches; Sequence submits a queued setting.
   procedure Collect (C : CuBit.Messages.CompletionEntry;
                      Sequence : in out Interfaces.Unsigned_64);
   --  The newest decoded values; Fresh_* is cleared by Take_*.
   procedure Take_Clock (Value : out CuBit.Clocks.Snapshot; Fresh : out Boolean);
   procedure Take_Audio (Value : out CuBit.Audio_Control.State; Fresh : out Boolean);
end Desktop_Status_Refresh;
