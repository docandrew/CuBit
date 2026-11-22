with Interfaces;
with System;
with Compositor_Damage;
with Desktop_Vulkan_Startup;
with Compositor_Pool;
with Compositor_Row_Copy;
-- Serialized owner for an exclusive CPU output writer. Caller must hold that
-- writer/mapping for this entire lifetime; a ticket does not create authority.
-- Geometry must match the configured GPU target and output. The caller must
-- not run another CPU reader of this readback: final Advance retires its ticket.
package Desktop_Readback_Output with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   package P renames Compositor_Pool;
   package G renames Compositor_Row_Copy.G;
   subtype Byte_Count is Interfaces.Unsigned_64;
   use type Byte_Count;
   type Phase is (Idle, Transferring, Copying, Complete, Repaint, Quarantined);
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Current (S : State) return Phase;
   -- Lifetime counters survive acknowledgement. GPU bytes mean submitted
   -- readback payload, not a physical bus measurement or successful completion.
   -- CPU bytes count only rows actually copied, including subsequently cancelled work.
   function GPU_Bytes (S : State) return Byte_Count;
   function CPU_Bytes (S : State) return Byte_Count;
   function Counters_Saturated (S : State) return Boolean;
   -- Rows copied within the current repair region.
   function Rows_Copied (S : State) return Natural;
   function Matches (S : State; Writer : P.Ticket; Target : System.Address;
      Width, Height : G.Pixel_Edge; Bytes, Pitch : Natural) return Boolean;
   -- Called after scene completion, while retaining the destination writer.
   -- Takes the completed private target and submits its readback exactly once.
   -- Failure after taking the target quarantines it; never guesses retirement.
   procedure Begin_Transfer (S : in out State; Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and
         (if Accepted then Current (S) = Transferring and Rows_Copied (S) = 0 and
            Matches (S, Writer, Target, Width, Height, Bytes, Pitch));
   -- Snapshot the destination writer's repair history. The GPU target's own
   -- damage is not a substitute. Empty/out-of-bounds repairs reject before
   -- taking a target or issuing foreign work.
   procedure Begin_Region_Transfer (S : in out State; Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Repair : Compositor_Damage.State;
      Accepted : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid and Compositor_Damage.Valid (Repair),
       Post => Valid (S) and D.Valid;
   -- One fence observation per call, no copy and no wait. Copying means that
   -- Advance may run on a later event-loop turn, not that pixels are published.
   procedure Poll_Transfer (S : in out State; Writer : P.Ticket)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   procedure Begin_Copy (S : in out State; Readback, Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean)
     with Global => (Input => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- At most256KiB payload; foreign writer tickets reject without progress.
   -- Complete only after every row AND readback retirement succeeded.
   procedure Advance (S : in out State; Writer : P.Ticket;
      Byte_Budget : Natural; Accepted : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- Cancel only between synchronous copy calls. Repaint is NOT publication:
   -- caller must redraw the entire CPU output before presenting it.
   procedure Cancel (S : in out State; Writer : P.Ticket; Accepted : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- Caller consumes Complete or Repaint before rearming. Never releases an output pool.
   procedure Acknowledge (S : in out State; Writer : P.Ticket; Accepted : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
private
   type State is limited record
      Readback_Bytes, Copied_Bytes : Byte_Count := 0;
      Saturated : Boolean := False;
      Status : Phase := Idle;
      Source, Destination : P.Ticket := P.None;
      Target : System.Address := System.Null_Address;
      Width, Height : G.Pixel_Edge := 0;
      Bytes, Pitch, Next_Row : Natural := 0;
      Repairs : Compositor_Damage.State;
      Region : Compositor_Damage.Index := 1;
   end record;
   function Valid (S : State) return Boolean is
     (Compositor_Damage.Valid (S.Repairs) and then
      S.Next_Row <= Natural (S.Height) and then
      (if Compositor_Damage.Count (S.Repairs) > 0 then
         S.Region <= Compositor_Damage.Count (S.Repairs) and then
         Compositor_Damage.Bounds (S.Repairs).Right <= Natural (S.Width) and then
         Compositor_Damage.Bounds (S.Repairs).Bottom <= Natural (S.Height) and then
         S.Next_Row <= Compositor_Damage.Item (S.Repairs, S.Region).Bottom -
           Compositor_Damage.Item (S.Repairs, S.Region).Top) and then
      (if S.Status in Transferring | Copying | Complete then
         Compositor_Damage.Count (S.Repairs) > 0));
   function GPU_Bytes (S : State) return Byte_Count is (S.Readback_Bytes);
   function CPU_Bytes (S : State) return Byte_Count is (S.Copied_Bytes);
   function Counters_Saturated (S : State) return Boolean is (S.Saturated);
   function Current (S : State) return Phase is (S.Status);
   function Rows_Copied (S : State) return Natural is (S.Next_Row);
end Desktop_Readback_Output;
