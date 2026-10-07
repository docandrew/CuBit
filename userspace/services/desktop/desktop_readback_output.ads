with System;
with Desktop_Vulkan_Startup;
with Compositor_Pool;
with Compositor_Row_Copy;
-- Serialized owner for an exclusive CPU output writer. Caller must hold that
-- writer/mapping for this entire lifetime; a ticket does not create authority.
-- Geometry must match the configured GPU target and output. The caller must
-- not run another CPU reader of this readback: final Advance retires its ticket.
package Desktop_Readback_Output is
   package D renames Desktop_Vulkan_Startup;
   package P renames Compositor_Pool;
   package G renames Compositor_Row_Copy.G;
   type Phase is (Idle, Transferring, Copying, Complete, Repaint, Quarantined);
   type State is limited private;
   function Current (S : State) return Phase;
   function Rows_Copied (S : State) return Natural;
   function Matches (S : State; Writer : P.Ticket; Target : System.Address;
      Width, Height : G.Pixel_Edge; Bytes, Pitch : Natural) return Boolean;
   -- Called after scene completion, while retaining the destination writer.
   -- Takes the completed private target and submits its readback exactly once.
   -- Failure after taking the target quarantines it; never guesses retirement.
   procedure Begin_Transfer (S : in out State; Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean);
   -- One fence observation per call, no copy and no wait. Copying means that
   -- Advance may run on a later event-loop turn, not that pixels are published.
   procedure Poll_Transfer (S : in out State; Writer : P.Ticket);
   procedure Begin_Copy (S : in out State; Readback, Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean);
   -- At most256KiB payload; foreign writer tickets reject without progress.
   -- Complete only after every row AND readback retirement succeeded.
   procedure Advance (S : in out State; Writer : P.Ticket;
      Byte_Budget : Natural; Accepted : out Boolean);
   -- Cancel only between synchronous copy calls. Repaint is NOT publication:
   -- caller must redraw the entire CPU output before presenting it.
   procedure Cancel (S : in out State; Writer : P.Ticket; Accepted : out Boolean);
   -- Caller consumes Complete or Repaint before rearming. Never releases an output pool.
   procedure Acknowledge (S : in out State; Writer : P.Ticket; Accepted : out Boolean);
private
   type State is limited record
      Status : Phase := Idle;
      Source, Destination : P.Ticket := P.None;
      Target : System.Address := System.Null_Address;
      Width, Height : G.Pixel_Edge := 0;
      Bytes, Pitch, Next_Row : Natural := 0;
   end record;
   function Current (S : State) return Phase is (S.Status);
   function Rows_Copied (S : State) return Natural is (S.Next_Row);
end Desktop_Readback_Output;
