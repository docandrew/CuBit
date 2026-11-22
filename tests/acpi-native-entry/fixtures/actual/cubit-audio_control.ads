with Interfaces; use Interfaces;
with CuBit.Handles;

--  Distinct bearer authority, never inferred from playback access or PID.
package CuBit.Audio_Control is
   Service_Role : constant Unsigned_64 := 20;
   Authority_Tag : constant Unsigned_64 := 16#8000_0000_0000_0001#;
   Get_State : constant Unsigned_32 := 16#0507#;
   Set_State : constant Unsigned_32 := 16#0508#;
   subtype Percent is Natural range 0 .. 100;
   type State is record
      Level : Percent := 100;
      Muted : Boolean := False;
      Available : Boolean := False;
   end record;
   --  Service: the master-audio control endpoint (the program's startup
   --  handle for the role).
   procedure Read (Service : CuBit.Handles.Endpoint; Value : out State; Success : out Boolean);
   procedure Set (Service : CuBit.Handles.Endpoint; Level : Percent; Muted : Boolean;
                  Value : out State; Success : out Boolean);
end CuBit.Audio_Control;
