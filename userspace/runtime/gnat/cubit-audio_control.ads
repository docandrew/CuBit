with Interfaces; use Interfaces;

--  Distinct bearer authority, never inferred from playback access or PID.
package CuBit.Audio_Control is
   Service_Role : constant Unsigned_64 := 20;
   Authority_Tag : constant Unsigned_64 := 16#8000_0000_0000_0001#;
   Endpoint_Slot : constant Unsigned_64 := 26;
   Get_State : constant Unsigned_32 := 16#0507#;
   Set_State : constant Unsigned_32 := 16#0508#;
   subtype Percent is Natural range 0 .. 100;
   type State is record
      Level : Percent := 100;
      Muted : Boolean := False;
      Available : Boolean := False;
   end record;
   procedure Read (Value : out State; Success : out Boolean);
   procedure Set (Level : Percent; Muted : Boolean;
                  Value : out State; Success : out Boolean);
end CuBit.Audio_Control;
