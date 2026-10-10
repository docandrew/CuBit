with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_Objects;
procedure Lookup_Root_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (16, Capture, Max_Frame_Roots => 2);
   use N; use N.Owned;
   use type Object_Root_Set;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 64);
   Scratch : Root_Workspace;
   Keep, Expected : Object_Root_Set := [others => False];
   Root_Status : Root_Trace_Status;
   Timers, Checks : Natural := 0;
   type Scenario is (Wide_Field, Scalar_Field, Invalid_Backing);
   Current : Scenario;
   function Segment (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Segment;
   function Method (Name : String; Args : Byte; Code : Bytes) return Bytes is
     (Bytes'(16#14#,Byte (Code'Length + 6)) & Segment (Name) & Bytes'(1 => Args) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Fixture : constant Bytes := Method ("OUTR", 0, Bytes'(1 => 16#A4#) & Segment ("USE2") & Buffer_Value & Segment ("FLD0")) &
     Method ("USE2", 2, Timer & Bytes'(16#A4#,16#69#));
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Current'Image & Checks'Image; end if; end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
   begin
      Value := 0; Available := True; Timers := Timers + 1;
      Check (Timers = 1 and then Lookup_Test_Seen = 1 and then Frame_Root_Count (A) = 2);
      Expected := [others => False]; Expected (1) := True; Expected (2) := Current = Wide_Field;
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
   end Capture;
begin
   Input.Tables (1) := (Offset => 0, Extent => 64); Input.Data := [others => 16#5A#];
   for Width in Integer_Width loop
      for Mode in Scenario loop
         Current := Mode;
         declare
            OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result;
            Node : Node_ID; Bound_Status : Bind_Status;
         begin
            Reset (A, OK); Check (OK); Timers := 0; Lookup_Test_Seen := 0;
            Input.Count := (if Current = Invalid_Backing then 0 else 1);
            Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Bind_Table_Field (A, Root, "FLD0", ((Table => 1, Extent => 64), 0,
              (if Current = Scalar_Field then 8 else 72)), Node, Bound_Status);
            Check (Bound_Status = Bound);
            Invoke (A, Input, 1, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            Check (Lookup_Test_Seen = 1 and then Timers = (if Current = Invalid_Backing then 0 else 1));
            case Current is
               when Wide_Field =>
                  Check (Result.Status = Object_Returned and then Result.Object.ID = 2);
                  Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), 2) = Bytes'(1 .. 9 => 16#5A#));
               when Scalar_Field => Check (Result.Status = Returned and then Result.Value = 16#5A#);
               when Invalid_Backing => Check (Result.Status = Unsupported_Value);
            end case;
            Check (Frame_Root_Count (A) = 0);
            Expected := [others => False]; Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("LOOKUP ROOTS" & Checks'Image & " boundary" & Lookup_Test_Checks'Image);
end Lookup_Root_Tests;
