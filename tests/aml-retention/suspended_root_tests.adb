with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Suspended_Root_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Capture, Max_Frame_Roots => 3);
   use N; use N.Owned;
   use type Object_Root_Set;
   use type AML_Objects.Object_Kind;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Scratch : Root_Workspace;
   Keep, Expected : Object_Root_Set := [others => False];
   Root_Status : Root_Trace_Status;
   Calls, Checks : Natural := 0;
   type Scenario is (Actual_Arguments, Left_Operands, Failed_Callee);
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
   Buffer_One : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Buffer_Two : constant Bytes := [16#11#,4,16#0A#,1,16#66#];
   Fixture : constant Bytes :=
     Method ("OUTR", 0, Segment ("USE3") & Segment ("MK01") &
       Segment ("MK02") & Segment ("CHCK") & Timer & Bytes'(16#A4#,0)) &
     Method ("MK01", 0, Bytes'(1 => 16#A4#) & Buffer_One) &
     Method ("MK02", 0, Bytes'(1 => 16#A4#) & Buffer_Two) &
     Method ("CHCK", 0, Timer & Bytes'(16#A4#,0)) &
     Method ("USE3", 3, Timer & Bytes'(16#A4#,16#68#)) &
     Method ("OUTC", 0, Bytes'(16#A4#,16#73#) & Segment ("MK01") &
       Bytes'(1 => 16#73#) & Segment ("MK02") & Segment ("CHCK") & Bytes'(0,0)) &
     Method ("OUTE", 0, Bytes'(1 => 16#A4#) & Segment ("USE3") &
       Segment ("MK01") & Segment ("MK02") & Segment ("FAIL")) &
     Method ("FAIL", 0, Timer & Bytes'(1 => 16#A4#) & Segment ("NONE"));
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & " timer" & Calls'Image; end if; end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Store : constant AML_Objects.State := Value_Store (Snapshot (A));
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      Check (Calls <= (if Current = Actual_Arguments then 3 else 1)
        and then Frame_Root_Count (A) = (if Calls = 3 then 1 else 2));
      Check (AML_Objects.Live_Count (Store) = 2);
      for ID in 1 .. 2 loop
         Check (AML_Objects.Kind (Store, ID) = AML_Objects.Buffer_Object);
      end loop;
      -- At CHCK neither buffer is in any frame cell. The latest return bridge
      -- holds only MK02. MK01 is reachable solely through the suspended caller.
      Expected := [others => False]; Expected (1) := True; Expected (2) := Calls /= 3;
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
   end Capture;
begin
   for Width in Integer_Width loop
      for Mode in Scenario loop
      Current := Mode;
      declare OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result; begin
         Calls := 0;
         Reset (A, OK); Check (OK);
         Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
         Invoke (A, Input, (case Current is when Actual_Arguments => 1, when Left_Operands => 6, when Failed_Callee => 7), [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
         Check (Calls = (if Current = Actual_Arguments then 3 else 1) and then Frame_Root_Count (A) = 0);
         Check (Result.Status = (case Current is when Actual_Arguments => Returned,
           when Left_Operands => Object_Returned, when Failed_Callee => Unknown_Name));
         Expected := [others => False]; Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
         Check (Root_Status = Roots_Traced and then Keep = Expected);
      end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SUSPENDED ROOTS" & Checks'Image);
end Suspended_Root_Tests;
