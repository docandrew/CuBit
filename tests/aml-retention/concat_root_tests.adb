with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Concat_Root_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (16, Capture, Max_Frame_Roots => 2);
   use N; use N.Owned;
   use type Object_Root_Set;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Scratch : Root_Workspace;
   Keep, Expected : Object_Root_Set := [others => False];
   Root_Status : Root_Trace_Status;
   Timers, Checks : Natural := 0;
   type Scenario is (Discard_Result, Local_Cell, Package_Cell);
   Current : Scenario;
   function Segment (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Segment;
   function Method (Code : Bytes) return Bytes is
     (Bytes'(16#14#,Byte (Code'Length + 6)) & Segment ("OUTR") & Bytes'(1 => 0) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   String_Value : constant Bytes := [16#0D#,16#41#,0];
   Package_Target : constant Bytes := [16#88#,16#12#,3,1,0,0,0];
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Current'Image & Checks'Image; end if; end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
   begin
      Value := 0; Available := True; Timers := Timers + 1;
      Check (Timers = 1 and then Concat_Test_Seen = 1 and then Frame_Root_Count (A) = 1);
      Expected := [others => False]; Expected (3) := Current = Local_Cell;
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
   end Capture;
begin
   for Width in Integer_Width loop
      for Mode in Scenario loop
         Current := Mode;
         declare
            Target : constant Bytes := (case Current is when Discard_Result => Bytes'(1 => 0),
              when Local_Cell => Bytes'(1 => 16#60#), when Package_Cell => Package_Target);
            Fixture : constant Bytes := Method (Bytes'(1 => 16#73#) & Buffer_Value & String_Value & Target &
              Timer & Bytes'(16#A4#,0));
            OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result;
         begin
            Reset (A, OK); Check (OK); Timers := 0; Concat_Test_Seen := 0;
            Concat_Test_Expected := [others => False];
            for ID in 1 .. (if Current = Package_Cell then 4 else 2) loop Concat_Test_Expected (ID) := True; end loop;
            Concat_Test_Reference := Current = Package_Cell;
            Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Invoke (A, Input, 1, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            Check (Result.Status = Returned and then Timers = 1 and then Concat_Test_Seen = 1);
            Check (Frame_Root_Count (A) = 0);
            Expected := [others => False]; Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("CONCAT ROOTS" & Checks'Image & " boundary" & Concat_Test_Checks'Image);
end Concat_Root_Tests;
