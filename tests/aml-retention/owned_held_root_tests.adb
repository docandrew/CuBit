with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Owned_Held_Root_Tests is
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
   Expired_Name : Boolean;
   function Segment (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Segment;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Bytes'(16#14#,Byte (Code'Length + 6)) & Segment (Name) & Bytes'(1 => 0) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Fixture : constant Bytes :=
     Method ("OUTR", Segment ("INNR") & Timer & Bytes'(16#A4#,0)) &
     Method ("INNR", Bytes'(1 => 16#A4#) & Buffer_Value) &
     Method ("OUTN", Segment ("RNAM") & Timer & Bytes'(16#A4#,0)) &
     Method ("RNAM", Bytes'(1 => 16#08#) & Segment ("TEMP") & Buffer_Value &
       Bytes'(16#A4#,16#71#) & Segment ("TEMP")) &
     Method ("FREF", [16#70#,1,16#60#,16#A4#,16#71#,16#60#]) &
     Method ("OUTF", Bytes'(1 => 16#A4#) & Segment ("FREF"));
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Store : constant AML_Objects.State := Value_Store (Snapshot (A));
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      Check (Calls = 1 and then Frame_Root_Count (A) = 1);
      Check (AML_Objects.Live_Count (Store) = 1 and then AML_Objects.Kind (Store, 1) = AML_Objects.Buffer_Object
        and then AML_Objects.Byte_Data (Store, 1) = Buffer_Value (5 .. 5));
      Expected := [others => False]; Expected (1) := not Expired_Name;
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
   end Capture;
begin
   for Width in Integer_Width loop
      for Mode in Boolean loop
         Expired_Name := Mode; Calls := 0;
         declare OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result; begin
            Reset (A, OK); Check (OK);
            Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Invoke (A, Input, (if Mode then 3 else 1), [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            Check (Result.Status = Returned and then Calls = 1 and then Frame_Root_Count (A) = 0);
            Expected := [others => False]; Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
            Invoke (A, Input, 6, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            Check (Result.Status = Missing_Result and then Frame_Root_Count (A) = 0);
            Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("OWNED HELD ROOTS" & Checks'Image);
end Owned_Held_Root_Tests;
