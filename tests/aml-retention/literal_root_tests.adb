with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Literal_Root_Tests is
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
   type Scenario is (Valid_Arguments, Rejected_Package, Discarded_Concatenation);
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
   String_Value : constant Bytes := [16#0D#,16#41#,0];
   Good_Package : constant Bytes := [16#12#,2,0];
   Bad_Package : constant Bytes := [16#12#,3,1,16#AA#];
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
   begin
      Value := 0; Available := True; Timers := Timers + 1;
      Check (Timers = 1 and then Literal_Test_Seen = (if Current = Discarded_Concatenation then 2 else 3) and then Frame_Root_Count (A) = 1);
      -- USE3's consumed return remains held; discarded string/package roots clear.
      Expected := [others => False]; Expected (1) := Current /= Discarded_Concatenation;
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
   end Capture;
begin
   for Width in Integer_Width loop
      for Mode in Scenario loop
         Current := Mode;
         declare
            Fixture : constant Bytes :=
              (if Current = Discarded_Concatenation then
                 Method ("OUTR", 0, Bytes'(1 => 16#73#) & Buffer_Value & String_Value &
                   Bytes'(1 => 0) & Timer & Bytes'(16#A4#,0))
               else Method ("OUTR", 0, Segment ("USE3") & Buffer_Value &
              String_Value & (if Current = Rejected_Package then Bad_Package else Good_Package) & Timer & Bytes'(16#A4#,0)) &
              Method ("USE3", 3, Bytes'(16#A4#,16#68#)));
            OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result;
         begin
            Reset (A, OK); Check (OK); Timers := 0; Literal_Test_Seen := 0;
            Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Invoke (A, Input, 1, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            Check (Result.Status = (case Current is when Rejected_Package => Unsupported_Value,
              when Valid_Arguments | Discarded_Concatenation => Returned));
            Check (Literal_Test_Seen = (if Current = Discarded_Concatenation then 2 else 3)
              and then Timers = (if Current = Rejected_Package then 0 else 1));
            Check (Frame_Root_Count (A) = 0);
            Expected := [others => False]; Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("LITERAL ROOTS" & Checks'Image & " boundary" & Literal_Test_Checks'Image);
end Literal_Root_Tests;
