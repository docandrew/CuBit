with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Integers;
with AML_Namespace;
with AML_Table_Backing;
with Event_Count_Data;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Operation_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 64, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Audit_Value;
   use type N.Load_Status;
   use type E.Execution_Status;
   A, B : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Initialized : N.Initialization_Report;
   Result : C.Result;
   Desc : C.Value_Description;
   Element, Foreign, Stale, Local_Event : C.Value_Handle;
   Before : C.Audit_Value;
   Checks : Natural := 0;
   Event_Opcode : constant Bytes := [16#5B#, 16#02#];
   Signal_Opcode : constant Bytes := [16#5B#,16#24#];
   Wait_Opcode : constant Bytes := [16#5B#,16#25#];
   Extra : constant Bytes := Method ("MAKE", Event_Opcode & Enc ("TEMP") & [16#A4#] & Enc ("TEMP"))
     & Block ([16#14#], Enc ("PASS") & [1] & Signal_Opcode & [16#68#,16#A4#] & Wait_Opcode & [16#68#,0])
     & Method ("FORE", [16#A4#] & Wait_Opcode & Enc ("EV00") & [16#0B#,16#FF#,16#FF#]);
   procedure Check (Good : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label_Text; end if; end Check;
   procedure Load_Arena (Arena : in out C.Arena; Table : Bytes; Width : Integer_Width) is
   begin
      C.Reset (Arena, Status); Check (Status = C.Available, "reset");
      C.Load (Arena, Table & Extra, Width, Loaded, Status);
      Check (Status = C.Available and then Loaded = N.Loaded, "load");
      C.Seal (Arena, Initialized, Status); Check (Status = C.Available, "seal");
   end Load_Arena;
   procedure Invoke (Arena : in out C.Arena; Name : String;
     Args : C.Arguments := [others => (C.Immediate_Argument, 0)]; Count : C.Argument_Count := 0) is
      Node : N.Node_ID;
   begin
      C.Find_Child (Arena, N.Root, Name, Node, Status); Check (Status = C.Available and then Node /= N.Root, "method");
      C.Invoke (Arena, Input, Node, Args, Count, 10000, Result, Status);
   end Invoke;
   procedure Run (Table : Bytes; Width : Integer_Width) is
      Timeout : constant Integer_Value := AML_Integers.Normalize (Integer_Value'Last, Width);
   begin
      Load_Arena (A, Table, Width); Load_Arena (B, Table, Width);
      for Repeat in 1 .. 2 loop
         Invoke (A, "CNT0"); Check (Status = C.Available and then Result.Status = E.Object_Returned, "counted wait package");
         C.Describe (A, Result.Handle, Desc, Status);
         Check (Status = C.Available and then Desc.Kind = C.Package_Description and then Desc.Length = 3, "three waits");
         for I in 0 .. 2 loop
            C.Read_Element (A, Result.Handle, I, Element, Status); Check (Status = C.Available, "element");
            C.Describe (A, Element, Desc, Status);
            Check (Status = C.Available and then Desc.Kind = C.Integer_Description
              and then Desc.Number = (if I = 2 then Timeout else 0), "two consumed units then timeout");
            C.Release (A, Element, Status); Check (Status = C.Available, "release element");
         end loop;
         C.Release (A, Result.Handle, Status); Check (Status = C.Available, "release package");
         Invoke (A, "RST0"); Check (Status = C.Available and then Result.Status = E.Returned and then Result.Number = Timeout, "reset drains all units");
         Invoke (A, "TRUN"); Check (Status = C.Available and then Result.Status = E.Returned and then Result.Number = Timeout, "65536 becomes poll");
         Invoke (A, "TIME"); Check (Status = C.Available and then Result.Status = E.Unsupported, "default finite wait unavailable");
         Invoke (A, "FORE"); Check (Status = C.Available and then Result.Status = E.Unsupported, "forever never fabricated timeout");
      end loop;
      Load_Arena (A, Table, Width); Load_Arena (B, Table, Width);
      Invoke (A, "MAKE"); Check (Status = C.Available and then Result.Status = E.Object_Returned, "local counterpart event"); Local_Event := Result.Handle;
      Invoke (B, "MAKE"); Check (Status = C.Available and then Result.Status = E.Object_Returned, "foreign source event"); Foreign := Result.Handle;
      Before := C.Audit (A);
      Invoke (A, "PASS", [0 => (C.Retained_Argument, Foreign), others => (C.Immediate_Argument, 0)], 1);
      Check (Status = C.Invalid_Value, "foreign event handle rejected");
      Check (C.Audit (A) = Before, "foreign rejection preserves target arena");
      Invoke (B, "PASS", [0 => (C.Retained_Argument, Foreign), others => (C.Immediate_Argument, 0)], 1);
      Check (Status = C.Available and then Result.Status = E.Returned and then Result.Number = 0, "genuine owner event still operates");
      Stale := Foreign; C.Release (B, Foreign, Status); Check (Status = C.Available, "release source event");
      Before := C.Audit (B);
      Invoke (B, "PASS", [0 => (C.Retained_Argument, Stale), others => (C.Immediate_Argument, 0)], 1);
      Check (Status = C.Invalid_Value and then C.Audit (B) = Before, "stale event argument rejected unchanged");
      C.Release (A, Local_Event, Status); Check (Status = C.Available, "release local counterpart");
      Check (C.Retained_Count (A) = 0 and then C.Retained_Count (B) = 0, "no retained roots");
   end Run;
begin
   C.Initialize (A, Status); Check (Status = C.Available, "init A");
   C.Initialize (B, Status); Check (Status = C.Available, "init B");
   Run (Event_Count_Data.Revision_1, Bits_32); Run (Event_Count_Data.Revision_2, Bits_64);
   Ada.Text_IO.Put_Line ("Event operation checks" & Checks'Image);
end Event_Operation_Tests;
