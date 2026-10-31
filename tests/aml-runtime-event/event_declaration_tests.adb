with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Declaration_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type E.Execution_Status;
   use type N.Load_Status;
   use type N.State;
   A : N.Owned.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Outcome : E.Execution_Result;
   Loaded : N.Load_Status;
   Good : Boolean;
   Before : N.State;
   Checks : Natural := 0;
   Event_Opcode : constant Bytes := [16#5B#, 16#02#];
   function Declare_Event (Name : String) return Bytes is (Event_Opcode & Enc (Name));
   function Type_Of (Name : String) return Bytes is ([16#A4#, 16#8E#] & Enc (Name));
   Code : constant Bytes := Declare_Event ("EV00")
     & Method ("DECL", Declare_Event ("TEMP") & Type_Of ("TEMP"))
     & Method ("ROOT", Event_Opcode & [16#5C#] & Enc ("TMP1") & Type_Of ("TMP1"))
     & Method ("CLSH", Event_Opcode & [16#5C#] & Enc ("EV00") & [16#A4#, 0]);
   procedure Check (Value : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Label_Text; end if; end Check;
   procedure Invoke (Name : String) is
      Node : constant N.Node_ID := N.Child (N.Owned.Snapshot (A), N.Root, Name);
   begin
      Check (Node /= N.Root, "method present");
      N.Owned.Invoke (A, Input, Node, [others => (E.Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Outcome);
   end Invoke;
begin
   for Width in Integer_Width loop
      N.Owned.Reset (A, Good); Check (Good, "reset");
      N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "load");
      for Repeat in 1 .. 3 loop
         Invoke ("DECL"); Check (Outcome.Status = E.Returned and then Outcome.Value = 7, "local event type");
         Check (N.Count (N.Owned.Snapshot (A)) = 4, "local event cleaned");
         Invoke ("ROOT"); Check (Outcome.Status = E.Returned and then Outcome.Value = 7, "root path event type");
         Check (N.Child (N.Owned.Snapshot (A), N.Root, "TMP1") = N.Root, "root event method lifetime");
         Before := N.Owned.Snapshot (A);
         Invoke ("CLSH"); Check (Outcome.Status = E.Duplicate_Name, "duplicate event rejected");
         Check (N.Owned.Snapshot (A) = Before, "collision preserves full namespace and authority");
      end loop;
      for I in 1 .. 28 loop
         declare Name : constant String := "N0" & Character'Val (Character'Pos ('A') + (I - 1) / 26)
           & Character'Val (Character'Pos ('A') + (I - 1) mod 26);
         begin
            N.Owned.Load (A, [16#08#] & Enc (Name) & [0], Width, Loaded);
            Check (Loaded = N.Loaded, "fill bounded namespace");
         end;
      end loop;
      Before := N.Owned.Snapshot (A);
      Invoke ("DECL"); Check (Outcome.Status = E.Namespace_Limit, "event capacity rejects");
      Check (N.Owned.Snapshot (A) = Before, "failed insert preserves complete namespace");
   end loop;
   Ada.Text_IO.Put_Line ("Event declaration checks" & Checks'Image);
end Event_Declaration_Tests;
