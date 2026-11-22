with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Fatal;
with AML_Names;
with AML_Namespace;
with AML_Table_Backing;
with Interfaces;
with Root_Test_Support; use Root_Test_Support;
procedure Fatal_Authority_Tests is
   use type Bytes;
   use type Integer_Value;
   use type Interfaces.Unsigned_32;
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 16, Perform_Delay => AML_Delays.Unavailable_Provider);
   package O renames N.Owned;
   use type E.Execution_Status;
   use type N.Load_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   type Empty_Context is null record;
   function Valid (Environment : Empty_Context) return Boolean is
      pragma Unreferenced (Environment);
   begin return True; end Valid;
   function Lookup (Environment : Empty_Context; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width) return E.Binding_Result is
      pragma Unreferenced (Environment, Scope, Path, Width);
   begin return (Status => E.Failed_Binding, Failure => E.Fatal_Requested); end Lookup;
   function Method_Data (Environment : Empty_Context; ID : Natural) return E.Method_Definition is
      pragma Unreferenced (Environment, ID);
   begin return (Exists => False, Length => 0); end Method_Data;
   function Run is new E.Run_Bound (Empty_Context, Valid, Lookup, Method_Data);
   Fixed : constant Bytes := [16#5B#,16#32#,16#AA#,16#44#,16#33#,16#22#,16#11#];
   Code : constant Bytes := Method ("FATL", Fixed & [16#0A#,16#55#])
     & Method ("GOOD", [16#A4#,1]);
   Owner : O.Arena;
   Input : aliased AML_Table_Backing.State (1,1);
   Loaded : N.Load_Status;
   Initialized : N.Initialization_Report;
   Success : Boolean;
   Result : E.Execution_Result;
   procedure Invoke (Name : String) is
      Node : constant N.Node_ID := N.Child (O.Snapshot (Owner), N.Root, Name);
   begin
      Check (Node /= N.Root, "actual owned method");
      O.Invoke (Owner, Input, Node, [others => (E.Integer_Datum,0,Ordinary_Integer)],0,100,Result);
      Check (O.Frame_Root_Count (Owner) = 0, "owned frames retired");
   end Invoke;
begin
   for Width in Integer_Width loop
      declare
         Forged : constant E.Execution_Result := Run ([16#A4#] & Enc ("FAKE"), Width,
           [others => 0],0,100,(null record),0);
         In_Argument : constant E.Execution_Result := Run (Fixed & Enc ("FAKE"), Width,
           [others => 0],0,100,(null record),0);
      begin
         Check (Forged.Status = E.Unsupported_Value, "status-only callback cannot forge typed fatal");
         Check (In_Argument.Status = E.Unsupported_Value, "failed argument cannot synthesize outer fatal");
      end;
      O.Reset (Owner, Success); Check (Success, "owned reset");
      Check (not O.Fatal_Observation (Owner).Present, "initial observation empty");
      O.Load (Owner, Code, Width, Loaded); Check (Loaded = N.Loaded, "owned load");
      O.Initialize_Members (Owner, Initialized);
      Invoke ("FATL"); Check (Result.Status = E.Fatal_Requested, "owned fatal result");
      declare Seen : constant AML_Fatal.Observation := O.Fatal_Observation (Owner); begin
         Check (Seen.Present, "owned copied observation");
         if Seen.Present then
            Check (Seen.Item.Evaluated_Argument = 16#55#, "owned raw argument");
            Check (Seen.Item.Literal.Code = 16#11223344#, "owned literal code");
         end if;
      end;
      Invoke ("GOOD"); Check (Result.Status = E.Returned, "owned recovery");
      Check (not O.Fatal_Observation (Owner).Present, "new admitted invocation clears old observation");
      Invoke ("FATL");
      O.Reset (Owner, Success); Check (Success, "reset after fatal");
      Check (not O.Fatal_Observation (Owner).Present, "reset clears observation");
   end loop;
   Ada.Text_IO.Put_Line ("Fatal authority checks" & Checks'Image);
end Fatal_Authority_Tests;
