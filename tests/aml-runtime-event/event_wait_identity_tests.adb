with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Wait_Identity_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use type Bytes;
   use type Integer_Value;
   use type E.Execution_Status;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type N.State;
   A, Foreign : N.Owned.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Live, Stale, Other : AML_References.Reference;
   OK : Boolean;
   Loaded : N.Load_Status;
   Result : E.Execution_Result;
   Checks : Natural := 0;
   Code : constant Bytes := [16#5B#,16#02#] & Enc ("EV00") &
     Method ("SIGN", [16#5B#,16#24#] & Enc ("EV00") & [16#A4#,0]) &
     Block ([16#14#], Enc ("POLL") & [1,16#A4#,16#5B#,16#25#,16#68#,0]);
   procedure Check (Good : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label_Text; end if; end Check;
   function Locate (Arena : N.Owned.Arena; Part : String) return N.Node_ID is
      Found : constant N.Lookup_Result := N.Resolve (N.Owned.Snapshot (Arena), N.Root, Path (Part));
   begin Check (Found.Status = N.Found, "real node lookup"); return Found.Node; end Locate;
   procedure Poll (Ref : AML_References.Reference) is
      Args : E.Value_Arguments := [others => (E.Integer_Datum, 0, Ordinary_Integer)];
   begin
      Args (0) := (E.Reference_Datum, Ref);
      N.Owned.Invoke (A, Input, Locate (A, "POLL"), Args, 1, 1000, Result);
   end Poll;
begin
   for Width in Integer_Width loop
      N.Owned.Reset (A, OK); Check (OK, "reset owner");
      N.Owned.Reset (Foreign, OK); Check (OK, "reset foreign");
      N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "load owner");
      N.Owned.Load (Foreign, Code, Width, Loaded); Check (Loaded = N.Loaded, "load foreign");
      N.Owned.Make_Named_Identity (Foreign, Locate (Foreign, "EV00"), Other, OK);
      Check (OK, "foreign identity issued by actual owner");
      N.Owned.Make_Named_Identity (A, Locate (A, "EV00"), Stale, OK);
      Check (OK, "old owner identity");
      N.Owned.Reset (A, OK); Check (OK, "reset invalidates old identity");
      N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "reload owner");
      N.Owned.Make_Named_Identity (A, Locate (A, "EV00"), Live, OK);
      Check (OK and then N.Owned.Named_Identity_Matches (A, Live)
        and then not N.Owned.Named_Identity_Matches (A, Other)
        and then not N.Owned.Named_Identity_Matches (A, Stale), "current and rejected credentials");
      N.Owned.Invoke (A, Input, Locate (A, "SIGN"),
        [others => (E.Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Result);
      Check (Result.Status = E.Returned, "legitimate Event signaled");
      declare Before : constant N.State := N.Owned.Snapshot (A); begin
         Poll (Other); Check (Result.Status = E.Unsupported_Value, "foreign named Wait rejected");
         Check (N.Owned.Snapshot (A) = Before, "foreign rejection preserves event units and tree");
         Poll (Stale); Check (Result.Status = E.Unsupported_Value, "reset-stale named Wait rejected");
         Check (N.Owned.Snapshot (A) = Before, "stale rejection preserves event units and tree");
      end;
      Poll (Live); Check (Result.Status = E.Returned and then Result.Value = 0, "valid identity still consumes prior signal");
      Poll (Live); Check (Result.Status = E.Returned and then Result.Value =
        (if Width = Bits_32 then 16#FFFF_FFFF# else 16#FFFF_FFFF_FFFF_FFFF#), "valid signal consumed exactly once");
      Check (N.Owned.Frame_Root_Count (A) = 0, "no transient frame leak");
   end loop;
   Ada.Text_IO.Put_Line ("Event named Wait identity checks" & Checks'Image);
end Event_Wait_Identity_Tests;
