with Ada.Text_IO; use Ada.Text_IO;
with CCL.Catalog; use CCL.Catalog;
with CCL.Catalog.Completion; use CCL.Catalog.Completion;
with CCL.Interfaces.Clock;
with CCL.VM;
with CCL.Call_Context;
with Interfaces; use Interfaces;

procedure Main is
   Catalog, Empty : Interface_Catalog;
   Matches : Match_List;
   Error : Catalog_Error;
   Resolved : Resolved_Operation;
   Found : Boolean;
   procedure Check_Resolved is
   begin
      for I in 1 .. Matches.Count loop
         declare
            S : Suggestion renames Matches.Items (I);
         begin
            Resolve (Catalog, S.Name (1 .. S.Length), Resolved, Found);
            pragma Assert (Found and then Same_Operation (Resolved, S.Contract));
            pragma Assert (S.Contract.Import.Binding = 0);
         end;
      end loop;
   end Check_Resolved;
begin
   declare
      C : CCL.Call_Context.Context;
      procedure Expect (Source, Name : String; Arguments : Boolean := False) is
      begin
         CCL.Call_Context.Inspect (Source, Source'Length, C);
         pragma Assert (C.Available = (Name'Length > 0));
         pragma Assert (C.Name (1 .. C.Length) = Name);
         pragma Assert (C.Arguments_Started = Arguments);
      end Expect;
   begin
      Expect ("(clock.mon", "clock.mon");
      Expect ("( clock.monotonic-ms ", "clock.monotonic-ms", True);
      Expect ("(outer (inner 1) ", "outer", True);
      Expect ("(outer (inner ", "inner", True);
      Expect ("(clock.monotonic-ms)", "");
      Expect ("# (clock.mon", "");
      Expect ("(outer ""(ignored)"" ", "outer", True);
      Expect ("(outer ""unfinished", "");
      Expect ("(outer # (ignored" & ASCII.LF, "outer", True);
      Expect (") (clock.mon", "");
      Expect (String'(1 .. CCL.Call_Context.Maximum_Source => '('), "");
      Expect (String'(1 .. CCL.Call_Context.Maximum_Source + 1 => '('), "");
      Expect ("(" & String'(1 .. CCL.Call_Context.Maximum_Name + 1 => 'a'), "");
      declare S : constant String (201 .. 210) := "(clock.mon"; begin
         Expect (S, "clock.mon");
      end;
      declare S : constant String (Positive'Last - 9 .. Positive'Last) := "(clock.mon"; begin
         Expect (S, "clock.mon");
      end;
      for Offset in 0 .. 30 loop
         CCL.Call_Context.Inspect ("(outer (inner 1) ", Offset, C);
      end loop;
      CCL.Call_Context.Inspect ("(clock.mon", 11, C);
      pragma Assert (not C.Available);
   end;
   Initialize (Catalog); Initialize (Empty);
   Find (Empty, "", Matches);
   pragma Assert (Matches.Total = 0 and Matches.Count = 0);
   CCL.Interfaces.Clock.Publish (Catalog, Error);
   pragma Assert (Error = Catalog_Valid);
   Find (Catalog, "clock.mon", Matches);
   pragma Assert (Matches.Total = 1 and Matches.Count = 1);
   pragma Assert (Matches.Items (1).Name (1 .. Matches.Items (1).Length) = "clock.monotonic-ms");
   Check_Resolved;
   declare Shifted : constant String (101 .. 109) := "clock.mon"; begin
      Find (Catalog, Shifted, Matches);
      pragma Assert (Matches.Total = 1);
   end;
   Find (Catalog, "Clock", Matches);
   pragma Assert (Matches.Total = 0);
   Find (Catalog, String'(1 .. Maximum_Qualified_Name + 1 => 'a'), Matches);
   pragma Assert (Matches.Total = 0);
   Find (Empty, "clock", Matches);
   pragma Assert (Matches.Total = 0); -- no ambient catalog leakage
   Initialize (Catalog);
   for I in 1 .. MAX_INTERFACES loop
      declare
         D : Interface_Descriptor;
         Op : Operation_Descriptor;
         Name : String (1 .. MAX_NAME_LENGTH) := [others => 'a'];
      begin
         Name (1) := Character'Val (Character'Pos ('a') + I - 1);
         Define_Interface (Name, 1, 0, [1, 2, 3, Unsigned_64 (I)], D, Error);
         pragma Assert (Error = Catalog_Valid);
         for O in 1 .. MAX_OPERATIONS loop
            declare
               Op_Name : String (1 .. MAX_NAME_LENGTH) := [others => 'z'];
            begin
               Op_Name (1) := Character'Val (Character'Pos ('a') + O - 1);
               Define_Operation (Op_Name, 1,
                 (Argument => CCL.VM.Boolean_Value, Result => CCL.VM.Integer_Value,
                  others => <>), Op, Error);
               pragma Assert (Error = Catalog_Valid);
               Add_Operation (D, Op, Error);
               pragma Assert (Error = Catalog_Valid);
            end;
         end loop;
         Publish (Catalog, D, Error);
         pragma Assert (Error = Catalog_Valid);
      end;
   end loop;
   Find (Catalog, "", Matches);
   pragma Assert (Matches.Total = 256 and Matches.Count = Maximum_Suggestions);
   Check_Resolved;
   for C in Character range 'a' .. 'p' loop
      Find (Catalog, String'(1 => C), Matches);
      pragma Assert (Matches.Total = 16 and Matches.Count = 16);
      Check_Resolved;
      for I in 1 .. Matches.Count loop
         pragma Assert (Matches.Items (I).Length = Maximum_Qualified_Name);
      end loop;
   end loop;
   Put_Line ("PASS: bounded catalog completion, exact contracts, isolation, full capacity");
end Main;
