with Ada.Text_IO;
with CCL.Sessions; use CCL.Sessions;
with CCL.Language; use CCL.Language;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with Interfaces; use Interfaces;

procedure Main is
   A, B : Session;
   Outcome : Interpretation_Result;
   Item : Submission;
   Found : Boolean;
   Catalog : CCL.Catalog.Interface_Catalog;
   Error : CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Catalog_Error;

   procedure Check (Source : String; Kind : Static_Type; Image : String) is
   begin
      Submit (A, Source, Default_Fuel, Outcome);
      pragma Assert (Outcome.Status = Succeeded);
      pragma Assert (Result_Type (Outcome) = Kind);
      pragma Assert (Result_Image (Outcome) = Image);
      pragma Assert (Outcome.Fuel_Remaining <= Default_Fuel);
   end Check;
begin
   Initialize (A);
   Initialize (B);
   pragma Assert (Length (A) = 0);
   Recall (A, 1, Item, Found);
   pragma Assert (not Found and Item.Source_Length = 0);
   Check ("(+ 20 22)", Integer_Type, "Integer: 42");
   Check ("true", Boolean_Type, "Boolean: true");
   Check ("(to-string 42)", String_Type, "String: 42");
   Check ("(at ""abc"" 2)", Character_Type, "Character: b");
   Check ("(let ((x 9)) (* x x))", Integer_Type, "Integer: 81");
   Submit (A, "x", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Type_Check_Failed); -- no persistent binding
   pragma Assert (Length (B) = 0); -- isolated session
   Submit (A, "(+ 1", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Parse_Failed and Outcome.Diagnostic_Position > 0);
   Submit (A, "(/ 1 0)", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Evaluation_Division_By_Zero);
   Submit (A, "(+ 1 2)", 0, Outcome);
   pragma Assert (Outcome.Status = Evaluation_Fuel_Exhausted and Outcome.Fuel_Remaining = 0);
   Submit (A, "(+ 1 2)", 1, Outcome);
   pragma Assert (Outcome.Fuel_Remaining <= 1);
   Submit (A, String'(1 .. MAX_SOURCE_LENGTH + 1 => '1'), Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Parse_Failed and Outcome.Diagnostic = Source_Too_Long);
   Recall (A, Length (A), Item, Found);
   pragma Assert (Found and Item.Source_Truncated and Item.Source_Length = MAX_SOURCE_LENGTH);
   declare
      Offset_Source : constant String (101 .. 109) := "(+ 20 22)";
   begin
      Check (Offset_Source, Integer_Type, "Integer: 42");
   end;
   Clear_History (A);
   pragma Assert (Length (A) = 0);
   --  Several complete ring wraps, with oldest-to-newest inspection.
   for I in 1 .. Maximum_History * 3 loop
      Submit (A, Integer'Image (I), Default_Fuel, Outcome);
      pragma Assert (Outcome.Status = Succeeded);
   end loop;
   pragma Assert (Length (A) = Maximum_History);
   for I in History_Index loop
      Recall (A, I, Item, Found);
      pragma Assert (Found and Item.Outcome.Result_Value.Integer = Integer_64 (I + Maximum_History * 2));
   end loop;
   CCL.Catalog.Initialize (Catalog);
   CCL.Interfaces.Clock.Publish (Catalog, Error);
   pragma Assert (Error = CCL.Catalog.Catalog_Valid);
   Initialize (A, Catalog);
   Submit (A, "(clock.monotonic-ms)", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Host_Import_Required);
   Submit (B, "(clock.monotonic-ms)", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status /= Host_Import_Required and Outcome.Status /= Succeeded);
   Clear_History (A); -- does not pretend to revoke/change catalog visibility
   Submit (A, "(clock.monotonic-ms)", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status = Host_Import_Required);
   Initialize (A); -- explicit new empty discovery context
   Submit (A, "(clock.monotonic-ms)", Default_Fuel, Outcome);
   pragma Assert (Outcome.Status /= Host_Import_Required and Outcome.Status /= Succeeded);
   Ada.Text_IO.Put_Line ("CCL sessions: typed results, isolation, fuel, history, discovery boundaries PASS");
end Main;
