with Ada.Text_IO;
with CCL.Host_Values; use CCL.Host_Values;
with Result_Fixture;
procedure Result_Tests is
   Plain : Call_Result;
   Aliased_Item : aliased Call_Result;
   type Container is record
      Reply : Call_Result;
   end record;
   Nested : aliased Container;
   Replies : array (Value_Kind) of Call_Result;
   Checks : Natural := 0;
begin
   for Previous in Value_Kind loop
      for Kind in Value_Kind loop
         Result_Fixture.Reply (Previous, Plain);
         Aliased_Item := Plain;
         Nested.Reply := Plain;
         Replies := [others => Plain];
         pragma Assert (not Plain.Value'Constrained);
         pragma Assert (not Aliased_Item.Value'Constrained);
         pragma Assert (not Nested.Reply.Value'Constrained);
         Result_Fixture.Reply (Kind, Plain);
         Result_Fixture.Reply (Kind, Aliased_Item);
         Result_Fixture.Reply (Kind, Nested.Reply);
         Result_Fixture.Reply (Kind, Replies (Kind));
         pragma Assert (Plain.Success and Plain.Value.Kind = Kind);
         pragma Assert (Aliased_Item.Success and Aliased_Item.Value.Kind = Kind);
         pragma Assert (Nested.Reply.Success and Nested.Reply.Value.Kind = Kind);
         pragma Assert (Replies (Kind).Success and Replies (Kind).Value.Kind = Kind);
         Checks := Checks + 7;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Owned callback result: PASS" & Checks'Image & " checks");
end Result_Tests;
