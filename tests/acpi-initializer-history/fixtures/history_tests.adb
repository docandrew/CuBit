pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
with ACPI_Initializer_History;
procedure History_Tests is
   package H is new ACPI_Initializer_History (32);
   package Words is new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_64);
   use type H.Invocation_Number, H.Digest_Value;
   Run, Rejected : H.State;
   File : Ada.Text_IO.File_Type;
   Value : Interfaces.Unsigned_64;
   Item : H.Invocation;
   Expected : H.Digest_Value;
   Accepted : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end;
begin
   Check (H.Count (Run) = 0 and H.Complete (Run) and H.Digest (Run) = H.Empty_Digest);
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Ada.Command_Line.Argument (1));
   for I in H.Invocation_Count range 1 .. 32 loop
      Words.Get (File, Value); Item.Scope := H.Node_Position (Value);
      Words.Get (File, Value); Item.Method := H.Method_Position (Value);
      Words.Get (File, Value); Item.Access_Result := H.Outcome_Code (Value);
      Words.Get (File, Value); Item.Execution := H.Outcome_Code (Value);
      Words.Get (File, Value); Item.Reported_Charge := H.Fuel_Charge (Value);
      Words.Get (File, Expected);
      H.Append (Run, Item, Accepted);
      Check (Accepted and H.Count (Run) = I and H.Complete (Run));
      Check (H.Digest (Run) = Expected);
   end loop;
   Ada.Text_IO.Close (File);
   declare Prefix : constant H.State := Run; begin
      H.Append (Run, Item, Accepted);
      Check (not Accepted and not H.Complete (Run) and H.Count (Run) = H.Count (Prefix)
         and H.Digest (Run) = H.Digest (Prefix));
      for I in 1 .. 32 loop
         H.Append (Run, Item, Accepted);
         Check (not Accepted and not H.Complete (Run) and H.Count (Run) = H.Count (Prefix)
            and H.Digest (Run) = H.Digest (Prefix));
      end loop;
   end;
   H.Invalidate (Rejected);
   for I in 1 .. 32 loop
      H.Append (Rejected, Item, Accepted);
      Check (not Accepted and not H.Complete (Rejected) and H.Count (Rejected) = 0
         and H.Digest (Rejected) = H.Empty_Digest);
   end loop;
   Ada.Text_IO.Put_Line ("Initializer history checks" & Checks'Image);
end History_Tests;
