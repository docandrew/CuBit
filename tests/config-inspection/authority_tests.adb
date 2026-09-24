with Ada.Text_IO;
with Config_Authority; use Config_Authority;

procedure Authority_Tests is
   State, Before : Authority_State;
   Narrow, Broad, Empty, Too_Many : Rule_Set;
   Saved : Rule_Set;
   Accepted : Boolean;
   Result : Install_Result;
begin
   pragma Assert (not Allows (State, 42, "desktop.theme", Read_Config));
   Append (Narrow, "desktop", Read_Only, Accepted);
   pragma Assert (Accepted);
   Install (State, 42, Narrow, Result);
   pragma Assert (Result = Installed);
   pragma Assert (Allows (State, 42, "desktop.theme", Read_Config));
   pragma Assert (not Allows (State, 42, "desktop.theme", Write_Config));
   pragma Assert (not Allows (State, 42, "desktop2.theme", Read_Config));
   pragma Assert (not Allows (State, 43, "desktop.theme", Read_Config));
   pragma Assert (not Allows (State, 42, "", Read_Config));
   Append (Broad, "", Read_Write, Accepted);
   pragma Assert (Accepted);
   Install (State, 43, Broad, Result);
   pragma Assert (Result = Installed and Allows (State, 43, "", Read_Config));
   Install (State, 42, Empty, Result);
   pragma Assert (Result = Installed and not Allows (State, 42, "desktop.theme", Read_Config));
   pragma Assert (Allows (State, 43, "desktop.theme", Write_Config));
   Install (State, 42, Narrow, Result);
   Revoke (State, 42);
   pragma Assert (not Has_Profile (State, 42));
   pragma Assert (Allows (State, 43, "desktop.theme", Write_Config));
   Revoke (State, 42); -- Repeated and unknown revocation are harmless.
   Revoke (State, 999);
   Revoke (State, 0);
   pragma Assert (Allows (State, 43, "desktop.theme", Write_Config));
   for I in 1 .. Maximum_Rules loop
      Append (Too_Many, "test.", Read_Only, Accepted);
      pragma Assert (Accepted);
   end loop;
   Saved := Too_Many;
   Append (Too_Many, "", Read_Write, Accepted);
   pragma Assert (not Accepted and Too_Many = Saved);
   Saved := Narrow;
   Append (Narrow, String'(1 .. Maximum_Scope + 1 => 'x'), Read_Write, Accepted);
   pragma Assert (not Accepted and Narrow = Saved);
   Before := State;
   Install (State, 0, Broad, Result);
   pragma Assert (Result = Invalid_Subject and State = Before);
   Install (State, Subject_ID'Last, Broad, Result);
   pragma Assert (Result = Invalid_Subject and State = Before);
   -- Existing profile 43 plus 31 new subjects fills the table.
   for I in 1 .. Maximum_Subjects - 1 loop
      Install (State, Subject_ID (I), Narrow, Result);
      pragma Assert (Result = Installed);
   end loop;
   Before := State;
   Install (State, 100, Broad, Result);
   pragma Assert (Result = Capacity_Exceeded and State = Before);
   -- Updating at capacity succeeds and narrows rather than merges grants.
   Install (State, 43, Narrow, Result);
   pragma Assert (Result = Installed and not Allows (State, 43, "desktop.theme", Write_Config));
   Revoke (State, 1);
   Install (State, 100, Broad, Result);
   pragma Assert (Result = Installed and Allows (State, 100, "anything", Write_Config));
   pragma Assert (not Allows (State, 1, "desktop.theme", Read_Config));
   Ada.Text_IO.Put_Line ("PASS Config authority: default deny, isolation, replacement, revocation and capacity");
end Authority_Tests;
