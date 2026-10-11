with Ada.Text_IO;
with Config_Authority; use Config_Authority;
with CuBit.Process_IDs; use CuBit.Process_IDs;

procedure Authority_Tests is
   --  Subjects: well-formed identities (slot N, first life).
   function P (Slot : Slot_Number) return Process_ID is (To_Process_ID (Slot, 1));
   State, Before : Authority_State;
   Narrow, Broad, Empty, Too_Many : Rule_Set;
   Saved : Rule_Set;
   Accepted : Boolean;
   Result : Install_Result;
begin
   pragma Assert (not Allows (State, P (42), "desktop.theme", Read_Config));
   Append (Narrow, "desktop", Read_Only, Accepted);
   pragma Assert (Accepted);
   Install (State, P (42), Narrow, Result);
   pragma Assert (Result = Installed);
   pragma Assert (Allows (State, P (42), "desktop.theme", Read_Config));
   pragma Assert (not Allows (State, P (42), "desktop.theme", Write_Config));
   pragma Assert (not Allows (State, P (42), "desktop2.theme", Read_Config));
   pragma Assert (not Allows (State, P (43), "desktop.theme", Read_Config));
   pragma Assert (not Allows (State, P (42), "", Read_Config));
   Append (Broad, "", Read_Write, Accepted);
   pragma Assert (Accepted);
   Install (State, P (43), Broad, Result);
   pragma Assert (Result = Installed and Allows (State, P (43), "", Read_Config));
   Install (State, P (42), Empty, Result);
   pragma Assert (Result = Installed and not Allows (State, P (42), "desktop.theme", Read_Config));
   pragma Assert (Allows (State, P (43), "desktop.theme", Write_Config));
   Install (State, P (42), Narrow, Result);
   Revoke (State, P (42));
   pragma Assert (not Has_Profile (State, P (42)));
   pragma Assert (Allows (State, P (43), "desktop.theme", Write_Config));
   Revoke (State, P (42)); -- Repeated and unknown revocation are harmless.
   Revoke (State, P (999));
   Revoke (State, No_Subject);
   pragma Assert (Allows (State, P (43), "desktop.theme", Write_Config));
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
   Install (State, No_Subject, Broad, Result);
   pragma Assert (Result = Invalid_Subject and State = Before);
   Install (State, To_Process_ID (Slot => 0, Generation => 1), Broad, Result);
   pragma Assert (Result = Invalid_Subject and State = Before);
   -- Existing profile 43 plus 31 new subjects fills the table.
   for I in 1 .. Maximum_Subjects - 1 loop
      Install (State, P (Slot_Number (I)), Narrow, Result);
      pragma Assert (Result = Installed);
   end loop;
   Before := State;
   Install (State, P (100), Broad, Result);
   pragma Assert (Result = Capacity_Exceeded and State = Before);
   -- Updating at capacity succeeds and narrows rather than merges grants.
   Install (State, P (43), Narrow, Result);
   pragma Assert (Result = Installed and not Allows (State, P (43), "desktop.theme", Write_Config));
   Revoke (State, P (1));
   Install (State, P (100), Broad, Result);
   pragma Assert (Result = Installed and Allows (State, P (100), "anything", Write_Config));
   pragma Assert (not Allows (State, P (1), "desktop.theme", Read_Config));
   Ada.Text_IO.Put_Line ("PASS Config authority: default deny, isolation, replacement, revocation and capacity");
end Authority_Tests;
