with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capabilities.Endpoint_Disposal;
procedure Disposal_Tests is
   package D renames Capabilities.Endpoint_Disposal;
   Base : constant Capability :=
     (CAP_ENDPOINT, READ_ONLY, 123, (31, 0), 7);
   Table, Before : CapabilityTable;
   Installed, Expected : Capability;
   Removed : Boolean;
   Cases : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "endpoint disposal"; end if;
   end Check;
begin
   for Slot in CapabilitySlot loop
      for Fault in 0 .. 15 loop
         Installed := Base;
         Expected := Base;
         case Fault is
            when 0 => null;
            when 1 => Installed.capType := CAP_PROCESS;
            when 2 => Installed.object.ref := 32;
            when 3 => Installed.object.param := 1;
            when 4 => Installed.gen := 8;
            when 5 => Installed.authorityTag := 124;
            when 6 => Installed.rights := READ_WRITE;
            when 7 => Installed := NULL_CAPABILITY;
            when 8 => Expected.capType := CAP_PROCESS; Installed := Expected;
            when 9 => Expected.gen := 0; Installed := Expected;
            when 10 => Expected.object.ref := 0; Installed := Expected;
            when 11 => Expected.authorityTag := 0; Installed := Expected;
            when 12 => Expected := NULL_CAPABILITY; Installed := Expected;
            when 13 => Expected.rights := READ_WRITE; Installed := Expected;
            when 14 => Expected.rights (RIGHT_GRANT) := True; Installed := Expected;
            when 15 => Expected.gen := Generation'Last; Installed := Expected;
         end case;
         -- Distinct live sentinels make accidental neighbouring clears visible.
         Table := (others => (CAP_NOTIFICATION, READ_WRITE, 999, (17, 8), 12));
         Table (Slot) := Installed;
         Before := Table;
         D.Clear_If_Matching (Table, Slot, Expected, Removed);
         Check (Removed = (Fault in 0 | 13 | 14 | 15));
         for I in CapabilitySlot loop
            Check (Table (I) =
              (if I = Slot and Removed then NULL_CAPABILITY else Before (I)));
         end loop;
         if Removed then
            Before := Table;
            D.Clear_If_Matching (Table, Slot, Expected, Removed);
            Check (not Removed and Table = Before);
            -- Late cleanup must not erase a replacement with a fresh tag.
            Installed := Expected;
            Installed.authorityTag := Expected.authorityTag + 1;
            Table (Slot) := Installed;
            Before := Table;
            D.Clear_If_Matching (Table, Slot, Expected, Removed);
            Check (not Removed and Table = Before);
         end if;
         Cases := Cases + 1;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS endpoint conditional disposal cases=" & Cases'Image);
end Disposal_Tests;
