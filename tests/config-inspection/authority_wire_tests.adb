with Ada.Text_IO;
with Config_Authority; use Config_Authority;
with Config_Authority_Wire;

procedure Authority_Wire_Tests is
   package Wire renames Config_Authority_Wire;
   Item : String (1 .. Wire.Entry_Bytes) := [others => Character'Val (0)];
   Rules : Rule_Set;
   State : Authority_State;
   Installed_As : Install_Result;
   Accepted : Boolean;
   Checks : Natural := 0;

   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;

   procedure Reject (Data : String) is
   begin
      -- Begin with broad authority: failure must overwrite the output with
      -- empty rules, not leave broad or partially decoded rules behind.
      Rules := Empty_Rules;
      Append (Rules, "", Read_Write, Accepted); Check (Accepted);
      Wire.Decode (Data, Rules, Accepted);
      Check (not Accepted and Rules = Empty_Rules);
   end Reject;
begin
   Reject ("");
   Reject (String'(1 .. Wire.Entry_Bytes - 1 => Character'Val (0)));
   Reject (String'(1 .. Wire.Entry_Bytes + 1 => Character'Val (0)));
   Reject (String'(1 .. Wire.Maximum_Bytes + Wire.Entry_Bytes => Character'Val (0)));

   -- All possible wire rights and lengths, including no rights and explicit
   -- zero-length wildcard scopes. Empty *input* never becomes a wildcard.
   Item (9 .. Item'Last) := [others => 'x'];
   for Mask in 0 .. 255 loop
      for Length in 0 .. 255 loop
         Item (1) := Character'Val (Mask);
         Item (2) := Character'Val (Length);
         Wire.Decode (Item, Rules, Accepted);
         Check (Accepted = (Mask <= 3 and Length <= Maximum_Scope));
         if Accepted then
            Install (State, 42, Rules, Installed_As); Check (Installed_As = Installed);
            declare
               Scope : constant String := Item (9 .. 8 + Length);
            begin
               Check (Allows (State, 42, Scope, Read_Config) = (Mask mod 2 = 1));
               Check (Allows (State, 42, Scope, Write_Config) = (Mask >= 2));
               Check (not Allows (State, 42, Scope, Activate_Config));
               Check (not Allows (State, 43, Scope, Read_Config));
            end;
         else
            Check (Rules = Empty_Rules);
         end if;
      end loop;
   end loop;

   Item := [others => Character'Val (0)];
   Item (1) := Character'Val (1);
   Item (2) := Character'Val (7);
   Item (9 .. 15) := "desktop";
   for Byte in 3 .. 8 loop
      for Value in 1 .. 255 loop
         Item (Byte) := Character'Val (Value);
         Reject (Item);
      end loop;
      Item (Byte) := Character'Val (0);
   end loop;
   declare
      Shifted : String (500 .. 500 + Wire.Entry_Bytes - 1) := Item;
      Extreme : constant String (Integer'Last - (Wire.Entry_Bytes - 1) .. Integer'Last) := Item;
      Expected : Rule_Set;
      Entries : String (1 .. Wire.Maximum_Bytes);
   begin
      Wire.Decode (Item, Expected, Accepted); Check (Accepted);
      Wire.Decode (Shifted, Rules, Accepted); Check (Accepted and Rules = Expected);
      Wire.Decode (Extreme, Rules, Accepted); Check (Accepted and Rules = Expected);
      Shifted (Shifted'First) := Character'Val (4); Reject (Shifted);
      for I in 0 .. Maximum_Rules - 1 loop
         Entries (I * Wire.Entry_Bytes + 1 .. (I + 1) * Wire.Entry_Bytes) := Item;
      end loop;
      Wire.Decode (Entries, Rules, Accepted); Check (Accepted);
      for I in 0 .. Maximum_Rules - 1 loop
         Entries (I * Wire.Entry_Bytes + 1) := Character'Val (4);
         Reject (Entries);
         Entries (I * Wire.Entry_Bytes + 1) := Item (1);
      end loop;
      -- A malformed later entry cannot publish an earlier valid prefix.
      Install (State, 42, Expected, Installed_As); Check (Installed_As = Installed);
      Check (Allows (State, 42, "desktop.theme", Read_Config));
      Check (not Allows (State, 42, "desktop2.theme", Read_Config));
      Check (not Allows (State, 42, "desktop.theme", Write_Config));
   end;
   Ada.Text_IO.Put_Line ("Config authority wire: PASS" & Checks'Image & " checks");
end Authority_Wire_Tests;
