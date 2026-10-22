with System;
with CuBit.Config;
with CuBit.UI.Clipboard;
with Calc_Settings;

package body Calc_Host_Native is
   use type CuBit.UI.Clipboard.Outcome;
   use type CuBit.Config.ConfigStatus;
   use type System.Address;

   procedure Load_Options (H : in out Host; Item : out CCL_Calc_Options.Choice; Found : out Boolean) is
      pragma Unreferenced (H);
      Value : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
   begin
      Item := (others => <>);
      Found := False;
      CuBit.Config.get (Calc_Settings.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address
        and then Length in 1 .. Calc_Settings.Maximum_Source
      then
         declare
            --  Borrowed from Config's buffer: copied before the next call.
            Stored : String (1 .. Length) with Import, Address => Value;
            Source : constant String := Stored;
         begin
            Calc_Settings.Decode (Source, Item, Found);
         end;
      end if;
   end Load_Options;

   procedure Save_Options (H : in out Host; Item : CCL_Calc_Options.Choice) is
      pragma Unreferenced (H);
      Source : constant String := Calc_Settings.Source (Item);
      Status : CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.set (Calc_Settings.Config_Key, Source'Address, Source'Length, Status);
   end Save_Options;

   procedure Set_Surface (H : in out Host; Surface : Interfaces.Unsigned_64) is
   begin
      H.Surface := Surface;
   end Set_Surface;

   procedure Copy (H : in out Host; Text : String) is
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      CuBit.UI.Clipboard.Copy_Text (H.Surface, Text, CuBit.UI.Clipboard.Ordinary, Result);
   end Copy;

   procedure Request_Paste (H : in out Host) is
      Last : Natural;
      Kind : CuBit.UI.Clipboard.Sensitivity;
      Result : CuBit.UI.Clipboard.Outcome;
   begin
      CuBit.UI.Clipboard.Paste_Text (H.Surface, CuBit.UI.Clipboard.Ordinary, H.Paste, Last, Kind, Result);
      H.Paste_Ready := True;
      H.Paste_Length := (if Result = CuBit.UI.Clipboard.Done then Last - H.Paste'First + 1 else 0);
   end Request_Paste;

   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean) is
   begin
      Ready := H.Paste_Ready;
      H.Paste_Ready := False;
      Length := Natural'Min (H.Paste_Length, Into'Length);
      Into := [others => ' '];
      Into (Into'First .. Into'First + Length - 1) := H.Paste (1 .. Length);
   end Poll_Paste;
end Calc_Host_Native;
