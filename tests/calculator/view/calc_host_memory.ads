with Ada.Strings.Unbounded;
with CCL_Calc_Options;

--  The hosted stand-ins for CuBit's clipboard and Config: in memory. The
--  remembered options are kept as Config keeps them, the setting's
--  canonical source, and read back through the same typed path as on
--  CuBit (Calc_Settings). Linux-hosted stand-ins, not the desktop's
--  clipboard service or the config store.
package Calc_Host_Memory is
   type Host is limited private;
   function Clipboard (H : Host) return String;
   procedure Set_Clipboard (H : in out Host; Text : String);
   procedure Copy (H : in out Host; Text : String);
   procedure Request_Paste (H : in out Host);
   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean);
   procedure Load_Options (H : in out Host; Item : out CCL_Calc_Options.Choice; Found : out Boolean);
   procedure Save_Options (H : in out Host; Item : CCL_Calc_Options.Choice);
   --  The stored setting's value (Config's calculator.options).
   function Stored_Options (H : Host) return String;
   procedure Set_Stored_Options (H : in out Host; Source : String);
private
   type Host is limited record
      Options : Ada.Strings.Unbounded.Unbounded_String;
      Text : Ada.Strings.Unbounded.Unbounded_String;
      Pending : Boolean := False;
   end record;
end Calc_Host_Memory;
