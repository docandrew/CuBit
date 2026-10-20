with Ada.Strings.Unbounded;

--  The hosted stand-in for CuBit's clipboard: in memory. A Linux-hosted
--  stand-in, not the desktop's clipboard service.
package Calc_Host_Memory is
   type Host is limited private;
   function Clipboard (H : Host) return String;
   procedure Set_Clipboard (H : in out Host; Text : String);
   procedure Copy (H : in out Host; Text : String);
   procedure Request_Paste (H : in out Host);
   procedure Poll_Paste (H : in out Host; Into : out String; Length : out Natural; Ready : out Boolean);
private
   type Host is limited record
      Text : Ada.Strings.Unbounded.Unbounded_String;
      Pending : Boolean := False;
   end record;
end Calc_Host_Memory;
