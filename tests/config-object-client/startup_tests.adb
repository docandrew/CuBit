with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Config_Worker_Startup; use Config_Worker_Startup;

with CuBit.Process_IDs;
procedure Startup_Tests is
   Request : Message := NULL_MESSAGE;
   Valid : Message;
begin
   pragma Assert (not Valid_Attachment (Request));
   Request.tag := (label => Operation'Enum_Rep (Attach_Worker), length => 1,
                   protocol => 0, ipc => <>, reserved => 0);
   Request.words (0) := 123;
   pragma Assert (Valid_Attachment (Request));
   Valid := Request;
   for Case_ID in 1 .. 9 loop
      Request := Valid;
      case Case_ID is
         when 1 => Request.tag.label := Request.tag.label + 1;
         when 2 => Request.tag.length := 0;
         when 3 => Request.tag.length := 2;
         when 4 => Request.tag.protocol := 1;
         when 5 => Request.tag.reserved := 1;
         when 6 => Request.words (0) := 0;
         when 7 => Request.words (1) := 1;
         when 8 => Request.words (2) := 1;
         when 9 => Request.words (3) := 1;
      end case;
      pragma Assert (not Valid_Attachment (Request));
   end loop;
   Request := Valid;
   Request.words (0) := Unsigned_64'Last;
   pragma Assert (Valid_Attachment (Request));
   --  Envelope checking is not sender authorization. Native Config separately
   --  compares the kernel sender with the registered procmgr before attachment.
   declare
      package IDs renames CuBit.Process_IDs;
      Config : constant Process_ID := IDs.To_Process_ID (Slot => 22, Generation => 1);
      Other : constant Process_ID := IDs.To_Process_ID (Slot => 23, Generation => 1);
      Tag : constant Unsigned_64 := To_Word (Config);
      --  Slot 0 is ill formed, whatever its generation.
      Ill_Formed : constant Process_ID := IDs.To_Process_ID (Slot => 0, Generation => 1);
      --  The all-ones word is a well-formed identity, not a sentinel.
      All_Ones : constant Process_ID := IDs.From_Word (Unsigned_64'Last);
   begin
      pragma Assert (Authorized_Config_Request (Config, Tag, Config));
      pragma Assert (not Authorized_Config_Request (Config, 0, Config));
      pragma Assert (not Authorized_Config_Request (Other, Tag, Config));
      pragma Assert (not Authorized_Config_Request (Config, Tag + 1, Config));
      pragma Assert (not Authorized_Config_Request (No_Process, 0, No_Process));
      pragma Assert (not Authorized_Config_Request (Config, Tag, No_Process));
      pragma Assert (not Authorized_Config_Request (Config, Tag, Ill_Formed));
      pragma Assert (not Authorized_Config_Request
                       (Ill_Formed, To_Word (Ill_Formed), Ill_Formed));
      pragma Assert (Authorized_Config_Request (All_Ones, Unsigned_64'Last, All_Ones));
   end;
   Ada.Text_IO.Put_Line ("Config startup envelope/worker identity: 21 checks PASS");
end Startup_Tests;
