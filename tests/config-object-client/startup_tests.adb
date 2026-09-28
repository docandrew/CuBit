with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Config_Worker_Startup; use Config_Worker_Startup;

procedure Startup_Tests is
   Request : Message := NULL_MESSAGE;
   Valid : Message;
begin
   pragma Assert (not Valid_Attachment (Request));
   Request.tag := (Operation'Enum_Rep (Attach_Worker), 1, 0, 0);
   Request.words (0) := 123;
   pragma Assert (Valid_Attachment (Request));
   Valid := Request;
   for Case_ID in 1 .. 9 loop
      Request := Valid;
      case Case_ID is
         when 1 => Request.tag.label := Request.tag.label + 1;
         when 2 => Request.tag.length := 0;
         when 3 => Request.tag.length := 2;
         when 4 => Request.tag.flags := 1;
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
   pragma Assert (Authorized_Config_Request (22, 22, 22));
   pragma Assert (not Authorized_Config_Request (22, 0, 22));
   pragma Assert (not Authorized_Config_Request (23, 22, 22));
   pragma Assert (not Authorized_Config_Request (22, 23, 22));
   pragma Assert (not Authorized_Config_Request (0, 0, 0));
   pragma Assert (not Authorized_Config_Request (22, 22, 0));
   pragma Assert (not Authorized_Config_Request (22, 22, Unsigned_64'Last));
   pragma Assert (not Authorized_Config_Request (Unsigned_64'Last, Unsigned_64'Last, Unsigned_64'Last));
   Ada.Text_IO.Put_Line ("Config startup envelope/worker identity: 20 checks PASS");
end Startup_Tests;
