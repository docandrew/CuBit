pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with CuBit.Logging;
with CuBit.Messages;
with I2C_Log;
procedure Log_Tests is
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Natural'Image (Checks); end if;
   end Check;
   procedure Reentry is
   begin
      I2C_Log.Note ("close callback");
      I2C_Log.Disable ("repeat-close");
   end Reentry;
   Which : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
begin
   CuBit.Logging.Complete := Which = 0;
   CuBit.Logging.On_Disconnect := Reentry'Unrestricted_Access;
   I2C_Log.Note ("ready");
   Check (CuBit.Logging.Emissions = 1 and then CuBit.Messages.Lines = 1);
   I2C_Log.Disable ("grant-lifetime-event");
   Check (CuBit.Logging.Disconnects = 1 and then CuBit.Logging.Emissions = 1
          and then CuBit.Logging.Late_Emissions = 0);
   Check (CuBit.Messages.Lines = 3);
   I2C_Log.Disable ("repeat");
   Check (CuBit.Logging.Disconnects = 1 and then CuBit.Messages.Lines = 3);
   I2C_Log.Note ("after-close");
   I2C_Log.Stage ("stop", True, "quiescent");
   I2C_Log.Summary (False);
   Check (CuBit.Logging.Emissions = 1 and then CuBit.Logging.Late_Emissions = 0
          and then CuBit.Messages.Lines = 6);
   Check (I2C_Log.Hex ("val", 16#1234_5678_9ABC_DEF0#) = " val=0x123456789abcdef0");
   Ada.Text_IO.Put_Line ("I2C local log checks" & Natural'Image (Checks));
end Log_Tests;
