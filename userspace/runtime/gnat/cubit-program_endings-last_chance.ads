------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The end of a program the last-chance handler stops (EXC-001): it
--  reports once, then exits with Unhandled_Exception_Status.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Log_Records;

package CuBit.Program_Endings.Last_Chance with Preelaborate is

   --  The longest report the handler sends through the program's log: one
   --  log record's text.
   Maximum_Report_Length : constant := CuBit.Log_Records.Maximum_Text_Bytes;

   --  True for the first caller only (any thread): the last-chance handler
   --  reports once; a later entry (an exception while reporting, or on
   --  another thread meanwhile) stops without reporting.
   function First_Entry return Boolean;

   --  Text on the debug console (one write, nothing allocated).
   procedure Write_Console (Text : String);

   --  End the process with Unhandled_Exception_Status. Never returns.
   procedure Stop with No_Return;

end CuBit.Program_Endings.Last_Chance;
