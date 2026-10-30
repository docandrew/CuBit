------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The startup check every generated Startup package runs while it
--  elaborates, before the program's main (CAP-003).
--
--  @description
--  A program cannot skip it: the generated package calls Require from its
--  own body. If the loaded startup table lacks a required entry (or has it
--  with another kind), or the launch block carried no table at all, Require
--  says which entry and why on the console (and, through
--  CuBit.Startup_Check.Logged, as a Critical record when the program
--  declares logstore); then the process exits with Startup_Refused.
--  procmgr already refuses a launch whose required entry it cannot mint,
--  so a failure here means the table was lost on the way (the entry glue
--  dropped the launch block: CAP-003's R3), and running on with null
--  handles is what made that silent.
--
--  No secondary stack: services built with No_Secondary_Stack (netstack)
--  run it too.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Child_Exits;
with CuBit.Startup_State;

package CuBit.Startup_Check is

   --  sysexits.h's EX_CONFIG: the program's configuration (here its startup
   --  table) is wrong.
   Startup_Refused : constant CuBit.Child_Exits.Exit_Code := 78;

   --  The longest explanation Explain writes.
   Maximum_Text : constant := 320;
   subtype Text_Length is Natural range 0 .. Maximum_Text;

   --  Whether the loaded table holds Expected; when not, Text (1 .. Length)
   --  says which entry and why, for Program (the manifest's identity).
   procedure Explain
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String;
      Ready    : out Boolean;
      Text     : out String;
      Length   : out Text_Length)
   with Pre => Text'First = 1 and then Text'Length = Maximum_Text;

   --  Stop the process with Startup_Refused (after the caller reported why).
   procedure Stop with No_Return;

   --  Explain, and when not Ready print it and Stop.
   procedure Require
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String);

end CuBit.Startup_Check;
