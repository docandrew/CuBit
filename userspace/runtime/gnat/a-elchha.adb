-------------------------------------------------------------------------------
 -- CuBitOS
 -- Copyright (C) 2019-2026 Jon Andrew
 --
 -- Ada Exception Handling: the last-chance handler (EXC-001)
-------------------------------------------------------------------------------
--  Upstream GNAT's interface (a-elchha.ads, unchanged): called once with the
--  occurrence of an exception no handler took, from the propagation code
--  (Ada.Exceptions.Exception_Traces.Unhandled_Exception_Terminate) on the
--  thread that raised it. CuBit has no standard error, so instead of
--  upstream's stderr report it says it twice:
--    - on the debug console: "User-space exception NAME: MESSAGE" and the
--      traceback (code addresses, most recent call first; the program's
--      debug file, kept by image staging under its build-id, turns them
--      into source lines: addr2line -e <debug file> <addresses>);
--    - as an Error record through the program's own log (CuBit.Last_Words,
--      LOG-004), when the program has one: "unhandled exception NAME:
--      MESSAGE; traceback 0x... 0x...", within one record's text.
--  Then the process exits with Unhandled_Exception_Status
--  (CuBit.Program_Endings), which a launcher reads as the typed outcome
--  Failed (Unhandled_Exception).
--
--  Like upstream, nothing here allocates: no secondary stack (it may be
--  what ran out), no heap; the text is built in fixed buffers on the
--  primary stack. Library-level finalization is not run (upstream runs
--  Adafinal here; CuBit's configurable runtime finalizes no library-level
--  objects, RT-008). A second entry (an exception while reporting, on this
--  or another thread) stops at once.

with System.Storage_Elements; use System.Storage_Elements;
with System.Standard_Library; use System.Standard_Library;
with CuBit.Last_Words;
with CuBit.Program_Endings.Last_Chance;

procedure Ada.Exceptions.Last_Chance_Handler
  (Except : Exception_Occurrence)
is
   Console_Prefix : constant String := "User-space exception ";
   Log_Prefix : constant String := "unhandled exception ";
   Message_Separator : constant String := ": ";
   Console_Traceback : constant String := "  traceback:";
   Log_Traceback : constant String := "; traceback";
   Hex_Prefix : constant String := " 0x";
   Unknown_Name : constant String := "(unknown)";
   Reentered : constant String :=
     "Unhandled exception while reporting an unhandled exception; stopping";

   Maximum_Name_Length : constant := 256;
   --  Longer names (fully qualified, with their package) are cut

   Address_Text_Length : constant := Hex_Prefix'Length + 16;
   Console_Text_Length : constant :=
     Console_Prefix'Length + Maximum_Name_Length + Message_Separator'Length
     + Exception_Msg_Max_Length + 1 + Console_Traceback'Length
     + Max_Tracebacks * Address_Text_Length + 1;

   Console : String (1 .. Console_Text_Length);
   Console_Used : Natural := 0;
   Log : String (1 .. CuBit.Program_Endings.Last_Chance.Maximum_Report_Length);
   Log_Used : Natural := 0;

   procedure Put (Into : in out String; Used : in out Natural; S : String);
   procedure Put (Into : in out String; Used : in out Natural; S : String) is
   begin
      for C of S loop
         exit when Used >= Into'Last;
         Used := Used + 1;
         Into (Used) := C;
      end loop;
   end Put;

   --  The name, without its NUL; Unknown_Name for a null Id.
   procedure Put_Name (Into : in out String; Used : in out Natural);
   procedure Put_Name (Into : in out String; Used : in out Natural) is
   begin
      if Except.Id = null or else Except.Id.Name_Length <= 1 then
         Put (Into, Used, Unknown_Name);
      else
         Put (Into, Used,
              To_Ptr (Except.Id.Full_Name)
                (1 .. Natural'Min (Except.Id.Name_Length - 1, Maximum_Name_Length)));
      end if;
   end Put_Name;

   procedure Put_Message (Into : in out String; Used : in out Natural);
   procedure Put_Message (Into : in out String; Used : in out Natural) is
   begin
      if Except.Msg_Length > 0 then
         Put (Into, Used, Message_Separator);
         Put (Into, Used, Except.Msg (1 .. Except.Msg_Length));
      end if;
   end Put_Message;

   procedure Put_Traceback (Into : in out String; Used : in out Natural);
   procedure Put_Traceback (Into : in out String; Used : in out Natural) is
      Hex_Digits : constant String := "0123456789abcdef";
      Nibble_Bits : constant := 4;
      Nibbles : constant := Standard'Address_Size / Nibble_Bits;
   begin
      for J in 1 .. Except.Num_Tracebacks loop
         declare
            Value : Integer_Address := To_Integer (Except.Tracebacks (J));
            Text : String (1 .. Nibbles);
            First : Positive := Text'Last;
         begin
            for K in reverse Text'Range loop
               Text (K) := Hex_Digits (Hex_Digits'First + Natural (Value mod 2 ** Nibble_Bits));
               Value := Value / 2 ** Nibble_Bits;
               if Text (K) /= '0' then
                  First := K;
               end if;
            end loop;
            Put (Into, Used, Hex_Prefix);
            Put (Into, Used, Text (First .. Text'Last));
         end;
      end loop;
   end Put_Traceback;

begin
   if not CuBit.Program_Endings.Last_Chance.First_Entry then
      CuBit.Program_Endings.Last_Chance.Write_Console (Reentered & ASCII.LF);
      CuBit.Program_Endings.Last_Chance.Stop;
   end if;

   Put (Console, Console_Used, Console_Prefix);
   Put_Name (Console, Console_Used);
   Put_Message (Console, Console_Used);
   if Except.Num_Tracebacks > 0 then
      Put (Console, Console_Used, (1 => ASCII.LF));
      Put (Console, Console_Used, Console_Traceback);
      Put_Traceback (Console, Console_Used);
   end if;
   Put (Console, Console_Used, (1 => ASCII.LF));
   CuBit.Program_Endings.Last_Chance.Write_Console (Console (1 .. Console_Used));

   Put (Log, Log_Used, Log_Prefix);
   Put_Name (Log, Log_Used);
   Put_Message (Log, Log_Used);
   if Except.Num_Tracebacks > 0 then
      Put (Log, Log_Used, Log_Traceback);
      Put_Traceback (Log, Log_Used);
   end if;
   CuBit.Last_Words.Report (Log (1 .. Log_Used), 0);

   CuBit.Program_Endings.Last_Chance.Stop;
end Ada.Exceptions.Last_Chance_Handler;
