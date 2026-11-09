-------------------------------------------------------------------------------
 -- CuBitOS
 -- Copyright (C) 2019 Jon Andrew
 --
 -- Ada Exception Handling
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Last_Words;
with CuBit.Messages;

package body Ada.Exceptions.Last_Chance_Handler with
   SPARK_Mode => Off
is
   --  Allocation-free: the handler runs when the secondary stack may be the
   --  thing that ran out, so it never concatenates, never uses 'Image and
   --  writes only into a fixed buffer on the primary stack. It never
   --  returns; a second entry (a fault while reporting) stops at once.
   --
   --  It says it twice: on the debug console as before, and as an Error
   --  record through the program's own log (CuBit.Last_Words, LOG-004),
   --  when the program has one, so (logs.recent "<program>") shows it after
   --  the program is gone. The runtime calls the handler with the failing
   --  source location (or a Raise_Exception message), not the exception's
   --  name, so that is what both say.

   Max_Name_Length : constant := 160;
   Max_Line_Digits : constant := 10;
   Prefix : constant String := "User-space exception at ";
   Middle : constant String := " line ";
   Log_Prefix : constant String := "unhandled exception at ";
   Log_Message_Prefix : constant String := "unhandled exception: ";
   Log_Separator : constant Character := ':';
   Last_Chance_Exit : constant Unsigned_64 := 134;

   Entered : Boolean := False;

   procedure Stop with No_Return;
   procedure Stop is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_EXIT, Last_Chance_Exit);
      end loop;
   end Stop;

   --------------------------
   -- Last_Chance_Handler  --
   --------------------------
   procedure Last_Chance_Handler (msg : System.Address; line : Integer)
   is
      use type System.Address;
      Text : String (1 .. Prefix'Length + Max_Name_Length + Middle'Length + Max_Line_Digits + 1);
      Used : Natural := 0;
      Name_First, Name_Last, Line_First, Line_Last : Natural := 0;
      Log_Text : String (1 .. Log_Message_Prefix'Length + Max_Name_Length + 1 + Max_Line_Digits);
      Log_Used : Natural := 0;
      procedure Log_Put (S : String);
      procedure Log_Put (S : String) is
      begin
         for C of S loop
            if Log_Used < Log_Text'Last then
               Log_Used := Log_Used + 1;
               Log_Text (Log_Used) := C;
            end if;
         end loop;
      end Log_Put;

      procedure Put (C : Character);
      procedure Put (S : String);
      procedure Put_Name;
      procedure Put_Line_Number;

      procedure Put (C : Character) is
      begin
         if Used < Text'Last then
            Used := Used + 1;
            Text (Used) := C;
         end if;
      end Put;

      procedure Put (S : String) is
      begin
         for C of S loop
            Put (C);
         end loop;
      end Put;

      procedure Put_Name is
         Name : String (1 .. Max_Name_Length) with Import, Address => msg;
      begin
         for C of Name loop
            exit when C = ASCII.NUL;
            Put (C);
         end loop;
      end Put_Name;

      procedure Put_Line_Number is
         Digits_Text : String (1 .. Max_Line_Digits);
         Count : Natural := 0;
         Value : Natural := (if line < 0 then 0 else line);
      begin
         loop
            Count := Count + 1;
            Digits_Text (Max_Line_Digits - Count + 1) :=
              Character'Val (Character'Pos ('0') + Value mod 10);
            Value := Value / 10;
            exit when Value = 0 or else Count = Max_Line_Digits;
         end loop;
         Put (Digits_Text (Max_Line_Digits - Count + 1 .. Max_Line_Digits));
      end Put_Line_Number;
   begin
      if Entered then
         CuBit.Messages.debugPrint ("User-space exception while reporting an exception; stopping" & ASCII.LF);
         Stop;
      end if;
      Entered := True;
      Put (Prefix);
      Name_First := Used + 1;
      if msg = System.Null_Address then
         Put ("(unknown)");
      else
         Put_Name;
      end if;
      Name_Last := Used;
      Put (Middle);
      Line_First := Used + 1;
      Put_Line_Number;
      Line_Last := Used;
      Put (ASCII.LF);
      CuBit.Messages.debugPrint (Text (1 .. Used));
      --  "unhandled exception at main.adb:14", or with no line (a
      --  Raise_Exception message) "unhandled exception: <message>".
      if line > 0 then
         Log_Put (Log_Prefix);
         Log_Put (Text (Name_First .. Name_Last));
         Log_Put ((1 => Log_Separator));
         Log_Put (Text (Line_First .. Line_Last));
      else
         Log_Put (Log_Message_Prefix);
         Log_Put (Text (Name_First .. Name_Last));
      end if;
      CuBit.Last_Words.Report (Log_Text (1 .. Log_Used), (if line > 0 then line else 0));
      --  The raise point has no continuation: returning would jump to
      --  whatever the failed frame left behind.
      Stop;
   end Last_Chance_Handler;

end Ada.Exceptions.Last_Chance_Handler;
