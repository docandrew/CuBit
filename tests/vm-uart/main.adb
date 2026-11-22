--  VMM-001: hosted tests of the monitor's guest UART (Guest_UART: the
--  16550A's receive FIFO, LSR, IIR, IER and the IRQ edge), the keys vmm
--  sends (Serial_Keys) and the console line (Console_Line), with literal
--  oracles: register values from the PC16550D datasheet as Linux's 8250
--  driver reads them, and the bytes an xterm sends (ctlseqs: CSI A..D for
--  the cursor keys, CSI 3 ~ for Delete, DEL for Backspace).
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Guest_UART; use Guest_UART;
with Serial_Keys;
with Console_Line;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Put_Line ("vm-uart: FAIL " & What);
      end if;
   end Check;

   U : UART;
   V : Unsigned_8;
   E : Write_Effect;
   R : Boolean;

   function Get (Reg : Register) return Unsigned_8 is
      Value : Unsigned_8;
   begin
      Read (U, Reg, Value);
      return Value;
   end Get;

   procedure Put (Reg : Register; Value : Unsigned_8) is
   begin
      Write (U, Reg, Value, E);
   end Put;
begin
   -- Power-on: no FIFO, no interrupt, THR empty (LSR 60h), MSR DCD|DSR|CTS.
   Check (Get (Line_Status) = 16#60#, "LSR at reset");
   Check (Get (Interrupt_Identity) = 16#01#, "IIR at reset");
   Check (Get (Modem_Status) = 16#B0#, "MSR at reset");
   Check (Room (U) = 1, "no FIFO: RBR holds one byte");

   -- Linux's 8250 setup: DLAB divisor, 8N1, FIFO on with a 14-byte trigger
   -- (FCR C7h), OUT2 | RTS | DTR, then ERBFI.
   Put (Line_Control, 16#83#); Put (Data_Register, 1); Put (Interrupt_Enable, 0);
   Check (Get (Data_Register) = 1 and then Get (Interrupt_Enable) = 0, "divisor latch");
   Check (not E.Transmitted, "a DLL write is not a byte sent");
   Put (Line_Control, 16#03#);
   Put (Interrupt_Identity, 16#C7#);
   Check (Get (Interrupt_Identity) = 16#C1#, "IIR: FIFOs enabled, no interrupt (16550A)");
   Check (Room (U) = 16 and then Trigger_Level (U) = 14, "FIFO depth and trigger");
   Put (Modem_Control, 16#0B#);
   Put (Interrupt_Enable, IER_Received_Data);
   Check (not E.Raise_IRQ, "enabling ERBFI with no data raises nothing");

   -- A byte below the trigger: data ready, character timeout, one edge.
   Receive (U, Character'Pos ('a'), R);
   Check (R, "the first byte raises IRQ 4");
   Check (Get (Line_Status) = 16#61#, "LSR: data ready");
   Check (Get (Interrupt_Identity) = 16#CC#, "IIR: character timeout below the trigger");
   Receive (U, Character'Pos ('b'), R);
   Check (not R, "a second byte while the line is high is no new edge");
   Check (Get (Data_Register) = Character'Pos ('a') and then Get (Data_Register) = Character'Pos ('b'),
          "RBR pops in order");
   Check (Get (Line_Status) = 16#60#, "LSR: empty again");
   Check (Get (Interrupt_Identity) = 16#C1#, "IIR: the interrupt cleared by reading RBR");
   Check (Get (Data_Register) = 0, "RBR when empty");

   -- At the trigger level: received data available (IIR C4h).
   Put (Interrupt_Identity, 16#41#);   -- trigger 4, no clear
   Check (Trigger_Level (U) = 4, "trigger 4");
   for I in 1 .. 4 loop
      Receive (U, Unsigned_8 (I), R);
      Check (R = (I = 1), "one edge for four bytes");
   end loop;
   Check (Get (Interrupt_Identity) = 16#C4#, "IIR: received data at the trigger");
   V := Get (Data_Register);
   Check (V = 1 and then Get (Interrupt_Identity) = 16#CC#, "below the trigger again: timeout");

   -- Priority: received data above THRE; THRE taken by reading IIR.
   Put (Interrupt_Enable, IER_Received_Data or IER_THR_Empty);
   Check (not E.Raise_IRQ, "THRE pending under received data: the line stays high, no edge");
   Check (Get (Interrupt_Identity) = 16#CC#, "received data first");
   while Get (Line_Status) = 16#61# loop
      V := Get (Data_Register);
   end loop;
   Check (Get (Interrupt_Identity) = 16#C2#, "then THRE");
   Check (Get (Interrupt_Identity) = 16#C1#, "reading IIR took THRE");

   -- Every THR write is a new THRE interrupt: an edge each time.
   Put (Data_Register, Character'Pos ('x'));
   Check (E.Transmitted and then E.Byte = Character'Pos ('x') and then E.Raise_IRQ, "THR write sends and raises");
   Put (Data_Register, Character'Pos ('y'));
   Check (E.Raise_IRQ, "a second THR write raises again");
   Check (Get (Interrupt_Identity) = 16#C2#, "IIR: THRE after a write");

   -- OUT2 gates the line (the PC wiring).
   Put (Modem_Control, 16#03#);
   Receive (U, 7, R);
   Check (not R and then not Line (U), "no edge while OUT2 is clear");
   Put (Modem_Control, 16#0B#);
   Check (E.Raise_IRQ, "setting OUT2 with data waiting raises");

   -- Full FIFO: Room 0 (the monitor stops taking keys), nothing overruns.
   Put (Interrupt_Identity, 16#C7#);
   Check (E.Discarded = 1, "a FIFO clear reports the byte it discarded");
   for I in 1 .. 16 loop
      Receive (U, Unsigned_8 (I), R);
   end loop;
   Check (Room (U) = 0 and then Get (Line_Status) = 16#61#, "full: no room, no overrun bit");
   Check (Get (Interrupt_Identity) = 16#C4#, "IIR: 16 bytes are past the trigger");
   for I in 1 .. 16 loop
      Check (Get (Data_Register) = Unsigned_8 (I), "FIFO order");
   end loop;
   Put (Interrupt_Identity, 16#C1#);
   Check (E.Discarded = 0, "an FCR write without a clear discards nothing");

   -- Disabling ERBFI hides received data from IIR, not from LSR.
   Put (Interrupt_Enable, 0);
   Receive (U, 9, R);
   Check (not R and then Get (Interrupt_Identity) = 16#C1#
          and then Get (Line_Status) = 16#61#, "ERBFI off: polled input only");
   Put (Interrupt_Enable, IER_Received_Data);
   Check (E.Raise_IRQ, "enabling ERBFI with data waiting raises");

   -- Loopback mirrors MCR into MSR and sends nothing.
   Put (Modem_Control, 16#1A#);   -- loop, OUT2, RTS
   Check (Get (Modem_Status) = 16#90#, "loopback MSR: DCD, CTS");
   Put (Data_Register, 16#55#);
   Check (not E.Transmitted, "loopback: nothing sent");

   -- Keys.
   declare
      package K renames Serial_Keys;
      use type K.Sequence;
      function Seq (S : String) return K.Sequence is
         Q : K.Sequence;
      begin
         for C of S loop
            Q.Length := Q.Length + 1;
            Q.Bytes (Q.Length) := Character'Pos (C);
         end loop;
         return Q;
      end Seq;
      ESC : constant Character := Character'Val (16#1B#);
      Ctrl : constant Unsigned_64 := K.Ctrl_Modifier;
   begin
      Check (K.From_Key (16#1C#, 0) = Seq ([Character'Val (16#0D#)]), "Enter is CR");
      Check (K.From_Key (16#0E#, 0) = Seq ([Character'Val (16#7F#)]), "Backspace is DEL");
      Check (K.From_Key (16#0F#, 0) = Seq ([ASCII.HT]), "Tab");
      Check (K.From_Key (16#01#, 0) = Seq ([ESC]), "Escape");
      Check (K.From_Key (16#48#, 0) = Seq (ESC & "[A"), "Up");
      Check (K.From_Key (16#50#, 0) = Seq (ESC & "[B"), "Down");
      Check (K.From_Key (16#4D#, 0) = Seq (ESC & "[C"), "Right");
      Check (K.From_Key (16#4B#, 0) = Seq (ESC & "[D"), "Left");
      Check (K.From_Key (16#47#, 0) = Seq (ESC & "[H") and then K.From_Key (16#4F#, 0) = Seq (ESC & "[F"),
             "Home, End");
      Check (K.From_Key (16#53#, 0) = Seq (ESC & "[3~"), "Delete");
      Check (K.From_Key (16#2E#, Ctrl) = Seq ([Character'Val (3)]), "Ctrl+C is ETX");
      Check (K.From_Key (16#20#, Ctrl) = Seq ([Character'Val (4)]), "Ctrl+D is EOT");
      Check (K.From_Key (16#2C#, Ctrl or K.Shift_Modifier) = Seq ([Character'Val (16#1A#)]), "Ctrl+Shift+Z");
      Check (K.From_Key (16#10#, Ctrl) = Seq ([Character'Val (16#11#)])
             and then K.From_Key (16#19#, Ctrl) = Seq ([Character'Val (16#10#)])
             and then K.From_Key (16#26#, Ctrl) = Seq ([Character'Val (16#0C#)])
             and then K.From_Key (16#32#, Ctrl) = Seq ([Character'Val (16#0D#)]),
             "Ctrl+Q, P, L, M (row ends)");
      Check (K.From_Key (16#1A#, Ctrl) = Seq ([ESC]), "Ctrl+[ is ESC");
      Check (K.From_Key (16#2E#, K.Alt_Modifier) = Seq (ESC & "c"), "Alt+C is ESC c");
      Check (K.From_Key (16#2E#, 0).Length = 0, "a letter key alone sends nothing (its text event does)");
      Check (K.From_Key (16#39#, 0).Length = 0, "Space alone: its text event");
      Check (K.From_Text (Character'Pos ('$')) = Seq ("$") and then K.From_Text (16#7F#).Length = 0
             and then K.From_Text (16#E9#).Length = 0, "text: printable ASCII only");
   end;

   -- The console line.
   declare
      package C renames Console_Line;
      L : C.Line;
      Done : Boolean;
      Lines_Done : Natural := 0;
      procedure Feed (S : String) is
      begin
         for Ch of S loop
            C.Put (L, Character'Pos (Ch), Done);
            if Done then
               Lines_Done := Lines_Done + 1;
               C.Clear (L);
            end if;
         end loop;
      end Feed;
      function Text return String is (L.Text (1 .. L.Length));
      ESC : constant Character := Character'Val (16#1B#);
      BS : constant Character := Character'Val (8);
      CR : constant Character := Character'Val (13);
   begin
      Feed ("/ # ech");
      Check (Text = "/ # ech" and then L.Cursor = 7, "echo at the cursor");
      Feed (BS & " " & BS);
      Check (Text = "/ # ec " and then L.Cursor = 6, "BS space BS erases");
      Feed ("ho" & ESC & "[K");
      Check (Text = "/ # echo", "ESC [ K erases to the end");
      Feed ("x" & BS & ESC & "[J");
      Check (Text = "/ # echo" and then L.Cursor = 8, "BS ESC [ J: busybox's delete at the end");
      Feed (ESC & "[1;32m" & ESC & "[0m" & ESC & "[6n");
      Check (Text = "/ # echo", "SGR and DSR taken, not printed");
      Feed (ESC & "]0;title" & Character'Val (7) & "!");
      Check (Text = "/ # echo!", "OSC taken");
      Feed (ESC & "[3D" & "X");
      Check (Text = "/ # ecXo!" and then L.Cursor = 7, "ESC [ 3 D moves left");
      Feed (CR & "#");
      Check (Text = "# # ecXo!" and then L.Cursor = 1, "CR to column 0, overwrite");
      Feed (ESC & "[2K");
      Check (L.Length = 0, "ESC [ 2 K erases the line");
      Feed (CR & "a" & ASCII.HT & "b" & ASCII.LF);
      Check (Lines_Done = 1 and then L.Length = 0, "LF completes the line");
      Feed ("q" & ESC & "7z");
      Check (Text = "qz", "a two-byte ESC sequence is taken");
      C.Clear (L);
      Feed ("a" & ASCII.HT & "b");
      Check (Text = "a       b", "HT to the next multiple of eight");
      C.Clear (L);
      for I in 1 .. C.Width loop
         Feed ("w");
      end loop;
      Check (Lines_Done = 2 and then L.Length = 0, "writing past the last column wraps");
   end;

   Put_Line ("vm-uart:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
