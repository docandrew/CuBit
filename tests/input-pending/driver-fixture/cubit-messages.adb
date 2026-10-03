with Ada.Command_Line;
with Ada.Text_IO;
with CuBit.Input;
package body CuBit.Messages is
   Mode : constant String := Ada.Command_Line.Argument (1);
   Packets : constant Positive := (if Mode = "overflow" then 40 else 6);
   Byte_Index : Natural := 0;
   Byte_Limit : Natural := 0;
   IRQ_Waits, Timer_Waits, Refusals, Delivered : Natural := 0;
   Consumer : Unsigned_64 := 1;
   Overflow_Seen : Boolean := False;
   function Empty return Message is ((others => <>));
   procedure debugPrint (Text : String) is
   begin
      if Text = "ps2: pointer retention overflow; resynchronizing" & ASCII.LF then
         Overflow_Seen := True;
      end if;
   end debugPrint;
   function syscall (Number : Unsigned_64; Arg : Unsigned_64 := 0) return Unsigned_64 is
   begin
      pragma Assert (Number = SYSCALL_GETTIME or else Number = SYSCALL_SLEEP);
      return 100;
   end syscall;
   function getInfo (Number, Arg : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Number = SYSINFO_REGISTERED_DRIVER);
      return (if Arg = DRIVER_MOUSE then Consumer else 0);
   end getInfo;
   function portOutp8 (Port : Unsigned_16; Value : Unsigned_8) return Unsigned_64 is
   begin
      return 0;
   end portOutp8;
   function portInp8 (Port : Unsigned_16) return Unsigned_64 is
      Packet : Natural;
      Part : Natural;
   begin
      if Port = 16#64# then
         return (if Byte_Index < Byte_Limit then 16#21# else 0);
      end if;
      pragma Assert (Port = 16#60#);
      if Byte_Index = Byte_Limit then return 0; end if;
      Packet := Byte_Index / 3 + 1;
      Part := Byte_Index mod 3;
      Byte_Index := Byte_Index + 1;
      if Consumer = 2 then
         return (if Part = 0 then 8 else 0);
      elsif Mode /= "overflow" and Packet > 4 then
         -- A button down/up pair after the four -70 displacement packets.
         return (if Part = 0 then (if Packet = 5 then 9 else 8) else 0);
      end if;
      return (case Part is when 0 => 16#18#, when 1 => 186, when others => 0);
   end portInp8;
   function capSend (Slot : Unsigned_64; Msg : Message) return MessageTag is
   begin
      return (others => 0);
   end capSend;
   function trySendEvent (Dest : ProcessID; Msg : Message) return Boolean is
      Report : CuBit.Input.Source_Report;
      Valid : Boolean;
      Authenticated : Message := Msg;
      Expected : Natural;
      Payload : Unsigned_64;
   begin
      pragma Assert (Dest = Consumer);
      Authenticated.authorityTag := 1;
      CuBit.Input.Decode (Authenticated, Report, Valid);
      pragma Assert (Valid);
      if Timer_Waits = 0 then
         Refusals := Refusals + 1;
         return False;
      end if;
      Delivered := Delivered + 1;
      Expected := (if Mode = "overflow" then 32 + Delivered
                   elsif Mode = "replace" then 7 else Delivered);
      pragma Assert (Report.sequence = Unsigned_64 (Expected));
      pragma Assert (Report.flags (CuBit.Input.RESYNCHRONIZE) = (Delivered = 1));
      Payload := (if Mode = "replace" then 0
                  elsif Mode /= "overflow" and Expected > 4 then
                    (if Expected = 5 then 1 else 0)
                  else 16#FBA# * 256);
      pragma Assert (Report.payload = Payload);
      pragma Assert ((Report.snapshot and 16#FF#) = (Payload and 16#FF#));
      pragma Assert (CuBit.Input.Pointer_Time (Report.snapshot) = 100);
      return True;
   end trySendEvent;
   function Wait_Event return Message is
   begin
      IRQ_Waits := IRQ_Waits + 1;
      if IRQ_Waits = 1 then
         Byte_Limit := Packets * 3;
         return Empty;
      elsif Mode = "replace" and IRQ_Waits = 2 then
         pragma Assert (Timer_Waits = 1 and Delivered = 0);
         Byte_Index := 0;
         Byte_Limit := 3;
         return Empty;
      end if;
      raise Finished;
   end Wait_Event;
   function Poll_Event (Msg : out Message) return Boolean is
   begin
      Msg := Empty;
      return False;
   end Poll_Event;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      pragma Assert (Deadline = 101 and Byte_Index = Byte_Limit);
      Timer_Waits := Timer_Waits + 1;
      pragma Assert (Timer_Waits = 1);
      if Mode = "replace" then Consumer := 2; end if;
      return Deadline_Reached;
   end Wait_For_Activity_Until;
   procedure Verify is
   begin
      pragma Assert (Timer_Waits = 1 and Refusals >= Packets);
      pragma Assert (Delivered = (if Mode = "overflow" then 8
                                  elsif Mode = "replace" then 1 else 6));
      pragma Assert (Overflow_Seen = (Mode = "overflow"));
      Ada.Text_IO.Put_Line ("PS2-PUBLICATION: PASS " & Mode &
        " exact source packets, final timed retry, refusals=" & Refusals'Image);
   end Verify;
end CuBit.Messages;
