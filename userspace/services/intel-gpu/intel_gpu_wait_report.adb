------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Monotonic;
with CuBit.Log_Records;
with Intel_GPU_Diagnostics;
with Intel_GPU_Wait_Heartbeat;

package body Intel_GPU_Wait_Report is
   use Interfaces;
   package H renames Intel_GPU_Wait_Heartbeat;
   use type H.Line_Kind;

   Monitors : array (Site) of H.Monitor;
   Last_Offset : Unsigned_32 := 0;
   Last_Value : Unsigned_32 := 0;
   Observed : Boolean := False;

   --  How often a wait lets the diagnostics publisher run (grant request,
   --  grant reply, publication), independent of heartbeat lines.
   Publisher_Interval_Us : constant H.Microseconds := 50_000;
   Next_Publisher_Us : H.Microseconds := 0;

   Microseconds_Per_Millisecond : constant := 1_000;

   function Plus (Now, Interval : H.Microseconds) return H.Microseconds is
     (if Now <= H.Microseconds'Last - Interval then Now + Interval
      else H.Microseconds'Last);

   function Hex (Value : Unsigned_32) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Result : String (1 .. 10) := "0x00000000";
      Rest : Unsigned_32 := Value;
   begin
      for Index in reverse 3 .. Result'Last loop
         Result (Index) := Digits_Text (Natural (Rest and 16#F#) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Result;
   end Hex;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Observe (Offset, Value : Unsigned_32) is
   begin
      Last_Offset := Offset;
      Last_Value := Value;
      Observed := True;
   end Observe;

   procedure Pulse (S : Site) is
      Stamp : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      Line : H.Line_Kind;
   begin
      if not Stamp.Available then
         return;
      end if;
      H.Pulse (Monitors (S), Stamp.Microseconds, Line);
      if Line /= H.None then
         Intel_GPU_Diagnostics.Capture
           ("intel-gpu: stage " & Name (S) &
            (if Line = H.Entry_Line then " waiting" else " still waiting") &
            " elapsed-ms=" & Image (H.Elapsed (Monitors (S), Stamp.Microseconds) /
                                     Microseconds_Per_Millisecond) &
            " polls=" & Image (Unsigned_64 (Monitors (S).Polls)) &
            (if Polls_Registers (S) and then Observed
             then " reg=" & Hex (Last_Offset) & " val=" & Hex (Last_Value)
             else ""),
            (if Line = H.Entry_Line then CuBit.Log_Records.Information
             else CuBit.Log_Records.Warning));
         --  Capture ran the publisher.
         Next_Publisher_Us := Plus (Stamp.Microseconds, Publisher_Interval_Us);
      elsif Stamp.Microseconds >= Next_Publisher_Us then
         Next_Publisher_Us := Plus (Stamp.Microseconds, Publisher_Interval_Us);
         Intel_GPU_Diagnostics.Tick;
      end if;
   end Pulse;

end Intel_GPU_Wait_Report;
