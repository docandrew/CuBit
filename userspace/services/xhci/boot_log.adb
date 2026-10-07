with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
package body Boot_Log is
   Capture_Capacity : constant := 512;
   Lines : array (1 .. Capture_Capacity) of String (1 .. 96) := [others => [others => ' ']];
   Lengths : array (1 .. Capture_Capacity) of Natural := [others => 0];
   Used, Column, Sent : Natural := 0;
   Lost : Natural := 0;
   Writer : CuBit.Logging.Publisher;
   Granted, Requested, Finished : Boolean := False;
   Summary_Sent : Boolean := False;
   Due : Unsigned_64 := 1;
   Token : constant Unsigned_64 := 16#584C_0001#;
   function Deadline return Unsigned_64 is (if not Enabled or else Finished then 0 else Due);
   procedure Write (Text : String) is
   begin
      if not Enabled then return; end if;
      debugPrint (Text);
      if Finished then return; end if;
      for C of Text loop
         if C = ASCII.LF then
            if Used < Lines'Length and then Column > 0 then
               Used := Used + 1; Lengths (Used) := Column;
            elsif Used = Lines'Length then Lost := Lost + 1; end if;
            Column := 0;
         elsif C >= ' ' and then C <= '~' and then
           Used < Lines'Length and then Column < 96 then
            Column := Column + 1; Lines (Used + 1) (Column) := C;
         end if;
      end loop;
   end Write;
   procedure Poll is
      Receipt : aliased CompletionEntry;
      Found, Accepted : Boolean;
      Now : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
   begin
      if not Enabled or else Finished then return; end if;
      Now := syscall (SYSCALL_GETTIME);
      Found := Poll_Completion (Receipt'Address) /= 0;
      if Found then
         if Requested and then Receipt.token = Token then
            debugPrint ("xhci-log: grant reply received" & ASCII.LF);
            Requested := False;
            Granted := Receipt.status = COMPLETION_OK and then Receipt.msg.tag.label = 16#F000#;
         end if;
      end if;
      if Now < Due then return; end if;
      Due := Now + 100;
      if not Granted then
         if getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE) = 0 then
            Due := Now + 1000;
            return;
         end if;
         if not Requested then
            Msg.tag := (16#0228#, 0, 0, 0);
            Requested := capSubmit (15, Msg, Token);
            if Requested then debugPrint ("xhci-log: grant requested" & ASCII.LF); end if;
         end if;
         Due := Now + 1000;
      else
         if Sent < Used then
            declare
               Record_Value : constant CuBit.Log_Records.Decoded :=
                 CuBit.Log_Records.Make (Lines (Sent + 1) (1 .. Lengths (Sent + 1)));
            begin
               if Record_Value.Success then
                  CuBit.Logging.Emit (Writer, Record_Value.Value, Accepted);
               end if;
               Sent := Sent + 1;
            end;
         elsif not Summary_Sent then
            declare
               Summary : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
                 ("xhci startup capture: overflow" & Natural'Image (Lost) &
                  " publication losses" & Unsigned_64'Image (CuBit.Logging.Dropped (Writer)));
            begin
               if Summary.Success then
                  CuBit.Logging.Emit (Writer, Summary.Value, Accepted);
               end if;
               Summary_Sent := True;
            end;
         else
            debugPrint ("xhci: startup log published" & ASCII.LF);
            Finished := True;
         end if;
      end if;
   end Poll;
end Boot_Log;
