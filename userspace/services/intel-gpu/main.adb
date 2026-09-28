with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Boot;
with Intel_GPU_Resources; use Intel_GPU_Resources;
with Intel_GPU_Observation;
with Intel_GPU_Probe;
with Intel_GPU_Forcewake;
with Intel_GPU_Firmware_File;
with Intel_GPU_Firmware_Buffer;
with Intel_GPU_Firmware;
with Intel_GPU_GGTT;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Forcewake;
with System;
with System.Storage_Elements;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Monotonic;
with Intel_GPU_Reset_Pages;
with Intel_GPU_Native_Reset;
procedure Main is
   use type Intel_GPU_Firmware_File.Load_Status;
   Sender : ProcessID;
   Request : Message;
   Plan : Mapping_Plan;
   Result : Unsigned_64;
   Register_Virtual_Base : constant Unsigned_64 := 16#6000_0000#;
   Observation : Intel_GPU_Observation.Snapshot;
   Writer : CuBit.Logging.Publisher;
   Logging_Granted : Boolean := False;
   Logging_Uncertain : Boolean := False;
   GGC : Unsigned_16;
   Table_Bytes : Unsigned_64;
   Engine_Inventory : Intel_GPU_ADLN_Inventory.Inventory;
   Media_Fuse : Unsigned_32 := Unsigned_32'Last;
   Reset_Write_Base : constant Unsigned_64 := 16#6120_0000#;
   Reset_Pages_Mapped : Boolean := False;
   -- Keep the hardware timing diagnostic image mapping-only until the NUC
   -- clock is validated. The native reset adapter remains compiled/tested.
   Enable_Native_Reset : constant Boolean := False;
   function Reset_Authorized return Boolean is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Token : constant Unsigned_64 := 16#4947_0020#;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if not Reset_Pages_Mapped then return False; end if;
      Msg.tag := (16#022E#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return False; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now < Started or else Now - Started >= 30_000 then return False; end if;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return False; end if;
            else
               return Receipt.status = COMPLETION_OK and then
                 Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
                 Receipt.msg.words = [0, 0, 0, 0];
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
   end Reset_Authorized;
   function Map_Reset_Pages return String is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Granted : Boolean;
      Activity : Activity_Result;
      Clock : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      pragma Unreferenced (Activity);
   begin
      if Reset_Pages_Mapped then return "already-mapped (NOT reset)"; end if;
      if not Engine_Inventory.Valid then return "inventory-unavailable"; end if;
      if not Clock.Available then return "clock-unavailable"; end if;
      for Index in Intel_GPU_Reset_Pages.Page_Index loop
         declare
            Token : constant Unsigned_64 := 16#4947_0010# + Unsigned_64 (Index);
            Offset : constant Unsigned_64 := Intel_GPU_Reset_Pages.Offset (Index);
         begin
            Msg.tag := (16#022D#, 1, 0, 0);
            Msg.words := [Unsigned_64 (Index), 0, 0, 0];
            if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
            Granted := False;
            loop
               Now := syscall (SYSCALL_GETTIME);
               if Now < Started or else Now - Started >= 30_000 then return "grant-timeout"; end if;
               if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
                  if Receipt.status = COMPLETION_OK and then
                    Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
                    Receipt.msg.words = [0, 0, 0, 0]
                  then
                     if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
                  else
                     Granted := Receipt.status = COMPLETION_OK and then
                       Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
                       Receipt.msg.words = [0, 0, 0, 0];
                     exit;
                  end if;
               end if;
               Activity := Wait_For_Activity_Until
                 (if Now < Unsigned_64'Last then Now + 1 else Now);
            end loop;
            if not Granted then return "grant-denied"; end if;
            if Offset > Unsigned_64'Last - Plan.Physical_Base then return "address-overflow"; end if;
            if syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base + Offset,
                        Reset_Write_Base + Unsigned_64 (Index) * 4096, 1, 0) /= 0
            then return "map-denied"; end if;
         end;
      end loop;
      Reset_Pages_Mapped := True;
      return "ready (NOT reset)";
   end Map_Reset_Pages;
   procedure Publish_Snapshot (Text : String) is
      Value : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Receipt : aliased CompletionEntry;
      Now : Unsigned_64 := syscall (SYSCALL_GETTIME);
      Limit : constant Unsigned_64 :=
        (if Now <= Unsigned_64'Last - 30_000 then Now + 30_000 else Unsigned_64'Last);
      Phase : Natural range 0 .. 2 := 0;
      Accepted, Handled, Found : Boolean;
      Activity : Activity_Result;
      Msg : Message := NULL_MESSAGE;
      Peer : ProcessID;
      Grant_Token : constant Unsigned_64 := 16#4947_0001#;
      Publish_Token : constant Unsigned_64 := 16#4947_0002#;
      pragma Unreferenced (Activity);
   begin
      if not Value.Success or else Logging_Uncertain then return; end if;
      -- Never start another publication after an ambiguous completion.
      Logging_Uncertain := True;
      if Logging_Granted then
         CuBit.Logging.Emit (Writer, Value.Value, Publish_Token, Accepted);
         if not Accepted then return; end if;
         Phase := 2;
      end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         exit when Now >= Limit;
         if Phase = 0 and then getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE) /= 0 then
            Msg := NULL_MESSAGE;
            Msg.tag := (16#0229#, 0, 0, 0);
            if not capSubmit (15, Msg, Grant_Token) then exit; end if;
            Phase := 1;
         end if;
         if Poll_Completion (Receipt'Address) /= 0 then
            if Phase = 1 and Receipt.token = Grant_Token then
               if Receipt.status /= COMPLETION_OK or else
                 Receipt.msg.tag /= (16#F000#, 0, 0, 0) or else
                 Receipt.msg.words /= [0, 0, 0, 0]
               then exit; end if;
               Logging_Granted := True;
               CuBit.Logging.Emit (Writer, Value.Value, Publish_Token, Accepted);
               if not Accepted then exit; end if;
               Phase := 2;
            elsif Phase = 2 then
               CuBit.Logging.Complete (Writer, Receipt, Handled);
               if Handled then
                  Logging_Uncertain := CuBit.Logging.Pending (Writer) or else
                    CuBit.Logging.Dropped (Writer) /= 0;
                  debugPrint ("intel-gpu: snapshot publication losses" &
                    Unsigned_64'Image (CuBit.Logging.Dropped (Writer)) & ASCII.LF);
                  return;
               end if;
            end if;
         end if;
         Poll_Any_Ipc (Peer, Msg, Found);
         Activity := Wait_For_Activity_Until
           (if Limit - Now > 1000 then Now + 1000 else Limit);
      end loop;
      debugPrint ("intel-gpu: snapshot logging unavailable; retained serial evidence" & ASCII.LF);
      -- Writer is process-lived even on timeout: never free uncertain loans.
   end Publish_Snapshot;
   function Read_Register (Address : Unsigned_64) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (Address));
   begin
      return Value;
   end Read_Register;
   procedure Capture is new Intel_GPU_Observation.Capture (Read_Register);
   function Probe_Forcewake return String is
      Write_Base : constant Unsigned_64 := 16#6020_0000#;
      Token : constant Unsigned_64 := 16#4947_0003#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Activity : Activity_Result;
      Granted : Boolean := False;
      pragma Unreferenced (Activity);
      function Clock return Unsigned_64 is (syscall (SYSCALL_GETTIME));
      function Read_Ack (Offset : Unsigned_32) return Unsigned_32 is
         Allowed : Boolean := False;
      begin
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            Allowed := Allowed or Offset = Intel_GPU_ADLN_Inventory.Ack_Register (D);
         end loop;
         if not Allowed then return Unsigned_32'Last; end if;
         return Read_Register (Register_Virtual_Base + Unsigned_64 (Offset));
      end Read_Ack;
      procedure Write_Request (Offset, Value : Unsigned_32) is
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => System.Storage_Elements.To_Address
             (System.Storage_Elements.Integer_Address (Write_Base + Unsigned_64 (Offset mod 4096)));
         Allowed : Boolean := False;
      begin
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            Allowed := Allowed or Offset = Intel_GPU_ADLN_Inventory.Request_Register (D);
         end loop;
         if not Allowed or else
           (Value /= 16#10001# and Value /= 16#10000#)
         then raise Program_Error; end if;
         Register := Value;
      end Write_Request;
      procedure Pause is
         Ignored : Unsigned_64;
         pragma Unreferenced (Ignored);
      begin
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end Pause;
      package FW is new Intel_GPU_Forcewake (Read_Ack, Write_Request, Pause, Clock,
         Intel_GPU_ADLN_Inventory.Request_Register (Intel_GPU_ADLN_Inventory.GT),
         Intel_GPU_ADLN_Inventory.Ack_Register (Intel_GPU_ADLN_Inventory.GT));
      Object : FW.Lease;
      package All_FW is new Intel_GPU_ADLN_Forcewake (Read_Ack, Write_Request, Pause, Clock);
      All_OK : Boolean;
      Status : FW.Result;
      First_Fuse, Second_Fuse : Unsigned_32;
      use type FW.Result;
      function Describe (Value : FW.Result) return String is
      begin
         case Value is
            when FW.Ready => return "ready";
            when FW.Timed_Out => return "timeout";
            when FW.Invalid_MMIO => return "invalid-MMIO";
            when FW.Invalid_Clock => return "invalid-clock";
            when FW.Invalid_State => return "invalid-state";
         end case;
      end Describe;
   begin
      Msg.tag := (16#022A#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "grant-submit-failed"; end if;
      loop
         Now := Clock;
         if Now < Started or else Now - Started >= 30_000 then
            return "grant-timeout";
         end if;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               --  Devmgr is still collecting other drivers' ready messages.
               --  This completed request minted nothing; retry within budget.
               if not capSubmit (15, Msg, Token) then
                  return "grant-retry-failed";
               end if;
            else
            Granted := Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0];
            exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      if not Granted then return "grant-denied"; end if;
      --  Separate alias: retain the original read-only register mapping.
      if syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base + 16#A000#,
                  Write_Base, 1, 0) /= 0
      then return "write-map-denied"; end if;
      FW.Acquire (Object, 100, Status);
      if Status /= FW.Ready then return "acquire-" & Describe (Status); end if;
      -- Fuse register lies in the existing read-only mapping. Hold GT awake
      -- for both reads, and do not admit an inventory after a failed release.
      First_Fuse := Read_Register (Register_Virtual_Base +
        Unsigned_64 (Intel_GPU_ADLN_Inventory.Fuse_Register));
      Second_Fuse := Read_Register (Register_Virtual_Base +
        Unsigned_64 (Intel_GPU_ADLN_Inventory.Fuse_Register));
      FW.Release (Object, 100, Status);
      if Status = FW.Ready and First_Fuse = Second_Fuse then
         Media_Fuse := First_Fuse;
         Engine_Inventory := Intel_GPU_ADLN_Inventory.Decode
           (Unsigned_16 (Request.words (1) and 16#FFFF#),
            Unsigned_16 (Shift_Right (Request.words (1), 16) and 16#FFFF#),
            Media_Fuse);
      end if;
      if Status = FW.Ready and Engine_Inventory.Valid then
         All_FW.Acquire
           (Unsigned_16 (Request.words (1) and 16#FFFF#),
            Unsigned_16 (Shift_Right (Request.words (1), 16) and 16#FFFF#),
            Media_Fuse, All_OK);
         if not All_OK then return "release-ready; domains-acquire-failed"; end if;
         All_FW.Release (All_OK);
         if not All_OK then return "release-ready; domains-release-failed"; end if;
         return "release-ready; domains-release-ready";
      end if;
      return "release-" & Describe (Status);
   end Probe_Forcewake;
   function Hex (Value : Unsigned_32) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
   begin
      for Index in Text'Range loop
         Text (Index) := Hex_Digits
           (Natural (Shift_Right (Value, (8 - Index) * 4) and 15) + 1);
      end loop;
      return Text;
   end Hex;
   function Inspect_GGTT return String is
      Token : constant Unsigned_64 := 16#4947_0004#;
      Virtual : constant Unsigned_64 := 16#6040_0000#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Start : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now, Physical : Unsigned_64;
      Activity : Activity_Result;
      Low, High : Unsigned_32;
      Present : Natural := 0;
      pragma Unreferenced (Activity);
   begin
      if Table_Bytes = 0 then return "unavailable"; end if;
      Msg.tag := (16#022B#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now < Start or else Now - Start >= 30_000 then return "timeout"; end if;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
            else
               if Receipt.status /= COMPLETION_OK or else
                 Receipt.msg.tag /= (16#F000#, 2, 0, 0) or else
                 Receipt.msg.words (1 .. 3) /= [Table_Bytes, 0, 0]
               then return "grant-denied"; end if;
               Physical := Receipt.msg.words (0);
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      if Physical = 0 or else Physical mod 4096 /= 0 or else
        Physical > Unsigned_64'Last - Table_Bytes
      then return "map-denied"; end if;
      -- Every supported table is a multiple of 2MiB. Map disjoint chunks
      -- below MAP_DEVICE's per-call limit; keep every alias read-only.
      -- A partial failure leaves only process-lived read-only aliases and
      -- returns before inspecting any table entry. No retry/remap here.
      for Chunk in Unsigned_64 range 0 .. Table_Bytes / (2 * 1024 * 1024) - 1 loop
         if syscall (SYSCALL_MAP_DEVICE,
           Physical + Chunk * (2 * 1024 * 1024),
           Virtual + Chunk * (2 * 1024 * 1024), 512, 1) /= 0
         then return "map-denied"; end if;
      end loop;
      -- Diagnostic sampling, not an atomic table snapshot or a free-space map.
      Low := Read_Register (Virtual);
      High := Read_Register (Virtual + 4);
      if Low = Unsigned_32'Last and then High = Unsigned_32'Last then
         return "invalid-MMIO";
      end if;
      for Index in Unsigned_64 range 0 .. Table_Bytes / 8 - 1 loop
         if (Read_Register (Virtual + Index * 8) and 1) /= 0 then
            Present := Present + 1;
         end if;
      end loop;
      return "first=" & Hex (High) & Hex (Low) & " present=" & Natural'Image (Present) &
        " scanned=" & Unsigned_64'Image (Table_Bytes / 8);
   end Inspect_GGTT;
begin
   debugPrint ("intel-gpu: awaiting device-scoped bootstrap" & ASCII.LF);
   declare
      First : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      Last : CuBit.Monotonic.Reading;
      Moving : Boolean := False;
   begin
      if First.Available then
         for Poll in 1 .. 10_000 loop
            Last := CuBit.Monotonic.Read;
            exit when not Last.Available;
            exit when Last.Microseconds < First.Microseconds;
            if Last.Microseconds > First.Microseconds then Moving := True; exit; end if;
         end loop;
      end if;
      debugPrint ("intel-gpu: monotonic microsecond progress=" & Boolean'Image (Moving) & ASCII.LF);
   end;
   receive (Sender, Request);
   if Sender = 0 or else
     Sender /= getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR) or else
     Request.tag.label /= Intel_GPU_Boot.Configure_Label or else
     Request.tag.length /= 4 or else Request.tag.flags /= 0 or else
     Request.tag.reserved /= 0
   then
      debugPrint ("intel-gpu: bootstrap rejected" & ASCII.LF);
      return;
   end if;
   Plan := Intel_GPU_Boot.Decode
     ([Request.words (0), Request.words (1), Request.words (2), Request.words (3)]);
   GGC := Unsigned_16 (Shift_Right (Request.words (3), 16) and 16#FFFF#);
   Table_Bytes := Intel_GPU_GGTT.Table_Size (GGC);
   if Plan.Status /= Admitted then
      debugPrint ("intel-gpu: resource rejected " & Admission_Status'Image (Plan.Status) & ASCII.LF);
      return;
   end if;
   debugPrint ("intel-gpu: GGC=" & Hex (Unsigned_32 (GGC)) &
     " GGTT table bytes" & Unsigned_64'Image (Table_Bytes) & ASCII.LF);
   Result := syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base,
                      Register_Virtual_Base, Plan.Bytes / 4096, 1);
   if Result /= 0 then
      debugPrint ("intel-gpu: read-only mapping denied" & ASCII.LF);
      return;
   end if;
   debugPrint ("intel-gpu: read-only register mapping ready; firmware scanout retained" & ASCII.LF);
   -- Decode v3 admits only ADLN and authenticated devmgr D0 evidence.
   -- This is observation, not a power reference or scanout takeover. No writes.
   Capture (Intel_GPU_Probe.Alder_Lake_N, True, Register_Virtual_Base,
            Plan.Bytes, Observation);
   if Observation.Captured then
      for Name in Intel_GPU_Observation.Register_Name loop
         debugPrint ("intel-gpu: snapshot " &
           (case Name is
              when Intel_GPU_Observation.Firmware_Power_Control => "FIRMWARE_POWER_CONTROL",
              when Intel_GPU_Observation.Driver_Power_Control => "DRIVER_POWER_CONTROL") & "=" &
           Hex (Observation.Values (Name)) & ASCII.LF);
      end loop;
      declare
         Forcewake_Result : constant String := Probe_Forcewake;
         GGTT_Result : constant String := Inspect_GGTT;
         Firmware_Address : System.Address;
         Firmware_Bytes : Unsigned_64;
         Firmware_Plan : Intel_GPU_Firmware.Layout;
         Firmware_Status : Intel_GPU_Firmware_File.Load_Status;
      begin
      debugPrint ("intel-gpu: forcewake " & Forcewake_Result & ASCII.LF);
      debugPrint ("intel-gpu: GGTT read-only " & GGTT_Result & ASCII.LF);
      Intel_GPU_Firmware_File.Load
        (6, Firmware_Address, Firmware_Bytes, Firmware_Plan, Firmware_Status);
      debugPrint ("intel-gpu: firmware file " &
        Intel_GPU_Firmware_File.Name (Firmware_Status) &
        " bytes" & Unsigned_64'Image (Firmware_Bytes) &
        " code" & Unsigned_64'Image (Firmware_Plan.Code_Bytes) &
        " (NOT authenticated or uploaded)" & ASCII.LF);
      Publish_Snapshot ("intel-gpu: read-only snapshot; firmware=" &
        Hex (Observation.Values (Intel_GPU_Observation.Firmware_Power_Control)) &
        " driver=" & Hex (Observation.Values (Intel_GPU_Observation.Driver_Power_Control)) &
        "; scanout retained");
      Publish_Snapshot ("intel-gpu: forcewake=" & Forcewake_Result);
      Publish_Snapshot ("intel-gpu: clock startup=" &
        Unsigned_64'Image (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 0)) &
        " HPET id=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 1) and 16#FFFF_FFFF#)) &
        " period-fs=" & Unsigned_64'Image (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 2)));
      Publish_Snapshot ("intel-gpu: clock timer offset=" &
        Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 3) and 16#FFFF_FFFF#)) &
        " before=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 4) and 16#FFFF_FFFF#)) &
        " after=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 5) and 16#FFFF_FFFF#)));
      if Forcewake_Result = "release-ready; domains-release-ready" then
         declare
            Mapping_Result : constant String := Map_Reset_Pages;
         begin
            debugPrint ("intel-gpu: reset pages " & Mapping_Result & ASCII.LF);
            Publish_Snapshot ("intel-gpu: reset pages " & Mapping_Result);
         end;
      end if;
      Publish_Snapshot ("intel-gpu: media fuse=" & Hex (Media_Fuse) &
        " inventory-valid=" & Boolean'Image (Engine_Inventory.Valid));
      if Engine_Inventory.Valid then
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            if Engine_Inventory.Domains (D) then
               Publish_Snapshot ("intel-gpu: required forcewake " &
                 Intel_GPU_ADLN_Inventory.Domain'Image (D));
            end if;
         end loop;
      end if;
      Publish_Snapshot ("intel-gpu: file=" &
        Intel_GPU_Firmware_File.Name (Firmware_Status) &
        " bytes=" & Unsigned_64'Image (Firmware_Bytes));
      Publish_Snapshot ("intel-gpu: ggtt=" &
        Unsigned_64'Image (Table_Bytes) & "; " & GGTT_Result);
      if Firmware_Status = Intel_GPU_Firmware_File.Loaded then
         declare
            Prepared : constant String := Intel_GPU_Firmware_Buffer.Prepare
              (Firmware_Address, Firmware_Bytes);
         begin
            debugPrint ("intel-gpu: firmware buffer " & Prepared & ASCII.LF);
            Publish_Snapshot ("intel-gpu: firmware buffer " & Prepared);
         end;
      end if;
      declare
         Buffer_View : constant Intel_GPU_Firmware_Buffer.Prepared_Buffer :=
           Intel_GPU_Firmware_Buffer.Prepared;
      begin
         if Buffer_View.Ready then
            if Buffer_View.DMA_Address = 0 or else
              Buffer_View.DMA_Address mod 4096 /= 0 or else
              Buffer_View.Allocation_Bytes /= 1024 * 1024 or else
              Buffer_View.DMA_Address > 2 ** 32 - Buffer_View.Allocation_Bytes or else
              Buffer_View.CPU_Address /= 16#6100_0000# or else
              Buffer_View.Content_Bytes /= Firmware_Bytes or else
              Firmware_Status /= Intel_GPU_Firmware_File.Loaded
            then
               debugPrint ("intel-gpu: firmware descriptor inconsistent" & ASCII.LF);
               return;
            end if;
            Publish_Snapshot ("intel-gpu: firmware descriptor ready; bytes=" &
              Unsigned_64'Image (Buffer_View.Content_Bytes) & " capacity=" &
              Unsigned_64'Image (Buffer_View.Allocation_Bytes));
         else
            Publish_Snapshot ("intel-gpu: firmware descriptor unavailable");
         end if;
      end;
      end;
   else
      debugPrint ("intel-gpu: snapshot unavailable" & ASCII.LF);
   end if;
   if Enable_Native_Reset and then Reset_Authorized then
      Publish_Snapshot ("intel-gpu: native reset beginning; scanout retained");
         declare
            Reset_Result : constant String := Intel_GPU_Native_Reset.Execute (Media_Fuse);
         begin
            debugPrint ("intel-gpu: native reset " & Reset_Result & ASCII.LF);
            Publish_Snapshot ("intel-gpu: native reset " & Reset_Result);
         end;
   elsif Reset_Pages_Mapped and then Enable_Native_Reset then
      Publish_Snapshot ("intel-gpu: native reset authorization unavailable");
   end if;
   -- Retain device, backing and forcewake on success or uncertain failure.
   loop
      receive (Sender, Request);
      debugPrint ("intel-gpu: unsupported request ignored" & ASCII.LF);
   end loop;
end Main;
