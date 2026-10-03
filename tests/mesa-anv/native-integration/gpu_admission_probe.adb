with Interfaces; use Interfaces;
with CuBit.Capability_Grants;
with Intel_Render_Admission;
with Intel_Render_Admission_Native;
with Intel_Render_Admission_Dispatch;
with Intel_GPU_Render_Control;
with Intel_GPU_Device_Query;
with Native_GPU_Query;
with Native_GPU_Buffers;
package body GPU_Admission_Probe is
   use CuBit.Messages;
   package Core renames Intel_Render_Admission;
   package Native renames Intel_Render_Admission_Native;
   package GPU renames Intel_GPU_Render_Control;
   use type Core.Phase;
   Controller : GPU.Controller;
   Bound : Boolean := False;
   Remote_Label : constant Unsigned_32 := 16#0A31#;
   -- Recipient endpoints own slots40..55. Do not combine this probe with
   -- the legacy IPC pressure slot30..45 writer; budget owns slot60.
   Remote_Slot : constant CapabilitySlot := 59;
   Remote_Token : Unsigned_64 := 16#AD13_0000#;
   Remote_Pending : Boolean := False;
   procedure Server (Sender : ProcessID; Request : Message) is
      Payload : GPU.Words;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
      Remote_Request : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Identity : Unsigned_64;
      Stored : Natural;
      Recipient_Ready : Boolean := False;
   begin
      if not Bound then
         GPU.Bind (Controller, Unsigned_64 (Sender), Request.authorityTag);
         Bound := True;
      end if;
      if Request.tag.label = Intel_GPU_Device_Query.Label then
         declare
            package DQ renames Intel_GPU_Device_Query;
            Data : constant DQ.Snapshot := (16#46D2#, 0, True, 1, 16#FFFF#);
            Value : constant DQ.Words := DQ.Respond
              (Data, Request.tag.label, Request.tag.length, Request.tag.flags,
               Request.tag.reserved,
               [Request.words (0), Request.words (1), Request.words (2), Request.words (3)],
               Memory_Policy => DQ.Owned_WB_Explicit_Maintenance);
         begin
            Payload := [Value (0), Value (1), Value (2), Value (3)];
         end;
      elsif Request.tag.label = Remote_Label then
         Payload := [GPU.Bad_Request, GPU.Version, 0, 0];
         if Request.words = [1, 0, 0, 0] and not Remote_Pending then
            Remote_Request.tag := (GPU.Status_Label, 4, 0, 0);
            Remote_Request.words := [GPU.Version, 0, 0, 0];
            Remote_Token := Remote_Token + 1;
            Remote_Pending := capSubmit
              (Remote_Slot, Remote_Request, Remote_Token);
            Payload (0) := (if Remote_Pending then GPU.OK
                           else GPU.Unavailable);
         elsif Request.words = [2, 0, 0, 0] and Remote_Pending then
            Payload (0) := GPU.Unavailable;
            if Poll_Completion (Receipt'Address) = 1 then
               Remote_Pending := False;
               if Receipt.valid and Receipt.token = Remote_Token and
                 Receipt.status = COMPLETION_OK and
                 Receipt.from = syscall (SYSCALL_GETPID) and
                 Receipt.msg.tag = (GPU.Status_Label, 4, 0, 0)
               then
                  Payload := [GPU.OK, Receipt.msg.words (0), 0, 0];
               else
                  Payload (0) := GPU.Bad_State;
               end if;
            end if;
         end if;
      elsif Request.tag.label = GPU.Status_Label then
         Payload := GPU.Session_Status (Controller, Unsigned_64 (Sender),
           Request.authorityTag, True, Request.tag.label, Request.tag.length,
           Request.tag.flags, Request.tag.reserved,
           [Request.words (0), Request.words (1),
            Request.words (2), Request.words (3)]);
      else
         Identity := GPU.Activation_Identity
           (Controller, Unsigned_64 (Sender), Request.authorityTag,
            Request.tag.label, Request.tag.length, Request.tag.flags,
            Request.tag.reserved,
            [Request.words (0), Request.words (1),
             Request.words (2), Request.words (3)]);
         if Identity /= 0 then
            Stored := GPU.Storage_Index (Controller, Request.words (2));
            if Stored /= 0 then
               Recipient_Ready := CuBit.Capability_Grants.Endpoint_Matches
                 (CapabilitySlot (39 + Stored), Identity);
            end if;
         end if;
         GPU.Handle (Controller, Unsigned_64 (Sender), Request.authorityTag,
        True, Request.tag.label, Request.tag.length, Request.tag.flags,
        Request.tag.reserved,
        [Request.words (0), Request.words (1),
         Request.words (2), Request.words (3)], Payload,
         Recipient_Ready => Recipient_Ready);
      end if;
      Response.tag := (Request.tag.label, 4, 0, 0);
      Response.words := [Payload (0), Payload (1), Payload (2), Payload (3)];
      Delivered := reply (Sender, Response);
      if Delivered /= 1 then
         debugPrint ("TEST: FAIL GPU-ADMISSION-IPC reply" & ASCII.LF);
      end if;
   end Server;
   procedure Client (Slot : CapabilitySlot) is
      Item : Native.Broker_Request;
      Target : constant CuBit.Capability_Grants.Recipient :=
        CuBit.Capability_Grants.Capture (CAP_SLOT_SELF_PROC);
      Receipt : aliased CompletionEntry;
      type Receipt_Bytes is array (0 .. 87) of Unsigned_8
        with Component_Size => 8;
      Raw : Receipt_Bytes with Import, Address => Receipt'Address,
        Volatile;
      Token : Unsigned_64 := 16#AD11_0000#;
      Used : Boolean;
      Saw_Reservation, Saw_Abort : Boolean := False;
      Saw_Recipient_Slot : Boolean := False;
      Passed : Boolean := True;
      Ignored : Unsigned_64;
   begin
      -- The self endpoint is inspectable but deliberately lacks GRANT. Use
      -- it rather than an absent fixture slot: reach actual kernel delegation
      -- denial, not only the adapter's Endpoint_Matches precheck.
      Passed := CuBit.Capability_Grants.Endpoint_Matches
        (CapabilitySlot (CAP_SLOT_SELF),
         CuBit.Capability_Grants.Incarnation (Target));
      debugPrint ("GPU-ADMISSION-IPC self endpoint inspected=" &
        Boolean'Image (Passed) & ASCII.LF);
      Native.Start (Item, Target, Slot, CapabilitySlot (CAP_SLOT_SELF), 32);
      for Poll in 1 .. 5_000 loop
         case Native.State (Item) is
            when Core.Reserve_Ready | Core.Abort_Ready =>
               if Native.State (Item) = Core.Abort_Ready then
                  Saw_Abort := True;
               end if;
               Token := Token + 1;
               Native.Advance (Item, Token);
            when Core.Delegate_Ready =>
               Saw_Reservation := True;
               Native.Advance (Item, Token);
               Passed := Passed and Native.State (Item) in
                 Core.Delegate_Ready | Core.Abort_Ready;
            when Core.Activate_Ready | Core.Active =>
               Passed := False;
               Native.Cancel (Item);
            when others => null;
         end case;
         exit when Native.State (Item) in Core.Failed | Core.Quarantined;
         --  Poison the whole output, including padding, before the kernel
         --  writes it. Sender high bits and reserved bytes must be replaced.
         Raw := (others => 16#A5#);
         if Poll_Completion (Receipt'Address) = 1 then
            for Byte in 81 .. 87 loop
               Passed := Passed and Raw (Byte) = 0;
            end loop;
            debugPrint ("GPU-ADMISSION-IPC reply status=" &
              Unsigned_64'Image (Receipt.status) & " from=" &
              Unsigned_64'Image (Receipt.from) & " code=" &
              Unsigned_64'Image (Receipt.msg.words (0)) & ASCII.LF);
            if Native.State (Item) = Core.Reserve_Pending and
              Receipt.status = COMPLETION_OK and
              Receipt.msg.words (0) = GPU.OK
            then
               Saw_Recipient_Slot := Receipt.msg.words (3) in 40 .. 55;
               Passed := Passed and Saw_Recipient_Slot;
               debugPrint ("GPU-ADMISSION-IPC reserve recipient slot=" &
                 Unsigned_64'Image (Receipt.msg.words (3)) & ASCII.LF);
            end if;
            Native.Complete (Item, Receipt, Used);
            Passed := Passed and Used;
         end if;
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Passed := Passed and Saw_Reservation and Saw_Recipient_Slot and
        Saw_Abort and
        Native.State (Item) = Core.Failed;
      if not Passed then
         debugPrint ("GPU-ADMISSION-IPC state=" &
           Core.Phase'Image (Native.State (Item)) & " reserved=" &
           Boolean'Image (Saw_Reservation) & " abort=" &
           Boolean'Image (Saw_Abort) & ASCII.LF);
      end if;
      debugPrint ((if Passed then
        "GPU-ADMISSION-IPC: PASS async reserve, delegation denied, abort"
        else "TEST: FAIL GPU-ADMISSION-IPC client") & ASCII.LF);
   end Client;

   procedure Authorized_Client (Slot : CapabilitySlot;
                                Cross_Process : Boolean := False) is
      Item : Native.Broker_Request;
      Target : constant CuBit.Capability_Grants.Recipient :=
        CuBit.Capability_Grants.Capture
          (if Cross_Process then Slot else CAP_SLOT_SELF_PROC);
      Receipt : aliased CompletionEntry;
      Token : Unsigned_64 := 16#AD12_0000#;
      Used, Passed : Boolean := True;
      Saw_Active, Saw_Abort : Boolean := False;
      Ignored : Unsigned_64;
      Destination : constant CapabilitySlot :=
        (if Cross_Process then Remote_Slot else 32);
      function Status_Is (Expected : Unsigned_64) return Boolean is
         Msg : Message := NULL_MESSAGE;
         Tag : MessageTag;
         Delay_Result : Unsigned_64;
      begin
         if not Cross_Process then
            Msg.tag := (GPU.Status_Label, 4, 0, 0);
            Msg.words := [GPU.Version, 0, 0, 0];
            Tag := capCall (Destination, Msg);
            return Tag = (GPU.Status_Label, 4, 0, 0) and
              Msg.words (0) = Expected;
         end if;
         Msg.tag := (Remote_Label, 4, 0, 0);
         Msg.words := [1, 0, 0, 0];
         Tag := capCall (Slot, Msg);
         if Tag /= (Remote_Label, 4, 0, 0) or Msg.words (0) /= GPU.OK
         then return False; end if;
         for Poll in 1 .. 5_000 loop
            Msg.tag := (Remote_Label, 4, 0, 0);
            Msg.words := [2, 0, 0, 0];
            Tag := capCall (Slot, Msg);
            if Tag /= (Remote_Label, 4, 0, 0) then return False; end if;
            if Msg.words (0) /= GPU.Unavailable then
               return Msg.words (0) = GPU.OK and Msg.words (1) = Expected;
            end if;
            Delay_Result := syscall (SYSCALL_SLEEP, 1);
         end loop;
         return False;
      end Status_Is;
   begin
      Native.Start (Item, Target, Slot,
                    (if Cross_Process then Slot else 37), Destination);
      for Poll in 1 .. 5_000 loop
         case Native.State (Item) is
            when Core.Reserve_Ready | Core.Activate_Ready |
                 Core.Abort_Ready | Core.Delegate_Ready =>
               Saw_Abort := Saw_Abort or
                 Native.State (Item) = Core.Abort_Ready;
               Token := Token + 1;
               Native.Advance (Item, Token);
            when Core.Active =>
               Saw_Active := True;
               -- The installed cap is usable but not further delegable.
               if not Cross_Process then
                  Passed := Passed and
                    CuBit.Capability_Grants.Delegate_Endpoint
                      (Target, Destination, 33, 3, 1) = Unsigned_64'Last;
               end if;
               Passed := Passed and Status_Is (GPU.OK);
               Native.Cancel (Item);
            when others => null;
         end case;
         exit when Native.State (Item) in Core.Failed | Core.Quarantined;
         if Poll_Completion (Receipt'Address) = 1 then
            Native.Complete (Item, Receipt, Used);
            Passed := Passed and Used;
         end if;
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Passed := Passed and Saw_Active and Saw_Abort and
        Native.State (Item) = Core.Failed;
      if Saw_Active then
         -- Kernel cap may remain, but retired session authority must not.
         Passed := Passed and Status_Is (GPU.Denied);
      end if;
      debugPrint ((if Passed then
        (if Cross_Process then
          "GPU-ADMISSION-IPC: PASS cross-process activate, use, retire"
         else "GPU-ADMISSION-IPC: PASS authorized activate, attenuate, retire")
        else "TEST: FAIL GPU-ADMISSION-IPC authorized client") & ASCII.LF);
   end Authorized_Client;

   procedure Dispatch_Client (Slot : CapabilitySlot) is
      package D is new Intel_Render_Admission_Dispatch
        (2, 16#AD15_0000#, 16#AD15_FFFF#);
      Object : D.Dispatcher;
      First, Second : D.Ticket;
      Target : constant CuBit.Capability_Grants.Recipient :=
        CuBit.Capability_Grants.Capture (CAP_SLOT_SELF_PROC);
      Receipt : aliased CompletionEntry;
      Used, Passed : Boolean := True;
      Tick : Unsigned_64 := syscall (SYSCALL_GETTIME);
      Deadline : Unsigned_64 := 0;
      Waits : Natural := 0;
      function Status_Is (Endpoint : CapabilitySlot; Expected : Unsigned_64)
        return Boolean is
         Msg : Message := NULL_MESSAGE;
         Tag : MessageTag;
      begin
         Msg.tag := (GPU.Status_Label, 4, 0, 0);
         Msg.words := [GPU.Version, 0, 0, 0];
         Tag := capCall (Endpoint, Msg);
         return Tag = (GPU.Status_Label, 4, 0, 0) and
           Msg.words (0) = Expected;
      end Status_Is;
      procedure Pump (First_State, Second_State : Core.Phase) is
         Limit : Unsigned_64;
         Wake : Unsigned_64;
      begin
         Tick := syscall (SYSCALL_GETTIME);
         if Tick > Unsigned_64'Last - 5_000 then
            Passed := False;
            return;
         end if;
         Limit := Tick + 5_000;
         for Poll in 1 .. 5_000 loop
            Tick := syscall (SYSCALL_GETTIME);
            D.Step (Object, Tick);
            if Poll_Completion (Receipt'Address) = 1 then
               D.Complete (Object, Receipt, Tick, Used);
               Passed := Passed and Used;
            end if;
            exit when D.State (Object, First) = First_State and
              D.State (Object, Second) = Second_State;
            exit when Tick >= Limit;
            if not D.Runnable (Object) then
               -- Test watchdog bounds pending cleanup; it does not revoke
               -- capabilities or manufacture a successful retirement.
               Wake := Unsigned_64'Min (Limit, D.Next_Deadline (Object));
               Waits := Waits + 1;
               if Wait_For_Activity_Until (Wake) = Unavailable then
                  Passed := False;
                  exit;
               end if;
            end if;
         end loop;
         Passed := Passed and D.State (Object, First) = First_State and
           D.State (Object, Second) = Second_State;
      end Pump;
   begin
      if Tick > Unsigned_64'Last - 10_000 then
         debugPrint ("TEST: FAIL GPU-ADMISSION-IPC clock" & ASCII.LF);
         return;
      end if;
      Deadline := Tick + 5_000;
      D.Start (Object, Target, Slot, 37, 35, Tick, Deadline, First);
      D.Start (Object, Target, Slot, 37, 36, Tick, Deadline, Second);
      Passed := First /= 0 and Second /= 0 and First /= Second;
      Pump (Core.Active, Core.Active);
      if Passed then
         Passed := Status_Is (35, GPU.OK) and Status_Is (36, GPU.OK);
      end if;
      D.Cancel (Object, First);
      Pump (Core.Failed, Core.Active);
      if Passed then
         Passed := Status_Is (35, GPU.Denied) and Status_Is (36, GPU.OK);
      end if;
      D.Cancel (Object, Second);
      Pump (Core.Failed, Core.Failed);
      if Passed then Passed := Status_Is (36, GPU.Denied); end if;
      debugPrint ((if Passed then
        "GPU-ADMISSION-IPC: PASS dispatcher two sessions, isolated cancellation"
        else "TEST: FAIL GPU-ADMISSION-IPC dispatcher") & ASCII.LF);
      debugPrint ("GPU-ADMISSION-IPC activity waits=" & Natural'Image (Waits) & ASCII.LF);
      Memory_Client (Slot);
   end Dispatch_Client;

   procedure Memory_Client (Slot : CapabilitySlot) is
      use type Native_GPU_Query.Reply_Words;
      Value : aliased Native_GPU_Query.Reply_Words := [others => Unsigned_64'Last];
      Status : Unsigned_32;
      Passed : Boolean;
   begin
      -- Real native FFI and capability IPC, synthetic service policy only.
      Status := Native_GPU_Query.Execute (Unsigned_64 (Slot), 3, Value'Access);
      Passed := Status = 0 and then Value = [0, 1, 1, 0];
      Passed := Passed and then
        Native_GPU_Buffers.Memory_Contract (Unsigned_64 (Slot)) = 1 and then
        Native_GPU_Buffers.Memory_Contract (64) = 0;
      Status := Native_GPU_Query.Execute (Unsigned_64 (Slot), 4, Value'Access);
      Passed := Passed and Status = 0 and Value = [2, 1, 0, 0];
      Status := Native_GPU_Query.Execute (Unsigned_64 (Slot), 5, Value'Access);
      Passed := Passed and then Status = 1 and then Value = [0, 0, 0, 0];
      debugPrint ((if Passed then "TEST: PASS GPU-MEMORY-QUERY-IPC"
                   else "TEST: FAIL GPU-MEMORY-QUERY-IPC") & ASCII.LF);
   end Memory_Client;
end GPU_Admission_Probe;
