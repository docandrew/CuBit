with Interfaces; use Interfaces;
with System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with Native_GPU_Buffers;
with Native_GPU_Memory;
with Native_GPU_Query;
pragma Elaborate_All (Native_GPU_Query);

procedure Main is
   package Buffers renames Native_GPU_Buffers;
   package Memory renames Native_GPU_Memory;
   Slot : constant Unsigned_64 := CCL_Manifest_Bindings.Slot_Render;
   GPU_Address : constant Unsigned_64 := 16#1_0000_0000#;
   Handle, Mapping : aliased Unsigned_32 := 0;
   Reference, CPU, Retired : aliased Unsigned_64 := 0;
   Status : Unsigned_32;
   Failed : Boolean := False;
   Writer : CuBit.Logging.Publisher
     (CapabilitySlot (CCL_Manifest_Bindings.Slot_Logstore));
   Logging_Enabled : Boolean := True;
   function Discovery_Check (Endpoint : Unsigned_64) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_test_native_discovery";

   procedure Report (Text : String) is
      Value : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Ignore_Submitted : Boolean;
   begin
      debugPrint (Text & ASCII.LF);
      if Logging_Enabled and then Value.Success then
         --  A copy into the ring: no IPC, no completion to wait for.
         CuBit.Logging.Emit (Writer, Value.Value, Ignore_Submitted);
      end if;
   end Report;

   procedure Drain_Logger is
      Done, Drained : Boolean;
      Ignore : Unsigned_64;
   begin
      CuBit.Logging.Flush (Writer, Drained);
      CuBit.Logging.Disconnect (Writer, Done);
      if not Done then
         debugPrint ("RENDER-SESSION logger retiring; storage retained" & ASCII.LF);
      end if;
      while not Done loop
         CuBit.Logging.Disconnect (Writer, Done);
         if not Done then Ignore := syscall (SYSCALL_SLEEP, 100); end if;
      end loop;
   end Drain_Logger;

   procedure Check (Step : String; Value : Unsigned_32) is
   begin
      Report ("RENDER-SESSION " & Step & " status=" & Value'Image);
      if Value /= 0 then Failed := True; end if;
   end Check;

   procedure Exercise is
   begin
      Check ("health", Buffers.Session_Status (Slot));
      if Failed then return; end if;
      Check ("mesa-query-decoders", Discovery_Check (Slot));
      if Failed then return; end if;
      Status := Buffers.Memory_Contract (Slot);
      Report ("RENDER-SESSION memory-policy=" & Status'Image);
      if Status not in 1 .. 2 then Failed := True; return; end if;
      -- Explicit-maintenance memory is sufficient for this CPU-only test.
      -- No GPU execution or CPU/GPU coherence is inferred from these writes.
      Check ("create", Buffers.Create (Slot, 4096, Handle'Access));
      if Failed then return; end if;
      Check ("map", Buffers.Map
        (Slot, Handle, 0, 4096, 1, Mapping'Access, Reference'Access));
      if Failed then return; end if;
      Check ("acquire", Memory.Acquire (Slot, Reference, 0, 4096, 1, CPU'Access));
      if Failed then return; end if;
      if CPU = 0 or else CPU mod 4 /= 0 or else CPU > Unsigned_64'Last - 4095 then
         Failed := True;
      else
         declare
            type Page is array (Natural range 0 .. 1023) of Unsigned_32
              with Volatile_Components;
            View : Page with Import, Address =>
              System.Storage_Elements.To_Address
                (System.Storage_Elements.Integer_Address (CPU));
         begin
            View (0) := 16#4355_4249#;
            View (1023) := 16#5245_4E44#;
            if View (0) /= 16#4355_4249# or else View (1023) /= 16#5245_4E44# then
               Failed := True;
            end if;
         end;
      end if;
      Check ("return-borrow", Memory.Return_Borrow (Reference));
      if Failed then return; end if;
      -- A pending retirement is not permission to replay Map or free backing.
      Status := Buffers.Retire_Map (Slot, Mapping);
      if Status = 4 then
         Report ("RENDER-SESSION mapping retirement pending; backing retained");
      else
         Check ("retire-map", Status);
      end if;
      if Failed then return; end if;
      -- Use the same BO throughout; no CPU/DMA address crosses this API.
      -- Offline unbind/rebind tests names without freeing physical backing.
      Check ("bind", Buffers.Bind_GPU (Slot, Handle, GPU_Address, 0, 4096));
      if Failed then return; end if;
      Check ("unbind", Buffers.Unbind_GPU (Slot, Handle, GPU_Address, 0, 4096));
      if Failed then return; end if;
      Check ("rebind", Buffers.Bind_GPU (Slot, Handle, GPU_Address, 0, 4096));
      if Failed then return; end if;
      Check ("prepare-context", Buffers.Prepare_Context (Slot));
      if Failed then return; end if;
      Check ("register-context", Buffers.Register_Context (Slot));
      -- The driver executes its own initialization marker and disables the
      -- context before success. The app's BO contains data, never commands;
      -- do not submit it or close its name while the sealed VM retains it.
   end Exercise;
begin
   Report ("RENDER-SESSION admitted app entered");
   Exercise;
   -- Exactly one close attempt, also on error. Uncertain resources stay held
   -- by the driver; this app never treats session close as completed reclaim.
   Check ("close-session", Buffers.Close_Session (Slot, Retired'Access));
   if Failed then
      Report ("TEST: FAIL native render session");
   else
      Report ("RENDER-SESSION PASS private context initialized (NO application batch)");
   end if;
   Drain_Logger;
end Main;
