with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Native_GPU_Probe_Protocol;

-- Test-only RAM producer, not an Intel emulator or production supervisor.
-- The test manifest installs both service calls to this endpoint and grants
-- Desktop21 independently. Production devmgr/Intel admission is NOT covered.
procedure Owner is
   package Q renames Native_GPU_Probe_Protocol;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   type Pixels is array (Natural range 0 .. 4095) of Unsigned_32;
   Image : Pixels := [others => 16#FF00_0000#] with Alignment => 4096;
   From, Viewer : ProcessID := 0;
   Msg, Answer : Message;
   Root : G.Grant_Reference;
   Reference : Unsigned_64 := 0;
   Retiring, OK : Boolean := False;
   Ignore, Slot, Generation : Unsigned_64;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL gpu-viewer synthetic owner" & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   -- A visible magenta square distinguishes this fixture from the red NUC
   -- triangle. No claim about rasterization or GPU coherence follows.
   for Y in 16 .. 47 loop
      for X in 16 .. 47 loop Image (Y * 64 + X) := 16#FFFF_00FF#; end loop;
   end loop;
   Check (registerDriver (DRIVER_IPCTEST) /= Unsigned_64'Last);
   debugPrint ("gpu-viewer fixture: synthetic RAM producer ready" & ASCII.LF);
   loop
      receive (From, Msg);
      Answer := NULL_MESSAGE;
      Answer.tag := (Msg.tag.label, 4, 0, 0);
      Answer.words := [1, 1, 0, 0];
      if Msg.tag = (Q.Desktop_Binding_Label, 4, 0, 0) and then
        Msg.words = [1, 0, 0, 0] and then (Viewer = 0 or else Viewer = From)
      then
         Viewer := From;
         -- Desktop was granted by procmgr from the TEST manifest.
         Answer.words := [0, 1, 0, 0];
      elsif Msg.tag = (Q.Label, 4, 0, 0) and then Viewer /= 0 and then
        From = Viewer and then Q.Valid_Request (Q.Words (Msg.words))
      then
         if Msg.words (1) = 0 and then not Retiring then
            if Reference = 0 then
               Slot := syscall (SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID,
                 Unsigned_64 (From),
                 Unsigned_64 (System.Storage_Elements.To_Integer (Image'Address)),
                 4, 2);
               Check (Slot /= Unsigned_64'Last);
               Generation := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Slot);
               Check (Generation in 1 .. G.MAXIMUM_GENERATION);
               Root := (Slot, Generation);
               Reference := R.Encode (Root);
            end if;
            Answer.words := MessageWords (Q.Reply (Q.Success, Reference));
         elsif Msg.words (1) = 1 and then Reference /= 0 and then
           Msg.words (2) = Reference
         then
            if not Retiring then
               G.Revoke (Root, OK);
               Check (OK);
               Retiring := True;
            end if;
            if G.Retirement_Confirmed (Root) then
               Answer.words := MessageWords (Q.Reply (Q.Success));
               debugPrint ("GPU-VIEWER-ROOT-RETIRED: PASS" & ASCII.LF);
            else Answer.words := MessageWords (Q.Reply (Q.Pending)); end if;
         end if;
      end if;
      Ignore := reply (From, Answer);
      Check (Ignore = 1);
   end loop;
end Owner;
