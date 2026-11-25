------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Address-space tag checks (headless --test tlb-tags): no process sees
--  translations another address space left under its tag (PCID).
--
--  @description
--  Launches tlb-probe life after life through procmgr. Each life gets the
--  lowest free PID, so usually the previous life's, and with it the same
--  address-space tag. Every life reports whether any page of its fresh
--  Buffer read as anything but zero: an earlier life's translation kept
--  across the PID's reuse (exit, then reuse). Every second life is a
--  revoke life: this program grants it pages; one of its threads reads
--  them and waits, its main thread gives the acquisition back, and this
--  program revokes the grant, so the kernel unmaps the pages while this
--  program's address space runs and shoots down every CPU's translations.
--  The reader's next read must fault (the kernel kills the life), or the
--  life reports reading them (revocation, unmap).
--
--  Only CPUs with PCID and INVPCID tag translations, in kernels built with
--  TAGGED_TLB=1 (Intel; not the AMD hosts' KVM guests, nor QEMU's TCG,
--  which implements neither): elsewhere these checks cannot see a stale
--  translation and check the untagged path only.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Grant_References;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Memory_Grants;
with CuBit.Monotonic;
with CuBit.Process_IDs;
with CuBit.Waits;
with Check_Memory; use Check_Memory;
with Startup;

procedure Main is
   use ASCII;
   package GR renames CuBit.Grant_References;
   package G renames CuBit.Memory_Grants;
   --  The protocol (tlb-probe's main.adb has the same numbers).
   OP_HELLO  : constant Unsigned_32 := 16#0B10#;
   OP_FRESH  : constant Unsigned_32 := 16#0B11#;
   OP_GRANT  : constant Unsigned_32 := 16#0B12#;
   OP_REVOKE : constant Unsigned_32 := 16#0B13#;
   OP_STALE  : constant Unsigned_32 := 16#0B14#;
   OP_DONE   : constant Unsigned_32 := 16#0B15#;
   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   REPLY_FAILED : constant Unsigned_32 := 16#F002#;
   Mode_Exit   : constant Unsigned_64 := 0;
   Mode_Revoke  : constant Unsigned_64 := 1;
   Pattern : constant Unsigned_64 := 16#7AB1_E5C0_FFEE_0001#;

   Lives : constant := 12;
   --  A probe answers within Answer_Ms; a revoke life that faulted as it
   --  must says nothing more for Fault_Ms; a life's PID is free again (its
   --  process reclaimed) Reap_Ms after it ends.
   Answer_Ms : constant := 30_000;
   Fault_Ms : constant := 3_000;
   Reap_Ms : constant := 300;
   Retire_Ms : constant := 3_000;

   Process_Manager : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Services.Process_Manager.Handle (Startup.Process_Manager);
   --  Its Launch badge into the user session (CAP-012).
   Launch : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Services.Process_Manager.Handle (Startup.Launch);

   Failures : Natural := 0;
   Ignore : Unsigned_64;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("tlb-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("tlb-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (V : Unsigned_64) return String is
      S : constant String := V'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Reply (Label : Unsigned_32; A, B, C : Unsigned_64 := 0) return Message is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => Label, length => 3, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := A;
      M.words (1) := B;
      M.words (2) := C;
      return M;
   end Reply;

   --  procmgr's OP_LAUNCH (CuBit.Launch_Arguments): the file name in a page
   --  granted to procmgr, no arguments, through its Launch badge; the probe
   --  is in this program's launch table and asks only for ipc-test, an
   --  ordinary service in the user session's envelope.
   type Page is array (Positive range 1 .. Page_Bytes) of Unsigned_8
     with Alignment => Page_Bytes;
   Name_Page : Page := [others => 0];

   function Spawn_Probe return Boolean is
      Filename : constant String := "tlb-probe.app";
      Grant : G.Grant_Reference;
      Created, Revoked : Boolean;
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Name_Page := [others => 0];
      for I in Filename'Range loop
         Name_Page (I) := Character'Pos (Filename (I));
      end loop;
      G.Create_Via_Capability (Process_Manager, Name_Page'Address, 1, False, Grant, Created);
      if not Created then
         return False;
      end if;
      Msg.tag := (label => CuBit.Launch_Arguments.Launch_Operation,
                  length => CuBit.Launch_Arguments.Launch_Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [GR.Encode (Grant), Filename'Length, 0,
                    CuBit.Launch_Grants.Request_Word (0)];
      Tag := capCall (Launch, Msg, Deadline_After (Answer_Ms));
      G.Revoke (Grant, Revoked);
      return Tag.label = REPLY_OK;
   end Spawn_Probe;

   --  A process identity's slot (the PID, without its generation): what
   --  the address-space tag follows.
   function Slot_Of (Word : Unsigned_64) return Unsigned_64 is
     (Word mod 2 ** CuBit.Process_IDs.Slot_Bits);

   --  Slots earlier lives had, and where their Buffer was.
   Seen_Slots : array (1 .. Lives) of Unsigned_64 := [others => 0];
   Seen_Count : Natural := 0;
   First_Buffer : Unsigned_64 := 0;
   Reused, Same_Buffer, Revoke_Faults, Fresh_Lives : Natural := 0;

   --  Serve one life until it ends, then revoke what it was granted.
   procedure Serve (Life : Unsigned_64; Mode : Unsigned_64) is
      from : Process_ID;
      msg : Message;
      received : Boolean;
      Deadline : Unsigned_64 := CuBit.Monotonic.Milliseconds + Answer_Ms;
      Awaiting_Fault : Boolean := False;
      Reference : G.Grant_Reference;
      Granted, Retired : Boolean := False;
      Name : constant String := "life " & Image (Life);
      procedure Run is
      begin
         loop
            receiveUntil (Deadline, from, msg, received);
            if not received then
               if Awaiting_Fault then
                  Revoke_Faults := Revoke_Faults + 1;
                  Check (True, Name & ": the revoked grant faulted");
               else
                  Check (False, Name & ": the probe answered in time");
               end if;
               return;
            end if;
            if msg.tag.label = OP_HELLO then
               --  words: the probe's PID and its Buffer's address.
               if (for some I in 1 .. Seen_Count => Seen_Slots (I) = Slot_Of (msg.words (0))) then
                  Reused := Reused + 1;
                  if msg.words (1) = First_Buffer then
                     Same_Buffer := Same_Buffer + 1;
                  end if;
               elsif Seen_Count < Seen_Slots'Last then
                  Seen_Count := Seen_Count + 1;
                  Seen_Slots (Seen_Count) := Slot_Of (msg.words (0));
               end if;
               if First_Buffer = 0 then
                  First_Buffer := msg.words (1);
               end if;
               debugPrint ("tlb-check: " & Name & " pid " & Image (Slot_Of (msg.words (0))) &
                           (if Mode = Mode_Revoke then " (revoke)" else " (exit)") & LF);
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK, Life, Mode, Pattern));
            elsif msg.tag.label = OP_FRESH then
               --  words: pages of the fresh Buffer that read as not zero.
               Check (msg.words (0) = 0,
                      Name & ": a fresh life reads no earlier life's pages (" &
                      Image (msg.words (0)) & " not zero)");
               Fresh_Lives := Fresh_Lives + 1;
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
               Deadline := CuBit.Monotonic.Milliseconds + Answer_Ms;
            elsif msg.tag.label = OP_GRANT and then Mode = Mode_Revoke then
               declare
                  Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
               begin
                  if Saved.Saved then
                     G.Create_Via_Reply (Saved.Handle, Region'Address, Granted_Pages, False,
                                         Reference, Granted);
                     Ignore := replyCap
                       (Saved.Handle,
                        Reply ((if Granted then REPLY_OK else REPLY_FAILED),
                               (if Granted then GR.Encode (Reference) else 0)));
                  else
                     Ignore := CuBit.Messages.reply (from, Reply (REPLY_FAILED));
                  end if;
                  Check (Granted, Name & ": pages granted");
               end;
            elsif msg.tag.label = OP_REVOKE and then Granted then
               --  words: granted pages the reader saw the pattern in, and
               --  whether giving the acquisition back succeeded. Revoking
               --  now completes the revocation: the kernel unmaps the
               --  pages from the probe here, in this address space.
               Check (msg.words (0) = Granted_Pages, Name & ": the reader read the granted pages");
               Check (msg.words (1) = 1, Name & ": the probe gave the acquisition back");
               declare
                  Revoked : Boolean;
                  Retire_By : constant Unsigned_64 := CuBit.Monotonic.Milliseconds + Retire_Ms;
               begin
                  G.Revoke (Reference, Revoked);
                  while Revoked and then not G.Retirement_Confirmed (Reference)
                    and then CuBit.Monotonic.Milliseconds < Retire_By
                  loop
                     CuBit.Waits.Sleep_For (1);
                  end loop;
                  Retired := Revoked and then G.Retirement_Confirmed (Reference);
                  Check (Retired, Name & ": the grant revoked and retired");
               end;
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
               Awaiting_Fault := True;
               Deadline := CuBit.Monotonic.Milliseconds + Fault_Ms;
            elsif msg.tag.label = OP_STALE then
               Check (False, Name & ": the probe read its revoked grant (" &
                      Image (msg.words (0)) & " pages)");
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
               return;
            elsif msg.tag.label = OP_DONE then
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
               return;
            else
               Ignore := CuBit.Messages.reply (from, Reply (REPLY_FAILED));
            end if;
         end loop;
      end Run;
   begin
      Run;
      if Granted and then not Retired then
         declare
            Revoked : Boolean;
            Retire_By : constant Unsigned_64 := CuBit.Monotonic.Milliseconds + Retire_Ms;
         begin
            G.Revoke (Reference, Revoked);
            while Revoked and then not G.Retirement_Confirmed (Reference)
              and then CuBit.Monotonic.Milliseconds < Retire_By
            loop
               CuBit.Waits.Sleep_For (1);
            end loop;
            Check (Revoked and then G.Retirement_Confirmed (Reference),
                   Name & ": its grant retired after it ended");
         end;
      end if;
   end Serve;

begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   for P in 0 .. Granted_Pages - 1 loop
      Region (P * Words_Per_Page) := Pattern;
   end loop;
   Ignore := registerDriver (DRIVER_IPCTEST, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("tlb-check: register FAIL" & LF);
      return;
   end if;
   for Life in Unsigned_64 range 1 .. Lives loop
      if not Spawn_Probe then
         Check (False, "life " & Image (Life) & ": launched");
         exit;
      end if;
      Serve (Life, (if Life mod 2 = 0 then Mode_Revoke else Mode_Exit));
      CuBit.Waits.Sleep_For (Reap_Ms);
   end loop;
   Check (Fresh_Lives = Lives, "every life checked its fresh pages");
   Check (Revoke_Faults = Lives / 2, "every revoke life faulted on its revoked grant");
   --  The exit-then-reuse case needs the PID reused at the same address.
   Check (Reused >= Lives / 2 and then Same_Buffer = Reused,
          "later lives reused earlier lives' PIDs (" & Image (Unsigned_64 (Reused)) & " of" &
          Image (Unsigned_64 (Lives)) & ") with Buffer at the same address");
   debugPrint ((if Failures = 0 then "tlb-check: ALL PASS" else "tlb-check: FAIL") & LF);
end Main;
