------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One life of the address-space tag probe (headless --test tlb-tags).
--
--  @description
--  tlb-check launches this program life after life. A life usually gets the
--  PID, and so the address-space tag (PCID), the previous one had, and its
--  Buffer is at the same address. A life first reads every page of Buffer:
--  a fresh life's pages are zero, so anything else is a translation an
--  earlier life left under the tag. It then writes its life number into
--  every page. On a revoke life it then maps pages tlb-check grants it; a
--  second thread reads them (caching their translations on its CPU, often
--  another one: the main thread spins meanwhile) and waits. The main thread
--  gives the acquisition back (the pages stay mapped until the owner
--  revokes) and asks tlb-check to revoke the grant: the kernel unmaps the
--  pages while tlb-check's address space runs, and shoots down every CPU's
--  translations. The reader's next read must fault, and the kernel kills
--  this life. If it does not, a translation of the unmapped pages outlived
--  the shootdown under this life's tag.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Monotonic;
with CuBit.Waits;
with Probe_Memory; use Probe_Memory;
with Startup;

procedure Main is
   use ASCII;
   package GR renames CuBit.Grant_References;
   --  The protocol (tlb-check's main.adb has the same numbers).
   OP_HELLO  : constant Unsigned_32 := 16#0B10#;
   OP_FRESH  : constant Unsigned_32 := 16#0B11#;
   OP_GRANT  : constant Unsigned_32 := 16#0B12#;
   OP_REVOKE : constant Unsigned_32 := 16#0B13#;
   OP_STALE  : constant Unsigned_32 := 16#0B14#;
   OP_DONE   : constant Unsigned_32 := 16#0B15#;
   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   Mode_Revoke : constant Unsigned_64 := 1;
   Answer_Ms : constant := 30_000;
   --  The reader reads within Reader_Ms; after the unmap it faults (or
   --  reports surviving) within Fault_Ms.
   Reader_Ms : constant := 5_000;
   Fault_Ms : constant := 2_000;

   Server : constant CuBit.Handles.Endpoint := Startup.Ipc_Test;

   function Image (V : Unsigned_64) return String is
      S : constant String := V'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   --  A call to tlb-check; the answer's words come back in Words.
   type Words_Out is array (0 .. 3) of Unsigned_64;
   function Call (Label : Unsigned_32; A, B : Unsigned_64; Words : out Words_Out)
     return Boolean
   is
      M : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      M.tag := (label => Label, length => 2, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := A;
      M.words (1) := B;
      Tag := capCall (Server, M, Deadline_After (Answer_Ms));
      for I in Words'Range loop
         Words (I) := M.words (I);
      end loop;
      return Tag.label = REPLY_OK;
   end Call;

   Answer : Words_Out;
   Life, Mode, Pattern : Unsigned_64;
   Stale : Unsigned_64 := 0;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   if not Call (OP_HELLO, Own_Process_Word,
                Unsigned_64 (To_Integer (Buffer'Address)), Answer)
   then
      debugPrint ("tlb-probe: FAIL no answer to hello" & LF);
      return;
   end if;
   Life := Answer (0);
   Mode := Answer (1);
   Pattern := Answer (2);

   --  Read every page before writing any: each read loads a translation.
   for P in 0 .. Buffer_Pages - 1 loop
      if Buffer (P * Words_Per_Page) /= 0 or else
        Buffer (P * Words_Per_Page + Words_Per_Page - 1) /= 0
      then
         Stale := Stale + 1;
      end if;
   end loop;
   for P in 0 .. Buffer_Pages - 1 loop
      Buffer (P * Words_Per_Page) := Life;
      Buffer (P * Words_Per_Page + Words_Per_Page - 1) := Life;
   end loop;
   if not Call (OP_FRESH, Stale, 0, Answer) then
      debugPrint ("tlb-probe: FAIL no answer to fresh" & LF);
      return;
   end if;

   if Mode /= Mode_Revoke then
      if not Call (OP_DONE, 0, 0, Answer) then
         debugPrint ("tlb-probe: FAIL no answer to done" & LF);
      end if;
      return;
   end if;

   declare
      package W renames CuBit.Waits;
      Reference : GR.Reference;
      Ok : Boolean;
      Ignore : Unsigned_64;
      Until_Ms : Unsigned_64;
   begin
      if not Call (OP_GRANT, 0, 0, Answer) then
         debugPrint ("tlb-probe: FAIL no grant" & LF);
         return;
      end if;
      Reference := GR.Decode (Answer (0));
      CuBit.Memory_Grants.Acquire_Via_Capability
        (Server, Reference, 0, Granted_Pages * Page_Bytes,
         CuBit.Memory_Grants.Read_Access, Probe_Memory.Mapped, Ok);
      if not Ok then
         debugPrint ("tlb-probe: FAIL grant not mapped" & LF);
         return;
      end if;
      Probe_Memory.Pattern := Pattern;
      if Start_Reader (New_Stack) = CREATE_FAILED then
         debugPrint ("tlb-probe: FAIL no reader thread" & LF);
         return;
      end if;
      --  Spin, not sleep, so an idle CPU takes the reader.
      Until_Ms := CuBit.Monotonic.Milliseconds + Reader_Ms;
      while Phase = Started and then CuBit.Monotonic.Milliseconds < Until_Ms loop
         null;
      end loop;
      if Phase /= Read then
         debugPrint ("tlb-probe: FAIL the reader did not read" & LF);
         return;
      end if;
      --  Still mapped after this, until tlb-check revokes the grant.
      CuBit.Memory_Grants.Return_Acquisition (Reference, Ok);
      if not Call (OP_REVOKE, Seen_Before, (if Ok then 1 else 0), Answer) then
         debugPrint ("tlb-probe: FAIL no answer to revoke" & LF);
         return;
      end if;
      debugPrint ("tlb-probe: life " & Image (Life) &
                  " reads its revoked grant on another thread" & LF);
      Phase := Unmapped;
      Ignore := W.Futex_Wake (W.Private_Word (Phase'Address), 1);
      Until_Ms := CuBit.Monotonic.Milliseconds + Fault_Ms;
      while Phase = Unmapped and then CuBit.Monotonic.Milliseconds < Until_Ms loop
         Ignore := W.Futex_Wait (W.Private_Word (Phase'Address), Unsigned_64 (Unmapped),
                                 Deadline_After (Fault_Ms));
      end loop;
      if Phase = Survived then
         debugPrint ("tlb-probe: FAIL life " & Image (Life) & " read a revoked grant (" &
                     Image (Seen_After) & " pages)" & LF);
      else
         debugPrint ("tlb-probe: FAIL life " & Image (Life) & " reader neither faulted nor read" & LF);
      end if;
      if not Call (OP_STALE, Seen_After, 0, Answer) then
         null;
      end if;
   end;
end Main;
