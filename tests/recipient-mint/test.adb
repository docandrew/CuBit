------------------------------------------------------------------------------
--  Hosted mock-kernel test of the runtime's capability wrappers (CAP-002).
--
--  The old premise of this test (CuBit.Capability_Grants minting into a
--  caller-chosen slot of an incarnation-bound recipient, and delegating an
--  endpoint into slot N of a running process) no longer exists: nothing is
--  placed at a number, and a running process gains a capability only in a
--  reply to a request that accepted one. The same properties, in the
--  handle world:
--  * a recipient is captured once, by handle, from an endpoint or process
--    of the caller's own table; a later observation never replaces it; a
--    refused, stale, closed or forged handle captures nothing;
--  * an install goes through a CSPACE handle and its refusals (Sealed,
--    Target_Gone, Not_Transferable, ...) are typed, never success;
--  * a derivation carries exactly the rights asked (never wider), and never
--    changes kind;
--  * a capability given in a reply the request did not accept is refused,
--    and only the kernel's flags say a reply carried one;
--  * an offer is taken only when one was made.
--  The kernel's own decisions (narrowing, sealing, generations, limits) are
--  proved and tested in tests/capability-install and tests/capability-tables.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;
with CuBit.Handles.Recipient_Fixture;
with CuBit.Messages;
with CuBit.Process_IDs;
with Kernel_Mock;

procedure Test is
   package ABI renames Capability_ABI;
   package F renames CuBit.Handles.Recipient_Fixture;
   package IPC renames CuBit.Handles.IPC;
   package PIDs renames CuBit.Process_IDs;
   use type ABI.Refusal;
   use type ABI.Kind;
   use type ABI.Derivation;
   use type IPC.Carried_Kind;
   use type IPC.Reply_Outcome;
   use type PIDs.Process_ID;

   Failures : Natural := 0;
   Checks   : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  The raw ABI numbers the wrappers must use (Capability_ABI's table).
   Inspect_Number : constant Unsigned_64 := 84;
   Install_Number : constant Unsigned_64 := 138;
   --  A handle word: generation 7, slot 4 (the kernel chose both).
   Generation_Step : constant Unsigned_64 := 2 ** ABI.Slot_Field_Bits;
   Handle_Word   : constant Unsigned_64 := 7 * Generation_Step + 4;
   Placed_Word   : constant Unsigned_64 := 3 * Generation_Step + 9;
   --  The process the handle names: slot 42, generation 7.
   Identity      : constant PIDs.Process_ID := PIDs.To_Process_ID (42, 7);
   Identity_Word : constant Unsigned_64 := PIDs.To_Word (Identity);
   Kind_Last     : constant Unsigned_64 := ABI.Kind'Enum_Rep (ABI.Kind'Last);
   Badge         : constant Unsigned_64 := 16#55#;
   --  A word past every refusal code (Capability_ABI: 1 .. 255).
   Unpublished_Code : constant Unsigned_64 := 16#1000#;

   Held_Endpoint : constant Endpoint := F.Endpoint_Word (Handle_Word);
   Held_Process  : constant CuBit.Handles.Process := F.Process_Word (Handle_Word);
   Space         : constant Cspace := F.Cspace_Word (Handle_Word);

   procedure Describe_As (Kind : ABI.Kind; Rights : Unsigned_64; Object : Unsigned_64) is
   begin
      Kernel_Mock.Inspect_Result := 0;
      Kernel_Mock.Live := True;
      Kernel_Mock.Inspection :=
        [ABI.Capability_Kind => ABI.Kind'Enum_Rep (Kind),
         ABI.Rights_Mask     => Rights,
         ABI.Authority_Tag   => 0,
         ABI.Object          => Object,
         ABI.Parameter       => 0,
         ABI.Generation      => 7];
   end Describe_As;

   procedure Refuse_Inspection (Why : ABI.Refusal) is
   begin
      Kernel_Mock.Inspect_Result := ABI.Refusal_Word (Why);
      Kernel_Mock.Live := False;
   end Refuse_Inspection;

   Target : Recipient;
begin
   --  Capture: only an endpoint or process names a recipient.
   for Kind in ABI.Kind loop
      Describe_As (Kind, ABI.Right_Read or ABI.Right_Write, Identity_Word);
      Target := Capture (Held_Endpoint);
      Check (Valid (Target) = (Kind in ABI.Endpoint | ABI.Process),
             "capture by kind " & ABI.Kind'Image (Kind));
      Check (Kernel_Mock.Last_Number = Inspect_Number
             and then Kernel_Mock.Last_Arguments (0) = Handle_Word,
             "inspection names the handle word, never a slot number");
   end loop;
   Describe_As (ABI.Process, ABI.Right_Read, Identity_Word);
   Check (Valid (Capture (Held_Process)) and then Process_ID (Capture (Held_Process)) = Identity,
          "a process handle captures its process");

   --  An identity is captured once: a later observation does not replace it.
   Describe_As (ABI.Endpoint, ABI.Right_Read, Identity_Word);
   Target := Capture (Held_Endpoint);
   Describe_As (ABI.Endpoint, ABI.Right_Read, PIDs.To_Word (PIDs.To_Process_ID (42, 8)));
   Check (Process_ID (Target) = Identity and then Incarnation (Target) = Identity,
          "a captured incarnation is kept");

   --  Refused handles capture nothing: stale (closed or reused), revoked,
   --  never issued (forged), or of a kind the kernel does not know.
   for Why in ABI.Refusal loop
      Refuse_Inspection (Why);
      Check (not Valid (Capture (Held_Endpoint)),
             "refused handle captures nothing: " & ABI.Refusal'Image (Why));
      declare
         D : constant IPC.Description := IPC.Describe (Held_Endpoint);
      begin
         Check (not D.Current and then D.Why = Why,
                "refusal kept typed: " & ABI.Refusal'Image (Why));
      end;
   end loop;
   --  A nonzero word that is no refusal code is no description either.
   Kernel_Mock.Inspect_Result := Unpublished_Code;
   Kernel_Mock.Live := False;
   Check (not IPC.Describe (Held_Endpoint).Current, "an untyped failure is no description");
   Describe_As (ABI.Endpoint, ABI.Right_Read, Identity_Word);
   Kernel_Mock.Inspection (ABI.Capability_Kind) := Kind_Last + 1;
   Check (not Valid (Capture (Held_Endpoint)), "unknown kind captures nothing");
   Describe_As (ABI.Endpoint, ABI.Right_Read, 0);
   Check (not Valid (Capture (Held_Endpoint)), "no object captures nothing");

   --  Endpoint_Matches: an endpoint with Read to exactly that identity.
   for Kind in ABI.Kind loop
      Describe_As (Kind, ABI.Right_Read, Identity_Word);
      Check (Endpoint_Matches (Held_Endpoint, Identity) = (Kind = ABI.Endpoint),
             "match by kind " & ABI.Kind'Image (Kind));
   end loop;
   for Mask in Unsigned_64 range 0 .. ABI.All_Rights loop
      Describe_As (ABI.Endpoint, Mask, Identity_Word);
      Check (Endpoint_Matches (Held_Endpoint, Identity) = ((Mask and ABI.Right_Read) /= 0),
             "match needs Read");
   end loop;
   Describe_As (ABI.Endpoint, ABI.Right_Read, Identity_Word);
   Check (not Endpoint_Matches (Held_Endpoint, PIDs.To_Process_ID (42, 8)), "another life");
   Check (not Endpoint_Matches (Held_Endpoint, PIDs.To_Process_ID (43, 7)), "another slot");
   Check (not Endpoint_Matches (Held_Endpoint, PIDs.No_Process), "no process");
   Refuse_Inspection (ABI.Stale_Handle);
   Check (not Endpoint_Matches (Held_Endpoint, Identity), "a stale handle matches nothing");

   --  Installs go through a CSPACE handle; every refusal (Sealed once the
   --  child runs, Target_Gone after it exits, ...) is typed, never success.
   declare
      Copy   : constant Derivation := CuBit.Handles.Copy (Held_Endpoint, Read_Only);
      Result : Install_Result;
   begin
      for Why in ABI.Refusal loop
         Kernel_Mock.Answer := ABI.Refusal_Word (Why);
         Result := CuBit.Handles.Control.Install (Space, Copy);
         Check (not Result.Installed and then Result.Why = Why,
                "install refusal kept typed: " & ABI.Refusal'Image (Why));
      end loop;
      Check (Kernel_Mock.Last_Number = Install_Number
             and then Kernel_Mock.Last_Arguments (0) = Handle_Word,
             "install names its CSPACE handle");
      Kernel_Mock.Answer := 0;
      Result := CuBit.Handles.Control.Install (Space, Copy);
      Check (not Result.Installed, "a zero answer is no handle");
      Kernel_Mock.Answer := Placed_Word;
      Result := CuBit.Handles.Control.Install (Space, Copy);
      Check (Result.Installed and then F.Word_Of (Result.Placed) = Placed_Word,
             "an installed handle is the kernel's word");
      Check (Kernel_Mock.Last_Derivation = F.Words_Of (Copy),
             "the kernel receives the derivation as built");
   end;

   --  A derivation carries exactly the rights asked, never more.
   for Mask in Unsigned_64 range 0 .. ABI.All_Rights loop
      declare
         Asked : constant Rights :=
           (Read    => (Mask and ABI.Right_Read) /= 0,
            Write   => (Mask and ABI.Right_Write) /= 0,
            Execute => (Mask and ABI.Right_Execute) /= 0,
            Grant   => (Mask and ABI.Right_Grant) /= 0,
            Revoke  => (Mask and ABI.Right_Revoke) /= 0);
         Words : constant ABI.Derivation :=
           F.Words_Of (CuBit.Handles.Copy (Held_Endpoint, Asked));
         Tagged_Words : constant ABI.Derivation :=
           F.Words_Of (Badged (Held_Endpoint, Asked, Badge));
      begin
         Check (Words (ABI.Rights) = Mask and then Words (ABI.Source) = Handle_Word
                and then Words (ABI.Flags) = 0 and then Words (ABI.Tag) = 0,
                "copy asks exactly its rights");
         Check (Tagged_Words (ABI.Rights) = Mask
                and then Tagged_Words (ABI.Flags) = ABI.Set_Tag
                and then Tagged_Words (ABI.Tag) = Badge,
                "badged copy asks exactly its rights and tag");
      end;
   end loop;

   --  A derivation never changes kind: one asked as another kind never
   --  reaches the kernel.
   declare
      Before : constant Natural := Kernel_Mock.Calls;
      As_Process : constant CuBit.Handles.Process :=
        CuBit.Handles.Control.Derive (CuBit.Handles.Copy (Held_Endpoint, Read_Only));
      As_Endpoint : constant Endpoint :=
        CuBit.Handles.Control.Derive (CuBit.Handles.Copy (Held_Process, Read_Only));
   begin
      Check (not Valid (As_Process) and then not Valid (As_Endpoint)
             and then Kernel_Mock.Calls = Before,
             "a derivation of another kind is refused before the kernel");
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Would_Amplify);
      Check (not Valid (Endpoint'(CuBit.Handles.Control.Derive
                                    (CuBit.Handles.Copy (Held_Endpoint, Read_Write)))),
             "a refused derivation is the null handle");
   end;

   --  Reply_With: only a request that accepted a capability gets one.
   declare
      Copy   : constant Derivation := CuBit.Handles.Copy (Held_Endpoint, Read_Only);
      Result : IPC.Reply_Result;
   begin
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Not_Accepted);
      Result := IPC.Reply_With (Identity, CuBit.Messages.NULL_MESSAGE, Copy);
      Check (Result.Outcome = IPC.Refused and then Result.Why = ABI.Not_Accepted,
             "a capability offered to a request that did not accept one is refused");
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Would_Amplify);
      Result := IPC.Reply_With (Identity, CuBit.Messages.NULL_MESSAGE, Copy);
      Check (Result.Outcome = IPC.Refused and then Result.Why = ABI.Would_Amplify,
             "a widening reply copy is refused");
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Limit_Reached);
      Result := IPC.Reply_With (Identity, CuBit.Messages.NULL_MESSAGE, Copy);
      Check (Result.Outcome = IPC.Delivered_Refused and then Result.Why = ABI.Limit_Reached,
             "a full requester gets the reply and the reason");
      Kernel_Mock.Answer := Handle_Word;
      Result := IPC.Reply_With (Identity, CuBit.Messages.NULL_MESSAGE, Copy);
      Check (Result.Outcome = IPC.Transferred, "an accepted transfer");
      Check (Kernel_Mock.Last_Arguments (0) = Identity_Word
             and then Kernel_Mock.Last_Derivation = F.Words_Of (Copy),
             "the reply names its requester and derivation");
   end;

   --  What a reply carries is said by the kernel's flags only.
   declare
      Reply_Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Got : IPC.Carried;
   begin
      Reply_Message.authorityTag := Handle_Word;
      Got := IPC.Carried_By (Reply_Message);
      Check (Got.Kind = IPC.Absent,
             "a handle-shaped tag without the kernel's flag carries nothing");
      Reply_Message.tag.ipc := (Capability_Transferred => True, others => False);
      Reply_Message.authorityTag := 5;
      Check (IPC.Carried_By (Reply_Message).Kind /= IPC.Present,
             "a transferred flag with no handle word carries nothing");
      Reply_Message.tag.ipc := (Capability_Refused => True, others => False);
      Reply_Message.authorityTag := ABI.Refusal_Word (ABI.Limit_Reached);
      Got := IPC.Carried_By (Reply_Message);
      Check (Got.Kind = IPC.Refused and then Got.Why = ABI.Limit_Reached,
             "a refused transfer says why");
      Reply_Message.tag.ipc := (Capability_Transferred => True, others => False);
      Reply_Message.authorityTag := Handle_Word;
      Got := IPC.Carried_By (Reply_Message);
      Check (Got.Kind = IPC.Present, "a transferred handle");
      Describe_As (ABI.Device_Memory, ABI.Right_Read, 0);
      declare
         A : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Got);
      begin
         Check (not A.OK and then A.Why = ABI.Wrong_Kind,
                "a carried handle of another kind is refused");
      end;
      Refuse_Inspection (ABI.Stale_Handle);
      declare
         A : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Got);
      begin
         Check (not A.OK and then A.Why = ABI.Stale_Handle, "a stale carried handle is refused");
      end;
      Describe_As (ABI.Endpoint, ABI.Right_Read, Identity_Word);
      declare
         A : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Got);
      begin
         Check (A.OK and then F.Word_Of (A.Value) = Handle_Word, "a carried endpoint accepted");
      end;
   end;

   --  Offers: taken only when one was made.
   declare
      Got : IPC.Carried;
      Offered : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Not_Accepted);
      Got := IPC.Accept_Offered (Identity);
      Check (Got.Kind = IPC.Refused and then Got.Why = ABI.Not_Accepted,
             "nothing offered, nothing taken");
      Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Not_Permitted);
      Got := IPC.Accept_Offered (Identity);
      Check (Got.Kind = IPC.Refused and then Got.Why = ABI.Not_Permitted,
             "no offer taken outside the call it came with");
      Kernel_Mock.Answer := Handle_Word;
      Got := IPC.Accept_Offered (Identity);
      Check (Got.Kind = IPC.Present and then Got.Word = Handle_Word, "an offer taken");
      Offered.tag.ipc := (Capability_Offered => True, others => False);
      Check (IPC.Offers (Offered), "an offered request is seen");
      Check (not IPC.Offers (CuBit.Messages.NULL_MESSAGE), "a plain request offers nothing");
   end;

   --  Close: a closed or stale handle is refused, with its reason.
   Kernel_Mock.Answer := ABI.Refusal_Word (ABI.Stale_Handle);
   declare
      Closed : constant IPC.Close_Result := IPC.Close (Held_Endpoint);
   begin
      Check (not Closed.Closed and then Closed.Why = ABI.Stale_Handle,
             "closing a closed handle is refused");
   end;
   Kernel_Mock.Answer := 0;
   Check (IPC.Close (Held_Endpoint).Closed, "a close");

   Ada.Text_IO.Put_Line ("capability wrappers:" & Checks'Image & " checks," & Failures'Image &
                         " failures (mock kernel)");
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("recipient capture, install, transfer and offer wrappers PASS (mock kernel)");
   else
      Ada.Text_IO.Put_Line ("recipient wrappers FAIL");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test;
