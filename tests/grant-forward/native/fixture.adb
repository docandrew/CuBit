with Interfaces; use Interfaces;
with System; use System;
with Ada.Unchecked_Conversion;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CCL_Manifest_Bindings;
with Native_GPU_Presentation;
with Native_GPU_Buffers;
with Native_GPU_Memory;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;

package body Fixture is
   package MG renames CuBit.Memory_Grants;
   package GR renames CuBit.Grant_References;
   Owner_Cap : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_ipc_test;
   Reader_Cap : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_ccl_test_host;
   New_Root : constant Unsigned_32 := 16#B101#;
   Revoke_Root : constant Unsigned_32 := 16#B102#;
   Root_Status : constant Unsigned_32 := 16#B103#;
   Mutate_Root : constant Unsigned_32 := 16#B104#;
   Exit_Owner : constant Unsigned_32 := 16#B105#;
   Hold_Child : constant Unsigned_32 := 16#B201#;
   Return_Child : constant Unsigned_32 := 16#B202#;
   Exit_Reader : constant Unsigned_32 := 16#B203#;
   Hold_Intermediary_Exit : constant Unsigned_32 := 16#B204#;
   Await_Owner_Exit : constant Unsigned_32 := 16#B205#;
   Reply_OK : constant Unsigned_32 := 16#F000#;
   function Number is new Ada.Unchecked_Conversion (System.Address, Unsigned_64);
   type Bytes is array (Natural range <>) of Unsigned_8;
   Buffer : Bytes (0 .. 8191) := [others => 16#B7#] with Alignment => 4096, Volatile;
   Ignore : Unsigned_64;

   procedure Check (Value : Boolean; Name : String) is
   begin
      if not Value then
         debugPrint ("TEST: FAIL grant-forward " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;

   function Call (Slot : CapabilitySlot; Op : Unsigned_32; Word : Unsigned_64 := 0)
     return Unsigned_64
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (Op, 4, 0, 0);
      Msg.words (0) := Word;
      Msg.tag := capCall (Slot, Msg);
      Check (Msg.tag.label = Reply_OK, "reply");
      return Msg.words (0);
   end Call;

   procedure Run (Role : Natural) is
      Parent, Child, Rejected : MG.Grant_Reference;
      Mapped, Denied_Address : System.Address;
      OK : Boolean;
      From : ProcessID;
      Msg : Message;
      Raw, Generation : Unsigned_64;
      Reply_Word : Unsigned_64;
      Map_Reply : MessageWords;

      procedure Wait_For_Closure is
      begin
         for Attempt in 1 .. 200 loop
            MG.Acquire (Child, From, 0, 4096, MG.Read_Access, Denied_Address, OK);
            exit when not OK;
            MG.Return_Acquisition (Child, OK);
            Check (OK, "return temporary probe acquisition");
            Ignore := syscall (SYSCALL_SLEEP, 10);
         end loop;
         Check (not OK, "exit closes child admission");
      end Wait_For_Closure;

      procedure Check_Content (Expected : Unsigned_8) is
         View : Bytes (0 .. 4095) with Import, Address => Mapped, Volatile;
      begin
         for Index in View'Range loop
            Check (View (Index) = Expected, "live shared child content");
         end loop;
      end Check_Content;

      procedure Desktop_Test is
         package D renames CuBit.Desktop_Protocol;
         package P renames Native_GPU_Presentation;
         use type D.Status_Code;
         Created : D.Creation_Result;
         Wire_Child : aliased Unsigned_64 := 0;
         function Presenter_Open (Render_Slot, Desktop_Slot, Surface : Unsigned_64)
           return Unsigned_32
           with Import, Convention => C, External_Name => "presenter_probe_open";
         function Presenter_Release return Unsigned_32
           with Import, Convention => C, External_Name => "presenter_probe_release";
         function Send (Request : D.Wire_Message) return D.Wire_Message is
            Message : CuBit.Messages.Message := CuBit.Desktop_Messages.From_Wire (Request);
         begin
            Message.tag := capCall (CAP_SLOT_DESKTOP, Message);
            return CuBit.Desktop_Messages.To_Wire (Message);
         end Send;
      begin
         Check (D.Decode_Hello_Result (Send (D.Encode_Hello (D.Current_Revision))).Status = D.Success,
                "desktop handshake");
         Created := D.Decode_Creation_Result (Send (D.Encode_Create ((32, 32, D.Plain_Surface))));
         Check (Created.Status = D.Success, "desktop surface");
         Raw := Call (Owner_Cap, New_Root, 2);
         Check (GR.Valid_Wire (Raw), "desktop root wire"); Parent := GR.Decode (Raw);
         Check (P.Forward (CAP_SLOT_DESKTOP, Raw, 4096, 4096, Wire_Child'Access) = 1
                and Wire_Child = 0, "forward requires root acquisition");
         MG.Acquire_Via_Capability (Owner_Cap, Parent, 0, 8192, MG.Read_Access, Mapped, OK);
         Check (OK, "desktop root acquisition");
         Check (P.Attach_Linear (CAP_SLOT_DESKTOP, Unsigned_64 (Created.Surface),
                 Raw, 32, 32, 128) /= 0, "desktop rejects foreign-owner root");
         Check (P.Forward (CAP_SLOT_DESKTOP, Raw, 4096, 4096, Wire_Child'Access) = 0,
                "desktop child derivation");
         Check (P.Attach_Linear (CAP_SLOT_DESKTOP, Unsigned_64 (Created.Surface),
                 Wire_Child, 32, 32, 127) = 7, "linear pitch validated");
         Check (P.Attach_Linear (CAP_SLOT_DESKTOP, Unsigned_64 (Created.Surface),
                 Wire_Child, 32, 32, 128) = 0, "desktop acquired child");
         MG.Return_Acquisition (Parent, OK); Check (OK, "desktop root return");
         Check (Call (Owner_Cap, Revoke_Root) = Parent.generation, "desktop retains root");
         Check (P.Retire (Wire_Child) = 1, "attachment is not a release fence");
         Check (D.Decode_Status (Send (D.Encode_Present ((Created.Surface, (0, 0, 32, 32)))),
                 D.Present_Surface) = D.Success, "present retained child");
         Check (P.Retire (Wire_Child) = 1, "present is not a release fence");
         Check (D.Decode_Status (Send (D.Encode_Destroy ((Surface => Created.Surface))),
                 D.Destroy_Surface) = D.Success, "destroy releases child");
         Check (P.Retire (Wire_Child) = 0, "child retired after destroy");
         Check (Call (Owner_Cap, Root_Status) = 0, "desktop root hold released");
         Created := D.Decode_Creation_Result (Send (D.Encode_Create ((32, 32, D.Plain_Surface))));
         Check (Created.Status = D.Success, "C presenter surface");
         Check (Presenter_Open (Owner_Cap, CAP_SLOT_DESKTOP, Unsigned_64 (Created.Surface)) = 0,
                "C presenter attached");
         Check (Presenter_Release = 4, "C presenter retained by Desktop");
         Check (D.Decode_Status (Send (D.Encode_Destroy ((Surface => Created.Surface))),
                 D.Destroy_Surface) = D.Success, "C presenter destroy");
         Check (Presenter_Release = 0, "C presenter fully retired");
         Check (Presenter_Release = 0, "C presenter idempotent retirement");
         Check (Call (Owner_Cap, Root_Status) = 0, "C presenter root retired");
         debugPrint ("GRANT-FORWARD-DESKTOP: PASS" & ASCII.LF);
      end Desktop_Test;
   begin
      if Role = 5 then
         Desktop_Test;
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
      if Role < 2 then
         if Role = 0 then
            for Index in 0 .. 4095 loop
               Buffer (Index) := 16#A3#;
            end loop;
         end if;
         Check (registerDriver (if Role = 0 then DRIVER_IPCTEST else DRIVER_CCL_TEST)
                  /= Unsigned_64'Last, "register service");
         loop
            receive (From, Msg);
            Reply_Word := 1;
            if Role = 0 then
               case Msg.tag.label is
                  when 16#0A23# =>
                     -- Synthetic RAM-only owner implementing the production
                     -- map wire shape. No Intel admission or GPU is exercised.
                     Map_Reply := [2, 1, 0, 0];
                     if Msg.tag.length = 4 and Msg.tag.flags = 0 and Msg.tag.reserved = 0 then
                        if Msg.words = [1 + 3 * 2 ** 32, 1, 4096, 4096] then
                           Raw := syscall (SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID,
                             From, Number (Buffer'Address) + 4096, 1, 2);
                           Check (Raw /= Unsigned_64'Last, "C presenter create root");
                           Generation := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Raw);
                           Check (Generation in 1 .. MG.MAXIMUM_GENERATION, "C root generation");
                           Parent := (Raw, Generation);
                           Map_Reply := [0, 1, 1, GR.Encode (Parent)];
                        elsif Msg.words = [1 + 2 * 2 ** 32, 1, 0, 0] then
                           if not MG.Retirement_Confirmed (Parent) then
                              MG.Revoke (Parent, OK);
                              Check (OK, "C presenter revoke root");
                           end if;
                           Map_Reply := [(if MG.Retirement_Confirmed (Parent) then 0 else 4), 1, 0, 0];
                        end if;
                     end if;
                  when New_Root =>
                     for Index in 4096 .. 8191 loop
                        Buffer (Index) := 16#B7#;
                     end loop;
                     -- Current reply authority authenticates the recipient;
                     -- grant creation checks its incarnation again under lock.
                     Raw := syscall (SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID,
                       From, Number (Buffer'Address), 2, Msg.words (0));
                     Check (Raw /= Unsigned_64'Last, "create root");
                     Generation := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Raw);
                     Check (Generation in 1 .. MG.MAXIMUM_GENERATION, "root generation");
                     Parent := (Raw, Generation);
                     Reply_Word := GR.Encode (Parent);
                  when Revoke_Root =>
                     MG.Revoke (Parent, OK); Check (OK, "revoke root");
                     Reply_Word := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Parent.slot);
                  when Root_Status =>
                     Reply_Word := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Parent.slot);
                  when Mutate_Root | Exit_Owner =>
                     for Index in 4096 .. 8191 loop
                        Buffer (Index) := 16#C9#;
                     end loop;
                  when others => Check (False, "owner operation");
               end case;
            else
               case Msg.tag.label is
                  when Hold_Child | Hold_Intermediary_Exit =>
                     Check (GR.Valid_Wire (Msg.words (0)), "child wire");
                     Child := GR.Decode (Msg.words (0));
                     MG.Acquire (Child, From, 0, 4096, MG.Read_Access, Mapped, OK);
                     Check (OK, "reader acquires child");
                     MG.Acquire (Child, From, 0, 4096, MG.Write_Access, Denied_Address, OK);
                     Check (not OK, "child stays readonly");
                     MG.Acquire (Child, From, 0, 4097, MG.Read_Access, Denied_Address, OK);
                     Check (not OK, "child extent enforced");
                     MG.Derive_Via_Capability (Owner_Cap, Child, 0, 1, False, Rejected, OK);
                     Check (not OK, "terminal child cannot forward");
                  when Return_Child | Exit_Reader =>
                     MG.Acquire (Child, From, 0, 4096, MG.Read_Access, Denied_Address, OK);
                     Check (not OK, "revocation closes child admission");
                  when Await_Owner_Exit =>
                     Wait_For_Closure;
                  when others => Check (False, "reader operation");
               end case;
               Check_Content
                 (if Msg.tag.label in Hold_Child | Hold_Intermediary_Exit
                  then 16#B7# else 16#C9#);
               if Msg.tag.label in Return_Child | Await_Owner_Exit then
                  MG.Return_Acquisition (Child, OK); Check (OK, "return child");
               end if;
            end if;
            declare
               Exit_Now : constant Boolean := Msg.tag.label in Exit_Reader | Exit_Owner;
               Wait_Intermediary : constant Boolean := Msg.tag.label = Hold_Intermediary_Exit;
            begin
               if Msg.tag.label = 16#0A23# then
                  Msg.tag := (16#0A23#, 4, 0, 0);
                  Msg.words := Map_Reply;
               else
                  Msg.tag := (Reply_OK, 1, 0, 0);
                  Msg.words (0) := Reply_Word;
               end if;
               Ignore := replyCap (CapabilitySlot'Last, Msg);
               if Wait_Intermediary then
                  -- Reply first so the intermediary can exit. Keep the original
                  -- child acquisition throughout its teardown, not a copy.
                  Wait_For_Closure;
                  Check_Content (16#B7#);
                  MG.Return_Acquisition (Child, OK);
                  Check (OK, "return child after intermediary exit");
                  for Attempt in 1 .. 200 loop
                     Raw := Call (Owner_Cap, Root_Status);
                     exit when Raw = 0;
                     Ignore := syscall (SYSCALL_SLEEP, 10);
                  end loop;
                  Check (Raw = 0, "root retired after intermediary exit");
                  debugPrint ("GRANT-FORWARD-INTERMEDIARY-EXIT: PASS" & ASCII.LF);
                  Ignore := syscall (SYSCALL_EXIT);
                  loop null; end loop;
               end if;
               if Exit_Now then
                  Ignore := syscall (SYSCALL_EXIT);
                  loop null; end loop;
               end if;
            end;
         end loop;
      end if;

      if Role in 3 .. 4 then
         Raw := Call (Owner_Cap, New_Root, 2);
         Check (GR.Valid_Wire (Raw), "exit root wire"); Parent := GR.Decode (Raw);
         MG.Acquire_Via_Capability (Owner_Cap, Parent, 0, 8192, MG.Read_Access, Mapped, OK);
         Check (OK, "exit root acquisition");
         MG.Derive_Via_Capability (Reader_Cap, Parent, 1, 1, False, Child, OK);
         Check (OK, "exit child derivation");
         Check (Call (Reader_Cap,
           (if Role = 3 then Hold_Intermediary_Exit else Hold_Child), GR.Encode (Child)) = 1,
           "exit reader holds");
         if Role = 3 then
            -- Deliberately leave both the received root and owned child live.
            Ignore := syscall (SYSCALL_EXIT);
            loop null; end loop;
         end if;
         -- Keep a direct root reader as well as the forwarded child across
         -- owner teardown. The grant store must outlive the owner's process
         -- address space and continue accepting returns from its borrowers.
         Check (Call (Owner_Cap, Exit_Owner) = 1, "owner exit requested");
         Check (Call (Reader_Cap, Await_Owner_Exit) = 1, "reader survives owner exit");
         Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Child.slot) = 0,
                "child retired after owner exit");
         declare
            View : Bytes (0 .. 8191) with Import, Address => Mapped, Volatile;
         begin
            for Index in View'Range loop
               Check (View (Index) = (if Index < 4096 then 16#A3# else 16#C9#),
                 "direct root survives owner death and child retirement");
            end loop;
         end;
         MG.Acquire_Via_Capability
           (Owner_Cap, Parent, 0, 8192, MG.Read_Access, Denied_Address, OK);
         Check (not OK, "dead owner denies new root reader");
         MG.Return_Acquisition (Parent, OK);
         Check (OK, "direct root return after owner death");
         MG.Return_Acquisition (Parent, OK);
         Check (not OK, "duplicate dead-owner root return denied");
         debugPrint ("GRANT-FORWARD-OWNER-ROOT-DRAIN: PASS" & ASCII.LF);
         debugPrint ("GRANT-FORWARD-OWNER-EXIT: PASS" & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;

      -- First confirm ordinary grants do not acquire forwarding authority.
      Raw := Call (Owner_Cap, New_Root, 0);
      Check (GR.Valid_Wire (Raw), "root wire"); Parent := GR.Decode (Raw);
      MG.Acquire_Via_Capability (Owner_Cap, Parent, 0, 8192, MG.Read_Access, Mapped, OK);
      Check (OK, "acquire ordinary root");
      MG.Derive_Via_Capability (Reader_Cap, Parent, 0, 1, False, Child, OK);
      Check (not OK, "default denies forwarding");
      MG.Return_Acquisition (Parent, OK); Check (OK, "ordinary return");
      Check (Call (Owner_Cap, Revoke_Root) = 0, "ordinary retirement");

      for Death in Boolean loop
         Raw := Call (Owner_Cap, New_Root, 2);
         Check (GR.Valid_Wire (Raw), "forwardable root wire"); Parent := GR.Decode (Raw);
         MG.Derive_Via_Capability (Reader_Cap, Parent, 0, 1, False, Child, OK);
         Check (not OK, "derive requires acquisition");
         MG.Acquire_Via_Capability (Owner_Cap, Parent, 0, 8192, MG.Read_Access, Mapped, OK);
         Check (OK, "acquire forwardable root");
         MG.Derive_Via_Capability (Reader_Cap, Parent, 0, 1, True, Child, OK);
         Check (not OK, "write escalation denied");
         MG.Derive_Via_Capability (Reader_Cap, Parent, 2, 1, False, Child, OK);
         Check (not OK, "range escalation denied");
         Rejected := Parent; Rejected.generation := Parent.generation + 1;
         MG.Derive_Via_Capability (Reader_Cap, Rejected, 0, 1, False, Child, OK);
         Check (not OK, "stale parent denied");
         Check (syscall (SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
           Reader_Cap, Parent.slot, Parent.generation, 0, 1, 2) = Unsigned_64'Last,
           "unknown derivation flags denied");
         Check (syscall (SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
           CapabilitySlot'Last, Parent.slot, Parent.generation, 0, 1, 0) = Unsigned_64'Last,
           "missing recipient authority denied");
         Check (syscall (SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
           Reader_Cap, Parent.slot, Parent.generation, Unsigned_64'Last, 1, 0) = Unsigned_64'Last,
           "wide offset rejected before narrowing");
         MG.Derive_Via_Capability (Reader_Cap, Parent, 1, 1, False, Child, OK);
         Check (OK, "derive readonly subrange");
         Check (Call (Reader_Cap, Hold_Child, GR.Encode (Child)) = 1, "reader holds");
         MG.Return_Acquisition (Parent, OK); Check (OK, "return root acquisition");
         Check (Call (Owner_Cap, Revoke_Root) = Parent.generation, "root hold survives revoke");
         Check (Call (Owner_Cap, Mutate_Root) = 1, "owner updates shared backing");
         Check (Call (Reader_Cap, (if Death then Exit_Reader else Return_Child)) = 1,
                "reader completion");
         for Attempt in 1 .. 200 loop
            exit when syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Child.slot) = 0;
            Ignore := syscall (SYSCALL_SLEEP, 10);
         end loop;
         Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Child.slot) = 0,
                "child mapping retired");
         Check (Call (Owner_Cap, Root_Status) = 0, "root hold released");
      end loop;
      debugPrint ("GRANT-FORWARD-CHECK: PASS" & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT);
   end Run;
end Fixture;
