pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.TLS_Protocol; use CuBit.TLS_Protocol;

--  Headless check for tls.svc (tls-service regression). This app holds no
--  network authority and links no TLS library: every connection goes
--  through tls.svc, limited to its manifest's tls-scope.
procedure Main is
   Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Tls;
   Size : constant := 4_096;
   Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, Size);
   Buffer : String (1 .. Size) with Import,
     Address => To_Address (Integer_Address (Allocation));
   Reference : CuBit.Memory_Grants.Grant_Reference;
   Granted : Boolean;
   Passed : Boolean := True;
   Reply : Message;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False; end if;
      debugPrint ("tls-check: " & Name & (if Condition then " PASS" else " FAIL") &
                  ASCII.LF);
   end Check;

   procedure Call (Label : Unsigned_32; Length : Unsigned_8;
                   W0, W1, W2, W3 : Unsigned_64 := 0) is
   begin
      Reply := NULL_MESSAGE;
      Reply.tag.label := Label;
      Reply.tag.length := Length;
      Reply.words := [W0, W1, W2, W3];
      Reply.tag := capCall (Slot, Reply);
   end Call;

   procedure Open (Target : String) is
   begin
      Buffer (1 .. Target'Length) := Target;
      Call (Open_Operation, Unsigned_8 (Target'Length), Reference.slot, Size, 0,
            Reference.generation);
   end Open;

   function Failed_With (Code : Failure) return Boolean is
     (Reply.tag.label = Reply_Error and then
      Reply.words (0) = Unsigned_64 (Failure'Enum_Rep (Code)));

   procedure Report_Failure (Name : String) is
   begin
      if Reply.tag.label = Reply_Error and then
        Reply.words (0) in 1 .. Unsigned_64 (Failure'Enum_Rep (Failure'Last))
      then
         debugPrint ("tls-check: " & Name & ": " &
                     CuBit.TLS_Protocol.Name (Failure'Enum_Val (Reply.words (0))) &
                     ASCII.LF);
      end if;
   end Report_Failure;

   Channel : Unsigned_64;
begin
   Check (Allocation /= Unsigned_64'Last, "transfer allocation");
   if Allocation = Unsigned_64'Last then return; end if;
   CuBit.Memory_Grants.Create_Via_Capability
     (Slot, Buffer'Address, 1, True, Reference, Granted);
   Check (Granted, "transfer grant");
   if not Granted then return; end if;

   --  Without network approval at launch, procmgr installs no TLS scopes,
   --  so even the manifest's own name is denied. The harness starts this app
   --  once unapproved and once approved, and needs both results.
   Open ("tls-test.cubit.internal:18460");
   if Failed_With (Scope_Denied) then
      debugPrint ("TEST: PASS tls-unapproved" & ASCII.LF);
      return;
   end if;
   Check (Reply.tag.label = Reply_OK, "approved launch has its tls-scope");
   if Reply.tag.label = Reply_OK then
      Call (Shut_Operation, 1, Reply.words (0));
   end if;

   --  Authority checks happen before any network activity.
   Open ("other.cubit.internal:18460");
   Check (Failed_With (Scope_Denied), "name outside tls-scope denied");
   Open ("tls-test.cubit.internal:18464");
   Check (Failed_With (Scope_Denied), "port outside tls-scope denied");
   Open ("10.0.2.2:18460");
   Check (Failed_With (Malformed_Request), "IP literal refused");
   Open ("x.tls-test.cubit.internal:18460");
   Check (Failed_With (Scope_Denied), "subdomain of exact scope denied");
   Call (Set_Scopes_Operation, 4, 1, 1, Reference.slot, Reference.generation);
   Check (Reply.tag.label /= Reply_OK, "client cannot install scopes");

   --  A verified exchange through the service.
   Open ("TLS-Test.cubit.internal:18460");
   Report_Failure ("valid certificate");
   Check (Reply.tag.label = Reply_OK, "valid certificate: verified channel opens");
   if Reply.tag.label = Reply_OK then
      Channel := Reply.words (0);
      Call (Info_Operation, 1, Channel);
      Check (Reply.tag.label = Reply_OK and then Reply.words (0) = 1,
             "channel reports TLS 1.3");
      Buffer (1 .. 4) := "PING";
      Call (Write_Operation, 3, Channel, 0, 4);
      Check (Reply.tag.label = Reply_OK and then Reply.words (0) = 4,
             "plaintext written");
      Call (Read_Operation, 4, Channel, 0, 64, syscall (SYSCALL_GETTIME) + 15_000);
      Check (Reply.tag.label = Reply_OK and then Reply.words (0) = 4 and then
             Buffer (1 .. 4) = "PONG", "plaintext reply received");
      Call (Read_Operation, 3, Channel + 1, 0, 64);
      Check (Failed_With (Unknown_Channel), "unknown channel refused");
      Call (Shut_Operation, 1, Channel);
      Check (Reply.tag.label = Reply_OK, "channel closes");
      Call (Write_Operation, 3, Channel, 0, 4);
      Check (Failed_With (Unknown_Channel), "closed channel refused");
   end if;

   --  Certificate failures are typed.
   Open ("tls-test.cubit.internal:18461");
   Report_Failure ("wrong host name");
   Check (Failed_With (Certificate_Untrusted), "wrong host name rejected");
   Open ("tls-test.cubit.internal:18462");
   Report_Failure ("untrusted root");
   Check (Failed_With (Certificate_Untrusted), "untrusted root rejected");
   Open ("tls-test.cubit.internal:18463");
   Report_Failure ("expired certificate");
   Check (Failed_With (Certificate_Expired) or else Failed_With (Certificate_Untrusted),
          "expired certificate rejected");

   if Passed then
      debugPrint ("TEST: PASS tls-service" & ASCII.LF);
   else
      debugPrint ("TEST: FAIL tls-service" & ASCII.LF);
   end if;
end Main;
