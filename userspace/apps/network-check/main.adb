with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Network_Authority; use CuBit.Network_Authority;

procedure Main is
   Connect_Slot : constant CapabilitySlot := 30;
   Listen_Slot : constant CapabilitySlot := 31;
   Inspect_Slot : constant CapabilitySlot := 11;
   OK_Label : constant Unsigned_32 := 16#F000#;
   Error_Label : constant Unsigned_32 := 16#F001#;
   Open_Label : constant Unsigned_32 := 16#0420#;
   Write_Label : constant Unsigned_32 := 16#0421#;
   Read_Label : constant Unsigned_32 := 16#0422#;
   Shut_Label : constant Unsigned_32 := 16#0423#;
   Passed : Boolean := True;
   Request : Message;
   Reply_Tag : MessageTag;
   Listener, Replacement, Channel : Unsigned_64;
   Inspection : array (0 .. 5) of Unsigned_64 := [others => 0];
   Result : Unsigned_64;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False; end if;
      debugPrint ("network-check: " & Name & (if Condition then " PASS" else " FAIL") & ASCII.LF);
   end Check;
   procedure Bind (Slot : CapabilitySlot; Address, Port : Unsigned_64) is
   begin
      Request := NULL_MESSAGE;
      Request.tag.label := OP_BIND; Request.tag.length := 2;
      Request.words (0) := Address; Request.words (1) := Port;
      Reply_Tag := capCall (Slot, Request);
   end Bind;
   procedure Close_Listener (Slot : CapabilitySlot; Handle : Unsigned_64) is
   begin
      Request := NULL_MESSAGE; Request.tag.label := OP_CLOSE_LISTENER;
      Request.tag.length := 1; Request.words (0) := Handle;
      Reply_Tag := capCall (Slot, Request);
   end Close_Listener;
begin
   Bind (Inspect_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "general endpoint cannot listen");
   Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), Listen_Slot,
                      Unsigned_64 (To_Integer (Inspection'Address)));
   if Result = 1 and Inspection (0) = 0 then
      Bind (Listen_Slot, 16#0A00_020F#, 8080);
      Check (Reply_Tag.label /= OK_Label, "manifest does not approve itself");
      if Passed then debugPrint ("TEST: PASS network-unapproved" & ASCII.LF); end if;
      return;
   end if;
   Check (Result = 1 and Inspection (0) = 1 and Inspection (2) >= First_Grant_Tag,
          "approved endpoint carries scope tag");
   Bind (Connect_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "outbound cannot listen");
   Bind (Listen_Slot, 16#0A00_020F#, 8081);
   Check (Reply_Tag.label = Error_Label, "wrong listen port denied");
   Bind (Listen_Slot, 16#0A00_0210#, 8080);
   Check (Reply_Tag.label = Error_Label, "wrong listen address denied");
   Bind (Listen_Slot, 0, 8080);
   Check (Reply_Tag.label = Error_Label, "wildcard listen denied");
   Bind (Listen_Slot, 16#0A00_020F#, 65536 + 8080);
   Check (Reply_Tag.label = Error_Label, "port truncation denied");
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = OK_Label, "exact listener admitted");
   Listener := Request.words (0);
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Check (Reply_Tag.label = Error_Label, "duplicate bind denied");
   Close_Listener (Connect_Slot, Listener);
   Check (Reply_Tag.label = Error_Label, "wrong grant cannot close listener");
   Close_Listener (Listen_Slot, Listener);
   Check (Reply_Tag.label = OK_Label, "owner closes listener");
   Bind (Listen_Slot, 16#0A00_020F#, 8080);
   Replacement := Request.words (0);
   Check (Reply_Tag.label = OK_Label and Replacement /= Listener, "listener handle not reused");
   Close_Listener (Listen_Slot, Listener);
   Check (Reply_Tag.label = Error_Label, "stale listener rejected");
   Close_Listener (Listen_Slot, Replacement);
   Check (Reply_Tag.label = OK_Label, "replacement listener closes");

   Request := NULL_MESSAGE; Request.tag.label := OP_INSTALL_SCOPE;
   Request.tag.length := 3; Request.authorityTag := Policy_Authority_Tag;
   Request.words (0) := syscall (SYSCALL_GETPID);
   Request.words (2) := Descriptor (Broad_Outbound_TCP);
   Reply_Tag := capCall (Inspect_Slot, Request);
   Check (Reply_Tag.label = Error_Label, "forged policy tag rejected");
   Request := NULL_MESSAGE; Request.tag.label := 16#0432#;
   Reply_Tag := capCall (Connect_Slot, Request);
   Check (Reply_Tag.label = Error_Label, "outbound cannot configure network");

   declare
      Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, 4096);
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Success : Boolean;
      Buffer : String (1 .. 4096) with Import,
        Address => To_Address (Integer_Address (Allocation));
      procedure Open (Name : String; Generation : Unsigned_64 := 0) is
      begin
         Buffer (1 .. Name'Length) := Name;
         Request := NULL_MESSAGE; Request.tag.label := Open_Label;
         Request.tag.length := Unsigned_8 (Name'Length);
         Request.words (0) := Reference.slot; Request.words (1) := 4096;
         Request.words (3) := (if Generation = 0 then Reference.generation else Generation);
         Reply_Tag := capCall (Connect_Slot, Request);
      end Open;
   begin
      Check (Allocation /= Unsigned_64'Last, "transfer allocation");
      if Allocation = Unsigned_64'Last then return; end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Connect_Slot, Buffer'Address, 1, True, Reference, Success);
      Check (Success, "transfer grant");
      if not Success then return; end if;
      Open ("@net:tcp:10.0.3.2:18443");
      Check (Reply_Tag.label = Error_Label, "outbound wrong prefix denied");
      Open ("@net:tcp:10.0.2.2:18444");
      Check (Reply_Tag.label = Error_Label, "outbound wrong port denied");
      Open ("@net:tcp:example.com:18443");
      Check (Reply_Tag.label = Error_Label, "undeclared DNS denied");
      Open ("@net:tcp:10.0.2.2:18443", Reference.generation + 1);
      Check (Reply_Tag.label = Error_Label, "wrong transfer generation denied");
      Open ("@net:tcp:10.0.2.2:18443");
      Check (Reply_Tag.label = OK_Label, "permitted outbound connects");
      if Reply_Tag.label = OK_Label then
         Channel := Request.words (0);
         Buffer (1 .. 4) := "PING";
         Request := NULL_MESSAGE; Request.tag.label := Write_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
         Reply_Tag := capCall (Listen_Slot, Request);
         Check (Reply_Tag.label = Error_Label, "wrong grant cannot write channel");
         -- capCall overwrites Request, so rebuild after the intentional denial.
         Request := NULL_MESSAGE; Request.tag.label := Write_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
         Reply_Tag := capCall (Connect_Slot, Request);
         Check (Reply_Tag.label = OK_Label and Request.words (0) = 4, "authorized write");
         Request := NULL_MESSAGE; Request.tag.label := Read_Label;
         Request.tag.length := 3; Request.words (0) := Channel; Request.words (2) := 4;
         Reply_Tag := capCall (Connect_Slot, Request);
         Check (Reply_Tag.label = OK_Label and Request.words (0) = 4 and Buffer (1 .. 4) = "PONG",
                "authorized reply received");
         Request := NULL_MESSAGE; Request.tag.label := Shut_Label;
         Request.tag.length := 1; Request.words (0) := Channel;
         Reply_Tag := capCall (Connect_Slot, Request);
         Check (Reply_Tag.label = OK_Label, "authorized channel closes");
      end if;
      CuBit.Memory_Grants.Revoke (Reference, Success);
      Check (Success, "transfer revoke");
   end;
   if Passed then debugPrint ("TEST: PASS network-authority" & ASCII.LF); end if;
end Main;
