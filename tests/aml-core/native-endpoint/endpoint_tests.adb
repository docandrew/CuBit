with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with ACPI_Region_Policy; use ACPI_Region_Policy;
with ACPI_Backend_Endpoint;
with Region_Mock;
with ACPI_FADT.Transactions;
procedure Endpoint_Tests is
   package Endpoint is new ACPI_Backend_Endpoint
     (Region_Mock.State, Region_Mock.Transact);
   Region : State;
   Hardware : Region_Mock.State;
   Request, Reply : Message := NULL_MESSAGE;
   Ticket : Unsigned_64;
   Accepted : Boolean;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "check" & Checks'Image; end if;
   end Check;
   procedure Call (Status : Unsigned_64; Executes : Boolean := False) is
      Before : constant Natural := Hardware.Calls;
   begin
      -- Poison all output fields to detect stale metadata/value leakage.
      Reply := (tag => (Unsigned_32'Last, 255, 255, 65535),
                authorityTag => Unsigned_64'Last,
                words => (others => Unsigned_64'Last));
      Ticket := Unsigned_64'Last;
      Endpoint.Dispatch (Region, Hardware, Request, Reply, Ticket);
      Check (Reply.tag = (16#F002#, 4, 0, 0));
      Check (Reply.authorityTag = 0 and Reply.words (0) = Status
             and Reply.words (3) = 0);
      Check (Hardware.Calls = Before + Boolean'Pos (Executes));
      if Status /= 2 then
         Check (Reply.words (1) = 0 and Reply.words (2) = 0);
      end if;
      if Status /= 3 then Check (Ticket = 0); end if;
   end Call;
begin
   Install (Region, (Tag => 16#1234#, Base => 16#1000#, Length => 12,
     Space => Memory_Space, Readable => True, Writable => True,
     Widths => (others => True),
     Register_Authority => (Resource_ID => 71, Readable => True,
       Writable => True, Delegable => False, Write_Mask => 16#FFFFFF#),
     Register_Width => ACPI_FADT.Transactions.Qword_Access,
     others => <>), Accepted);
   Check (Accepted);
   Request := (tag => (0, 4, 0, 0), authorityTag => 16#1234#,
               words => (Epoch (Region), 71, 0, 0));
   Hardware.Next_Value := 16#FEDC_BA98_7654_3210#;
   Call (2, True);
   Check (Reply.words (1) = 16#7654_3210# and
          Reply.words (2) = 16#FEDC_BA98#);
   Check (Hardware.Last_Address = 16#1000# and not Hardware.Last_Write);
   Request.authorityTag := 0;
   Request.tag.reserved := 16#1234#;
   Request.words := (others => 16#1234#);
   Call (0);
   Request := (tag => (0, 4, 0, 0), authorityTag => 16#1235#,
               words => (Epoch (Region), 71, 0, 0));
   Call (0);
   Request.authorityTag := 16#1234#;
   for Reserved in Unsigned_16 range 1 .. Unsigned_16'Last loop
      Request.tag.reserved := Reserved;
      Call (1);
   end loop;
   Request.tag.reserved := 0;
   for Flags in Unsigned_8 range 1 .. Unsigned_8'Last loop
      Request.tag.flags := Flags;
      Call (1);
   end loop;
   Request.tag.flags := 0;
   for Length in Unsigned_8 loop
      if Length /= 4 then Request.tag.length := Length; Call (1); end if;
   end loop;
   Request.tag.length := 4;
   Request.words (1) := 8; -- An offset is not a register ID.
   Call (0);
   Request.words (1) := Unsigned_64'Last;
   Call (0);
   Request.words (1) := 71;
   Request.words (0) := Epoch (Region) + 1;
   Call (0);
   Request.words (0) := Epoch (Region);
   Request.tag.label := 1;
   Request.words (2) := 16#1000000#;
   Call (0); -- A known ID does not authorize bits outside its write mask.
   Request.words (2) := 16#ABCDEF#;
   Call (2, True);
   Check (Hardware.Last_Write and Hardware.Last_Input = 16#ABCDEF#);
   Check (Reply.words (1) = 0 and Reply.words (2) = 0);
   Hardware.Complete := False;
   Call (3, True);
   Check (Ticket /= 0 and Busy (Region) and not Active (Region));
   Check (Ticket = Receipt (Region));
   Call (0);
   Check (Busy (Region));
   Put_Line ("native endpoint checks PASS" & Checks'Image);
end Endpoint_Tests;
