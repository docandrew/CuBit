with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Region_Policy; use ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with Region_Mock;
with Region_IO_Instance;
with ACPI_Region_Protocol;
with Hardware_Authority;
procedure Region_Tests is
   S, Before : State;
   C : Configuration := (Tag => 17, Base => 4096, Length => 64,
     Readable => True, Writable => True, Widths => [others => True], others => <>);
   Accepted : Boolean;
   R : Decision;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   type Numbers is array (Positive range <>) of Unsigned_64;
   Stamps : constant Numbers := [0, 16, 17, 18, Unsigned_64'Last];
   Offsets : constant Numbers := [0, 1, 7, 8, 56, 60, 63, 64, 65, Unsigned_64'Last];
   function Bytes (W : Access_Width) return Unsigned_64 is
     (2 ** Access_Width'Pos (W));
begin
   declare
      use Hardware_Authority;
      Parent, Child : Permission;
      Expected : Boolean;
   begin
      Check (not Valid (Parent));
      Parent.Resource_ID := 71; Child.Resource_ID := 71;
      for P in 0 .. 7 loop
         Parent.Readable := P mod 2 = 1;
         Parent.Writable := (P / 2) mod 2 = 1;
         Parent.Delegable := P / 4 = 1;
         for Q in 0 .. 7 loop
            Child.Readable := Q mod 2 = 1;
            Child.Writable := (Q / 2) mod 2 = 1;
            Child.Delegable := Q / 4 = 1;
            for PM in Unsigned_64 range 0 .. 15 loop
               Parent.Write_Mask := PM;
               for CM in Unsigned_64 range 0 .. 15 loop
                  Child.Write_Mask := CM;
                  Expected := Parent.Delegable and Valid (Parent) and Valid (Child)
                    and (not Child.Readable or Parent.Readable)
                    and (not Child.Writable or Parent.Writable)
                    and (CM or PM) = PM;
                  Check (Can_Derive (Parent, Child) = Expected);
                  for Write in Boolean loop
                     for Value in Unsigned_64 range 0 .. 15 loop
                        Check (not Can_Derive (Parent, Child) or else
                          not Permits (Child, 71, Write, Value) or else
                          Permits (Parent, 71, Write, Value));
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Parent := (71, True, True, True, Unsigned_64'Last);
      Child := Parent; Child.Resource_ID := 72;
      Check (not Can_Derive (Parent, Child));
      Child := Parent; Child.Resource_ID := 0;
      Check (not Can_Derive (Parent, Child));
      Child := Parent; Parent.Delegable := False;
      Check (not Can_Derive (Parent, Child));
      Check (not Permits (Parent, 72, True, 0));
   end;
   Check (not Active (S) and Epoch (S) = 0);
   Check (not Resolve (S, 17, 0, 0, ACPI_FADT.Transactions.Byte_Access, False).Allowed);
   for Can_Read in Boolean loop
      for Can_Write in Boolean loop
         for Mask in 0 .. 15 loop
            Revoke (S);
            C.Readable := Can_Read; C.Writable := Can_Write;
            for W in Access_Width loop C.Widths (W) := (Mask / 2 ** Access_Width'Pos (W)) mod 2 = 1; end loop;
            Before := S;
            Install (S, C, Accepted);
            Check (Accepted = (Can_Read or Can_Write));
            if Accepted then
               Check (Epoch (S) = Epoch (Before) + 1);
               for Stamp of Stamps loop
                  for Offset of Offsets loop
                     for W in Access_Width loop
                        for Write in Boolean loop
                           R := Resolve (S, Stamp, Epoch (S), Offset, W, Write);
                           Check (R.Allowed = (Stamp = 17 and then C.Widths (W)
                             and then (if Write then Can_Write else Can_Read)
                             and then Offset < 64 and then Offset + Bytes (W) <= 64
                             and then (4096 + Offset) mod Bytes (W) = 0));
                           if R.Allowed then Check (R.Address = 4096 + Offset); end if;
                        end loop;
                     end loop;
                  end loop;
               end loop;
               Check (not Resolve (S, 17, Epoch (S) - 1, 0,
                 ACPI_FADT.Transactions.Byte_Access, False).Allowed);
               Before := S;
               Install (S, C, Accepted);
               Check (not Accepted and S = Before);
            else
               Check (S = Before);
            end if;
         end loop;
      end loop;
   end loop;
   Revoke (S);
   Check (not Resolve (S, 17, Epoch (S), 0, ACPI_FADT.Transactions.Byte_Access, False).Allowed);
   C := (Tag => 17, Base => Unsigned_64'Last, Length => 1, Readable => True,
     Widths => [others => True], others => <>);
   Install (S, C, Accepted); Check (Accepted);
   R := Resolve (S, 17, Epoch (S), 0, ACPI_FADT.Transactions.Byte_Access, False);
   Check (R.Allowed and then R.Address = Unsigned_64'Last);
   Check (not Resolve (S, 17, Epoch (S), 1, ACPI_FADT.Transactions.Byte_Access, False).Allowed);
   Revoke (S);
   C.Length := 2; Install (S, C, Accepted); Check (not Accepted);
   C.Base := 65535; C.Length := 1; C.Space := IO_Space;
   Install (S, C, Accepted); Check (not Accepted); -- Qword I/O not admitted
   C.Widths (ACPI_FADT.Transactions.Qword_Access) := False;
   Install (S, C, Accepted); Check (Accepted);
   Revoke (S);
   C.Length := 2; Install (S, C, Accepted); Check (not Accepted);
   C.Length := 1; C.Tag := 0; Install (S, C, Accepted); Check (not Accepted);
   declare
      First_Ticket, Second_Ticket, Failed_Ticket : Unsigned_64;
      Old_Epoch : Unsigned_64;
   begin
      C := (Tag => 17, Base => 4096, Length => 8, Readable => True,
        Widths => [others => True], others => <>);
      Install (S, C, Accepted); Check (Accepted);
      Old_Epoch := Epoch (S);
      Begin_Access (S, 0, Old_Epoch, 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, Failed_Ticket);
      Check (not R.Allowed and Failed_Ticket = 0 and not Busy (S));
      Begin_Access (S, 17, Old_Epoch, 0, ACPI_FADT.Transactions.Qword_Access,
        False, R, First_Ticket);
      Check (R.Allowed and Busy (S) and not Ready_To_Release (S));
      Before := S;
      Begin_Access (S, 17, Old_Epoch, 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, Failed_Ticket);
      Check (not R.Allowed and Failed_Ticket = 0 and S = Before);
      Finish_Access (S, First_Ticket - 1, Accepted);
      Check (not Accepted and S = Before);
      Finish_Access (S, First_Ticket, Accepted);
      Check (Accepted and not Busy (S) and not Ready_To_Release (S));
      Begin_Access (S, 17, Old_Epoch, 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, Second_Ticket);
      Check (R.Allowed and Second_Ticket > First_Ticket);
      Before := S;
      Finish_Access (S, First_Ticket, Accepted);
      Check (not Accepted and S = Before); -- delayed duplicate completion
      Revoke (S);
      Check (not Active (S) and Busy (S) and not Ready_To_Release (S));
      Before := S;
      Install (S, (C with delta Base => 8192), Accepted);
      Check (not Accepted and S = Before);
      Begin_Access (S, 17, Old_Epoch, 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, Failed_Ticket);
      Check (not R.Allowed and S = Before);
      Finish_Access (S, Second_Ticket, Accepted);
      Check (Accepted and Ready_To_Release (S));
      Install (S, (C with delta Base => 8192), Accepted);
      Check (Accepted and Epoch (S) > Old_Epoch);
      Begin_Access (S, 17, Old_Epoch, 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, Failed_Ticket);
      Check (not R.Allowed and not Busy (S));
      Begin_Access (S, 17, Epoch (S), 0, ACPI_FADT.Transactions.Byte_Access,
        False, R, First_Ticket);
      Check (R.Allowed and then R.Address = 8192 and then First_Ticket > Second_Ticket);
      Before := S;
      Finish_Access (S, Second_Ticket, Accepted);
      Check (not Accepted and S = Before);
      Finish_Access (S, First_Ticket, Accepted);
      Check (Accepted);
      Before := S;
      Finish_Access (S, First_Ticket, Accepted);
      Check (not Accepted and S = Before);
   end;
   declare
      package IO renames Region_IO_Instance;
      use type IO.Outcome;
      H, Prior_H : Region_Mock.State;
      Item : IO.Request;
      Reply : IO.Response;
   begin
      Revoke (S);
      C := (Tag => 17, Base => 4096, Length => 8, Readable => True, Writable => True,
        Widths => [others => True], others => <>);
      Install (S, C, Accepted); Check (Accepted);
      Item.Token := Epoch (S);
      Before := S; Prior_H := H;
      IO.Execute (S, H, 0, Item, Reply);
      Check (Reply.Status = IO.Denied and S = Before and H.Calls = 0);
      Item.Offset := 8;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Denied and S = Before and H.Calls = 0);
      Item.Offset := 0; Item.Value := 1;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Malformed and S = Before and H.Calls = Prior_H.Calls);
      Item.For_Write := True; Item.Value := 256;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Malformed and H.Calls = 0);
      Item.Value := 255;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Done and H.Calls = 1 and H.Last_Address = 4096
        and H.Last_Input = 255 and H.Last_Write and Reply.Value = 0 and not Busy (S));
      Item.For_Write := False; Item.Value := 0; H.Next_Value := 42;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Done and Reply.Value = 42 and H.Calls = 2 and not Busy (S));
      H.Next_Value := 256;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Backend_Fault and Reply.Value = 0 and H.Calls = 3
        and Ready_To_Release (S));
      Install (S, C, Accepted); Check (Accepted);
      Item.Token := Epoch (S); H.Complete := False;
      Item.For_Write := True; Item.Value := 7;
      IO.Execute (S, H, 17, Item, Reply);
      Check (Reply.Status = IO.Indeterminate and Reply.Value = 0 and H.Calls = 4
        and Busy (S) and not Active (S) and not Ready_To_Release (S));
      declare
         Ticket : constant Unsigned_64 := Reply.Pending_Ticket;
      begin
         Before := S;
         IO.Execute (S, H, 17, Item, Reply);
         Check (Reply.Status = IO.Denied and S = Before and H.Calls = 4);
         Finish_Access (S, Ticket, Accepted); -- trusted confirmation of completion
         Check (Accepted and Ready_To_Release (S));
      end;
   end;
   declare
      package IO renames Region_IO_Instance;
      H : Region_Mock.State;
      Message, Reply : ACPI_Region_Protocol.Packet;
      Pending : Unsigned_64;
      procedure Rejected (Stamp : Unsigned_64 := 17; Status : IO.Outcome := IO.Malformed) is
         Old_Calls : constant Natural := H.Calls;
      begin
         Before := S;
         IO.Dispatch (S, H, Stamp, Message, Reply, Pending);
         Check (Reply.Data (0) = Unsigned_64 (IO.Outcome'Pos (Status))
           and Reply.Data (1) = 0 and Reply.Data (2) = 0 and Reply.Data (3) = 0);
         Check (H.Calls = Old_Calls and S = Before and Pending = 0);
      end Rejected;
   begin
      C.Register_Authority := (Resource_ID => 71, Readable => True, Writable => True,
        Delegable => False, Write_Mask => Unsigned_64'Last);
      C.Register_Width := ACPI_FADT.Transactions.Qword_Access;
      C.Register_Authority.Write_Mask := Unsigned_64'Last;
      Revoke (S); Install (S, C, Accepted); Check (Accepted);
      Message := (Label => IO.Read_Operation, Data => [Epoch (S), 71, 0, 0], others => <>);
      Rejected (0, IO.Denied); Rejected (18, IO.Denied);
      for Rsv in Unsigned_16 range 1 .. Unsigned_16'Last loop
         Message.Reserved := Rsv; Rejected;
      end loop;
      Message.Reserved := 0;
      for Flags in Unsigned_8 range 1 .. Unsigned_8'Last loop
         Message.Flags := Flags; Rejected;
      end loop;
      Message.Flags := 0;
      for Length in Unsigned_8 loop
         if Length /= 4 then Message.Length := Length; Rejected; end if;
      end loop;
      Message.Length := 4;
      for Bits in Unsigned_64 range 0 .. 255 loop
         if Bits /= 0 then Message.Data (2) := Bits; Rejected; end if;
      end loop;
      Message.Data (2) := Unsigned_64'Last; Rejected;
      Message.Data (2) := 0;
      Message.Label := 2; Rejected;
      Message.Label := Unsigned_32'Last; Rejected;
      Message.Label := IO.Read_Operation;
      Message.Data (3) := 17; Rejected; -- authority tag in payload grants nothing
      Message.Data (3) := 0;
      Message.Data (1) := Unsigned_64'Last; Rejected (Status => IO.Denied);
      Message.Data (1) := 71;
      Message.Data (0) := Epoch (S) - 1; Rejected (Status => IO.Denied);
      Message.Data (0) := Epoch (S); Message.Data (2) := 0;
      H.Next_Value := 16#FEDC_BA98_7654_3210#;
      IO.Dispatch (S, H, 17, Message, Reply, Pending);
      Check (Reply.Label = IO.Operation_Reply and Reply.Length = 4
        and Reply.Flags = 0 and Reply.Reserved = 0 and Pending = 0);
      Check (Reply.Data (0) = IO.Outcome'Pos (IO.Done) and Reply.Data (1) = 16#7654_3210#
        and Reply.Data (2) = 16#FEDC_BA98# and Reply.Data (3) = 0 and H.Calls = 1);
      H.Complete := False;
      Message.Label := IO.Write_Operation; Message.Data (2) := Unsigned_64'Last;
      IO.Dispatch (S, H, 17, Message, Reply, Pending);
      Check (Reply.Data (0) = IO.Outcome'Pos (IO.Indeterminate)
        and Reply.Data (1) = 0 and Reply.Data (2) = 0 and Reply.Data (3) = 0
        and Pending /= 0 and Busy (S));
      Finish_Access (S, Pending, Accepted); Check (Accepted);
      -- Named bindings are frozen with the epoch and cannot be edited live.
      H.Complete := True;
      C.Register_Authority.Resource_ID := 0;
      Install (S, C, Accepted); Check (Accepted);
      Message := (Label => IO.Read_Operation, Data => [Epoch (S), 0, 0, 0], others => <>);
      Rejected (Status => IO.Denied);
      Revoke (S);
      C.Register_Authority.Resource_ID := 71;
      C.Register_Offset := 4; -- Misaligned even though the ID is authorized.
      Install (S, C, Accepted); Check (Accepted);
      Message.Data (0) := Epoch (S); Message.Data (1) := 71;
      Rejected (Status => IO.Denied);
      Revoke (S);
      C.Register_Offset := 8; -- Aligned but outside the eight-byte region.
      Install (S, C, Accepted); Check (Accepted);
      Message.Data (0) := Epoch (S);
      Rejected (Status => IO.Denied);
      Revoke (S);
      C.Register_Offset := 0; C.Register_Authority.Write_Mask := 16#F#;
      Install (S, C, Accepted); Check (Accepted);
      Message.Data (0) := Epoch (S); Message.Label := IO.Write_Operation;
      for Value in Unsigned_64 range 16 .. 255 loop
         Message.Data (2) := Value; Rejected (Status => IO.Denied);
      end loop;
      Before := S; C.Register_Authority.Write_Mask := Unsigned_64'Last;
      Install (S, C, Accepted); Check (not Accepted and S = Before);
   end;
   Ada.Text_IO.Put_Line ("ACPI-REGION-POLICY: PASS" & Checks'Image);
end Region_Tests;
