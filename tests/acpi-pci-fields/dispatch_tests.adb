pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capability_ABI;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Catalog;
with ACPI_Backend_Firmware_Memory;
with ACPI_Backend_PCI_Reads;
with ACPI_Backend_PCI_Dispatch;
with ACPI_PCI_Protocol;
with ACPI_PCI_Config_Reads;
procedure Dispatch_Tests is
   package B renames ACPI_Backend_Binding_Types;
   package P renames ACPI_PCI_Config_Reads;
   package Q renames ACPI_PCI_Protocol;
   use type Q.Encoding_Outcome, Q.Outcome;
   package F renames ACPI_Backend_Firmware_Memory;
   Mode : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   Physical : constant Unsigned_64 := 16#E000_0000#;
   Virtual_First : constant Unsigned_64 := 16#4000_0000#;
   Scope : constant Unsigned_64 := 16#9000_0000_0000_002A#;
   Window_Bytes : constant Unsigned_64 := (if Mode = 9 then P.Bus_Bytes / 2 else P.Bus_Bytes);
   Readable : Boolean := True;
   Writable : constant Boolean := Mode = 10;
   Fresh_Class : Boolean := Mode /= 2;
   Delay_Inspection : Boolean := False;
   Now : B.Tick := 10;
   Checks, Loads, Mappings, Inspections : Natural := 0;
   Last_Address : Unsigned_64 := 0;
   use type B.Tick, B.Outcome, F.Outcome;
   use type P.Outcome, P.T.Access_Width;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Mode'Image & ":" & Checks'Image; end if; end Check;
   procedure Inspect_Memory (Authority : Natural; Item : out B.Description) is
   begin
      Inspections := Inspections + 1;
      Item := (Current => Authority = 1, Kind => B.Memory_Resource,
        Base => Physical, Length => Window_Bytes, Readable => Readable,
        Writable => Writable, Delegable => False);
   end Inspect_Memory;
   procedure Inspect_Ports (Authority : Natural; Item : out B.Description) is
      pragma Unreferenced (Authority);
   begin Item := (others => <>); end Inspect_Ports;
   procedure Map_Memory (Authority : Natural; First, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean) is
   begin
      Check (Authority = 1 and then First = Physical and then Virtual = Virtual_First);
      Check (Pages * 4096 = Window_Bytes and then Read_Only = not Writable);
      Mappings := Mappings + 1; Mapped := True;
   end Map_Memory;
   procedure Clock (Value : out B.Tick; Available : out Boolean) is
   begin Value := Now; Available := True; end Clock;
   procedure Unused_Memory (Virtual : Unsigned_64; Width : P.T.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Virtual, Width, For_Write, Input);
   begin Check (False); Output := 0; Completed := False; end Unused_Memory;
   procedure Unused_Ports (Authority : Natural; Port : Unsigned_16; Width : P.T.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Authority, Port, Width, For_Write, Input);
   begin Check (False); Output := 0; Completed := False; end Unused_Ports;
   package C is new ACPI_Backend_Catalog
     (Natural, 0, Natural, 0, Inspect_Memory, Inspect_Ports, Map_Memory, Clock,
      Unused_Memory, Unused_Ports, 2, 2, 2, Virtual_First, Virtual_First + 2 * P.Bus_Bytes, 1024);
   use type C.W.Window_ID;
   Hardware : C.Context;
   function Is_PCI (Window : C.W.Window_ID) return Boolean is
      Facts : constant F.Kernel_Facts :=
        (Current => True, Memory_Handle => True,
         Rights => (Readable, Writable, False, False, False),
         Tag => (if Fresh_Class then Capability_ABI.Firmware_PCI_Config_Tag else Capability_ABI.Firmware_NVS_Tag),
         First => Physical, Bytes => Window_Bytes);
      Actual : constant F.Report := F.Decode (Facts);
   begin
      if Delay_Inspection then Now := 100; end if;
      return Window = 1 and then Actual.Status = F.Admitted and then F.Allows_PCI_Reads (Actual.Memory);
   end Is_PCI;
   procedure Transfer (Virtual : Unsigned_64; Width : P.T.Access_Width;
      Value : out Unsigned_64; Completed : out Boolean);
   Clock_Calls : Natural := 0;
   Clock_Fault : Boolean := True;
   procedure Request_Clock (Value : out B.Tick; Available : out Boolean) is
   begin
      Clock_Calls := Clock_Calls + 1; Value := Now; Available := True;
      if Clock_Fault then
         if Mode = 13 then Available := False;
         elsif Mode = 14 and then Clock_Calls mod 2 = 0 then Value := Now - 1;
         elsif Mode = 15 and then Clock_Calls mod 2 = 0 then Value := 100;
         end if;
      end if;
   end Request_Clock;
   package R is new ACPI_Backend_PCI_Reads (C, Hardware, Is_PCI, Request_Clock, Transfer);
   package D is new ACPI_Backend_PCI_Dispatch (R);
   use type R.Outcome, R.Phase, R.Epoch_Number, R.Record_ID;
   State : R.Context;
   Header : R.Header_Report;
   Code : R.Outcome;
   Reason : P.Outcome;
   Value : Unsigned_64;
   Host : constant P.Region := (Device => (0,0,0,0), Base => 0, Length => 256);
   procedure Call_Read
     (State : in out R.Context; Scope : Unsigned_64; Epoch : R.Epoch_Number; ID : R.Record_ID;
      Region : P.Region; Offset : Unsigned_64; Width : P.T.Access_Width;
      Value : out Unsigned_64; Result : out R.Outcome; Reason : out P.Outcome;
      Deadline : B.Tick; For_Write : Boolean := False) is
      Encoded : constant Q.Encoded_Request := Q.Read_Request
        (Unsigned_64 (Epoch), Unsigned_64 (ID), Region, Offset, Width,
         Q.Microsecond_Deadline (if Deadline = 0 or else Deadline = B.No_Finite_Deadline then 100 else Deadline), For_Write);
      Input, Output : Q.Packet;
      Reply : Q.Reply;
   begin
      Value := 0; Reason := P.Invalid_Window;
      case Encoded.Status is
         when Q.Missing_Identity => Result := R.Denied; return;
         when Q.Invalid_Deadline => Result := R.Deadline_Expired; return;
         when Q.Rejected_Geometry => Result := R.Policy_Refused; Reason := Encoded.Reason; return;
         when Q.Encoded => Input := Encoded.Item;
      end case;
      -- Test peer may send an expired/unbounded deadline even though the
      -- ordinary encoder refuses it. The receiving reader must check again.
      Input.Data (3) := Unsigned_64 (Deadline);
      D.Dispatch (State, Scope, Input, Output);
      Reply := Q.Decode_Reply (Output, Q.Read, Width); Check (Reply.Valid);
      case Reply.Status is
         when Q.Done => Result := R.Done; Value := Reply.Value; Reason := P.Read_Ready;
         when Q.Denied => Result := R.Denied;
         when Q.Stale => Result := R.Stale;
         when Q.Catalog_Changed => Result := R.Catalog_Changed;
         when Q.Not_Ready => Result := R.Not_Ready;
         when Q.Backing_Unavailable | Q.Unavailable => Result := R.Backing_Unavailable;
         when Q.Policy_Refused => Result := R.Policy_Refused; Check (Reply.Has_Reason); Reason := Reply.Reason;
         when Q.Clock_Unavailable => Result := R.Clock_Unavailable;
         when Q.Clock_Reversed => Result := R.Clock_Reversed;
         when Q.Deadline_Expired => Result := R.Deadline_Expired;
         when Q.Fault => Result := R.Read_Failed;
         when Q.Reentered => Result := R.Reentered;
         when others => Check (False); Result := R.Invalid_Record;
      end case;
   end Call_Read;
   procedure Transfer (Virtual : Unsigned_64; Width : P.T.Access_Width;
      Value : out Unsigned_64; Completed : out Boolean) is
      Inner_Code : R.Outcome;
      Inner_Reason : P.Outcome;
      Inner_Value : Unsigned_64;
   begin
      Loads := Loads + 1; Last_Address := Virtual;
      Check (Virtual >= Virtual_First and then Virtual - Virtual_First <= P.Bus_Bytes - Unsigned_64 (P.T.Octets (Width)));
      if Mode = 8 then
         Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 16#60#, P.T.Dword_Access,
           Inner_Value, Inner_Code, Inner_Reason, 100);
         Check (Inner_Code = R.Reentered and then Inner_Value = 0);
      end if;
      Value := (case Width is when P.T.Byte_Access => 16#A5#,
        when P.T.Word_Access => 16#BEEF#, when others => 16#E000_0000#);
      if Mode = 7 then Value := 16#100#; end if;
      Completed := Mode /= 6;
   end Transfer;
   Bound : B.Report;
   Accepted : Boolean;
   procedure Refuse (Region : P.Region; Offset : Unsigned_64; Width : P.T.Access_Width;
      Expected : P.Outcome; Write : Boolean := False) is
      Before : constant Natural := Loads;
   begin
      Call_Read (State, Scope, Header.Epoch, Header.ID, Region, Offset, Width, Value, Code, Reason, 100, Write);
      Check (Code = R.Policy_Refused and then Reason = Expected and then Value = 0);
      Check (Loads = Before and then R.Status (State) = R.Serving);
   end Refuse;
begin
   Check (Mode in 1 .. 20);
   C.Bind_Memory (Hardware, (ID => 1, Space => C.W.P.Memory_Space,
     Base => Physical, Length => Window_Bytes, Readable => True, Writable => Writable, Delegable => False),
     1, 100, Bound);
   Check (Bound.Status = B.Bound and then Mappings = 1);
   C.Authorize_Scope (Hardware, Scope, Accepted); Check (Accepted);
   C.Seal (Hardware, Accepted); Check (Accepted);
   R.Configure (State, Scope, 11, 1, Code, Key => (if Mode = 20 then (7, 1) else (0, 0)));
   if Mode = 20 then
      Check (Code = R.Configured);
      R.Header (State, Scope, Header);
      declare
         Output : Q.Packet;
         Reply : Q.Reply;
         Region : P.Region := ((7, 1, 31, 7), 0, P.Function_Bytes);
         Encoded : constant Q.Encoded_Request := Q.Read_Request
           (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID), Region,
            P.Function_Bytes - 4, P.T.Dword_Access, 100, Key => (7, 1));
      begin
         D.Dispatch (State, Scope, Q.Header_Request, Output);
         Reply := Q.Decode_Reply (Output, Q.Header);
         Check (Reply.Valid and then Reply.Status = Q.Not_Ready and then Loads = 0);
         D.Dispatch (State, Scope, Q.Header_Request ((7, 1)), Output);
         Reply := Q.Decode_Reply (Output, Q.Header);
         Check (Reply.Valid and then Reply.Status = Q.Ready);
         Check (Encoded.Status = Q.Encoded);
         D.Dispatch (State, Scope, Encoded.Item, Output);
         Reply := Q.Decode_Reply (Output, Q.Read, P.T.Dword_Access);
         Check (Reply.Valid and then Reply.Status = Q.Done and then Loads = 1);
         Check (Last_Address = Virtual_First + P.Bus_Bytes - 4);
         Region.Device.Bus := 2;
         R.Read (State, Scope, Header.Epoch, Header.ID, Region, 0, P.T.Dword_Access,
           Value, Code, Reason, 100);
         Check (Code = R.Policy_Refused and then Reason = P.Unsupported_Bus and then Loads = 1);
      end;
      Ada.Text_IO.Put_Line ("mode20 keyed reader/dispatch PASS"); return;
   end if;
   if Mode in 2 | 9 | 10 then
      Check (Code = R.Invalid_Record and then R.Status (State) = R.Failed and then Loads = 0);
   else
      Check (Code = R.Configured);
      R.Header (State, Scope, Header);
      declare Output : Q.Packet; Reply : Q.Reply; begin
         D.Dispatch (State, Scope, Q.Header_Request, Output);
         Reply := Q.Decode_Reply (Output, Q.Header);
         Check (Reply.Valid and then Reply.Status = Q.Ready and then Reply.Epoch = Unsigned_64 (Header.Epoch)
           and then Reply.ID = Unsigned_64 (Header.ID));
      end;
      Check (Header.Status = R.Ready and then Header.ID = 11 and then Header.Epoch /= R.No_Epoch
        and then Header.First = Physical);
      declare Output : Q.Packet; Reply : Q.Reply;
         Input : Q.Packet := Q.Describe_Request (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID));
      begin
         D.Dispatch (State, Scope, Input, Output); Reply := Q.Decode_Reply (Output, Q.Describe);
         Check (Reply.Valid and then Reply.Status = Q.Ready and then Reply.First = Physical
           and then Reply.Bytes = P.Bus_Bytes and then Loads = 0);
         D.Dispatch (State, Scope + 1, Input, Output); Reply := Q.Decode_Reply (Output, Q.Describe);
         Check (Reply.Valid and then Reply.Status = Q.Denied and then Loads = 0);
         Input := Q.Describe_Request (Unsigned_64 (Header.Epoch) + 1, Unsigned_64 (Header.ID));
         D.Dispatch (State, Scope, Input, Output); Reply := Q.Decode_Reply (Output, Q.Describe);
         Check (Reply.Valid and then Reply.Status = Q.Stale and then Loads = 0);
         Input := Q.Describe_Request (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID) + 1);
         D.Dispatch (State, Scope, Input, Output); Reply := Q.Decode_Reply (Output, Q.Describe);
         Check (Reply.Valid and then Reply.Status = Q.Denied and then Loads = 0);
         Input.Length := 0;
         D.Dispatch (State, Scope, Input, Output); Reply := Q.Decode_Reply (Output, Q.Describe);
         Check (Reply.Valid and then Reply.Status = Q.Malformed and then Loads = 0);
      end;
      if Mode in 17 .. 19 then
         if Mode = 17 then Fresh_Class := False;
         elsif Mode = 18 then Readable := False;
         else C.Disable_Window (Hardware, 1, Accepted); Check (Accepted); end if;
         declare Output : Q.Packet; Reply : Q.Reply;
         begin
            D.Dispatch (State, Scope, Q.Describe_Request
              (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID)), Output);
            Reply := Q.Decode_Reply (Output, Q.Describe);
            Check (Reply.Valid and then Reply.Status =
              (if Mode = 19 then Q.Catalog_Changed else Q.Backing_Unavailable));
            Check (Reply.First = 0 and then Reply.Bytes = 0 and then Output.Data (1) = 0
              and then Output.Data (2) = 0 and then Loads = 0);
            Check (R.Status (State) = R.Failed);
            D.Dispatch (State, Scope, Q.Describe_Request
              (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID)), Output);
            Reply := Q.Decode_Reply (Output, Q.Describe);
            Check (Reply.Valid and then Reply.Status = Q.Not_Ready and then Loads = 0);
         end;
         Ada.Text_IO.Put_Line ("PASS revoked PCI metadata scenario=" & Mode'Image & " checks=" & Checks'Image);
         return;
      end if;
      Call_Read (State, Scope + 1, Header.Epoch, Header.ID, Host, 0, P.T.Byte_Access, Value, Code, Reason, 100);
      Check (Code = R.Denied and then Loads = 0);
      Call_Read (State, Scope, Header.Epoch + 1, Header.ID, Host, 0, P.T.Byte_Access, Value, Code, Reason, 100);
      Check (Code = R.Stale and then Loads = 0);
      Call_Read (State, Scope, Header.Epoch, 12, Host, 0, P.T.Byte_Access, Value, Code, Reason, 100);
      Check (Code = R.Denied and then Loads = 0);
      case Mode is
         when 1 =>
            declare
               Encoded : constant Q.Encoded_Request := Q.Read_Request (Unsigned_64 (Header.Epoch), Unsigned_64 (Header.ID),
                 Host, 16#60#, P.T.Dword_Access, 100);
               Input, Output : Q.Packet;
               Reply : Q.Reply;
               Before : constant Natural := Loads;
            begin
               Check (Encoded.Status = Q.Encoded);
               for Bad_Frame in 1 .. 5 loop
                  Input := Encoded.Item;
                  case Bad_Frame is
                     when 1 => Input.Length := 3;
                     when 2 => Input.Flags := 1;
                     when 3 => Input.Reserved := 1;
                     when 4 => Input.Data (0) := 0;
                     when others => Input.Data (2) := Input.Data (2) or Shift_Left (Unsigned_64'(1),48);
                  end case;
                  D.Dispatch (State, Scope, Input, Output); Reply := Q.Decode_Reply (Output,Q.Read);
                  Check (Reply.Valid and then Reply.Status = Q.Malformed and then Loads = Before);
               end loop;
               for Policy in 1 .. 3 loop
                  Input := Encoded.Item;
                  case Policy is
                     when 1 => Input.Data (2) := Input.Data (2) or Shift_Left (Unsigned_64'(1),47);
                     when 2 => Input.Data (2) := Input.Data (2) or Shift_Left (Unsigned_64'(3),45);
                     when others => Input.Data (2) := Input.Data (2) and not Shift_Left (Unsigned_64'(16#1FFF#),20);
                  end case;
                  D.Dispatch (State,Scope,Input,Output); Reply := Q.Decode_Reply (Output,Q.Read);
                  Check (Reply.Valid and then Reply.Status = Q.Policy_Refused and then Reply.Has_Reason and then Loads = Before);
                  Check (Reply.Reason = (if Policy = 1 then P.Write_Refused elsif Policy = 2 then P.Unsupported_Width else P.Invalid_Region));
               end loop;
            end;
            Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 16#60#, P.T.Dword_Access, Value, Code, Reason, 100);
            Check (Code = R.Done and then Last_Address = Virtual_First + 16#60#);
            Check ((Shift_Right (Value, 26) and 16#3F#) = 56);
            Refuse ((Host with delta Device => (0,1,0,0)), 0, P.T.Byte_Access, P.Unsupported_Bus);
            Refuse ((Host with delta Device => (1,0,0,0)), 0, P.T.Byte_Access, P.Unsupported_Segment);
            Refuse (Host, 0, P.T.Byte_Access, P.Write_Refused, True);
            Refuse (Host, 0, P.T.Qword_Access, P.Unsupported_Width);
            Refuse (Host, 1, P.T.Dword_Access, P.Misaligned);
            Refuse (Host, 256, P.T.Byte_Access, P.Outside_Region);
            Refuse (Host, Unsigned_64'Last, P.T.Byte_Access, P.Outside_Region);
            Refuse ((Host with delta Base => 4092, Length => 8), 0, P.T.Dword_Access, P.Invalid_Region);
            Refuse ((Host with delta Base => Unsigned_64'Last), 0, P.T.Byte_Access, P.Invalid_Region);
            for Device in P.Device_Number loop
               for Func in P.Function_Number loop
                  for Width in P.T.Access_Width range P.T.Byte_Access .. P.T.Dword_Access loop
                     declare
                        Region : constant P.Region := ((0,0,Device,Func),0,P.Function_Bytes);
                        Bytes : constant Unsigned_64 := Unsigned_64 (P.T.Octets (Width));
                     begin
                        for Edge in Boolean loop
                           declare Offset : constant Unsigned_64 := (if Edge then P.Function_Bytes - Bytes else 0); begin
                              Call_Read (State, Scope, Header.Epoch, Header.ID, Region, Offset, Width, Value, Code, Reason, 100);
                              Check (Code = R.Done and then Reason = P.Read_Ready);
                              Check (Last_Address = Virtual_First +
                                (Unsigned_64 (Device) * P.Functions_Per_Device + Unsigned_64 (Func)) * P.Function_Bytes + Offset);
                           end;
                        end loop;
                     end;
                  end loop;
               end loop;
            end loop;
         when 3 => Readable := False;
         when 4 => Now := 101;
         when 16 => Delay_Inspection := True;
         when 11 => Fresh_Class := False;
         when 5 => C.Disable_Window (Hardware, 1, Accepted); Check (Accepted);
         when others => null;
      end case;
      if Mode /= 1 then
         Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 16#60#,
           (if Mode = 7 then P.T.Byte_Access else P.T.Dword_Access), Value, Code, Reason,
           (if Mode = 12 then B.No_Finite_Deadline else 100));
         if Mode = 8 then Check (Code = R.Done and then Loads = 1);
         elsif Mode in 13 .. 16 then
            Check (Code = (if Mode = 13 then R.Clock_Unavailable elsif Mode = 14 then R.Clock_Reversed else R.Deadline_Expired));
            Check (R.Status (State) = R.Serving and then Loads = 0 and then Value = 0);
            Clock_Fault := False; Delay_Inspection := False; Now := 10;
            Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 16#60#, P.T.Dword_Access,
              Value, Code, Reason, 1000);
            Check (Code = R.Done and then Loads = 1);
         elsif Mode in 4 | 12 then
            Check (Code = R.Deadline_Expired and then R.Status (State) = R.Serving and then Loads = 0);
            -- The retained mapping outlives its startup mapping deadline.
            Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 16#60#, P.T.Dword_Access,
              Value, Code, Reason, 1000);
            Check (Code = R.Done and then Loads = 1);
         else
            Check (Value = 0 and then R.Status (State) = R.Failed);
            Check (Code = (if Mode = 5 then R.Catalog_Changed elsif Mode in 6 | 7 then R.Read_Failed else R.Backing_Unavailable));
            declare Before : constant Natural := Loads; begin
               Call_Read (State, Scope, Header.Epoch, Header.ID, Host, 0, P.T.Byte_Access, Value, Code, Reason, 100);
               Check (Code = R.Not_Ready and then Loads = Before);
            end;
         end if;
      end if;
   end if;
   Ada.Text_IO.Put_Line ("PASS PCI dispatcher scenario=" & Mode'Image & " checks=" & Checks'Image);
end Dispatch_Tests;
