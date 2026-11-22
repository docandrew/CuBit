------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with Startup;

package body RTW89_Wifi_Link is
   package NL renames CuBit.Net_Link;
   package WS renames CuBit.Wifi_Scan;
   package Ring renames CuBit.Wifi_Driver.Rings;
   use type System.Address;

   REPLY_OK : constant Unsigned_32 := 16#F000#;
   ONE_WAY  : constant := 1;
   --  The Command_Wait's completion token (RTW89_Link uses its own).
   Wait_Token : constant Unsigned_64 := 16#5749_4649#;   --  "WIFI"
   --  wifi.svc answers Attach at once: a missed deadline means it is gone.
   Attach_Deadline_MS : constant Unsigned_64 := 2_000;

   Service    : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Handle     : Unsigned_64 := 0;
   Grant_Base : System.Address := System.Null_Address;
   Scan_Generation : NL.Generation := NL.Never_Reported;
   Receiving_Out : Ring.Producer := Ring.New_Producer;    --  our frames to wifi.svc
   Sending_In    : Ring.Consumer := Ring.New_Consumer;    --  wifi.svc's frames to us
   Parked  : Boolean := False;
   Pending : Pending_Command;
   Counted : Statistics;

   function Is_Attached return Boolean is (Grant_Base /= System.Null_Address);

   procedure Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Fence;

   procedure Compiler_Barrier is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Compiler_Barrier;

   function Word_At (Offset : Natural) return System.Address is (Grant_Base + Storage_Offset (Offset));

   procedure Store (Offset : Natural; Value : Unsigned_32) is
      Cell : Unsigned_32 with Volatile, Import, Address => Word_At (Offset);
   begin
      Cell := Value;
   end Store;

   function Load (Offset : Natural) return Unsigned_32 is
      Cell : Unsigned_32 with Volatile, Import, Address => Word_At (Offset);
   begin
      return Cell;
   end Load;

   procedure Ring_Doorbell is
      Ignore : Boolean;
   begin
      Ignore := capSubmit
        (Service,
         (tag => (label => D.Label (D.Receive_Doorbell), length => 1, protocol => ONE_WAY, ipc => <>,
                  reserved => 0),
          authorityTag => 0, words => [Handle, 0, 0, 0]),
         NO_COMPLETION_TOKEN);
   end Ring_Doorbell;

   procedure Attach (Address : NL.Hardware_Address; Status : out Attach_Status) is
      Call : Message :=
        (tag => (label => D.Label (D.Attach), length => 1, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0, words => [NL.Address_Word (Address), 0, 0, 0]);
      Answer : MessageTag;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      OK : Boolean;
   begin
      if not Startup.Wifi.Present then
         Status := No_Wifi_Service;
         return;
      end if;
      Service := Startup.Wifi.Value;
      Answer := capCall (Service, Call, Deadline_After (Attach_Deadline_MS));
      if Answer.label /= REPLY_OK or else Call.words (0) not in D.Handle
        or else not CuBit.Grant_References.Valid_Wire (Call.words (1))
      then
         Status := Refused;
         return;
      end if;
      Handle := Call.words (0);
      Grant := CuBit.Grant_References.Decode (Call.words (1));
      CuBit.Memory_Grants.Acquire_Via_Capability
        (Service, Grant, 0, D.Grant_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, OK);
      if not OK then
         Status := Grant_Refused;
         return;
      end if;
      Grant_Base := Mapped;
      Status := Attached;
      Park;
   end Attach;

   procedure Publish (Result : WS.Scan_Result) is
      Block : WS.Block;
   begin
      if not Is_Attached then
         return;
      end if;
      Scan_Generation := NL.Begin_Write (Scan_Generation);
      Store (D.Scan_At + WS.Generation_Word * WS.Word_Bytes, Scan_Generation);
      Compiler_Barrier;
      Block := WS.Encode (Result, Scan_Generation);
      for W in WS.Generation_Word + 1 .. WS.Block_Words - 1 loop
         Store (D.Scan_At + W * WS.Word_Bytes, Block (W));
      end loop;
      Compiler_Barrier;
      Scan_Generation := NL.End_Write (Scan_Generation);
      Store (D.Scan_At + WS.Generation_Word * WS.Word_Bytes, Scan_Generation);
   end Publish;

   procedure Deliver (Kind : D.Frame_Kind; Frame : Frame_Bytes) is
      OK : Boolean;
   begin
      if not Is_Attached or else Frame'Length not in 1 .. D.Maximum_Frame then
         Counted.Dropped := Counted.Dropped + 1;
         return;
      end if;
      Ring.Accept_Consumed (Receiving_Out, Ring.Index (Load (D.Receive_Header_At + D.Consumed_At)), OK);
      if not OK or else Ring.Space (Receiving_Out) = 0 then
         Counted.Dropped := Counted.Dropped + 1;
         return;
      end if;
      declare
         Slot_At : constant Natural := D.Receive_Slots_At + Ring.Next_Slot (Receiving_Out) * D.Slot_Bytes;
         Cells : Frame_Bytes (0 .. D.Slot_Bytes - 1) with Import, Volatile, Address => Word_At (Slot_At);
      begin
         for I in 0 .. Frame'Length - 1 loop
            Cells (D.Frame_At + I) := Frame (Frame'First + I);
         end loop;
         Store (Slot_At + D.Length_At, Unsigned_32 (Frame'Length));
         Store (Slot_At + D.Kind_At, Unsigned_32 (D.Frame_Kind'Enum_Rep (Kind)));
      end;
      Ring.Commit (Receiving_Out);
      Fence;
      Store (D.Receive_Header_At + D.Produced_At, Unsigned_32 (Receiving_Out.Produced));
      Counted.Delivered := Counted.Delivered + 1;
      Ring_Doorbell;
   end Deliver;

   procedure Next_Frame (Kind : out D.Frame_Kind; Frame : out Frame_Buffer; Length : out Natural;
                         Found : out Boolean) is
      OK : Boolean;
   begin
      Kind := D.Management;
      Frame := [others => 0];
      Length := 0;
      Found := False;
      if not Is_Attached then
         return;
      end if;
      Fence;
      Ring.Accept_Produced (Sending_In, Ring.Index (Load (D.Transmit_Header_At + D.Produced_At)), OK);
      if not OK then
         return;
      end if;
      while Sending_In.Available > 0 and then not Found loop
         declare
            Slot_At : constant Natural := D.Transmit_Slots_At + Ring.Head_Slot (Sending_In) * D.Slot_Bytes;
            Size : constant Unsigned_32 := Load (Slot_At + D.Length_At);
            Kind_Word : constant Unsigned_32 := Load (Slot_At + D.Kind_At);
            Cells : Frame_Bytes (0 .. D.Slot_Bytes - 1) with Import, Volatile, Address => Word_At (Slot_At);
         begin
            if D.Fits (Size)
              and then (Kind_Word = D.Frame_Kind'Enum_Rep (D.Management)
                        or else Kind_Word = D.Frame_Kind'Enum_Rep (D.EAPOL))
            then
               Kind := (if Kind_Word = D.Frame_Kind'Enum_Rep (D.Management) then D.Management else D.EAPOL);
               Length := Natural (Size);
               for I in 0 .. Length - 1 loop
                  Frame (I) := Cells (D.Frame_At + I);
               end loop;
               Found := True;
               Counted.Taken := Counted.Taken + 1;
            else
               Counted.Refused_Frames := Counted.Refused_Frames + 1;
            end if;
         end;
         Ring.Release (Sending_In);
      end loop;
      Store (D.Transmit_Header_At + D.Consumed_At, Unsigned_32 (Sending_In.Consumed));
   end Next_Frame;

   function Take (Entry_Found : CompletionEntry) return Boolean is
   begin
      if Entry_Found.token /= Wait_Token then
         return False;
      end if;
      Parked := False;
      if Entry_Found.status = COMPLETION_OK and then D.Is_Command (Entry_Found.msg.tag.label) then
         Pending := (Present => True, Label => Entry_Found.msg.tag.label, Words => Entry_Found.msg.words);
      end if;
      return True;
   end Take;

   procedure Park is
   begin
      if not Is_Attached or else Parked or else Pending.Present then
         return;
      end if;
      Parked := capSubmit
        (Service,
         (tag => (label => D.Label (D.Command_Wait), length => 1, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0, words => [Handle, 0, 0, 0]),
         Wait_Token);
   end Park;

   function Next_Command return Pending_Command is
      C : constant Pending_Command := Pending;
   begin
      Pending := (others => <>);
      return C;
   end Next_Command;

   procedure Done (Label : Unsigned_32; Result : D.Outcome) is
      Ignore : Boolean;
   begin
      if not Is_Attached then
         return;
      end if;
      Ignore := capSubmit
        (Service,
         (tag => (label => D.Label (D.Command_Done), length => 3, protocol => ONE_WAY, ipc => <>, reserved => 0),
          authorityTag => 0,
          words => [Handle, Unsigned_64 (Label), Unsigned_64 (D.Outcome'Enum_Rep (Result)), 0]),
         NO_COMPLETION_TOKEN);
   end Done;

   function Counts return Statistics is (Counted);

end RTW89_Wifi_Link;
