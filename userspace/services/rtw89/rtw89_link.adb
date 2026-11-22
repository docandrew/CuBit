------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with Startup;

package body RTW89_Link is
   package NL renames CuBit.Net_Link;
   package WS renames CuBit.Wifi_Scan;
   package Frames renames CuBit.Frame_Rings;

   --  netstack's answer labels (as every client spells them).
   REPLY_OK : constant Unsigned_32 := 16#F000#;
   --  A message's protocol field: one-way (a doorbell, no reply).
   ONE_WAY : constant := 1;
   use type System.Address;

   --  The Wireless MTU: Ethernet's (docs/wifi.md, "Integration").
   Wireless_MTU : constant := 1_500;

   Netstack   : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Handle     : NL.Interface_Handle;
   Grant_Base : System.Address := System.Null_Address;
   Link_Generation : NL.Generation := NL.Never_Reported;
   Scan_Generation : NL.Generation := NL.Never_Reported;
   Rung_Epoch : Unsigned_32 := 0;
   Own_Name : NL.Interface_Name;

   function Is_Attached return Boolean is (Grant_Base /= System.Null_Address);

   function Interface_Name return String is (Own_Name.Text (1 .. Own_Name.Length));

   procedure Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Fence;

   procedure Compiler_Barrier is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Compiler_Barrier;

   procedure Store (Offset : Natural; Value : Unsigned_32) is
      Cell : Unsigned_32 with Volatile, Import, Address => Grant_Base + Storage_Offset (Offset);
   begin
      Cell := Value;
   end Store;

   --  One receive doorbell per netstack wake epoch (docs/net-link.md 2).
   procedure Ring is
      Epoch_Cell : Unsigned_32 with Volatile, Import,
        Address => Grant_Base + Storage_Offset (Frames.Receive_Header_At + Frames.Wake_At);
      Epoch : Unsigned_32;
      Message_Out : constant Message :=
        (tag => (label => NL.Label (NL.Receive_Doorbell),
                 length => Unsigned_8 (NL.Request_Words (NL.Receive_Doorbell)),
                 protocol => ONE_WAY, ipc => <>, reserved => 0),
         authorityTag => 0,
         words => [0 => NL.Handle_Word (Handle), others => 0]);
      Ignore : Boolean;
   begin
      Fence;
      Epoch := Epoch_Cell;
      if Epoch /= 0 and then Epoch /= Rung_Epoch then
         Rung_Epoch := Epoch;
         Ignore := capSubmit (Netstack, Message_Out, NO_COMPLETION_TOKEN);
      end if;
   end Ring;

   procedure Publish_Link (State : NL.Link_State) is
      Report : constant NL.Report_Words := NL.Encode (State);
   begin
      Link_Generation := NL.Begin_Write (Link_Generation);
      Store (Frames.Link_Word_At (Frames.Link_Generation_Word), Link_Generation);
      Compiler_Barrier;
      for W in NL.Report_Index loop
         Store (Frames.Link_Word_At (W), Report (W));
      end loop;
      Compiler_Barrier;
      Link_Generation := NL.End_Write (Link_Generation);
      Store (Frames.Link_Word_At (Frames.Link_Generation_Word), Link_Generation);
      Ring;
   end Publish_Link;

   procedure Attach (Address : NL.Hardware_Address; Location : NL.PCI_Location;
                     Status : out Attach_Status) is
      Request : constant NL.Attach_Request :=
        (Kind => NL.Wireless, Address => Address, Size => Wireless_MTU,
         Offload => NL.No_Offloads, Identity => (Kind => NL.PCI_Function, Location => Location));
      Words : constant NL.Words3 := NL.Encode (Request);
      Call : Message :=
        (tag => (label => NL.Label (NL.Attach), length => Unsigned_8 (NL.Request_Words (NL.Attach)),
                 protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words => [0 => Words (0), 1 => Words (1), 2 => Words (2), others => 0]);
      Answer : MessageTag;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      OK : Boolean;
   begin
      Own_Name := NL.Name_Of (Request.Identity, Address);
      if not Startup.Netstack.Present then
         Status := No_Netstack;
         return;
      end if;
      Netstack := Startup.Netstack.Value;
      Answer := capCall (Netstack, Call, Wait_Forever);
      if Answer.label /= REPLY_OK or else not NL.Valid_Handle_Word (Call.words (0))
        or else not CuBit.Grant_References.Valid_Wire (Call.words (1))
      then
         Status := Refused;
         return;
      end if;
      Handle := NL.Handle_Of (Call.words (0));
      Grant := CuBit.Grant_References.Decode (Call.words (1));
      CuBit.Memory_Grants.Acquire_Via_Capability
        (Netstack, Grant, 0, Frames.Grant_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, OK);
      if not OK then
         Status := Grant_Refused;
         return;
      end if;
      Grant_Base := Mapped;
      Status := Attached;
      Publish_Link ((Condition => NL.Down, Reason => NL.Not_Associated));
   end Attach;

   procedure Report_Up (Detail : NL.Up_Detail) is
   begin
      if Is_Attached then
         Publish_Link ((Condition => NL.Up, Detail => Detail));
      end if;
   end Report_Up;

   procedure Report_Down (Reason : NL.Down_Reason) is
   begin
      if Is_Attached then
         Publish_Link ((Condition => NL.Down, Reason => Reason));
      end if;
   end Report_Down;

   package Frame_Ring renames Frames.Rings;
   Receive_Producer : Frame_Ring.Producer := Frame_Ring.New_Producer;
   Transmit_Consumer : Frame_Ring.Consumer := Frame_Ring.New_Consumer;
   Delivered_Since_Ring : Boolean := False;
   Parked : Boolean := False;
   Wake_Epoch : Unsigned_32 := 0;
   Counted : Statistics;
   --  The Transmit_Wait's completion token (RTW89_Wifi_Link has its own).
   Transmit_Token : constant Unsigned_64 := 16#4E45_5453#;   --  "NETS"

   function Load (Offset : Natural) return Unsigned_32 is
      Cell : Unsigned_32 with Volatile, Import, Address => Grant_Base + Storage_Offset (Offset);
   begin
      return Cell;
   end Load;

   procedure Deliver (Frame : Frame_Bytes) is
      OK : Boolean;
   begin
      if not Is_Attached or else Frame'Length not in Frames.Minimum_Frame .. Frames.Maximum_Frame then
         Counted.Dropped := Counted.Dropped + 1;
         return;
      end if;
      Frame_Ring.Accept_Consumed (Receive_Producer, Frame_Ring.Index (Load (Frames.Receive_Header_At + Frames.Consumed_At)), OK);
      if not OK or else Frame_Ring.Space (Receive_Producer) = 0 then
         Counted.Dropped := Counted.Dropped + 1;
         return;
      end if;
      declare
         Slot_At : constant Natural := Frames.Receive_Slot_At (Frame_Ring.Next_Slot (Receive_Producer));
         Cells : Frame_Bytes (0 .. Frames.Slot_Bytes - 1) with Import, Volatile,
           Address => Grant_Base + Storage_Offset (Slot_At);
      begin
         for I in 0 .. Frame'Length - 1 loop
            Cells (Frames.Frame_At + I) := Frame (Frame'First + I);
         end loop;
         Store (Slot_At + Frames.Length_At, Unsigned_32 (Frame'Length));
      end;
      Frame_Ring.Commit (Receive_Producer);
      Fence;
      Store (Frames.Receive_Header_At + Frames.Produced_At, Unsigned_32 (Receive_Producer.Produced));
      Counted.Delivered := Counted.Delivered + 1;
      Delivered_Since_Ring := True;
   end Deliver;

   procedure Ring_Receive is
   begin
      if Delivered_Since_Ring then
         Delivered_Since_Ring := False;
         Ring;
      end if;
   end Ring_Receive;

   procedure Next_Frame (Frame : out Ethernet_Buffer; Length : out Natural; Found : out Boolean) is
      OK : Boolean;
   begin
      Frame := [others => 0];
      Length := 0;
      Found := False;
      if not Is_Attached then
         return;
      end if;
      Fence;
      Frame_Ring.Accept_Produced (Transmit_Consumer, Frame_Ring.Index (Load (Frames.Transmit_Header_At + Frames.Produced_At)), OK);
      if not OK then
         return;
      end if;
      while Transmit_Consumer.Available > 0 and then not Found loop
         declare
            Slot_At : constant Natural := Frames.Transmit_Slot_At (Frame_Ring.Head_Slot (Transmit_Consumer));
            Size : constant Unsigned_32 := Load (Slot_At + Frames.Length_At);
            Cells : Frame_Bytes (0 .. Frames.Slot_Bytes - 1) with Import, Volatile,
              Address => Grant_Base + Storage_Offset (Slot_At);
         begin
            if Frames.Fits (Size) then
               Length := Natural (Size);
               for I in 0 .. Length - 1 loop
                  Frame (I) := Cells (Frames.Frame_At + I);
               end loop;
               Found := True;
               Counted.Taken := Counted.Taken + 1;
            end if;
         end;
         Frame_Ring.Release (Transmit_Consumer);
      end loop;
      Store (Frames.Transmit_Header_At + Frames.Consumed_At, Unsigned_32 (Transmit_Consumer.Consumed));
   end Next_Frame;

   function Take (Entry_Found : CuBit.Messages.CompletionEntry) return Boolean is
   begin
      if Entry_Found.token /= Transmit_Token then
         return False;
      end if;
      Parked := False;
      return True;
   end Take;

   procedure Park is
   begin
      if not Is_Attached or else Parked then
         return;
      end if;
      --  A new wake epoch: netstack answers this wait when it queues
      --  frames after seeing it (docs/net-link.md).
      Wake_Epoch := (if Wake_Epoch = Unsigned_32'Last then 1 else Wake_Epoch + 1);
      Store (Frames.Transmit_Header_At + Frames.Wake_At, Wake_Epoch);
      Fence;
      Parked := capSubmit
        (Netstack,
         (tag => (label => NL.Label (NL.Transmit_Wait),
                  length => Unsigned_8 (NL.Request_Words (NL.Transmit_Wait)),
                  protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0, words => [0 => NL.Handle_Word (Handle), others => 0]),
         Transmit_Token);
   end Park;

   function Counts return Statistics is (Counted);

   procedure Publish (Result : WS.Scan_Result) is
      Block : WS.Block;
   begin
      if not Is_Attached then
         return;
      end if;
      Scan_Generation := NL.Begin_Write (Scan_Generation);
      Store (Frames.Scan_At + WS.Generation_Word * WS.Word_Bytes, Scan_Generation);
      Compiler_Barrier;
      Block := WS.Encode (Result, Scan_Generation);
      for W in WS.Generation_Word + 1 .. WS.Block_Words - 1 loop
         Store (Frames.Scan_At + W * WS.Word_Bytes, Block (W));
      end loop;
      Compiler_Barrier;
      Scan_Generation := NL.End_Write (Scan_Generation);
      Store (Frames.Scan_At + WS.Generation_Word * WS.Word_Bytes, Scan_Generation);
      Ring;
   end Publish;

end RTW89_Link;
