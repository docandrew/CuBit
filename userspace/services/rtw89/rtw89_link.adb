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
with CuBit.Frame_Rings;
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
