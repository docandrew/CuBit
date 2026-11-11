pragma Ada_2022;
with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Handles.IPC;
with CuBit.ACPI_Memory_Classes;
with Devmgr_Authority;
with Devmgr_Heap;
with Devmgr_Claim_Ledgers.PCI;
with CuBit.Firmware_Snapshot;
with System;
with Firmware_Tables.MCFG;
with Firmware_Tables.PCI_Windows;
package body Devmgr_PCI_Broker is
   package H renames CuBit.Handles;
   package IPC renames CuBit.Handles.IPC;
   package L is new Devmgr_Authority.Ledgers.PCI;
   package FT renames Firmware_Tables;
   package M renames FT.MCFG;
   package W renames FT.PCI_Windows;
   package P renames C.P;
   use type Q.Access_Mode;
   use type H.ABI.Kind, W.Outcome;
   use type L.Prepare_Status, IPC.Reply_Outcome;
   use type Q.Disposition, CuBit.ACPI_Memory_Classes.Memory_Kind;
   Claims : L.Ledger;

   function Reader_Claims (Key : Q.Bus_Key) return Natural is
     (L.Reader_Claims (Claims, Key));
   function Count return Natural is (L.Count (Claims));
   function Claim_At (Index : Positive) return C.Claim is
     (L.Claim_At (Claims, Index));

   type Bus_Search is record
      Key : Q.Bus_Key;
      First : Unsigned_64 := 0;
      Found, Invalid : Boolean := False;
   end record;
   procedure Consider (State : in out Bus_Search; Item : M.Allocation) is
   begin
      if not W.Valid (Item, Unsigned_64'Last) then State.Invalid := True; return; end if;
      declare
         Selected : constant W.Selection := W.Select_Bus
           (Item, M.Segment_Number (State.Key.Segment), M.Bus_Number (State.Key.Bus),
            W.Read_Only, Unsigned_64'Last);
      begin
         if Selected.Status = W.Ready then
            if State.Found then State.Invalid := True;
            else State.Found := True; State.First := Selected.First;
            end if;
         end if;
      end;
   end Consider;
   procedure Scan_Bus is new M.Scan (Bus_Search, Consider);
   procedure Resolve
     (Data : FT.Bytes; Key : Q.Bus_Key; Memory : out H.Device_Memory;
      Backing : out C.Window; Why : out Q.Disposition)
   is
      Header : M.Table_Metadata;
      Search : Bus_Search := (Key => Key, others => <>);
      First : Unsigned_64;
   begin
      Memory := H.No_Device_Memory;
      Backing := (others => <>); Why := Q.Outside_MCFG;
      Scan_Bus (Data, Search, Header);
      if not Header.Valid or else Header.Revision /= 1 or else Header.Reserved /= 0
        or else Header.Count = 0 or else Search.Invalid or else not Search.Found then return; end if;
      First := Search.First;
      Why := Q.No_Root;
      for K in 1 .. Devmgr_Authority.PCI_Root_Count loop
         declare
            Root : constant Devmgr_Authority.ACPI_Window :=
              Devmgr_Authority.PCI_Root_At (K);
            D : constant IPC.Description := IPC.Describe (Root.Memory);
         begin
            -- Tags have kernel-enforced read-only/uncached semantics. The
            -- inspected extent must be precisely this independently selected
            -- bus, not a covering general device-memory capability.
            if D.Current and then D.Kind = H.ABI.Device_Memory
              and then D.Authority_Tag = Capability_ABI.Firmware_PCI_Config_Tag
              and then D.Held.Read and then D.Held.Grant and then not D.Held.Write
              and then D.Object = First and then D.Parameter = Q.Bus_Bytes
              and then Root.First = D.Object and then Root.Bytes = D.Parameter
              and then Root.Kind = CuBit.ACPI_Memory_Classes.PCI_Config
            then
               Memory := Root.Memory;
               Backing := (Root => C.Root_ID (K), Key => Key, First => First);
               Why := Q.Granted;
               return;
            end if;
         end;
      end loop;
   end Resolve;

   procedure Serve_Copied
     (From : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      MCFG : FT.Bytes; Catalogue_Ready : Boolean; Result : out Observation)
   is
      package CM renames CuBit.Messages;
      Packet : constant Q.Packet :=
        (Label => Msg.tag.label, Length => Msg.tag.length,
         Flags => Unsigned_64 (Capability_ABI.Flag_Byte
           (0, Capability_ABI.IPC_Flags (Msg.tag.ipc))),
         Protocol => Unsigned_64 (Msg.tag.protocol),
         Reserved => Unsigned_64 (Msg.tag.reserved),
         Words => Q.Word_Array (Msg.words));
      Decoded : constant Q.Decoded_Request := Q.Decode
        (Packet, Has_Capability => Msg.tag.ipc.Capability_Transferred);
      Response : CM.Message := CM.NULL_MESSAGE;
      Memory : H.Device_Memory;
      Backing : C.Window;
      Prepared : L.Preparation;
      Capacity : Devmgr_Authority.Capacity_Report;
      Applied : Boolean := False;
      Ignore : Unsigned_64;
      First : Unsigned_64 := 0;
      Read_Only : constant H.Rights := (Read => True, others => False);
   begin
      Result := (Reader => From, others => <>);
      if Decoded.Valid then
         Result.Request := Decoded.Item;
      end if;
      if Msg.authorityTag /= Q.Backend_Tag or else not P.Is_Process (From) then
         Result.Status := Q.Not_Approved;
      elsif not Decoded.Valid then
         Result.Status := Q.Malformed;
      elsif Decoded.Item.Mode /= Q.Read_Only then
         Result.Status := Q.Write_Refused;
      elsif not Catalogue_Ready then
         Result.Status := Q.Unavailable;
      else
         Resolve (MCFG, Decoded.Item.Key, Memory, Backing, Result.Status);
         if Result.Status = Q.Granted then
            L.Prepare (Claims, From, Msg.authorityTag, Decoded.Item,
                       Backing, Prepared, Capacity);
            Result.Status := Prepared.Why;
            if Prepared.Status in L.Prepared | L.Replay_Ready then
               Result.Status := Q.Granted;
               Result.Record_Number := Prepared.ID;
               First := Backing.First;
            end if;
         end if;
      end if;
      Response.tag := (label => Q.Reply_Label, length => Q.Message_Words,
                        protocol => 0, ipc => <>, reserved => 0);
      Response.words := [Msg.words (0),
                         Unsigned_64 (Q.Disposition'Enum_Rep (Result.Status)),
                         Unsigned_64 (Result.Record_Number), First];
      if Result.Status /= Q.Granted then
         Ignore := CM.reply (From, Response);
         return;
      end if;
      declare
         Sent : constant IPC.Reply_Result := IPC.Reply_With
           (From, Response, H.Copy (Memory, Read_Only));
      begin
         Result.Delivery :=
           (case Sent.Outcome is
               when IPC.Transferred => C.Transferred,
               when IPC.Delivered_Refused => C.Refused,
               when IPC.Refused => C.Uncertain);
         if Prepared.Status = L.Prepared then
            L.Complete (Claims, Prepared.Index, Prepared.ID,
                        Result.Delivery, Applied);
         end if;
         if Sent.Outcome = IPC.Refused then
            -- Reply authority is retained on this outcome. A denied response
            -- cannot undo an uncertain delivery record or grant retry rights.
            Response.words := [Msg.words (0),
              Unsigned_64 (Q.Disposition'Enum_Rep (Q.Delivery_Uncertain)), 0, 0];
            Ignore := CM.reply (From, Response);
         end if;
      end;
   end Serve_Copied;

   type Copy_Status is (Copied, Missing, Unavailable);
   generic
      type Consumer_State is limited private;
      with procedure Consume
        (State : in out Consumer_State; Data : FT.Bytes; Status : Copy_Status);
   procedure Copy_MCFG (Snapshot : H.Firmware_Snapshot; State : in out Consumer_State);
   procedure Copy_MCFG (Snapshot : H.Firmware_Snapshot; State : in out Consumer_State) is
      package FS renames CuBit.Firmware_Snapshot;
      use type FS.Outcome, FS.Byte_Count, FS.Table_ID, System.Address;
      A : constant FS.Authority := FS.Held (Snapshot);
      Catalog : FS.Catalog_Info;
      Description : FS.Table_Description;
      Status : FS.Outcome;
      Selected : FS.Table_ID := 0;
      Extent : Natural := 0;
      Signature : constant String := "MCFG";
      Empty : constant FT.Bytes (1 .. 0) := [];
      Block : System.Address := System.Null_Address;
      Ready : Boolean := False;
   begin
      FS.Info (A, Catalog, Status);
      if Status /= FS.Ok or else not Catalog.Ready then Consume (State, Empty, Unavailable); return; end if;
      for K in FS.Table_ID range 1 .. FS.Table_ID (Catalog.Table_Count) loop
         FS.Describe (A, K, Description, Status);
         if Status /= FS.Ok then Consume (State, Empty, Unavailable); return; end if;
         if (for all I in FS.Signature_Index => Description.Signature (I) = Character'Pos (Signature (I))) then
            if Selected /= 0 or else Description.Extent < M.Fixed_Size
              or else Description.Extent > FS.Byte_Count (Natural'Last) then
               Consume (State, Empty, Unavailable); return;
            end if;
            Selected := K; Extent := Natural (Description.Extent);
         end if;
      end loop;
      if Selected = 0 then Consume (State, Empty, Missing); return; end if;
      Block := Devmgr_Heap.Allocate (Unsigned_64 (Extent), FT.Byte'Alignment);
      if Block /= System.Null_Address then
         declare
            Data : FT.Bytes (1 .. Extent) with Import, Address => Block;
            Offset : Natural := 0;
         begin
            while Offset < Extent loop
               declare
                  Wanted : constant Positive := Positive'Min (FS.Chunk_Max, Extent - Offset);
                  Part : FS.Byte_Array (1 .. Wanted);
                  Count : FS.Byte_Count;
               begin
                  FS.Copy_Part (A, Selected, FS.Byte_Count (Offset), Part, Count, Status);
                  exit when Status /= FS.Ok or else Count /= FS.Byte_Count (Wanted);
                  Data (Offset + 1 .. Offset + Wanted) := FT.Bytes (Part);
                  Offset := Offset + Wanted;
               end;
            end loop;
            Ready := Offset = Extent;
            if Ready then Consume (State, Data, Copied); end if;
         end;
         Devmgr_Heap.Release (Block);
      end if;
      if not Ready then Consume (State, Empty, Unavailable); end if;
   end Copy_MCFG;
   type Request_State is record
      From : CuBit.Messages.Process_ID;
      Msg : CuBit.Messages.Message;
      Result : Observation;
   end record;
   procedure Consume_Request (State : in out Request_State; Data : FT.Bytes; Status : Copy_Status) is
   begin Serve_Copied (State.From, State.Msg, Data, Status = Copied, State.Result); end Consume_Request;
   procedure Read_Request is new Copy_MCFG (Request_State, Consume_Request);
   procedure Serve
     (From : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Snapshot : H.Firmware_Snapshot; Result : out Observation) is
      Empty : constant FT.Bytes (1 .. 0) := [];
      State : Request_State := (From => From, Msg => Msg, Result => <>);
   begin
      -- Authentication/framing remain ahead of all snapshot access/allocation.
      if Msg.authorityTag /= Q.Backend_Tag or else not P.Is_Process (From)
        or else not Q.Decode
          ((Label => Msg.tag.label, Length => Msg.tag.length,
            Flags => Unsigned_64 (Capability_ABI.Flag_Byte (0, Capability_ABI.IPC_Flags (Msg.tag.ipc))),
            Protocol => Unsigned_64 (Msg.tag.protocol), Reserved => Unsigned_64 (Msg.tag.reserved),
            Words => Q.Word_Array (Msg.words)),
           Msg.tag.ipc.Capability_Transferred).Valid then
         Serve_Copied (From, Msg, Empty, False, Result); return;
      end if;
      Read_Request (Snapshot, State); Result := State.Result;
   end Serve;
   function Name (Status : Exclusion_Status) return String is
     (case Status is
        when Collected => "COLLECTED", when No_MCFG => "NO_MCFG",
        when Source_Unavailable => "SOURCE_UNAVAILABLE", when Malformed_MCFG => "MALFORMED_MCFG",
        when Invalid_MCFG_Allocation => "INVALID_MCFG_ALLOCATION", when Store_Refused => "STORE_REFUSED");
   procedure Read_Exclusions
     (Snapshot : H.Firmware_Snapshot; Status : out Exclusion_Status) is
      procedure Add (Result : in out Exclusion_Status; Item : M.Allocation) is
         Accepted : Boolean;
      begin
         if Result /= Collected then return; end if;
         if not W.Valid (Item, Unsigned_64'Last) then Result := Invalid_MCFG_Allocation; return; end if;
         Record_Exclusion
           (Item.Base + Unsigned_64 (Item.First_Bus) * W.Bus_Bytes,
            (Unsigned_64 (Item.Last_Bus) - Unsigned_64 (Item.First_Bus) + 1) * W.Bus_Bytes, Accepted);
         if not Accepted then Result := Store_Refused; end if;
      end Add;
      procedure Scan is new M.Scan (Exclusion_Status, Add);
      procedure Consume_Facts (Result : in out Exclusion_Status; Data : FT.Bytes; Copied_Status : Copy_Status) is
         Header : M.Table_Metadata;
      begin
         if Copied_Status = Missing then Result := No_MCFG;
         elsif Copied_Status /= Copied then Result := Source_Unavailable;
         else
            -- Admit header policy before callbacks can retain any exclusion.
            -- The second validation in Scan is linear and binds the same bytes.
            Header := M.Decode (Data);
            if not Header.Valid or else Header.Revision /= 1 or else Header.Reserved /= 0 or else Header.Count = 0 then
               Result := Malformed_MCFG;
            else
               Result := Collected; Scan (Data, Result, Header);
            end if;
         end if;
      end Consume_Facts;
      procedure Read_Facts is new Copy_MCFG (Exclusion_Status, Consume_Facts);
      Result : Exclusion_Status := Source_Unavailable;
   begin
      Read_Facts (Snapshot, Result); Status := Result;
   end Read_Exclusions;
end Devmgr_PCI_Broker;
