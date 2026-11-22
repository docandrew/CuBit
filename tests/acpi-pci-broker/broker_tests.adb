pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Test_Broker;
with CuBit.Handles.IPC;
with CuBit.Firmware_Snapshot;
with CuBit.Process_IDs;
with Devmgr_Authority;
with Devmgr_PCI_Broker;
with Test_Heap;
procedure Broker_Tests is
   package B renames Devmgr_PCI_Broker;
   package Q renames B.Q;
   package C renames B.C;
   package CM renames CuBit.Messages;
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   package FS renames CuBit.Firmware_Snapshot;
   package P renames CuBit.Process_IDs;
   use type Q.Disposition, Q.Request_ID, Q.Record_ID, C.Delivery, C.Reader_State;
   use type P.Process_ID, B.Exclusion_Status;
   Checks : Natural := 0;
   Reader : constant P.Process_ID := P.From_Word (16#1234_5678_0000_0001#);
   Request : Q.Request := (ID => 1, Key => (0, 1), Mode => Q.Read_Only);
   Msg : CM.Message;
   R : B.Observation;
   First : constant Unsigned_64 := 16#C010_0000#;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with What; end if;
   end Check;
   procedure Put (At_Byte, Bytes : Positive; Value : Unsigned_64) is
   begin
      for K in 0 .. Bytes - 1 loop
         FS.Data (At_Byte + K) := Unsigned_8 (Shift_Right (Value, K * 8) and 255);
      end loop;
   end Put;
   procedure Checksum is
      Sum : Unsigned_8 := 0;
   begin
      FS.Data (10) := 0;
      for K in 1 .. FS.Length loop Sum := Sum + FS.Data (K); end loop;
      FS.Data (10) := -Sum;
   end Checksum;
   procedure Table (Allocations : Natural := 300) is
   begin
      FS.Data := [others => 0];
      FS.Length := 44 + 16 * Allocations;
      FS.Data (1 .. 4) := [77, 67, 70, 71];
      Put (5, 4, Unsigned_64 (FS.Length)); FS.Data (9) := 1;
      for K in 0 .. Allocations - 1 loop
         Put (45 + 16 * K, 8, 16#C000_0000# + Unsigned_64 (K) * 16#1000_0000#);
         Put (53 + 16 * K, 2, Unsigned_64 (K));
         FS.Data (55 + 16 * K) := 0; FS.Data (56 + 16 * K) := 255;
      end loop;
      Checksum;
   end Table;
   procedure Reset is
   begin
      Request.ID := Request.ID + 1;
      Request.Key := (0, 1); Request.Mode := Q.Read_Only;
      Table;
      FS.Fail_Copy := False; FS.Short_Copy := False; FS.Duplicate := False;
      FS.Fail_Describe := False; FS.Not_Ready_Now := False;
      FS.Fail_After_First := False; FS.Missing := False; FS.Fail_Info := False;
      FS.Copies := 0; FS.Describe_Calls := 0;
      Test_Heap.Budget := Unsigned_64'Last;
      IPC.Next_Outcome := IPC.Transferred;
      IPC.Inspected := (Current => True, Kind => H.ABI.Device_Memory,
        Held => (Read | Grant => True, others => False),
        Authority_Tag => Capability_ABI.Firmware_PCI_Config_Tag,
        Object => First, Parameter => Q.Bus_Bytes);
      Devmgr_Authority.Held_Root :=
        (Memory => H.Test_Broker.Memory, First => First, Bytes => Q.Bus_Bytes, others => <>);
      Devmgr_Authority.Root_Count := 1;
   end Reset;
   procedure Make_Message is
      Packet : constant Q.Packet := Q.Encode (Request);
   begin
      Msg := (tag => (label => Packet.Label, length => Packet.Length,
                      ipc => Capability_ABI.Accepting, others => <>),
              authorityTag => Q.Backend_Tag, words => CM.MessageWords (Packet.Words));
   end Make_Message;
   procedure Call (Expected : Q.Disposition) is
   begin
      Make_Message;
      B.Serve (Reader, Msg, H.Test_Broker.Snapshot, R);
      Check (R.Status = Expected, "status " & Q.Disposition'Image (R.Status) &
             " expected " & Q.Disposition'Image (Expected));
      Check (CM.Last_Reply.words (0) = Unsigned_64 (Request.ID), "correlation");
   end Call;
   Exclusions, Refuse_At : Natural := 0;
   procedure Record_Exclusion (First, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Exclusions := Exclusions + 1;
      Check (First = 16#C000_0000# + Unsigned_64 (Exclusions - 1) * 16#1000_0000#,
        "exact exclusion including late entries");
      Check (Bytes = 16#1000_0000#, "whole advertised allocation excluded");
      Accepted := Refuse_At = 0 or else Exclusions /= Refuse_At;
   end Record_Exclusion;
   procedure Read_Facts is new B.Read_Exclusions (Record_Exclusion);
   procedure Facts (Expected : B.Exclusion_Status; Count : Natural) is
      Status : B.Exclusion_Status;
   begin
      Exclusions := 0; Read_Facts (H.Test_Broker.Snapshot, Status);
      Check (Status = Expected, "typed exclusion status " & B.Name (Status));
      Check (Exclusions = Count, "exclusion callbacks" & Exclusions'Image & " expected" & Count'Image);
   end Facts;
   Saved : Q.Record_ID;
   Before : Natural;
begin
   Reset; Call (Q.Granted);
   Check (FS.Copies = 2, "complete table copied beyond one chunk");
   Check (IPC.Last_Rights = H.Mask (H.Read_Only), "copy has read only");
   Check (B.Reader_Claims (Request.Key) = 1, "reader recorded");
   Check (B.Claim_At (1).Reader = Reader, "full opaque reader");
   Saved := R.Record_Number; Before := B.Count;
   Call (Q.Granted);
   Check (B.Count = Before and R.Record_Number = Saved, "stable replay");
   Reset; Request.Mode := Q.Read_Write; Call (Q.Write_Refused);
   Reset; FS.Fail_Copy := True; Call (Q.Unavailable);
   Reset; FS.Short_Copy := True; Call (Q.Unavailable);
   Reset; FS.Fail_After_First := True; Call (Q.Unavailable);
   Check (FS.Copies = 2, "partial table discarded");
   Reset; FS.Duplicate := True; Call (Q.Unavailable);
   Reset; FS.Fail_Describe := True; Call (Q.Unavailable);
   Reset; FS.Not_Ready_Now := True; Call (Q.Unavailable);
   Reset; Test_Heap.Budget := Unsigned_64 (FS.Length - 1); Call (Q.Unavailable);
   Reset; FS.Data (10) := FS.Data (10) + 1; Call (Q.Outside_MCFG);
   Reset; FS.Data (41) := 1; Checksum; Call (Q.Outside_MCFG);
   Reset; FS.Data (9) := 2; Checksum; Call (Q.Outside_MCFG);
   Reset; Request.Key.Segment := 500; Call (Q.Outside_MCFG);
   Reset; FS.Data (55) := 2; Checksum; Call (Q.Outside_MCFG);
   Reset; Devmgr_Authority.Root_Count := 0; Call (Q.No_Root);
   Reset; IPC.Inspected := (Current => False, others => <>); Call (Q.No_Root);
   Reset; IPC.Inspected.Held.Write := True; Call (Q.No_Root);
   Reset; IPC.Inspected.Held.Grant := False; Call (Q.No_Root);
   Reset; IPC.Inspected.Held.Read := False; Call (Q.No_Root);
   Reset; IPC.Inspected.Authority_Tag := 0; Call (Q.No_Root);
   Reset; IPC.Inspected.Object := First + Q.Page_Bytes; Call (Q.No_Root);
   Reset; IPC.Inspected.Parameter := Q.Bus_Bytes * 2; Call (Q.No_Root);
   Reset; Devmgr_Authority.Held_Root.First := First + Q.Page_Bytes; Call (Q.No_Root);
   Reset; IPC.Next_Outcome := IPC.Delivered_Refused; Call (Q.Granted);
   Check (R.Delivery = C.Refused, "table refusal recorded");
   Check (B.Claim_At (B.Count).State = C.Reply_Refused, "refused ledger state");
   Call (Q.Unavailable);
   Reset; IPC.Next_Outcome := IPC.Refused; Call (Q.Granted);
   Check (R.Delivery = C.Uncertain, "uncertainty recorded");
   Check (B.Claim_At (B.Count).State = C.Reply_Uncertain, "uncertain ledger state");
   Check (CM.Last_Reply.words (1) = Q.Disposition'Enum_Rep (Q.Delivery_Uncertain), "uncertain reply");
   Call (Q.Delivery_Uncertain);
   Reset; Make_Message; Msg.authorityTag := 0;
   B.Serve (Reader, Msg, H.Test_Broker.Snapshot, R);
   Check (R.Status = Q.Not_Approved and FS.Describe_Calls = 0, "badge before table access");
   Reset; Make_Message; Msg.tag.ipc.Capability_Offered := True;
   B.Serve (Reader, Msg, H.Test_Broker.Snapshot, R);
   Check (R.Status = Q.Malformed and FS.Describe_Calls = 0, "offered capability refused");
   Reset; Make_Message; Msg.words (0) := 0;
   B.Serve (Reader, Msg, H.Test_Broker.Snapshot, R);
   Check (R.Status = Q.Malformed and FS.Describe_Calls = 0, "zero request refused");
   for Bus in Q.Bus_Number loop
      Reset;
      Request.Key.Bus := Bus;
      IPC.Inspected.Object := 16#C000_0000# + Unsigned_64 (Bus) * Q.Bus_Bytes;
      Devmgr_Authority.Held_Root.First := IPC.Inspected.Object;
      Call (Q.Granted);
      Check (CM.Last_Reply.words (3) = IPC.Inspected.Object, "bus selected independently");
      Check (IPC.Last_Rights = H.Mask (H.Read_Only), "all bus copies read only");
   end loop;
   Reset; Facts (B.Collected, 300);
   Check (FS.Copies = 2, "all exclusion entries require complete second chunk");
   Reset; FS.Fail_After_First := True; Facts (B.Source_Unavailable, 0);
   Check (FS.Copies = 2, "copy prefix never presented to exclusions");
   Reset; FS.Fail_Copy := True; Facts (B.Source_Unavailable, 0);
   Reset; FS.Short_Copy := True; Facts (B.Source_Unavailable, 0);
   Reset; FS.Fail_Describe := True; Facts (B.Source_Unavailable, 0);
   Reset; FS.Fail_Info := True; Facts (B.Source_Unavailable, 0);
   Reset; FS.Not_Ready_Now := True; Facts (B.Source_Unavailable, 0);
   Reset; FS.Duplicate := True; Facts (B.Source_Unavailable, 0);
   Reset; Test_Heap.Budget := Unsigned_64 (FS.Length - 1); Facts (B.Source_Unavailable, 0);
   Reset; FS.Missing := True; Facts (B.No_MCFG, 0);
   Check (FS.Copies = 0, "absent MCFG has no invented byte source");
   Reset; FS.Data (10) := FS.Data (10) + 1; Facts (B.Malformed_MCFG, 0);
   Reset; FS.Data (9) := 2; Checksum; Facts (B.Malformed_MCFG, 0);
   Reset; FS.Data (41) := 1; Checksum; Facts (B.Malformed_MCFG, 0);
   Reset; FS.Data (55 + 299 * 16) := 2; FS.Data (56 + 299 * 16) := 1; Checksum;
   Facts (B.Invalid_MCFG_Allocation, 299);
   Reset; Put (45, 8, Unsigned_64'Last - 4095); Checksum; Facts (B.Invalid_MCFG_Allocation, 0);
   Reset; Refuse_At := 3; Facts (B.Store_Refused, 3);
   Ada.Text_IO.Put_Line ("PCI broker model checks" & Checks'Image);
end Broker_Tests;
