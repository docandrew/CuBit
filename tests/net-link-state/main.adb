------------------------------------------------------------------------------
--  Hosted tests: CuBit.Net_Link (attach and report codecs, handles, the
--  seqlock) and Link_Tracker (netstack's link state machine), against a
--  simulated driver that writes its link block while netstack reads it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Management;
with Link_Tracker;

procedure Main is
   package NL renames CuBit.Net_Link;
   package LT renames Link_Tracker;
   package NM renames CuBit.Net_Management;
   use type NM.Words4, NM.Words2, NM.Configuration, NM.Interface_Summary,
            NM.Event_Report, NM.Route_Entry, NM.Operation, NL.Operation;
   use type NL.Link_Condition, NL.Link_Kind, NL.Hardware_Address, NL.Words3,
            NL.Report_Words, NL.Link_State, NL.Attach_Request, NL.Interface_Handle,
            LT.Observation;
   use type LT.Outcome, LT.Transition;

   package R64 is new Ada.Numerics.Discrete_Random (Unsigned_64);
   Gen : R64.Generator;
   function Rand return Unsigned_64 is (R64.Random (Gen));
   function Below (N : Unsigned_64) return Unsigned_64 is (Rand mod N);

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   function Random_Address return NL.Hardware_Address is
      A : NL.Hardware_Address;
   begin
      for I in A'Range loop
         A (I) := Unsigned_8 (Below (256));
      end loop;
      return A;
   end Random_Address;

   function Random_Request return NL.Attach_Request is
      R : NL.Attach_Request;
   begin
      R.Kind := (if Below (2) = 0 then NL.Ethernet else NL.Wireless);
      R.Address := Random_Address;
      R.Address (0) := R.Address (0) and not NL.Group_Bit;
      R.Address (5) := R.Address (5) or 1;   --  never all zeros
      R.Size := NL.MTU (NL.Minimum_MTU + Below (NL.Maximum_MTU - NL.Minimum_MTU + 1));
      R.Offload := (Below (2) = 0, Below (2) = 0, Below (2) = 0);
      if Below (2) = 0 then
         R.Identity := (Kind => NL.PCI_Function, Location => NL.PCI_Location (Below (16#10000#)));
      else
         R.Identity := (Kind => NL.Factory_Address);
      end if;
      return R;
   end Random_Request;

   function Random_State return NL.Link_State is
   begin
      case Below (4) is
         when 0 =>
            return (Condition => NL.Down,
                    Reason => NL.Down_Reason'Val (Below (NL.Down_Reason'Pos (NL.Down_Reason'Last) + 1)));
         when 1 =>
            return (Condition => NL.Dormant);
         when 2 =>
            return (Condition => NL.Up, Detail => (Kind => NL.Ethernet, Speed => Unsigned_32 (Below (100_000))));
         when others =>
            declare
               N : NL.Network_Identity;
            begin
               N.Length := NL.SSID_Length (Below (NL.SSID_Octets + 1));
               for I in 1 .. N.Length loop
                  N.SSID (I) := Unsigned_8 (Below (256));
               end loop;
               N.Security := NL.Wireless_Security'Val (Below (4));
               return (Condition => NL.Up,
                       Detail => (Kind => NL.Wireless, Speed => Unsigned_32 (Below (10_000)),
                                  BSSID => Random_Address, Network => N));
            end;
      end case;
   end Random_State;

   --  The simulated driver's link block.
   Block_Generation : NL.Generation := NL.Never_Reported;
   Block_Words : NL.Report_Words := [others => 0];

   procedure Publish (S : NL.Link_State) is
   begin
      Block_Generation := NL.Begin_Write (Block_Generation);
      Block_Words := NL.Encode (S);
      Block_Generation := NL.End_Write (Block_Generation);
   end Publish;

   O : LT.Observation;
   Step : LT.Step;

   procedure Read_Block is
   begin
      LT.Observe (O, Block_Generation, Block_Words, Block_Generation, Step);
   end Read_Block;

   Up_Ethernet : constant NL.Link_State :=
     (Condition => NL.Up, Detail => (Kind => NL.Ethernet, Speed => 1_000));
   Down_No_Carrier : constant NL.Link_State := (Condition => NL.Down, Reason => NL.No_Carrier);
begin
   R64.Reset (Gen, 20261023);

   --  Attach: round trips, and decoded words always re-encode to themselves.
   for I in 1 .. 100_000 loop
      declare
         R : constant NL.Attach_Request := Random_Request;
         D : constant NL.Decoded_Attach := NL.Decode (NL.Encode (R));
      begin
         Check (D.Valid and then D.Request = R, "attach round trip");
      end;
   end loop;
   for I in 1 .. 200_000 loop
      declare
         W : NL.Words3 := [Rand, Rand, Rand];
         D : NL.Decoded_Attach;
      begin
         --  Mostly valid-looking words with one field disturbed.
         if Below (2) = 0 then
            W := NL.Encode (Random_Request);
            W (Natural (Below (3))) := W (Natural (Below (3))) xor Shift_Left (1, Natural (Below (64)));
         end if;
         D := NL.Decode (W);
         Check (not D.Valid or else (NL.Encode (D.Request) = W and then NL.Valid (D.Request)),
                "attach decode is exact");
      end;
   end loop;
   declare
      R : NL.Attach_Request := Random_Request;
      W : NL.Words3;
   begin
      R.Address (0) := R.Address (0) or NL.Group_Bit;
      Check (not NL.Decode (NL.Encode (R)).Valid, "group address refused");
      R.Address := NL.No_Address;
      Check (not NL.Decode (NL.Encode (R)).Valid, "zero address refused");
      W := NL.Encode (Random_Request);
      W (0) := (W (0) and not Shift_Left (NL.Field_Mask_16, NL.MTU_Shift)) or
               Shift_Left (Unsigned_64 (NL.Minimum_MTU - 1), NL.MTU_Shift);
      Check (not NL.Decode (W).Valid, "MTU below the minimum refused");
      W := NL.Encode (Random_Request);
      W (0) := (W (0) and not NL.Field_Mask_8) or 3;
      Check (not NL.Decode (W).Valid, "unknown link kind refused");
   end;

   --  Handles round trip; the epoch is never zero.
   for I in 1 .. 100_000 loop
      declare
         H : constant NL.Interface_Handle :=
           (Slot => Unsigned_32 (Below (2 ** 32)), Epoch => Unsigned_32 (1 + Below (2 ** 32 - 1)));
      begin
         Check (NL.Valid_Handle_Word (NL.Handle_Word (H)) and then
                NL.Handle_Of (NL.Handle_Word (H)) = H, "handle round trip");
      end;
   end loop;
   Check (not NL.Valid_Handle_Word (12345), "epoch zero is no handle");

   --  Reports: round trips and exact decoding.
   for I in 1 .. 100_000 loop
      declare
         S : constant NL.Link_State := Random_State;
         D : constant NL.Decoded_Report := NL.Decode (NL.Encode (S));
      begin
         Check (D.Valid and then D.State = S, "report round trip");
      end;
   end loop;
   for I in 1 .. 200_000 loop
      declare
         W : NL.Report_Words := NL.Encode (Random_State);
         K : constant NL.Report_Index := NL.Report_Index (1 + Below (Unsigned_64 (NL.Report_Index'Last)));
         D : NL.Decoded_Report;
      begin
         W (K) := W (K) xor Shift_Left (1, Natural (Below (32)));
         D := NL.Decode (W);
         Check (not D.Valid or else NL.Encode (D.State) = W, "report decode is exact");
      end;
   end loop;

   --  The tracker against the simulated driver.
   Read_Block;
   Check (Step.Found = LT.Same and then not LT.Usable (O), "nothing reported: not usable");
   Publish (Up_Ethernet);
   Read_Block;
   Check (Step.Found = LT.Taken and then Step.Change = LT.Went_Up and then LT.Usable (O),
          "first Up taken");
   Read_Block;
   Check (Step.Found = LT.Same and then Step.Change = LT.Unchanged, "no new report: same");
   Publish (Down_No_Carrier);
   Read_Block;
   Check (Step.Change = LT.Went_Down and then not LT.Usable (O), "Down taken");
   Publish (Up_Ethernet);
   Read_Block;
   Check (Step.Change = LT.Went_Up, "Up again");

   --  A flap the reader never saw: two reports, Up before and after.
   Publish (Down_No_Carrier);
   Publish (Up_Ethernet);
   Read_Block;
   Check (Step.Found = LT.Taken and then Step.Reports = 2 and then Step.Change = LT.Bounced,
          "coalesced flap is a bounce");

   --  A torn read: the driver began writing between the reader's reads.
   declare
      Before : constant NL.Generation := Block_Generation;
      Seen   : constant LT.Observation := O;
   begin
      Block_Generation := NL.Begin_Write (Block_Generation);
      Block_Words := NL.Encode (Down_No_Carrier);
      LT.Observe (O, Before, Block_Words, Block_Generation, Step);
      Check (Step.Found = LT.Unstable and then O = Seen, "torn read takes nothing");
      LT.Observe (O, Block_Generation, Block_Words, Block_Generation, Step);
      Check (Step.Found = LT.Unstable, "odd generation takes nothing");
      Block_Generation := NL.End_Write (Block_Generation);
      Read_Block;
      Check (Step.Change = LT.Went_Down, "finished write taken");
   end;

   --  Words that are no report: Down, Hardware_Fault.
   Publish (Up_Ethernet);
   Read_Block;
   Block_Generation := NL.Begin_Write (Block_Generation);
   Block_Words (NL.Condition_Word) := 16#77#;
   Block_Generation := NL.End_Write (Block_Generation);
   Read_Block;
   Check (Step.Found = LT.Refused and then Step.Change = LT.Went_Down and then
          O.State = LT.Fault_State, "bad report refused as a fault");

   --  Wireless: roaming within the ESS moves, another ESS bounces.
   declare
      N : NL.Network_Identity;
      Here, There, Elsewhere : NL.Link_State;
   begin
      N.Length := 4;
      N.SSID (1 .. 4) := [67, 117, 66, 105];
      N.Security := NL.WPA3_Personal;
      Here := (Condition => NL.Up, Detail => (Kind => NL.Wireless, Speed => 600,
               BSSID => [2, 0, 0, 0, 0, 1], Network => N));
      There := (Condition => NL.Up, Detail => (Kind => NL.Wireless, Speed => 600,
                BSSID => [2, 0, 0, 0, 0, 2], Network => N));
      N.Security := NL.Open;
      Elsewhere := (Condition => NL.Up, Detail => (Kind => NL.Wireless, Speed => 600,
                    BSSID => [2, 0, 0, 0, 0, 2], Network => N));
      Publish (Here);
      Read_Block;
      Check (Step.Change = LT.Went_Up, "wireless up");
      Publish (There);
      Read_Block;
      Check (Step.Change = LT.Moved, "roam within the ESS moves");
      Publish (Elsewhere);
      Read_Block;
      Check (Step.Change = LT.Bounced, "another ESS (same SSID, other security) bounces");
      Publish ((Condition => NL.Dormant));
      Read_Block;
      Check (Step.Change = LT.Went_Down and then not LT.Usable (O), "dormant is not usable");
   end;

   --  Random interleavings: whatever the driver writes and whenever the
   --  reader looks, the reader never takes a torn report, ends with the
   --  driver's last state once the driver is quiet, and reports Starts
   --  whenever the link is Up after having been anything else since the
   --  last look.
   for Round in 1 .. 20_000 loop
      declare
         Published : Natural := 0;
         Was_Usable : constant Boolean := LT.Usable (O);
         Went_Away : Boolean := False;
         Last : NL.Link_State := O.State;
      begin
         for K in 1 .. Below (4) loop
            Last := Random_State;
            Publish (Last);
            Published := Published + 1;
            if Last.Condition /= NL.Up then
               Went_Away := True;
            end if;
         end loop;
         Read_Block;
         if Published > 0 then
            Check (Step.Found = LT.Taken and then O.State = Last, "latest report taken");
            if Last.Condition = NL.Up and then (Went_Away or else not Was_Usable) then
               Check (LT.Starts (Step.Change), "every way back to Up starts traffic");
            end if;
            if Was_Usable and then Last.Condition /= NL.Up then
               Check (LT.Stops (Step.Change), "leaving Up stops traffic");
            end if;
         else
            Check (Step.Found = LT.Same, "quiet driver: same");
         end if;
      end;
   end loop;

   --  Management codecs: random values round-trip, random words decode
   --  only to values that re-encode to them.
   for I in 1 .. 50_000 loop
      declare
         H : constant NL.Interface_Handle :=
           (Slot => Unsigned_32 (Below (2 ** 32)), Epoch => Unsigned_32 (1 + Below (2 ** 32 - 1)));
         C : constant NM.Configuration :=
           (Address => Unsigned_32 (Below (2 ** 32)), Prefix => NM.IPv4_Prefix (Below (33)),
            Gateway => Unsigned_32 (Below (2 ** 32)), Metric => NM.Route_Metric (Below (16#10000#)));
         DC : constant NM.Decoded_Configuration := NM.Decode_Configuration (NM.Encode (H, C));
         S : constant NM.Interface_Summary :=
           (Identity => (if Below (2) = 0 then (Kind => NL.PCI_Function, Location => NL.PCI_Location (Below (16#10000#)))
                         else (Kind => NL.Factory_Address)),
            Address => Random_Address,
            Kind => (if Below (2) = 0 then NL.Ethernet else NL.Wireless),
            Condition => NL.Link_Condition'Val (Below (3)),
            Settings => C, Sequence => Unsigned_32 (Below (2 ** 32)));
         DS : constant NM.Decoded_Summary := NM.Decode_Summary (NM.Encode (S));
         E : constant NM.Event_Report :=
           (Sequence => Unsigned_32 (Below (2 ** 32)), Handle => H,
            Event => NM.Interface_Event'Val (Below (3)), Condition => NL.Link_Condition'Val (Below (3)),
            Ups => NM.Up_Count (Below (16#10000#)),
            Reason => NL.Down_Reason'Val (Below (6)), Generation => Unsigned_32 (Below (2 ** 32)),
            Latest => Unsigned_32 (Below (2 ** 32)));
         DE : constant NM.Decoded_Event := NM.Decode_Event (NM.Encode (E));
         R : constant NM.Route_Entry :=
           (Destination => Unsigned_32 (Below (2 ** 32)), Prefix => NM.IPv4_Prefix (Below (33)),
            Usable => Below (2) = 0, Metric => NM.Route_Metric (Below (16#10000#)),
            Gateway => Unsigned_32 (Below (2 ** 32)), Slot => Unsigned_32 (Below (2 ** 32)));
         DR : constant NM.Decoded_Route := NM.Decode_Route (NM.Encode (R));
         W4 : constant NM.Words4 := [Rand, Rand, Rand, Rand];
         W2 : constant NM.Words2 := [Rand, Rand];
      begin
         Check (DC.Valid and then DC.Handle = H and then DC.Item = C, "configuration round trip");
         Check (DS.Valid and then DS.Item = S, "summary round trip");
         Check (DE.Valid and then DE.Item = E, "event round trip");
         Check (DR.Valid and then DR.Item = R, "route round trip");
         declare
            X : constant NM.Decoded_Configuration := NM.Decode_Configuration (W4);
            Y : constant NM.Decoded_Summary := NM.Decode_Summary (W4);
            Z : constant NM.Decoded_Event := NM.Decode_Event (W4);
            Q : constant NM.Decoded_Route := NM.Decode_Route (W2);
         begin
            Check (not X.Valid or else NM.Encode (X.Handle, X.Item) = W4, "configuration decode exact");
            Check (not Y.Valid or else NM.Encode (Y.Item) = W4, "summary decode exact");
            Check (not Z.Valid or else NM.Encode (Z.Item) = W4, "event decode exact");
            Check (not Q.Valid or else NM.Encode (Q.Item) = W2, "route decode exact");
         end;
      end;
   end loop;
   for Op in NM.Operation loop
      Check (NM.Is_Operation (NM.Label (Op)) and then NM.Operation_Of (NM.Label (Op)) = Op,
             "management label round trip");
   end loop;
   for Op in NL.Operation loop
      Check (NL.Operation_Of (NL.Label (Op)) = Op, "link label round trip");
   end loop;

   if Failures = 0 then
      Put_Line ("net-link-state: PASS");
   else
      Put_Line ("net-link-state: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Main;
