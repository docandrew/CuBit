------------------------------------------------------------------------------
--  Hosted tests for the input source rings (IPC-009, DEV-002): the code
--  Desktop and the drivers run, over regions in this process.
--
--  What it shows:
--  * a full ring is back pressure: nothing is overwritten or dropped, every
--    report comes out once, in order; a loss the producer had to accept is
--    visible to the consumer (its overflow count);
--  * wakes: a sleeping consumer is owed one wake per arming, a busy one none;
--    a producer waiting for room is woken when the consumer takes;
--  * two sources at once: reports leave the set oldest acquisition first,
--    and each source's own order is kept exactly;
--  * hot-plug: a source added mid-session is read at once; one that goes
--    away is drained first; one that comes back is a new source;
--  * identity: the source a report is delivered as is the ring's, chosen by
--    the set; nothing a producer writes changes it;
--  * admission: only the input sources' badge, producing, within limits.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Input;
with CuBit.Input_Rings;
with Input_Source_Set;
with Input_Ring_Admission;

procedure Main is
   package IR renames CuBit.Input_Rings;
   package EC renames IR.Channels;
   use type CuBit.Input.Report_Words;
   use type Input_Ring_Admission.Verdict;
   use type CuBit.Input.Device_Class;

   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL " & What);
      end if;
   end Check;

   Page : constant := 4_096;
   type Region is array (1 .. 4 * Page) of System.Storage_Elements.Storage_Element
     with Alignment => Page;
   type Region_Access is access Region;
   function New_Region return Region_Access is (new Region'(others => 0));

   --  A keyboard report with sequence Sequence acquired at Us microseconds.
   function Key (Sequence, Us : Unsigned_64; Device : CuBit.Input.Device_Class := CuBit.Input.KEYBOARD)
     return CuBit.Input.Report_Words
   is
      Delivery : constant CuBit.Input.Delivery_Class :=
        (if Device = CuBit.Input.KEYBOARD then CuBit.Input.ORDERED_TRANSITION
         else CuBit.Input.ACCUMULABLE_DISPLACEMENT);
   begin
      return CuBit.Input.Encode_Words
        ((sourceIdentity => 0, sequence => Sequence, generation => 1, device => Device,
          delivery => Delivery, flags => CuBit.Input.NO_REPORT_FLAGS, payload => 16#1E#,
          snapshot => CuBit.Input.Source_Snapshot (0, Us)), Us);
   end Key;

   ---------------------------------------------------------------------------
   --  The channel under back pressure.
   ---------------------------------------------------------------------------
   procedure Full_Ring is
      Producer_Pages : constant Region_Access := New_Region;
      Consumer_Page : constant Region_Access := New_Region;
      P : EC.Producer;
      C : EC.Consumer;
      Pushed, Ring, Got : Boolean;
      Item : CuBit.Input.Report_Words;
      Next_Out : Unsigned_64 := 1;
      Next_In : Unsigned_64 := 1;
      Wakes : Natural := 0;
   begin
      Check (EC.Producer_Pages * Page <= Region'Length, "the producer's region holds the ring");
      EC.Attach_Producer (P, Producer_Pages.all'Address, Consumer_Page.all'Address);
      EC.Attach_Consumer (C, Consumer_Page.all'Address, Producer_Pages.all'Address);
      --  Fill it: the ring takes exactly its slots, then says full.
      for N in 1 .. EC.Rings.Slots loop
         EC.Push (P, Key (Next_In, Next_In), Pushed, Ring);
         Check (Pushed and then not Ring, "a busy consumer owes no wake");
         Next_In := Next_In + 1;
      end loop;
      EC.Push (P, Key (Next_In, Next_In), Pushed, Ring);
      Check (not Pushed, "a full ring refuses (back pressure)");
      --  The producer gives up nothing silently: it marks a loss it accepts.
      EC.Mark_Overflow (P);
      Check (EC.Peer_Overflows (C) = 1, "the consumer sees the producer's overflow");
      --  It arms for room; the consumer's take owes it one wake.
      Check (EC.Arm_For_Room (P), "the producer arms for room");
      EC.Take (C, Item, Got, Ring);
      Check (Got and then Item = Key (Next_Out, Next_Out), "the oldest report comes out first");
      Check (Ring, "taking wakes a producer waiting for room");
      Next_Out := Next_Out + 1;
      EC.Disarm_For_Room (P);
      --  Interleave: the ring stays in order across many wraps.
      for Round in 1 .. 20 * EC.Rings.Slots loop
         EC.Push (P, Key (Next_In, Next_In), Pushed, Ring);
         if Pushed then
            Next_In := Next_In + 1;
         end if;
         if Round mod 3 /= 0 then
            EC.Take (C, Item, Got, Ring);
            if Got then
               Check (Item = Key (Next_Out, Next_Out), "every report once, in order");
               Next_Out := Next_Out + 1;
            end if;
         end if;
      end loop;
      loop
         EC.Take (C, Item, Got, Ring);
         exit when not Got;
         Check (Item = Key (Next_Out, Next_Out), "the rest in order");
         Next_Out := Next_Out + 1;
      end loop;
      Check (Next_Out = Next_In, "nothing was lost or duplicated");
      --  A sleeping consumer: one wake per arming, however many reports.
      Check (EC.Arm (C), "the consumer arms with nothing waiting");
      for N in 1 .. 8 loop
         EC.Push (P, Key (Next_In, Next_In), Pushed, Ring);
         Next_In := Next_In + 1;
         if Ring then
            Wakes := Wakes + 1;
         end if;
      end loop;
      Check (Wakes = 1, "one wake per arming");
      Check (not EC.Arm (C), "arming sees the reports waiting");
      --  A producer writing garbage into its own produced word garbles only
      --  its own traffic: the consumer refuses an index past the ring.
      EC.Disarm (C);
      declare
         Produced : Unsigned_32 with Import, Volatile,
           Address => Producer_Pages.all'Address;
         Taken : Natural := 0;
      begin
         Produced := Produced + Unsigned_32 (EC.Rings.Slots) * 4;
         loop
            EC.Take (C, Item, Got, Ring);
            exit when not Got;
            Taken := Taken + 1;
            Check (Item = Key (Next_Out, Next_Out), "the real reports, in order");
            Next_Out := Next_Out + 1;
         end loop;
         Check (Taken = 8, "a forged produced index yields nothing more");
      end;
   end Full_Ring;

   ---------------------------------------------------------------------------
   --  The consumer's set.
   ---------------------------------------------------------------------------
   type Delivery_Record is record
      Words : CuBit.Input.Report_Words;
      Source : Unsigned_64;
   end record;
   Delivered : array (1 .. 256) of Delivery_Record;
   Delivered_Count : Natural := 0;
   Woken : Natural := 0;

   procedure Record_Delivery (Words : CuBit.Input.Report_Words; Source : Unsigned_64) is
   begin
      Delivered_Count := Delivered_Count + 1;
      Delivered (Delivered_Count) := (Words, Source);
   end Record_Delivery;
   procedure Wake (Slot : Positive) is
      pragma Unreferenced (Slot);
   begin
      Woken := Woken + 1;
   end Wake;

   package Set is new Input_Source_Set (Capacity => 4, Wake_Producer => Wake, Deliver => Record_Delivery);

   procedure Sources is
      Keyboard_Pages, Pointer_Pages, Pad_Pages, Second_Pad_Pages : constant Region_Access := New_Region;
      Keyboard_Index, Pointer_Index, Pad_Index, Second_Pad_Index : constant Region_Access := New_Region;
      Keyboard, Pointer, Pad, Second_Pad : EC.Producer;
      Keyboard_Source, Pointer_Source, Pad_Source, Second_Pad_Source : Unsigned_64;
      Slot_K, Slot_P, Slot_D, Slot_E : Natural;
      Pushed, Ring : Boolean;
      Count : Natural;
      Last_Key, Last_Pointer : Unsigned_64 := 0;
   begin
      EC.Attach_Producer (Keyboard, Keyboard_Pages.all'Address, Keyboard_Index.all'Address);
      EC.Attach_Producer (Pointer, Pointer_Pages.all'Address, Pointer_Index.all'Address);
      Slot_K := Set.Free_Slot;
      Set.Add (Slot_K, Keyboard_Index.all'Address, Keyboard_Pages.all'Address, Keyboard_Source);
      Slot_P := Set.Free_Slot;
      Set.Add (Slot_P, Pointer_Index.all'Address, Pointer_Pages.all'Address, Pointer_Source);
      Check (Slot_K /= Slot_P and then Keyboard_Source /= Pointer_Source, "two rings, two sources");
      Check (Keyboard_Source > Set.Ring_Source_Base and then Pointer_Source > Set.Ring_Source_Base,
             "ring identities are the set's");
      --  Interleaved acquisitions: keys at 10, 30, 50, 70; pointer at 20, 40, 60.
      for N in 1 .. 4 loop
         EC.Push (Keyboard, Key (Unsigned_64 (N), Unsigned_64 (N * 20 - 10)), Pushed, Ring);
      end loop;
      for N in 1 .. 3 loop
         EC.Push (Pointer, Key (Unsigned_64 (N), Unsigned_64 (N * 20), CuBit.Input.RELATIVE_POINTER),
                  Pushed, Ring);
      end loop;
      Set.Drain (64, Count);
      Check (Count = 7, "both sources drained");
      for N in 1 .. Delivered_Count loop
         Check (CuBit.Input.Source_Time_Us (CuBit.Input.To_Message (Delivered (N).Words)) =
                  Unsigned_64 (N * 10),
                "arrival order across sources, by acquisition");
         if Delivered (N).Source = Keyboard_Source then
            Check (Delivered (N).Words.Sequence = Last_Key + 1, "the keyboard's own order kept");
            Last_Key := Delivered (N).Words.Sequence;
         elsif Delivered (N).Source = Pointer_Source then
            Check (Delivered (N).Words.Sequence = Last_Pointer + 1, "the pointer's own order kept");
            Last_Pointer := Delivered (N).Words.Sequence;
         else
            Check (False, "a report from a source the set does not know");
         end if;
      end loop;
      --  A producer cannot name itself: whatever its words say, it is
      --  delivered as its ring.
      Delivered_Count := 0;
      declare
         Forged : CuBit.Input.Report_Words := Key (5, 100);
         Report : CuBit.Input.Source_Report;
         Valid : Boolean;
      begin
         Forged.Payload := Pointer_Source;
         Forged.Snapshot := Forged.Snapshot or Pointer_Source;
         EC.Push (Keyboard, Forged, Pushed, Ring);
         Set.Drain (64, Count);
         Check (Count = 1 and then Delivered (1).Source = Keyboard_Source, "delivered as its own ring");
         CuBit.Input.Decode_Words (Delivered (1).Words, Delivered (1).Source, Report, Valid);
         Check (Report.sourceIdentity = Keyboard_Source, "decoded as its own ring");
         CuBit.Input.Decode_Words (Delivered (1).Words, 0, Report, Valid);
         Check (not Valid, "no identity, no report");
      end;
      --  Hot-plug: a tablet arrives mid-session and is read at once.
      Delivered_Count := 0;
      EC.Attach_Producer (Pad, Pad_Pages.all'Address, Pad_Index.all'Address);
      Slot_D := Set.Free_Slot;
      Set.Add (Slot_D, Pad_Index.all'Address, Pad_Pages.all'Address, Pad_Source);
      Check (Set.Count = 3 and then Pad_Source /= Keyboard_Source and then Pad_Source /= Pointer_Source,
             "a source added mid-session");
      EC.Push (Pad, Key (1, 200, CuBit.Input.RELATIVE_POINTER), Pushed, Ring);
      EC.Push (Pad, Key (2, 210, CuBit.Input.RELATIVE_POINTER), Pushed, Ring);
      Set.Drain (1, Count);
      Check (Count = 1 and then Delivered (1).Source = Pad_Source, "the new source is read");
      --  It goes away with one report still in its ring: drained, then gone.
      Set.Drain_Slot (Slot_D);
      Check (Delivered_Count = 2 and then Delivered (2).Words.Sequence = 2, "what it sent before leaving counts");
      Set.Remove (Slot_D);
      Check (Set.Count = 2 and then not Set.Active (Slot_D), "the source is gone");
      --  It comes back: a new source, not the old one.
      EC.Attach_Producer (Second_Pad, Second_Pad_Pages.all'Address, Second_Pad_Index.all'Address);
      Slot_E := Set.Free_Slot;
      Set.Add (Slot_E, Second_Pad_Index.all'Address, Second_Pad_Pages.all'Address, Second_Pad_Source);
      Check (Second_Pad_Source /= Pad_Source, "a device that comes back is a new source");
      --  Arming: a quiet set arms; a report makes it not quiet.
      Check (Set.Quiet, "a quiet set arms every ring");
      EC.Push (Keyboard, Key (6, 300), Pushed, Ring);
      Check (Ring, "a report wakes the sleeping consumer");
      Check (not Set.Quiet, "a report waiting: not quiet");
      Set.Disarm_All;
   end Sources;

   ---------------------------------------------------------------------------
   --  Admission.
   ---------------------------------------------------------------------------
   procedure Admission is
      package A renames Input_Ring_Admission;
   begin
      Check (A.Decide (CuBit.Input.Source_Ring_Tag, True, 0, 1) = A.Admitted, "an input source is admitted");
      Check (A.Decide (0, True, 0, 1) = A.Not_An_Input_Endpoint, "no badge: refused");
      Check (A.Decide (CuBit.Input.Publisher_Tag, True, 0, 1) = A.Not_An_Input_Endpoint,
             "the event lane's badge opens no ring");
      Check (A.Decide (CuBit.Input.Source_Ring_Tag + 1, True, 0, 1) = A.Not_An_Input_Endpoint,
             "another badge: refused");
      Check (A.Decide (CuBit.Input.Source_Ring_Tag, False, 0, 1) = A.Not_Producing, "a consumer: refused");
      Check (A.Decide (CuBit.Input.Source_Ring_Tag, True, IR.Sources_Per_Driver, 1) = A.Source_Limit,
             "past the driver's sources: refused");
      Check (A.Decide (CuBit.Input.Source_Ring_Tag, True, 0, 0) = A.Source_Limit, "a full set: refused");
   end Admission;

begin
   Full_Ring;
   Sources;
   Admission;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("INPUT-RINGS: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("INPUT-RINGS: FAIL" & Failures'Image & " of" & Checks'Image);
   end if;
end Main;
