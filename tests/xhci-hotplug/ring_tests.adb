--  Producer rings against a model controller that follows Link TRBs, and
--  the event ring against a model controller producer (xHCI 1.2, 4.9.2,
--  4.9.4, 4.11.5.1). Thousands of wraps, every lag the queue allows.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Rings; use XHCI_Rings;

procedure Ring_Tests is
   type Slot is record
      Cycle : Cycle_Bit := 0;
      Link  : Boolean := False;
      Value : Natural := 0;
   end record;
   Ring : array (Ring_Index) of Slot;
   P : Producer := Fresh_Producer;
   --  The model controller's dequeue state.
   Dequeue : Ring_Index := 0;
   CCS : Cycle_Bit := 1;
   Next_Value, Expected : Natural := 1;
   Consumed_Total : Natural := 0;

   --  Consume TRBs the controller owns, following Link TRBs; returns how
   --  many transfer TRBs it took (at most Limit).
   procedure Controller (Limit : Natural; Taken : out Natural) is
   begin
      Taken := 0;
      while Taken < Limit loop
         exit when Ring (Dequeue).Cycle /= CCS;
         if Ring (Dequeue).Link then
            Dequeue := 0;
            CCS := CCS + 1;   --  Toggle Cycle
         else
            pragma Assert (Ring (Dequeue).Value = Expected, "controller saw TRBs out of order");
            Expected := Expected + 1;
            Dequeue := Dequeue + 1;
            Taken := Taken + 1;
         end if;
      end loop;
   end Controller;

   procedure Produce is
      Placed : Placement;
   begin
      Advance (P, Placed);
      Ring (Placed.Index) := (Cycle => Placed.Cycle, Link => False, Value => Next_Value);
      Next_Value := Next_Value + 1;
      if Placed.Link_After then
         Ring (Link_Index) := (Cycle => Placed.Link_Cycle, Link => True, Value => 0);
      end if;
   end Produce;

   Outstanding, Taken : Natural := 0;
   Seed : Unsigned_32 := 16#2545_F491#;
   function Random (Bound : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Natural (Seed mod Unsigned_32 (Bound));
   end Random;
   Depth : constant := 8;

   --  Event ring.
   type Event_Slot is record
      Cycle : Cycle_Bit := 0;
      Value : Natural := 0;
   end record;
   Events : array (Event_Index) of Event_Slot;
   C : Consumer := Fresh_Consumer;
   Enqueue_Index : Event_Index := 0;
   PCS : Cycle_Bit := 1;
   Produced, Seen : Natural := 0;
begin
   --  The initial Link TRB, as Init_Ring writes it (cycle 1, TC).
   Ring (Link_Index) := (Cycle => 1, Link => True, Value => 0);
   for Round in 1 .. 200_000 loop
      if Outstanding < Depth and then (Random (3) /= 0 or else Outstanding = 0) then
         Produce;
         Outstanding := Outstanding + 1;
      else
         Controller (Random (Outstanding) + 1, Taken);
         Outstanding := Outstanding - Taken;
         Consumed_Total := Consumed_Total + Taken;
      end if;
   end loop;
   Controller (Outstanding, Taken);
   Consumed_Total := Consumed_Total + Taken;
   pragma Assert (Consumed_Total = Next_Value - 1, "controller lost TRBs");
   pragma Assert (Restart_Pointer (16#1000#, (Tail => 5, Cycle => 0)) = 16#1000# + 5 * 16);
   pragma Assert (Restart_Pointer (16#1000#, (Tail => 5, Cycle => 1)) = 16#1000# + 5 * 16 + 1);
   pragma Assert (Names_TRB (16#1000#, 16#1000# + 62 * 16));
   pragma Assert (not Names_TRB (16#1000#, 16#1000# + 63 * 16), "the Link TRB names no transfer");
   pragma Assert (not Names_TRB (16#1000#, 16#1008#));
   pragma Assert (not Names_TRB (16#1000#, 16#0FF0#));

   --  Event ring: the controller writes with its cycle and toggles at the
   --  end of the segment; software owns a TRB when the cycles match.
   for Round in 1 .. 100_000 loop
      declare
         Room : constant Natural := Event_Entries - 1 - (Produced - Seen);
         Burst : constant Natural := (if Room = 0 then 0 else Random (Room) + 1);
      begin
         for I in 1 .. Burst loop
            Produced := Produced + 1;
            Events (Enqueue_Index) := (Cycle => PCS, Value => Produced);
            if Enqueue_Index = Event_Index'Last then
               Enqueue_Index := 0;
               PCS := PCS + 1;
            else
               Enqueue_Index := Enqueue_Index + 1;
            end if;
         end loop;
         for I in 1 .. Random (Event_Entries) loop
            exit when not Owned (C, Unsigned_32 (Events (C.Head).Cycle));
            Seen := Seen + 1;
            pragma Assert (Events (C.Head).Value = Seen, "event lost or repeated");
            Consume (C);
         end loop;
      end;
   end loop;
   pragma Assert (Dequeue_Register (16#2000#, (Head => 3, Cycle => 0)) = 16#2000# + 48 + 8,
                  "ERDP carries EHB");
   Ada.Text_IO.Put_Line ("ring-tests: PASS" & Natural'Image (Consumed_Total) & " TRBs," &
                         Natural'Image (Seen) & " events");
end Ring_Tests;
