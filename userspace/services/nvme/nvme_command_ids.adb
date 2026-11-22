------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body NVMe_Command_Ids with SPARK_Mode is

   procedure Lemma_Unique (Table : Tracker; First, Second : Flight_Slot) is
   begin
      pragma Assert (Identifier (Table, First) mod Slot_Span =
                       Command_Identifier (First));
      pragma Assert (Identifier (Table, Second) mod Slot_Span =
                       Command_Identifier (Second));
   end Lemma_Unique;

   procedure Issue
     (Table : in out Tracker; Slot : Flight_Slot; Id : out Command_Identifier)
   is
      Before : constant Generation := Table (Slot).Count;
   begin
      Table (Slot) := (State => Busy, Count => Before + 1);
      Id := Identifier (Table, Slot);
      --  Same low bits, different generation bits.
      pragma Assert (Id / Slot_Span = Command_Identifier (Before + 1));
      pragma Assert (Identifier_Of (Slot, Before) / Slot_Span =
                       Command_Identifier (Before));
   end Issue;

   procedure Expire (Table : in out Tracker; Slot : Flight_Slot) is
   begin
      Table (Slot).State := Expired;
   end Expire;

   procedure Retire
     (Table  : in out Tracker;
      Id     : Command_Identifier;
      Result : out Match)
   is
      Low : constant Natural := Slot_Of (Id);
   begin
      --  Only the slot in Id's low bits can carry Id.
      for Slot in Flight_Slot loop
         if Slot /= Low then
            pragma Assert (Identifier (Table, Slot) mod Slot_Span =
                             Command_Identifier (Slot));
            pragma Assert (not Names (Table, Slot, Id));
         end if;
         pragma Loop_Invariant
           (for all Other in Flight_Slot'First .. Slot =>
              (if Other /= Low then not Names (Table, Other, Id)));
      end loop;
      if Low not in Flight_Slot or else not Names (Table, Low, Id) then
         Result := No_Match;
         return;
      end if;
      if Table (Low).State = Busy then
         Result := (Kind => Current, Slot => Low);
      else
         Result := (Kind => Late, Slot => Low);
      end if;
      Table (Low).State := Free;
   end Retire;

   procedure Reset_All (Table : in out Tracker) is
   begin
      for Slot in Flight_Slot loop
         Table (Slot).State := Free;
         pragma Loop_Invariant
           (for all Other in Flight_Slot'First .. Slot =>
              Table (Other).State = Free);
         pragma Loop_Invariant
           (for all Other in Flight_Slot =>
              Table (Other).Count = Table'Loop_Entry (Other).Count);
      end loop;
   end Reset_All;

end NVMe_Command_Ids;
