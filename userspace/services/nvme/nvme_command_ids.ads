------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which I/O command identifiers are outstanding, and what a completion
--  that arrives means (BUG-003).
--
--  Each in-flight slot (NVMe_Layout.Flight_Slot) owns one data slice and
--  carries at most one command. Its identifier is the slot number in the
--  low bits and the slot's generation above it, as Linux's NVMe driver
--  tags identifiers with a generation counter: the identifiers of two
--  outstanding commands always differ (the spec requires them unique per
--  submission queue, 4.2), and a stale completion never matches a newer
--  command on the same slot.
--
--  A command that outlives its deadline is Expired, not freed: the
--  controller may still write its data slice. The slot is reused only
--  after that command's completion arrives (Late) or after a controller
--  reset (Reset_All, which ends every command the controller held). A
--  completion naming no outstanding command is Foreign and changes nothing.
------------------------------------------------------------------------------
with NVMe_Protocol; use NVMe_Protocol;
with NVMe_Layout; use NVMe_Layout;

package NVMe_Command_Ids with SPARK_Mode is

   --  Low identifier bits: the slot; the rest: its generation.
   Slot_Bits : constant := 3;
   Slot_Span : constant := 2 ** Slot_Bits;
   pragma Compile_Time_Error (Flight_Slots > Slot_Span, "slots outgrow their bits");
   type Generation is mod Command_Identifier'Modulus / Slot_Span;

   function Identifier_Of (Slot : Flight_Slot; Count : Generation)
     return Command_Identifier
   is (Command_Identifier (Count) * Slot_Span + Command_Identifier (Slot));

   --  Free: no command. Busy: a command within its deadline. Expired: a
   --  command past its deadline that the controller may still own.
   type Slot_State is (Free, Busy, Expired);

   type Slot_Record is record
      State : Slot_State := Free;
      Count : Generation := 0;
   end record;

   type Tracker is array (Flight_Slot) of Slot_Record;
   Empty : constant Tracker := [others => (State => Free, Count => 0)];

   function Outstanding (Table : Tracker; Slot : Flight_Slot) return Boolean is
     (Table (Slot).State /= Free);

   function Identifier (Table : Tracker; Slot : Flight_Slot)
     return Command_Identifier
   is (Identifier_Of (Slot, Table (Slot).Count));

   function All_Free (Table : Tracker) return Boolean is
     (for all Slot in Flight_Slot => Table (Slot).State = Free);

   function Any_Busy (Table : Tracker) return Boolean is
     (for some Slot in Flight_Slot => Table (Slot).State = Busy);

   --  Distinct slots never share an identifier, whatever their generations.
   procedure Lemma_Unique (Table : Tracker; First, Second : Flight_Slot)
     with Ghost,
          Pre  => First /= Second,
          Post => Identifier (Table, First) /= Identifier (Table, Second);

   --  Starts a command on a free slot under a fresh identifier.
   procedure Issue
     (Table : in out Tracker; Slot : Flight_Slot; Id : out Command_Identifier)
     with Pre  => Table (Slot).State = Free,
          Post => Table (Slot).State = Busy
                  and then Table (Slot).Count = Table'Old (Slot).Count + 1
                  and then Id = Identifier (Table, Slot)
                  and then Id /= Identifier (Table'Old, Slot)
                  and then (for all Other in Flight_Slot =>
                              (if Other /= Slot
                               then Table (Other) = Table'Old (Other)));

   --  The command on Slot outlived its deadline.
   procedure Expire (Table : in out Tracker; Slot : Flight_Slot)
     with Pre  => Table (Slot).State = Busy,
          Post => Table (Slot) = (State => Expired, Count => Table'Old (Slot).Count)
                  and then (for all Other in Flight_Slot =>
                              (if Other /= Slot
                               then Table (Other) = Table'Old (Other)));

   --  What a completion's identifier names.
   type Match_Kind is
     (Current,   --  a Busy command: Slot is freed
      Late,      --  an Expired command: Slot is freed, the slice is ours again
      Foreign);  --  no outstanding command: nothing changes

   --  Slot only means something for Current and Late.
   type Match is record
      Kind : Match_Kind := Foreign;
      Slot : Flight_Slot := Flight_Slot'First;
   end record;
   No_Match : constant Match := (Kind => Foreign, Slot => Flight_Slot'First);

   function Slot_Of (Id : Command_Identifier) return Natural is
     (Natural (Id mod Slot_Span));

   function Names (Table : Tracker; Slot : Flight_Slot; Id : Command_Identifier)
     return Boolean
   is (Table (Slot).State /= Free and then Identifier (Table, Slot) = Id);

   procedure Retire
     (Table  : in out Tracker;
      Id     : Command_Identifier;
      Result : out Match)
     with Post =>
       (case Result.Kind is
          when Current =>
            Names (Table'Old, Result.Slot, Id)
            and then Table'Old (Result.Slot).State = Busy
            and then Table (Result.Slot).State = Free
            and then Table (Result.Slot).Count = Table'Old (Result.Slot).Count
            and then (for all Other in Flight_Slot =>
                        (if Other /= Result.Slot
                         then Table (Other) = Table'Old (Other))),
          when Late =>
            Names (Table'Old, Result.Slot, Id)
            and then Table'Old (Result.Slot).State = Expired
            and then Table (Result.Slot).State = Free
            and then Table (Result.Slot).Count = Table'Old (Result.Slot).Count
            and then (for all Other in Flight_Slot =>
                        (if Other /= Result.Slot
                         then Table (Other) = Table'Old (Other))),
          when Foreign =>
            Table = Table'Old
            and then (for all Slot in Flight_Slot =>
                        not Names (Table'Old, Slot, Id)));

   --  After a controller reset: the controller holds no command, so every
   --  slot is free. Generations are kept, so no identifier from before the
   --  reset names a command issued after it until its generation wraps.
   procedure Reset_All (Table : in out Tracker)
     with Post => All_Free (Table)
                  and then (for all Slot in Flight_Slot =>
                              Table (Slot).Count = Table'Old (Slot).Count);

end NVMe_Command_Ids;
