with Interfaces; use Interfaces;
-- Retained GuC-to-host (G2H) events awaiting the service loop (GPU-001).
-- Pure FIFO, no I/O, proved at GNATprove level 2. An event is never
-- dropped: a full queue refuses the push, and the caller faults visibly.
--
-- Sizing (docs/gpu-async-submission.md H5). The G2H ring is 16 KiB, as in
-- i915; i915 reserves 4 KiB of it for unsolicited events and counts reply
-- credits for the rest. The service loop drains the whole ring every turn
-- and then empties this queue, so one full ring of the smallest frames
-- (CT header + HXG header) is the most a turn can retain.
package Intel_GPU_GuC_Event_Queue with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   G2H_Ring_Bytes : constant := 16 * 1024;
   G2H_Ring_Words : constant := G2H_Ring_Bytes / 4;
   -- i915 intel_guc_ct.c: G2H space kept free for unsolicited events.
   Unsolicited_Reserve_Words : constant := 4 * 1024 / 4;
   -- Smallest CT frame: CT header plus HXG header.
   Minimum_Frame_Words : constant := 2;
   Capacity : constant := G2H_Ring_Words / Minimum_Frame_Words;

   -- CT header length field bound, and how much of each payload is kept.
   -- Every G2H event Linux v6.16 defines carries at most four payload
   -- DWORDs after its HXG header; longer frames keep their first words
   -- and are reported as truncated, never silently shortened.
   Max_Payload_Words : constant := 255;
   Retained_Words : constant := 8;
   subtype Payload_Length is Natural range 0 .. Max_Payload_Words;
   subtype Retained_Index is Positive range 1 .. Retained_Words;
   type Retained_Payload is array (Retained_Index) of Unsigned_32;
   type Event is record
      Fence : Unsigned_16 := 0;
      Length : Payload_Length := 0; -- full CT payload length, header included
      Words : Retained_Payload := [others => 0];
   end record;
   function Truncated (Item : Event) return Boolean is
     (Item.Length > Retained_Words);
   function Kept_Words (Item : Event) return Natural is
     (if Truncated (Item) then Retained_Words else Item.Length);

   subtype Count_Type is Natural range 0 .. Capacity;
   subtype Position_Type is Positive range 1 .. Capacity;
   type Queue is private;
   function Length (Object : Queue) return Count_Type;
   -- Most events ever held at once (diagnostic high-water mark).
   function Peak (Object : Queue) return Count_Type;
   -- Position 1 is the oldest event.
   function Element (Object : Queue; Position : Position_Type) return Event
     with Pre => Position <= Length (Object);

   procedure Push (Object : in out Queue; Item : Event; Accepted : out Boolean)
     with Post => Accepted = (Length (Object'Old) < Capacity) and then
       (if Accepted then
          Length (Object) = Length (Object'Old) + 1 and
          Element (Object, Length (Object)) = Item and
          (for all P in 1 .. Length (Object'Old) =>
             Element (Object, P) = Element (Object'Old, P)) and
          Peak (Object) >= Length (Object)
        else Object = Object'Old);

   procedure Pop (Object : in out Queue; Item : out Event; Found : out Boolean)
     with Post => Found = (Length (Object'Old) > 0) and then
       (if Found then
          Item = Element (Object'Old, 1) and
          Length (Object) = Length (Object'Old) - 1 and
          (for all P in 1 .. Length (Object) =>
             Element (Object, P) = Element (Object'Old, P + 1)) and
          Peak (Object) = Peak (Object'Old)
        else Object = Object'Old);
private
   subtype Slot is Natural range 0 .. Capacity - 1;
   type Slots is array (Slot) of Event;
   type Queue is record
      Items : Slots := [others => (others => <>)];
      First : Slot := 0;
      Count : Count_Type := 0;
      Most : Count_Type := 0;
   end record;
   -- Ring index of a position, without modular arithmetic.
   function Index (First : Slot; Position : Position_Type) return Slot is
     (if First + Position - 1 < Capacity then First + Position - 1
      else First + Position - 1 - Capacity);
   function Length (Object : Queue) return Count_Type is (Object.Count);
   function Peak (Object : Queue) return Count_Type is (Object.Most);
   function Element (Object : Queue; Position : Position_Type) return Event is
     (Object.Items (Index (Object.First, Position)));
end Intel_GPU_GuC_Event_Queue;
