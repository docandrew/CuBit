with Interfaces; use Interfaces;

-- Bounded presentation of a Rust-owned logical tab list. Rows are local
-- widget positions, never logical tab IDs. Publication replaces the entire
-- snapshot; callers must cancel captured row actions when Same_Mapping is false.
package Servo_Tab_Projection with SPARK_Mode, Pure is
   Capacity : constant := 32;
   subtype Slot is Positive range 1 .. Capacity;
   subtype Selection is Natural range 0 .. Capacity;
   type Caption is array (Positive range 1 .. 64) of Unsigned_8
     with Convention => C;
   type Row is record
      ID : Unsigned_64 := 0;
      Text : Caption := [others => 0];
      Length : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
   end record with Convention => C, Size => 640;
   for Row use record
      ID at 0 range 0 .. 63;
      Text at 8 range 0 .. 511;
      Length at 72 range 0 .. 31;
      Reserved at 76 range 0 .. 31;
   end record;
   type Rows is array (Slot) of Row with Convention => C;
   type Snapshot is record
      Total : Unsigned_64 := 0;
      Active : Unsigned_64 := 0;
      Count : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
      Items : Rows := [others => <>];
   end record with Convention => C, Size => 20_672;
   for Snapshot use record
      Total at 0 range 0 .. 63;
      Active at 8 range 0 .. 63;
      Count at 16 range 0 .. 31;
      Reserved at 20 range 0 .. 31;
      Items at 24 range 0 .. 20_479;
   end record;

   -- Strictly increasing, nonzero IDs; the active tab must be represented.
   -- Unused rows and text tail bytes are canonical zeroes. Invalid input is
   -- rejected in full, leaving the old projection and its mapping unchanged.
   function Valid (Value : Snapshot) return Boolean;
   function Active_Row (Value : Snapshot) return Selection
     with Pre => Valid (Value);
   function ID_At (Value : Snapshot; Position : Natural) return Unsigned_64
     with Pre => Valid (Value);
   function Same_Mapping (Left, Right : Snapshot) return Boolean;
   procedure Publish
     (Current : in out Snapshot; Incoming : Snapshot; Accepted : out Boolean)
     with Pre => Valid (Current), Post => Valid (Current) and then
       (if Accepted then Current = Incoming else Current = Current'Old);
end Servo_Tab_Projection;
