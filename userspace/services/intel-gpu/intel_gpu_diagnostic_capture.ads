with Interfaces;
generic
   Capacity : Positive;
   Maximum_Length : Positive;
package Intel_GPU_Diagnostic_Capture with SPARK_Mode is
   -- Single-owner CPU capture, never a shared publication page. Overflow
   -- retires the oldest queued record, retaining the latest/final stage.
   subtype Text_Length is Natural range 0 .. Maximum_Length;
   type Captured_Record is record
      Text : String (1 .. Maximum_Length) := [others => ' '];
      Length : Text_Length := 0;
   end record;
   type Queue is limited private;
   function Count (Item : Queue) return Natural;
   function Lost (Item : Queue) return Interfaces.Unsigned_64;
   function Latest (Item : Queue) return Captured_Record;
   procedure Append (Item : in out Queue; Text : String)
     with Pre => Text'Length in 1 .. Maximum_Length,
       Post => Count (Item) in 1 .. Capacity;
   -- Returns a private copy; a publisher owns its separate immutable loan.
   procedure Take (Item : in out Queue; Value : out Captured_Record; Found : out Boolean)
     with Post => Count (Item) + Boolean'Pos (Found) = Count (Item)'Old
       and Found = (Count (Item)'Old > 0);
private
   subtype Slot is Natural range 0 .. Capacity - 1;
   type Entries is array (Slot) of Captured_Record;
   type Queue is limited record
      Data : Entries;
      Head : Slot := 0;
      Used : Natural range 0 .. Capacity := 0;
      Dropped : Interfaces.Unsigned_64 := 0;
      Last : Captured_Record;
   end record;
end Intel_GPU_Diagnostic_Capture;
