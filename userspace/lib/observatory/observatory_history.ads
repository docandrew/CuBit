with Interfaces; use Interfaces;
package Observatory_History with SPARK_Mode is
   Capacity : constant := 64;
   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Count is Natural range 0 .. Capacity;
   type Identity is record
      Source, Publisher, Key, Kind, Unit : Unsigned_64 := 0;
   end record;
   type Sample is record
      Upper, Added : Unsigned_64 := 0;
      Has_Latency, Has_Delta, Lossy : Boolean := False;
   end record;
   type History is private;
   function Length (Item : History) return Count;
   function Sample_At (Item : History; Position : Index) return Sample
     with Pre => Position < Length (Item);
   procedure Clear (Item : out History) with Post => Length (Item) = 0;
   -- A changed identity or decreasing cumulative count starts a new series.
   -- After a pause, the first delta is unavailable instead of inventing rate.
   procedure Break_Continuity (Item : in out History);
   procedure Observe (Item : in out History; Series : Identity;
      Total, Upper : Unsigned_64; Lossy : Boolean)
     with Post => Length (Item) > 0;
   subtype Pixel_Height is Natural range 0 .. 512;
   subtype Positive_Height is Pixel_Height range 1 .. Pixel_Height'Last;
   -- Integer-only proportional bars: floor(Value * Height / Maximum),
   -- without forming that potentially overflowing product. At most 512 steps.
   function Scale (Value, Maximum : Unsigned_64; Height : Positive_Height)
     return Pixel_Height with Pre => Value <= Maximum,
       Post => Scale'Result <= Height;
private
   type Samples is array (Index) of Sample;
   type History is record
      Values : Samples := [others => (others => <>)];
      Used : Count := 0;
      Next : Index := 0;
      Series : Identity;
      Last_Total : Unsigned_64 := 0;
      Continuous : Boolean := False;
   end record with Type_Invariant => (if History.Continuous then History.Used > 0);
   function Length (Item : History) return Count is (Item.Used);
end Observatory_History;
