with Interfaces;
with Compositor_Elapsed;
package Compositor_Frame_Trace with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   subtype Output is Natural range 0 .. 1;
   type Record_Value is record
      Output_ID : Output := 0;
      Session, Frame : Tick := 0;
      Submitted, Completed : Tick := Compositor_Elapsed.Unavailable;
   end record;
   function Valid (Value : Record_Value) return Boolean is
     (Value.Session /= 0 and Value.Frame /= 0 and
      Compositor_Elapsed.Measure (Value.Submitted, Value.Completed).Valid);
   Maximum_Records : constant := 64;
   subtype Count_Type is Natural range 0 .. Maximum_Records;
   subtype Record_Index is Positive range 1 .. Maximum_Records;
   type State is private;
   function Count (S : State) return Count_Type;
   function Lost (S : State) return Natural;
   function Invalid (S : State) return Natural;
   function Item (S : State; Index : Record_Index) return Record_Value
     with Pre => Index <= Count (S), Post => Valid (Item'Result);
   procedure Add (S : in out State; Value : Record_Value)
     with Post =>
       (if Valid (Value) and Count (S'Old) < Maximum_Records then
          Count (S) = Count (S'Old) + 1 and then Item (S, Count (S)) = Value
        else Count (S) = Count (S'Old)) and then
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I));
   procedure Reset (S : out State)
     with Post => Count (S) = 0 and Lost (S) = 0 and Invalid (S) = 0;
private
   type Records is array (Record_Index) of Record_Value;
   type State is record
      Used : Count_Type := 0;
      Dropped, Bad : Natural := 0;
      Values : Records;
   end record with Dynamic_Predicate =>
     (for all I in 1 .. Used => Valid (Values (I)));
   function Count (S : State) return Count_Type is (S.Used);
   function Lost (S : State) return Natural is (S.Dropped);
   function Invalid (S : State) return Natural is (S.Bad);
   function Item (S : State; Index : Record_Index) return Record_Value is (S.Values (Index));
end Compositor_Frame_Trace;
