with Interfaces;
with Compositor_Elapsed;
--  Successful source draws and submissions, keyed by output writer identity.
--  Draw work may later be overwritten or occluded; no visibility claim.
package Compositor_Render_Trace with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   type Phase is (Draw, Submit);
   subtype Output is Natural range 0 .. 1;
   type Record_Value is record
      Kind : Phase := Draw;
      Output_ID : Output := 0;
      Buffer, Writer_Epoch, Writer_Serial : Tick := 0;
      Surface, Source_Epoch, Source_Ticket : Tick := 0;
      Session, Frame : Tick := 0;
      Observed : Tick := Compositor_Elapsed.Unavailable;
   end record;
   function Valid (V : Record_Value) return Boolean is
     (V.Buffer in 1 .. 3 and V.Writer_Epoch /= 0 and V.Writer_Serial /= 0 and
      V.Observed /= Compositor_Elapsed.Unavailable and
      (case V.Kind is
         when Draw => V.Surface /= 0 and V.Source_Epoch /= 0 and
           V.Source_Ticket /= 0 and V.Session = 0 and V.Frame = 0,
         when Submit => V.Surface = 0 and V.Source_Epoch = 0 and
           V.Source_Ticket = 0 and V.Session /= 0 and V.Frame /= 0));
   Maximum_Records : constant := 64;
   subtype Count_Type is Natural range 0 .. Maximum_Records;
   subtype Record_Index is Positive range 1 .. Maximum_Records;
   type State is private;
   function Count (S : State) return Count_Type;
   function Lost (S : State) return Natural;
   function Invalid (S : State) return Natural;
   function Unsupported (S : State) return Natural;
   function Item (S : State; Index : Record_Index) return Record_Value
     with Pre => Index <= Count (S), Post => Valid (Item'Result);
   function Increment (Value : Natural) return Natural is
     (if Value = Natural'Last then Value else Value + 1);
   procedure Add (S : in out State; Value : Record_Value)
     with Post => Unsupported (S) = Unsupported (S'Old) and then
       (if Valid (Value) and Count (S'Old) < Maximum_Records then
          Count (S) = Count (S'Old) + 1 and then Item (S, Count (S)) = Value
        else Count (S) = Count (S'Old)) and then
       Lost (S) = (if Valid (Value) and Count (S'Old) = Maximum_Records
                   then Increment (Lost (S'Old)) else Lost (S'Old)) and then
       Invalid (S) = (if not Valid (Value) then Increment (Invalid (S'Old))
                      else Invalid (S'Old)) and then
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I));
   procedure Note_Unsupported (S : in out State)
     with Post => Unsupported (S) = Increment (Unsupported (S'Old)) and
       Count (S) = Count (S'Old) and Lost (S) = Lost (S'Old) and
       Invalid (S) = Invalid (S'Old) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I));
   procedure Reset (S : out State)
     with Post => Count (S) = 0 and Lost (S) = 0 and Invalid (S) = 0 and Unsupported (S) = 0;
private
   type Records is array (Record_Index) of Record_Value;
   type State is record
      Used : Count_Type := 0;
      Dropped, Bad, Unsupported_Count : Natural := 0;
      Values : Records;
   end record with Dynamic_Predicate =>
     (for all I in 1 .. Used => Valid (Values (I)));
   function Count (S : State) return Count_Type is (S.Used);
   function Lost (S : State) return Natural is (S.Dropped);
   function Invalid (S : State) return Natural is (S.Bad);
   function Unsupported (S : State) return Natural is (S.Unsupported_Count);
   function Item (S : State; Index : Record_Index) return Record_Value is
     (S.Values (Index));
end Compositor_Render_Trace;
