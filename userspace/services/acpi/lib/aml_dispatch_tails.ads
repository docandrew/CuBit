pragma Ada_2022;
with AML_Profile_Events;
-- Passive copied syntax history. A batch table labels the executing module;
-- it is NOT the original table containing a called method. Positions remain
-- frame-relative dispatch positions, never authoritative failure offsets.
package AML_Dispatch_Tails with Pure, SPARK_Mode is
   use type AML_Profile_Events.Event, AML_Profile_Events.Logical_Table_ID;
   Capacity : constant Positive := 8;
   subtype Count is Natural range 0 .. Capacity;
   subtype Position is Positive range 1 .. Capacity;
   type Observation is record
      Syntax : AML_Profile_Events.Event;
      Batch_Table : AML_Profile_Events.Logical_Table_ID := AML_Profile_Events.Unknown_Table;
   end record;
   type Context is private;
   function Length (Item : Context) return Count;
   function Batch (Item : Context) return AML_Profile_Events.Logical_Table_ID;
   function Read (Item : Context; Ordinal : Position) return Observation
     with Pre => Ordinal <= Length (Item),
       Annotate => (GNATprove, Inline_For_Proof);
   procedure Observe (Item : in out Context; Event : AML_Profile_Events.Event)
     with Post =>
       (if Event.Kind in AML_Profile_Events.Statement_Visit | AML_Profile_Events.Operand_Visit then
          Length (Item) = Count'Min (Length (Item'Old) + 1, Capacity)
          and then Read (Item, Length (Item)).Syntax = Event
          and then Read (Item, Length (Item)).Batch_Table = Batch (Item'Old)
        else Length (Item) = Length (Item'Old));
private
   type Entries is array (Position) of Observation;
   type Context is record
      Used : Count := 0;
      Next : Position := Position'First;
      Current : AML_Profile_Events.Logical_Table_ID := AML_Profile_Events.Unknown_Table;
      Items : Entries;
   end record with Type_Invariant =>
     (if Context.Used < Capacity then Context.Next = Context.Used + 1);
end AML_Dispatch_Tails;
