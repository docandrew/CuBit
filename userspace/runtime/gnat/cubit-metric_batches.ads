pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Records;

--  Producer-side batching for metric records. Two caller-owned pages
--  alternate: one fills while the other is submitted. Append never waits,
--  allocates or performs IPC; when no page can take a record it is dropped
--  and counted. The caller seals a page, submits it asynchronously and
--  reports its completion. Calls are serialized by the producer's loop.
package CuBit.Metric_Batches with Pure, SPARK_Mode is
   package Records renames CuBit.Metric_Records;
   use type Records.Slot_Words;
   use type Records.Page_Words;

   type Page_Id is range 1 .. 2;
   --  Page aligned so a native adapter can grant each page directly.
   Page_Alignment : constant := Records.Page_Bytes;
   type Page_Pair is array (Page_Id) of Records.Page_Words
     with Alignment => Page_Alignment;

   type Builder is private;
   function Filling (Item : Builder) return Page_Id;
   function Used (Item : Builder; Page : Page_Id) return Records.Record_Count;
   function In_Flight (Item : Builder; Page : Page_Id) return Boolean;
   function Dropped (Item : Builder) return Unsigned_64;
   --  Batch sequences never wrap; an exhausted builder seals nothing more.
   function Exhausted (Item : Builder) return Boolean;

   --  True when Append would accept a record now.
   function Has_Room (Item : Builder) return Boolean is
     (not In_Flight (Item, Filling (Item)) and then
      Used (Item, Filling (Item)) < Records.Maximum_Records);

   function Other (Page : Page_Id) return Page_Id is
     (if Page = Page_Id'First then Page_Id'Last else Page_Id'First);

   --  The filling page is in flight only while both pages are; idle pages
   --  other than the filling page are empty; in-flight pages are nonempty.
   function Consistent (Item : Builder) return Boolean is
     ((not In_Flight (Item, Filling (Item)) or else
         In_Flight (Item, Other (Filling (Item))))
      and then
        (for all P in Page_Id =>
           (if In_Flight (Item, P) then Used (Item, P) > 0
            elsif P /= Filling (Item) then Used (Item, P) = 0)));

   function Saturating_Increment (Value : Unsigned_64) return Unsigned_64 is
     (if Value = Unsigned_64'Last then Value else Value + 1);

   procedure Append
     (Item : in out Builder; Pages : in out Page_Pair;
      Value : Records.Metric_Record; Accepted : out Boolean)
     with Pre => Consistent (Item) and then Records.Valid (Value),
          Post => Consistent (Item) and then
            Filling (Item) = Filling (Item'Old) and then
            Accepted = Has_Room (Item'Old) and then
            (if Accepted then
               Used (Item, Filling (Item)) =
                 Used (Item'Old, Filling (Item)) + 1 and then
               Dropped (Item) = Dropped (Item'Old) and then
               Records.Slot (Pages (Filling (Item)),
                             Used (Item, Filling (Item))) =
                 Records.Encode (Value)
             else
               Used (Item, Filling (Item)) =
                 Used (Item'Old, Filling (Item)) and then
               Dropped (Item) = Saturating_Increment (Dropped (Item'Old)))
            and then
            Used (Item, Other (Filling (Item))) =
              Used (Item'Old, Other (Filling (Item)))
            and then
            (for all P in Page_Id =>
               In_Flight (Item, P) = In_Flight (Item'Old, P))
            and then
            Pages (Other (Filling (Item))) =
              Pages'Old (Other (Filling (Item)));

   --  Seals the filling page when it holds records and is not in flight.
   --  Page then stays untouched until Complete (Page).
   procedure Seal
     (Item : in out Builder; Pages : in out Page_Pair; Sealed : out Boolean;
      Page : out Page_Id; Bytes : out Unsigned_64)
     with Pre => Consistent (Item),
          Post => Consistent (Item) and then
            Sealed =
              (not In_Flight (Item'Old, Filling (Item'Old)) and then
               Used (Item'Old, Filling (Item'Old)) > 0 and then
               not Exhausted (Item'Old))
            and then
            (if Sealed then
               Page = Filling (Item'Old) and then In_Flight (Item, Page)
               and then Used (Item, Page) = Used (Item'Old, Page)
               and then Bytes = Records.Batch_Bytes (Used (Item, Page))
               and then Filling (Item) = Other (Page)
             else Item = Item'Old and then Pages = Pages'Old);

   procedure Complete (Item : in out Builder; Page : Page_Id)
     with Pre => Consistent (Item) and then In_Flight (Item, Page),
          Post => Consistent (Item) and then not In_Flight (Item, Page)
            and then Used (Item, Page) = 0 and then
            In_Flight (Item, Other (Page)) =
              In_Flight (Item'Old, Other (Page)) and then
            Dropped (Item) = Dropped (Item'Old);
private
   type Page_Counts is array (Page_Id) of Records.Record_Count;
   type Page_Flags is array (Page_Id) of Boolean;
   type Builder is record
      Fill : Page_Id := Page_Id'First;
      Counts : Page_Counts := [others => 0];
      Busy : Page_Flags := [others => False];
      Next : Unsigned_64 := Records.Batch_Sequence'First;
      Loss : Unsigned_64 := 0;
   end record;
   function Filling (Item : Builder) return Page_Id is (Item.Fill);
   function Used (Item : Builder; Page : Page_Id) return Records.Record_Count
     is (Item.Counts (Page));
   function In_Flight (Item : Builder; Page : Page_Id) return Boolean is
     (Item.Busy (Page));
   function Dropped (Item : Builder) return Unsigned_64 is (Item.Loss);
   function Exhausted (Item : Builder) return Boolean is
     (Item.Next not in Records.Batch_Sequence);
end CuBit.Metric_Batches;
