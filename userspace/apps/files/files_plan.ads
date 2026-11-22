with Interfaces; use Interfaces;
with Files_Limits; use Files_Limits;
with Files_Listing;

--  What an operation (copy, move, delete) will touch, in order
--  (docs/files-app.md, "Operations"): every folder before what it holds,
--  so copying makes folders first and deleting walks the plan backwards.
--  Paths are relative to the folder the sources sit in. The totals are
--  kept as items are added, so progress never scans. Also the small
--  policies around it: conflict names ("name (2).ext"), chunk lengths and
--  percentages. Pure policy, proved (tests/files-app/files_proof.gpr).
package Files_Plan with SPARK_Mode is
   type Item_Kind is (Folder_Item, File_Item);
   MAXIMUM_PATH : constant := 4_096;
   subtype Path_Length is Natural range 0 .. MAXIMUM_PATH;

   type Item is record
      Kind : Item_Kind := File_Item;
      First : Arena_Index := 1;
      Length : Path_Length := 0;
      Size : Unsigned_64 := 0;
   end record;
   type Item_Table is array (Entry_Id range <>) of Item;

   type Plan (Capacity : Entry_Capacity; Path_Bytes : Arena_Capacity) is record
      Count : Entry_Count := 0;
      Used : Arena_Count := 0;
      Files, Folders : Entry_Count := 0;
      Bytes : Unsigned_64 := 0;
      Items : Item_Table (1 .. Capacity);
      Paths : Files_Listing.Arena (1 .. Path_Bytes);
   end record;

   function Valid (P : Plan) return Boolean is
     (P.Count <= P.Capacity and then P.Used <= P.Path_Bytes and then P.Files <= P.Count
      and then P.Folders <= P.Count);
   function Has_Room (P : Plan; Length : Path_Length) return Boolean is
     (Valid (P) and then P.Count < P.Capacity and then Length <= P.Path_Bytes - P.Used);

   procedure Clear (P : in out Plan)
     with Post => Valid (P) and then P.Count = 0 and then P.Bytes = 0;
   procedure Add (P : in out Plan; Kind : Item_Kind; Relative : String; Size : Unsigned_64)
     with Pre => Relative'Length in 1 .. MAXIMUM_PATH and then Has_Room (P, Relative'Length),
          Post => Valid (P) and then P.Count = P.Count'Old + 1;
   function Relative (P : Plan; Index : Entry_Id) return String
     with Post => Relative'Result'First = 1 and then Relative'Result'Length <= MAXIMUM_PATH;
   function Kind (P : Plan; Index : Entry_Id) return Item_Kind is
     (if Index <= P.Count and then Index <= P.Capacity then P.Items (Index).Kind else File_Item);
   function Size (P : Plan; Index : Entry_Id) return Unsigned_64 is
     (if Index <= P.Count and then Index <= P.Capacity then P.Items (Index).Size else 0);

   --  The Attempt'th name tried when Name exists: "a.txt", then
   --  "a (2).txt", "a (3).txt"; the extension (after the last dot that is
   --  not the first character) stays last. Never longer than a component.
   MAXIMUM_ATTEMPT : constant := 999;
   subtype Attempt_Number is Positive range 1 .. MAXIMUM_ATTEMPT;
   function Conflict_Name (Name : String; Attempt : Attempt_Number) return String
     with Pre => Name'Length in 1 .. MAXIMUM_NAME_BYTES and then Name'First = 1,
          Post => Conflict_Name'Result'Length in 1 .. MAXIMUM_NAME_BYTES and then Conflict_Name'Result'First = 1;

   --  The next transfer: at most Chunk bytes, none past Size.
   function Next_Chunk (Size, Done : Unsigned_64; Chunk : Positive) return Natural is
     (if Done >= Size then 0 else Natural (Unsigned_64'Min (Size - Done, Unsigned_64 (Chunk))));
   --  Done of Total as 0 .. 100 (100 for nothing to do).
   subtype Percentage is Natural range 0 .. 100;
   function Percent (Done, Total : Unsigned_64) return Percentage is
     (if Total = 0 or else Done >= Total then 100
      elsif Total <= Unsigned_64'Last / 100 then Percentage (Unsigned_64'Min (Done * 100 / Total, 100))
      else Percentage (Unsigned_64'Min (Done / (Total / 100 + 1), 100)));
end Files_Plan;
