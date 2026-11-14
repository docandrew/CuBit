with Ada.Finalization;

--  A CCL value held in process memory, as large as its content: a sized
--  image on the heap (CuAlloc natively), copied whole when the holder is
--  assigned and freed with it, so a holder behaves as a value (no sharing,
--  no aliasing). A holder is built in place: Append and Append_Text grow
--  its rooms by doubling, and refuse (Full) only at the named ceilings
--  VALUE_CELL_CEILING and VALUE_TEXT_CEILING. A copy holds exactly the
--  cells and text in use.
--  The visible part is SPARK; the heap is behind the private part.
package CCL.Objects.Holders with SPARK_Mode is
   type Holder is private;

   --  Holds no value (Validate is False).
   function Is_Empty (Item : Holder) return Boolean;
   function No_Value return Holder
     with Post => Is_Empty (No_Value'Result);

   --  The used part of Object, held.
   function Hold (Object : Image) return Holder;
   function Hold (Object : Sized_Image) return Holder;

   --  The cells and the text bytes the value holds.
   function Cells_Of (Item : Holder) return Cell_Room_Count;
   function Bytes_Of (Item : Holder) return Text_Room_Count;
   --  Cell Index of the value (Unit_Cell outside the cells in use).
   function Cell_At (Item : Holder; Index : Positive) return Cell;

   --  The value, as a sized image of exactly its content (Empty of no
   --  schema and no room for an empty holder).
   function Content (Item : Holder) return Sized_Image;
   --  Use_Image gets the held image itself, not a copy.
   generic
      with procedure Use_Image (Object : Sized_Image);
   procedure Inspect (Item : Holder);

   function Validate (Item : Holder; Contract : Binding) return Boolean;
   --  The value in a page, when it fits one (an empty page of no schema,
   --  which validates against nothing, when it does not).
   function Fits_Page (Item : Holder) return Boolean;
   function Page (Item : Holder) return Image;

   --  An empty value of Contract to build; Append and Append_Text as
   --  CCL.Objects's, growing the holder until a ceiling refuses (Full).
   procedure Start (Item : out Holder; Contract : Binding);
   procedure Append (Item : in out Holder; Value : Cell; Result : out Build_Result);
   procedure Append_Text (Item : in out Holder; Value : String; Result : out Build_Result);
   --  Replace a cell already appended (a list's count, once its elements
   --  are known); nothing when Index is not in use.
   procedure Replace_Cell (Item : in out Holder; Index : Positive; Value : Cell);

   --  The same value (content, not storage).
   function "=" (Left, Right : Holder) return Boolean;
private
   pragma SPARK_Mode (Off);
   type Image_Access is access Sized_Image;
   type Holder is new Ada.Finalization.Controlled with record
      Object : Image_Access;
   end record;
   overriding procedure Adjust (Item : in out Holder);
   overriding procedure Finalize (Item : in out Holder);
end CCL.Objects.Holders;
