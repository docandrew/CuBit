with Ada.Unchecked_Deallocation;
with Interfaces;

package body CCL.Objects.Holders with SPARK_Mode => Off is
   --  The header's counters' operators, named (the native runtime's
   --  visibility of the parent's use clause differs from the host's).
   function "=" (Left, Right : Standard.Interfaces.Unsigned_32) return Boolean
     renames Standard.Interfaces."=";
   function ">" (Left, Right : Standard.Interfaces.Unsigned_32) return Boolean
     renames Standard.Interfaces.">";
   procedure Free is new Ada.Unchecked_Deallocation (Sized_Image, Image_Access);

   --  The rooms a builder starts with: a page's.
   FIRST_CELLS : constant := Maximum_Cells;
   FIRST_BYTES : constant := Maximum_Text_Bytes;

   --  Object's used part in storage of Cell_Room cells and Text_Room bytes.
   function Copy (Object : Sized_Image; Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count)
     return Image_Access
   is
      Cells : constant Natural := Natural'Min (Cells_Held (Object), Object.Cell_Room);
      Bytes : constant Natural := Natural'Min (Bytes_Held (Object), Object.Text_Room);
      Result : constant Image_Access := new Sized_Image (Cell_Room, Text_Room);
   begin
      Result.Schema := Object.Schema;
      Result.Version := Object.Version;
      Result.Used_Cells := Standard.Interfaces.Unsigned_32 (Natural'Min (Cells, Cell_Room));
      Result.Used_Bytes := Standard.Interfaces.Unsigned_32 (Natural'Min (Bytes, Text_Room));
      Result.Reserved := Object.Reserved;
      Result.Cells (1 .. Natural (Result.Used_Cells)) := Object.Cells (1 .. Natural (Result.Used_Cells));
      Result.Text (1 .. Natural (Result.Used_Bytes)) := Object.Text (1 .. Natural (Result.Used_Bytes));
      return Result;
   end Copy;

   function Is_Empty (Item : Holder) return Boolean is (Item.Object = null);

   function No_Value return Holder is (Ada.Finalization.Controlled with Object => null);

   function Hold (Object : Sized_Image) return Holder is
     (Ada.Finalization.Controlled with
      Object => Copy (Object, Natural'Min (Cells_Held (Object), Object.Cell_Room),
                      Natural'Min (Bytes_Held (Object), Object.Text_Room)));

   function Hold (Object : Image) return Holder is (Hold (To_Sized (Object)));

   function Cells_Of (Item : Holder) return Cell_Room_Count is
     (if Item.Object = null then 0 else Natural'Min (Cells_Held (Item.Object.all), Item.Object.Cell_Room));
   function Bytes_Of (Item : Holder) return Text_Room_Count is
     (if Item.Object = null then 0 else Natural'Min (Bytes_Held (Item.Object.all), Item.Object.Text_Room));

   function Cell_At (Item : Holder; Index : Positive) return Cell is
     (if Index <= Cells_Of (Item) then Item.Object.Cells (Index) else Unit_Cell);

   function Content (Item : Holder) return Sized_Image is
   begin
      if Item.Object = null then
         return (Cell_Room => 0, Text_Room => 0, others => <>);
      end if;
      return Item.Object.all;
   end Content;

   procedure Inspect (Item : Holder) is
   begin
      if Item.Object = null then
         Use_Image ((Cell_Room => 0, Text_Room => 0, others => <>));
      else
         Use_Image (Item.Object.all);
      end if;
   end Inspect;

   function Validate (Item : Holder; Contract : Binding) return Boolean is
     (Item.Object /= null and then Validate (Item.Object.all, Contract));

   function Fits_Page (Item : Holder) return Boolean is
     (Item.Object /= null and then Fits_Page (Item.Object.all));

   function Page (Item : Holder) return Image is
     (if Fits_Page (Item) then To_Page (Item.Object.all) else (others => <>));

   procedure Start (Item : out Holder; Contract : Binding) is
   begin
      Free (Item.Object);
      Item.Object := new Sized_Image'(Empty (Contract, FIRST_CELLS, FIRST_BYTES));
   end Start;

   --  Room for Cells more cells and Bytes more text bytes, growing within
   --  the ceilings.
   procedure Make_Room (Item : in out Holder; Cells, Bytes : Natural; Made : out Boolean) is
      Object : Image_Access renames Item.Object;
      Want_Cells, Want_Bytes : Natural;
      Grown : Image_Access;
   begin
      Made := False;
      if Object = null or else Object.Used_Cells > Standard.Interfaces.Unsigned_32 (Object.Cell_Room) or else
        Object.Used_Bytes > Standard.Interfaces.Unsigned_32 (Object.Text_Room)
      then
         return;
      end if;
      if Cells > VALUE_CELL_CEILING - Natural (Object.Used_Cells) or else
        Bytes > VALUE_TEXT_CEILING - Natural (Object.Used_Bytes)
      then
         return;
      end if;
      Want_Cells := Natural (Object.Used_Cells) + Cells;
      Want_Bytes := Natural (Object.Used_Bytes) + Bytes;
      if Want_Cells <= Object.Cell_Room and then Want_Bytes <= Object.Text_Room then
         Made := True;
         return;
      end if;
      declare
         New_Cells : Natural := Natural'Max (Object.Cell_Room, 1);
         New_Bytes : Natural := Natural'Max (Object.Text_Room, 1);
      begin
         while New_Cells < Want_Cells loop
            New_Cells := (if New_Cells > VALUE_CELL_CEILING / 2 then VALUE_CELL_CEILING else 2 * New_Cells);
         end loop;
         while New_Bytes < Want_Bytes loop
            New_Bytes := (if New_Bytes > VALUE_TEXT_CEILING / 2 then VALUE_TEXT_CEILING else 2 * New_Bytes);
         end loop;
         Grown := Copy (Object.all, New_Cells, New_Bytes);
      end;
      Free (Object);
      Object := Grown;
      Made := True;
   end Make_Room;

   procedure Append (Item : in out Holder; Value : Cell; Result : out Build_Result) is
      Made : Boolean;
   begin
      if Item.Object = null then
         Result := Invalid_Image;
         return;
      end if;
      Make_Room (Item, 1, 0, Made);
      if not Made then
         Result := (if Item.Object.Version /= Format_Version then Invalid_Image else Full);
         return;
      end if;
      Append (Item.Object.all, Value, Result);
   end Append;

   procedure Append_Text (Item : in out Holder; Value : String; Result : out Build_Result) is
      Made : Boolean;
   begin
      if Item.Object = null then
         Result := Invalid_Image;
         return;
      end if;
      Make_Room (Item, 1, Value'Length, Made);
      if not Made then
         Result := (if Item.Object.Version /= Format_Version then Invalid_Image else Full);
         return;
      end if;
      Append_Text (Item.Object.all, Value, Result);
   end Append_Text;

   procedure Replace_Cell (Item : in out Holder; Index : Positive; Value : Cell) is
   begin
      if Index <= Cells_Of (Item) then
         Item.Object.Cells (Index) := Value;
      end if;
   end Replace_Cell;

   function "=" (Left, Right : Holder) return Boolean is
     (if Left.Object = null or else Right.Object = null then Left.Object = Right.Object
      else Cells_Of (Left) = Cells_Of (Right) and then Bytes_Of (Left) = Bytes_Of (Right) and then
        Left.Object.Schema = Right.Object.Schema and then Left.Object.Version = Right.Object.Version and then
        Left.Object.Reserved = Right.Object.Reserved and then
        Left.Object.Cells (1 .. Cells_Of (Left)) = Right.Object.Cells (1 .. Cells_Of (Right)) and then
        Left.Object.Text (1 .. Bytes_Of (Left)) = Right.Object.Text (1 .. Bytes_Of (Right)));

   overriding procedure Adjust (Item : in out Holder) is
   begin
      if Item.Object /= null then
         Item.Object := Copy (Item.Object.all, Cells_Of (Item), Bytes_Of (Item));
      end if;
   end Adjust;

   overriding procedure Finalize (Item : in out Holder) is
   begin
      Free (Item.Object);
   end Finalize;
end CCL.Objects.Holders;
