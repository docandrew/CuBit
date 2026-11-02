with System.Storage_Elements;
with Interfaces;
with CCL.Asset_Codecs.Wallpaper;
with CuBit.Canonical_Readers;
with Desktop_Heap_Objects;

package body Desktop_Wallpaper_Catalog is
   package Codec renames CCL.Asset_Codecs.Wallpaper;
   package R renames CuBit.Canonical_Readers;
   use type System.Address;
   use type System.Storage_Elements.Storage_Count;
   use type A.Image_Name;

   LF : constant Character := Character'Val (10);

   type Wallpaper is record
      Name : A.Image_Name;
      Label_Length : Natural range 0 .. Maximum_Label := 0;
      Label : String (1 .. Maximum_Label) := [others => ' '];
      File_Length : Natural range 0 .. Maximum_File := 0;
      File : String (1 .. Maximum_File) := [others => ' '];
   end record;
   type Table is array (Wallpaper_Index range <>) of Wallpaper;
   package Heap is new Desktop_Heap_Objects (Wallpaper);

   --  A table in one CuAlloc block: Capacity entries, the first Used valid.
   type Store is record
      Base : System.Address := System.Null_Address;
      Capacity : Wallpaper_Count := 0;
      Used : Wallpaper_Count := 0;
   end record;
   --  The first table to hold anything, then doubling.
   Initial_Capacity : constant := 4;

   Current_Table : Store;   --  the catalog in use
   Staged : Store;          --  the catalog being read

   --  The line being read; one longer than a canonical value marks it
   --  overlong (a line is never longer than a typed value's budget).
   Line : String (1 .. R.Maximum_Canonical + 1);
   Line_Length : Natural range 0 .. Line'Last := 0;
   Line_Number : Natural := 0;
   Verdict : Load_Result;
   Stopped : Boolean := False;

   function Element (Item : Store; Index : Wallpaper_Index) return Wallpaper is
      Items : constant Table (1 .. Item.Capacity) with Import, Address => Item.Base;
   begin
      return Items (Index);
   end Element;

   procedure Release (Item : in out Store) is
   begin
      Heap.Free (Item.Base);
      Item := (others => <>);
   end Release;

   --  Append Value to Item, growing it; False when the heap is out.
   procedure Append (Item : in out Store; Value : Wallpaper; Done : out Boolean) is
   begin
      Done := False;
      if Item.Used = Item.Capacity then
         if Item.Capacity > Wallpaper_Count'Last / 2 then
            return;
         end if;
         declare
            Grown : constant Wallpaper_Count := Natural'Max (Initial_Capacity, 2 * Item.Capacity);
            Where : constant System.Address :=
              Heap.Allocate (System.Storage_Elements.Storage_Count (Grown) * Wallpaper'Max_Size_In_Storage_Elements);
         begin
            if Where = System.Null_Address then
               return;
            end if;
            declare
               Items : Table (1 .. Grown) with Import, Address => Where;
            begin
               for Index in 1 .. Item.Used loop
                  Items (Index) := Element (Item, Index);
               end loop;
            end;
            Heap.Free (Item.Base);
            Item.Base := Where;
            Item.Capacity := Grown;
         end;
      end if;
      declare
         Items : Table (1 .. Item.Capacity) with Import, Address => Item.Base;
      begin
         Item.Used := Item.Used + 1;
         Items (Item.Used) := Value;
      end;
      Done := True;
   end Append;

   function Printable (Text : String) return Boolean is
     (for all C of Text => C in ' ' .. '~');

   procedure Reject (Status : Load_Status) is
   begin
      if not Stopped then
         Stopped := True;
         Verdict := (Status => Status, Line => Line_Number, Count => 0);
      end if;
   end Reject;

   --  One complete line: a canonical Wallpaper with valid fields.
   procedure Take_Line is
      Item : Codec.Wallpaper_Value;
      Decoded : Boolean;
      Entry_Value : Wallpaper;
      Added : Boolean;
   begin
      Line_Number := Line_Number + 1;
      if Line_Length > R.Maximum_Canonical or else Line_Length = 0 then
         Reject (Malformed);
         return;
      end if;
      Codec.Decode (Line (1 .. Line_Length), Item, Decoded);
      if not Decoded then
         Reject (Malformed);
         return;
      end if;
      declare
         Name : constant String := R.Image (Item.name);
         Label : constant String := R.Image (Item.label);
         File : constant String := R.Image (Item.file);
      begin
         if not A.Valid_Image_Name (Name) then
            Reject (Bad_Name);
         elsif Label'Length not in 1 .. Maximum_Label or else not Printable (Label) then
            Reject (Bad_Label);
         elsif File /= Name & File_Extension then
            Reject (Bad_File);
         elsif R.Image (Item.attribution)'Length = 0 then
            Reject (No_Attribution);
         else
            Entry_Value.Name := A.To_Image_Name (Name);
            for Index in 1 .. Staged.Used loop
               if Element (Staged, Index).Name = Entry_Value.Name then
                  Reject (Duplicate_Name);
                  return;
               end if;
            end loop;
            Entry_Value.Label_Length := Label'Length;
            Entry_Value.Label (1 .. Label'Length) := Label;
            Entry_Value.File_Length := File'Length;
            Entry_Value.File (1 .. File'Length) := File;
            Append (Staged, Entry_Value, Added);
            if not Added then
               Reject (Out_Of_Memory);
            end if;
         end if;
      end;
   end Take_Line;

   procedure Begin_Load is
   begin
      Release (Staged);
      Line_Length := 0;
      Line_Number := 0;
      Stopped := False;
      Verdict := (others => <>);
   end Begin_Load;

   procedure Feed (Input : CuBit.QOI.Byte_Array) is
   begin
      for Byte of Input loop
         exit when Stopped;
         if Character'Val (Byte) = LF then
            Take_Line;
            Line_Length := 0;
         elsif Line_Length < Line'Last then
            Line_Length := Line_Length + 1;
            Line (Line_Length) := Character'Val (Byte);
         end if;
      end loop;
   end Feed;

   function Wants_More return Boolean is (not Stopped);

   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result) is
   begin
      if Read_Failed then
         Reject (Desktop_Wallpaper_Catalog.Read_Failed);
      elsif Line_Length > 0 then
         --  A last line without its line feed.
         Take_Line;
         Line_Length := 0;
      end if;
      if not Stopped and then Staged.Used = 0 then
         Reject (Empty);
      end if;
      if Stopped then
         Release (Staged);
      else
         Release (Current_Table);
         Current_Table := Staged;
         Staged := (others => <>);
         Verdict := (Status => Loaded, Line => 0, Count => Current_Table.Used);
      end if;
      Result := Verdict;
   end End_Load;

   procedure Load (Source : String; Result : out Load_Result) is
      Bytes : CuBit.QOI.Byte_Array (1 .. Source'Length);
   begin
      for Index in Bytes'Range loop
         Bytes (Index) := Interfaces.Unsigned_8 (Character'Pos (Source (Source'First + Index - 1)));
      end loop;
      Begin_Load;
      Feed (Bytes);
      End_Load (False, Result);
   end Load;

   function Describe (Result : Load_Result) return String is
     ((if Result.Line > 0 then "line" & Result.Line'Image & ": " else "") &
      (case Result.Status is
         when Loaded => "loaded" & Result.Count'Image & " wallpaper(s)",
         when Read_Failed => "not read whole",
         when Malformed => "not a canonical Wallpaper",
         when Bad_Name => "the name is not a-z, 0-9 and single '-' (at most" &
                          A.Maximum_Image_Name'Image & ")",
         when Bad_Label => "the label is not 1 to" & Maximum_Label'Image & " printable characters",
         when Bad_File => "the file is not <name>" & File_Extension,
         when No_Attribution => "the attribution is empty",
         when Duplicate_Name => "a name is declared twice",
         when Empty => "no wallpaper",
         when Out_Of_Memory => "no memory for the catalog"));

   function Count return Wallpaper_Count is (Current_Table.Used);

   function Name (Index : Wallpaper_Index) return A.Image_Name is
     (Element (Current_Table, Index).Name);

   function Label (Index : Wallpaper_Index) return String is
      Item : constant Wallpaper := Element (Current_Table, Index);
   begin
      return Item.Label (1 .. Item.Label_Length);
   end Label;

   function File (Index : Wallpaper_Index) return String is
      Item : constant Wallpaper := Element (Current_Table, Index);
   begin
      return Item.File (1 .. Item.File_Length);
   end File;

   function Find (Item : A.Image_Name) return Wallpaper_Count is
   begin
      for Index in 1 .. Current_Table.Used loop
         if Element (Current_Table, Index).Name = Item then
            return Index;
         end if;
      end loop;
      return No_Wallpaper;
   end Find;
end Desktop_Wallpaper_Catalog;
