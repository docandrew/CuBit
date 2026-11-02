with CuBit.Appearance;
with CuBit.QOI;
--  The wallpapers the desktop offers (docs/assets.md): the catalog of the
--  wallpaper package, Assets/cubit-wallpapers/<version>/catalog.ccl. Each
--  line is one CCL Wallpaper (CCL.Interfaces.Wallpapers) in its canonical
--  source, read with the decoder generated from that type
--  (CCL.Asset_Codecs.Wallpaper): the build checked it with the CCL type
--  system, and nothing here reads keywords. The first wallpaper is the
--  default. The entries live in a CuAlloc table sized to the catalog (no
--  fixed count); a name, label and file name each have a named ceiling.
--
--  A catalog that is malformed (a line that is not a canonical Wallpaper),
--  names a wallpaper twice, spells a name or file the package could not
--  hold, or does not fit in memory loads nothing: all or none. Settings then
--  offers only the solid colours and the backdrop is a flat colour.
--
--  One source today: the package on the system volume. A user wallpaper
--  directory would be a second catalog of the same type read the same way;
--  its entries would join this table after the package's (keeping the
--  package's first entry the default), with its own file root.
package Desktop_Wallpaper_Catalog is
   package A renames CuBit.Appearance;

   File_Extension : constant String := ".qoi";
   --  A label Settings can show on one row (CCL.Interfaces.Wallpapers).
   Maximum_Label : constant := 48;
   --  A file is a name with the extension: a plain file in the package.
   Maximum_File : constant := A.Maximum_Image_Name + File_Extension'Length;
   --  The longest catalog file read (a named ceiling on the read, not on
   --  the count: each canonical line is at most a typed value's budget).
   Maximum_Catalog_Bytes : constant := 4 * 1024 * 1024;

   subtype Wallpaper_Count is Natural;
   subtype Wallpaper_Index is Positive;
   No_Wallpaper : constant Wallpaper_Count := 0;
   Default_Wallpaper : constant Wallpaper_Index := 1;

   type Load_Status is
     (Loaded,          --  every line was a valid, distinct wallpaper
      Read_Failed,     --  the file could not be read whole
      Malformed,       --  a line is not a canonical Wallpaper (or is empty)
      Bad_Name,        --  a name is not a catalog name (A.Valid_Image_Name)
      Bad_Label,       --  a label is empty, too long or not printable
      Bad_File,        --  a file is not <name>.qoi
      No_Attribution,  --  an entry says nothing of where its picture came from
      Duplicate_Name,  --  two entries share a name
      Empty,           --  no wallpaper at all
      Out_Of_Memory);  --  the table could not grow
   type Load_Result is record
      Status : Load_Status := Empty;
      Line : Natural := 0;        --  the offending line, from 1 (0: none)
      Count : Wallpaper_Count := 0;
   end record;
   --  A reason for a log line, e.g. "line 3: a name is declared twice".
   function Describe (Result : Load_Result) return String;

   --  Read a catalog in pieces of any size. The catalog in use is replaced
   --  only when End_Load finds every line valid.
   procedure Begin_Load;
   procedure Feed (Input : CuBit.QOI.Byte_Array);
   --  False once a line was rejected: the rest need not be read.
   function Wants_More return Boolean;
   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result);
   --  The same with the whole text at once.
   procedure Load (Source : String; Result : out Load_Result);

   function Count return Wallpaper_Count;
   function Name (Index : Wallpaper_Index) return A.Image_Name
     with Pre => Index <= Count;
   function Label (Index : Wallpaper_Index) return String
     with Pre => Index <= Count;
   function File (Index : Wallpaper_Index) return String
     with Pre => Index <= Count;
   --  The wallpaper called Item, or No_Wallpaper.
   function Find (Item : A.Image_Name) return Wallpaper_Count
     with Post => Find'Result <= Count;
end Desktop_Wallpaper_Catalog;
