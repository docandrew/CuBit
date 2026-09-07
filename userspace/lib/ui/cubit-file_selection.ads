--  Bounded file-selection data shared by dialogs and platform adapters.
--  Names are data, never authority. No paths, enumeration, or I/O here.
package CuBit.File_Selection with SPARK_Mode is
   Maximum_Name_Length : constant := 64;
   Maximum_Files : constant := 64;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Length;
   subtype File_Count is Natural range 0 .. Maximum_Files;
   type File_Name is record
      Text : String (1 .. Maximum_Name_Length) := [others => ' '];
      Length : Name_Length := 0;
   end record;
   type Name_Array is array (Positive range 1 .. Maximum_Files) of File_Name;
   type File_List is record
      Names : Name_Array := [others => <>];
      Count : File_Count := 0;
   end record;
   function Valid_Leaf (Name : String) return Boolean;
   function Value (Name : File_Name) return String is
     (Name.Text (1 .. Name.Length));
   procedure Set (Name : out File_Name; Text : String; Accepted : out Boolean);
   procedure Append
     (Files : in out File_List; Name : String; Accepted : out Boolean);
end CuBit.File_Selection;
