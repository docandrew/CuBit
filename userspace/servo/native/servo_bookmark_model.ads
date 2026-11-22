with Interfaces;
package Servo_Bookmark_Model is
   type Icon_Pixels is array (1 .. 256) of Interfaces.Unsigned_32;
   Capacity : constant := 64;
   subtype ID is Natural range 0 .. Capacity;
   type Bookmark is record
      Used, Folder : Boolean := False;
      Parent : ID := 0;
      Icon : Icon_Pixels := [others => 0];
      Name : String (1 .. 256) := [others => ' '];
      Name_Last : Natural range 0 .. 256 := 0;
      URL : String (1 .. 1024) := [others => ' '];
      URL_Last : Natural range 0 .. 1024 := 0;
   end record;
   type Store is array (1 .. Capacity) of Bookmark;
   Max_Encoded : constant := 4 + Capacity * (6 + 256 + 1024 + 1024);
   function Valid (Data : Store) return Boolean;
   function Title (Data : Store; Item : ID) return String;
   function Address (Data : Store; Item : ID) return String;
   function Find_URL (Data : Store; URL : String) return ID;
   procedure Update (Data : in out Store; Item : in out ID; Parent : ID;
                     Name, URL : String; Folder : Boolean; OK : out Boolean);
   procedure Delete (Data : in out Store; Item : ID; OK : out Boolean);
   procedure Encode (Data : Store; Buffer : out String; Last : out Natural)
     with Pre => Buffer'First = 1 and Buffer'Length >= Max_Encoded;
   procedure Decode (Text : String; Data : out Store; OK : out Boolean);
end Servo_Bookmark_Model;
