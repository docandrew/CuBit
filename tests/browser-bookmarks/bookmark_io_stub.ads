with Servo_Bookmark_Model;
with System; with Interfaces; use Interfaces;
package Bookmark_IO_Stub is
   Fail_Save : Boolean := False;
   Saves : Natural := 0;
   Saved : Servo_Bookmark_Model.Store;
   function Icon (URL : System.Address; Length : Unsigned_32; Pixels : System.Address) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_bookmark_icon";
   function Load (Data : System.Address; Length : Unsigned_32) return Integer_32
     with Export, Convention => C, External_Name => "cubit_bookmarks_load";
   function Save (Data : System.Address; Length : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_bookmarks_save";
end Bookmark_IO_Stub;
