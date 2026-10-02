with Servo_Bookmark_Model;
package body Bookmark_IO_Stub is
   function Icon (URL : System.Address; Length : Unsigned_32; Pixels : System.Address) return Unsigned_32 is (0);
   function Load (Data : System.Address; Length : Unsigned_32) return Integer_32 is
      S : Servo_Bookmark_Model.Store := [others => (others => <>)];
      Folder, Item : Servo_Bookmark_Model.ID := 0; OK : Boolean;
   begin
      Servo_Bookmark_Model.Update (S, Folder, 0, "Development", "", True, OK);
      Servo_Bookmark_Model.Update (S, Item, Folder, "Servo", "https://servo.org/", False, OK);
      Item := 0; Servo_Bookmark_Model.Update (S, Item, Folder, "CuBit documentation", "https://example.com/cubit", False, OK);
      declare Text : String (1 .. Servo_Bookmark_Model.Max_Encoded); Last : Natural;
         Buffer : String (1 .. Natural (Length)) with Import, Address => Data;
      begin Servo_Bookmark_Model.Encode (S, Text, Last); Buffer (1 .. Last) := Text (1 .. Last); return Integer_32 (Last); end;
   end Load;
   function Save (Data : System.Address; Length : Unsigned_32) return Unsigned_32 is
      Buffer : String (1 .. Natural (Length)) with Import, Address => Data;
      OK : Boolean;
   begin
      if Fail_Save then return 0; end if;
      Servo_Bookmark_Model.Decode (Buffer, Saved, OK);
      if not OK then return 0; end if;
      Saves := Saves + 1; return 1;
   end Save;
end Bookmark_IO_Stub;
