with Ada.Text_IO; use Ada.Text_IO;
with Servo_Bookmark_Model; use Servo_Bookmark_Model;
procedure Bookmark_Tests is
   S, T, Before : Store := [others => (others => <>)];
   Folder, Child, Item : ID := 0; OK : Boolean;
   function Encoded (S : Store) return String is
      Buffer : String (1 .. Max_Encoded); Last : Natural;
   begin Encode (S, Buffer, Last); return Buffer (1 .. Last); end Encoded;
begin
   Update (S, Folder, 0, "Work", "", True, OK); pragma Assert (OK);
   Update (S, Child, Folder, "CuBit", "https://example.com/", False, OK); pragma Assert (OK);
   Before := S;
   Update (S, Folder, Folder, "Cycle", "", True, OK); pragma Assert (not OK and S = Before);
   Delete (S, Folder, OK); pragma Assert (not OK and S = Before);
   Update (S, Child, 0, "Moved", "https://example.com/a", False, OK); pragma Assert (OK);
   Delete (S, Folder, OK); pragma Assert (OK);
   declare Text : constant String := Encoded (S); begin
      Decode (Text, T, OK); pragma Assert (OK and T = S);
      for N in 0 .. Text'Length - 1 loop Decode (Text (1 .. N), T, OK); pragma Assert (not OK); end loop;
      Decode (Text & "extra", T, OK); pragma Assert (not OK);
   end;
   for N in 1 .. Capacity - 1 loop
      Item := 0; Update (S, Item, 0, "Page" & N'Image, "https://example.com/", False, OK); pragma Assert (OK);
   end loop;
   Before := S; Item := 0; Update (S, Item, 0, "Full", "https://example.com/", False, OK);
   pragma Assert (not OK and S = Before);
   Delete (S, Child, OK); pragma Assert (OK);
   Item := 0; Update (S, Item, 0, "Bad", "javascript:alert(1)", False, OK); pragma Assert (not OK);
   Put_Line ("PASS bookmarks: move, cycles, nonempty deletion, bounded capacity, strict decode/truncation, invalid URLs");
end Bookmark_Tests;
