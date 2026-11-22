with Interfaces; use Interfaces;
package body Servo_Bookmark_Model is
   function Clean (S : String) return Boolean is
     (S'Length > 0 and then (for all C of S => C in ' ' .. '~') and then
      (for some C of S => C /= ' '));
   function Web_URL (S : String) return Boolean is
     (Clean (S) and then ((S'Length > 7 and then S (S'First .. S'First + 6) = "http://") or else
                         (S'Length > 8 and then S (S'First .. S'First + 7) = "https://")) and then
      (for all C of S => C /= ' '));
   function Title (Data : Store; Item : ID) return String is
     (if Item = 0 then "Bookmarks" else Data (Item).Name (1 .. Data (Item).Name_Last));
   function Address (Data : Store; Item : ID) return String is
     (if Item = 0 then "" else Data (Item).URL (1 .. Data (Item).URL_Last));
   function Valid (Data : Store) return Boolean is
      P : ID;
   begin
      for I in Data'Range loop
         if Data (I).Used then
            if not Clean (Title (Data, I)) or else
              (if Data (I).Folder then Data (I).URL_Last /= 0 else not Web_URL (Address (Data, I)))
            then return False; end if;
            P := Data (I).Parent;
            for Depth in 1 .. Capacity loop
               exit when P = 0;
               if P = I or else not Data (P).Used or else not Data (P).Folder then return False; end if;
               P := Data (P).Parent;
            end loop;
            if P /= 0 then return False; end if;
         end if;
      end loop;
      return True;
   end Valid;
   function Find_URL (Data : Store; URL : String) return ID is
   begin
      for I in Data'Range loop
         if Data (I).Used and then not Data (I).Folder and then Address (Data, I) = URL then return I; end if;
      end loop;
      return 0;
   end Find_URL;
   procedure Update (Data : in out Store; Item : in out ID; Parent : ID;
                     Name, URL : String; Folder : Boolean; OK : out Boolean) is
      Candidate : Store := Data;
      Target : ID := Item;
   begin
      OK := False;
      if Name'Length > 256 or else URL'Length > 1024 then return; end if;
      if Target = 0 then
         for I in Candidate'Range loop
            if not Candidate (I).Used then Target := I; exit; end if;
         end loop;
      end if;
      if Target = 0 then return; end if;
      declare Previous : constant Icon_Pixels := Candidate (Target).Icon;
      begin
      Candidate (Target) := (Used => True, Folder => Folder, Parent => Parent, others => <>);
      if Item /= 0 and then Address (Data, Item) = URL then Candidate (Target).Icon := Previous; end if;
      end;
      Candidate (Target).Name_Last := Name'Length;
      Candidate (Target).Name (1 .. Name'Length) := Name;
      if not Folder then
         Candidate (Target).URL_Last := URL'Length;
         Candidate (Target).URL (1 .. URL'Length) := URL;
      end if;
      if not Valid (Candidate) then return; end if;
      Data := Candidate; Item := Target; OK := True;
   end Update;
   procedure Delete (Data : in out Store; Item : ID; OK : out Boolean) is
   begin
      OK := False;
      if Item = 0 or else not Data (Item).Used then return; end if;
      for E of Data loop
         if E.Used and then E.Parent = Item then return; end if;
      end loop;
      Data (Item) := (others => <>); OK := True;
   end Delete;
   procedure Encode (Data : Store; Buffer : out String; Last : out Natural) is
      procedure Byte (N : Natural) is
      begin Last := Last + 1; Buffer (Last) := Character'Val (N); end Byte;
      procedure Number (N : Natural) is
      begin Byte (N / 256); Byte (N mod 256); end Number;
      procedure Text (S : String) is
      begin Buffer (Last + 1 .. Last + S'Length) := S; Last := Last + S'Length; end Text;
   begin
      Last := 4;
      Buffer (1 .. 4) := "CBM2";
      for E of Data loop
         Byte (if not E.Used then 0 elsif E.Folder then 1 else 2);
         Byte (if E.Used then E.Parent else 0);
         Number (if E.Used then E.Name_Last else 0);
         Number (if E.Used then E.URL_Last else 0);
         for Pixel of E.Icon loop
            for Shift in reverse 0 .. 3 loop Byte (Natural (Shift_Right (Pixel, Shift * 8) and 255)); end loop;
         end loop;
         if E.Used then Text (E.Name (1 .. E.Name_Last)); Text (E.URL (1 .. E.URL_Last)); end if;
      end loop;
   end Encode;
   procedure Decode (Text : String; Data : out Store; OK : out Boolean) is
      Candidate : Store := [others => (others => <>)];
      At_Byte : Integer := Text'First + 4;
      Kind, Parent, N, U : Natural;
      function Byte return Natural is
         Value : Natural := Character'Pos (Text (At_Byte));
      begin At_Byte := At_Byte + 1; return Value; end Byte;
      function Number return Natural is
         Hi : constant Natural := Byte;
         Lo : constant Natural := Byte;
      begin return Hi * 256 + Lo; end Number;
   begin
      Data := [others => (others => <>)]; OK := False;
      if Text'Length < 4 or else Text'Length > Max_Encoded or else Text (Text'First .. Text'First + 3) /= "CBM2" then return; end if;
      for I in Candidate'Range loop
         if At_Byte + 5 > Text'Last then return; end if;
         Kind := Byte; Parent := Byte; N := Number; U := Number;
         if Kind > 2 or Parent > Capacity or N > 256 or U > 1024 or At_Byte + 1024 + N + U - 1 > Text'Last then return; end if;
         if Kind = 0 and then (Parent /= 0 or N /= 0 or U /= 0) then return; end if;
         Candidate (I).Used := Kind /= 0; Candidate (I).Folder := Kind = 1;
         Candidate (I).Parent := Parent;
         for Pixel of Candidate (I).Icon loop
            Pixel := 0;
            for Part in 1 .. 4 loop Pixel := Shift_Left (Pixel, 8) or Unsigned_32 (Byte); end loop;
         end loop;
         Candidate (I).Name_Last := N; Candidate (I).URL_Last := U;
         Candidate (I).Name (1 .. N) := Text (At_Byte .. At_Byte + N - 1); At_Byte := At_Byte + N;
         Candidate (I).URL (1 .. U) := Text (At_Byte .. At_Byte + U - 1); At_Byte := At_Byte + U;
      end loop;
      if At_Byte /= Text'Last + 1 or else not Valid (Candidate) then return; end if;
      Data := Candidate; OK := True;
   end Decode;
end Servo_Bookmark_Model;
