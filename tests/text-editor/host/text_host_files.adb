with Ada.Directories;
with Ada.Streams; use Ada.Streams;
with GNAT.OS_Lib;

package body Text_Host_Files is
   use Text_Hosts;
   use Ada.Streams.Stream_IO;

   SAVE_SUFFIX : constant String := ".text-save";

   procedure Grant (H : in out Host_Files; Root : String; Writable : Boolean; Label : String := "host") is
   begin
      if H.Root_Count < MAXIMUM_ROOTS then
         H.Root_Count := H.Root_Count + 1;
         H.Roots (H.Root_Count) := (Path => To_Unbounded_String (Root), Label => To_Unbounded_String (Label),
                                    Writable => Writable);
      end if;
   end Grant;

   procedure Offer (H : in out Host_Files; Name : String) is
   begin
      if H.Offered_Count < MAXIMUM_OFFERED then
         H.Offered_Count := H.Offered_Count + 1;
         H.Offered (H.Offered_Count) := To_Unbounded_String (Name);
      end if;
   end Offer;

   procedure Offer_Files (H : in out Host_Files; Depth : Natural; Limit : Positive) is
      procedure Walk (Folder : String; Left : Natural) is
         use Ada.Directories;
         Search : Search_Type;
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Folder, "", [Ordinary_File => True, Directory => True, others => False]);
         while More_Entries (Search) and then H.Offered_Count < Limit loop
            Get_Next_Entry (Search, Item);
            declare
               Simple : constant String := Simple_Name (Item);
            begin
               if Simple'Length > 0 and then Simple (Simple'First) /= '.' and then Simple /= "build" then
                  if Kind (Item) = Ordinary_File then
                     Offer (H, Full_Name (Item));
                  elsif Left > 0 then
                     Walk (Full_Name (Item), Left - 1);
                  end if;
               end if;
            end;
         end loop;
         End_Search (Search);
      exception
         when others => null;
      end Walk;
   begin
      for K in 1 .. H.Root_Count loop
         Walk (To_String (H.Roots (K).Path), Depth);
      end loop;
   end Offer_Files;

   procedure Set_Read_Limit (H : in out Host_Files; Bytes : Natural) is
   begin
      H.Read_Limit := Bytes;
   end Set_Read_Limit;

   function Clipboard (H : Host_Files) return String is (To_String (H.Clip));
   procedure Set_Clipboard (H : in out Host_Files; Text : String) is
   begin
      H.Clip := To_Unbounded_String (Text);
   end Set_Clipboard;

   --  The root Name lies under, 0 for none. No "." or ".." components.
   function Root_Of (H : Host_Files; Name : String) return Natural is
   begin
      for I in Name'Range loop
         if Name (I) = '.' and then (I = Name'First or else Name (I - 1) = '/')
           and then (I = Name'Last or else Name (I + 1) = '/'
                     or else (Name (I + 1) = '.' and then (I + 1 = Name'Last or else Name (I + 2) = '/')))
         then
            return 0;
         end if;
      end loop;
      for K in 1 .. H.Root_Count loop
         declare
            R : constant String := To_String (H.Roots (K).Path);
         begin
            if Name'Length > R'Length + 1 and then Name (Name'First .. Name'First + R'Length - 1) = R
              and then Name (Name'First + R'Length) = '/'
            then
               return K;
            end if;
         end;
      end loop;
      return 0;
   end Root_Of;

   procedure Start_Load (H : in out Host_Files; Name : String; Size : out Byte_Count;
                                    Status : out File_Status) is
   begin
      Size := 0;
      if Root_Of (H, Name) = 0 then
         Status := Not_Granted;
         return;
      end if;
      if Is_Open (H.Reading) then
         Close (H.Reading);
      end if;
      Open (H.Reading, In_File, Name);
      Size := Byte_Count (Ada.Streams.Stream_IO.Size (H.Reading));
      Status := Pending;
   exception
      when Name_Error =>
         Status := Not_Found;
      when others =>
         Status := Failed;
   end Start_Load;

   procedure Poll_Load (H : in out Host_Files; Into : out String; Got : out Natural;
                                   Status : out File_Status) is
      Want : constant Natural := (if H.Read_Limit = 0 then Into'Length else Natural'Min (Into'Length, H.Read_Limit));
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Want)) with Import, Address => Into'Address;
      Last : Stream_Element_Offset;
   begin
      Got := 0;
      if not Is_Open (H.Reading) then
         Status := Failed;
         return;
      end if;
      if Want > 0 then
         Read (H.Reading, Buffer, Last);
         Got := Natural (Last);
      end if;
      if End_Of_File (H.Reading) then
         Close (H.Reading);
         Status := Done;
      else
         Status := Pending;
      end if;
   exception
      when others =>
         Status := Failed;
   end Poll_Load;

   procedure Start_Save (H : in out Host_Files; Name : String; Status : out File_Status) is
      K : constant Natural := Root_Of (H, Name);
      Slash : Natural := Name'First - 1;
   begin
      if K = 0 then
         Status := Not_Granted;
         return;
      elsif not H.Roots (K).Writable then
         Status := Read_Only_Place;
         return;
      end if;
      for I in Name'Range loop
         if Name (I) = '/' then
            Slash := I;
         end if;
      end loop;
      H.Target := To_Unbounded_String (Name);
      H.Temporary := To_Unbounded_String (Name (Name'First .. Slash) & "." & Name (Slash + 1 .. Name'Last) & SAVE_SUFFIX);
      Create (H.Writing, Out_File, To_String (H.Temporary));
      Status := Done;
   exception
      when others =>
         Status := Failed;
   end Start_Save;

   procedure Save_Chunk (H : in out Host_Files; Bytes : String; Status : out File_Status) is
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Bytes'Length)) with Import, Address => Bytes'Address;
   begin
      Write (H.Writing, Buffer);
      Status := Done;
   exception
      when others =>
         Status := Failed;
   end Save_Chunk;

   procedure Finish_Save (H : in out Host_Files; Status : out File_Status) is
      Renamed : Boolean;
   begin
      Flush (H.Writing);
      Close (H.Writing);
      --  rename(2): replaces the target atomically.
      GNAT.OS_Lib.Rename_File (To_String (H.Temporary), To_String (H.Target), Renamed);
      Status := (if Renamed then Done else Failed);
   exception
      when others =>
         Status := Failed;
   end Finish_Save;

   procedure Poll_Save (H : in out Host_Files; Status : out File_Status) is
      pragma Unreferenced (H);
   begin
      Status := Done;
   end Poll_Save;

   procedure Copy (H : in out Host_Files; Text : String) is
   begin
      H.Clip := To_Unbounded_String (Text);
   end Copy;

   procedure Request_Paste (H : in out Host_Files) is
   begin
      H.Paste_Asked := True;
   end Request_Paste;

   procedure Poll_Paste
     (H : in out Host_Files; Into : out String; Length : out Natural; Ready, Truncated : out Boolean)
   is
      Text : constant String := To_String (H.Clip);
   begin
      Ready := H.Paste_Asked;
      H.Paste_Asked := False;
      Length := Natural'Min (Text'Length, Into'Length);
      Truncated := Text'Length > Into'Length;
      Into (Into'First .. Into'First + Length - 1) := Text (Text'First .. Text'First + Length - 1);
   end Poll_Paste;

   function Display_Name (H : Host_Files; Name : String) return String is
      K : constant Natural := Root_Of (H, Name);
   begin
      if K = 0 then
         return Name;
      end if;
      declare
         R : constant String := To_String (H.Roots (K).Path);
      begin
         return "@" & To_String (H.Roots (K).Label) & ":0" & Name (Name'First + R'Length .. Name'Last);
      end;
   end Display_Name;

   function Granted_Count (H : Host_Files) return Natural is (H.Offered_Count);
   function Granted_Name (H : Host_Files; Index : Positive) return String is
     (To_String (H.Offered (Index)));
end Text_Host_Files;
