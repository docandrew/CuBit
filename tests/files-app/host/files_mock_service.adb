with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Filesystem_Queues;
with CuBit.Directory_Pages;
with CuBit.Directory_Paths;
with CuBit.Filesystem_Events;
with CuBit.Volume_Descriptions;
with GNAT.OS_Lib;

package body Files_Mock_Service is
   package FQ renames CuBit.Filesystem_Queues;
   package Q renames FQ.Queues;
   use type Q.Token;
   package FE renames CuBit.Filesystem_Events;
   package VD renames CuBit.Volume_Descriptions;

   HOST_PREFIX : constant String := "@host:0/";
   SCRATCH_PREFIX : constant String := "@scratch:0/";
   SYNTHETIC_PREFIX : constant String := "@synthetic:";
   --  Entries in a synthetic folder's subfolders.
   SYNTHETIC_CHILD_ENTRIES : constant := 100;
   --  One synthetic entry in this many is a folder.
   SYNTHETIC_FOLDER_EVERY : constant := 10;
   MAXIMUM_OPEN_DIRECTORIES : constant := 64;
   MILLISECONDS : constant := 1_000;
   SLOT_BITS : constant := 32;

   Host_Root, Scratch_Root : Unbounded_String;
   Latency : Duration := 0.0;
   Answered_Count : Natural := 0 with Volatile;

   procedure Configure (Host_Root, Scratch_Root : String) is
   begin
      Files_Mock_Service.Host_Root := To_Unbounded_String (Host_Root);
      Files_Mock_Service.Scratch_Root := To_Unbounded_String (Scratch_Root);
   end Configure;

   procedure Set_Latency (Microseconds : Natural) is
   begin
      Latency := Duration (Microseconds) / 1_000_000;
   end Set_Latency;

   function Served return Natural is (Answered_Count);

   --  One listed entry of a host directory.
   type Host_Entry is record
      Name : Unbounded_String;
      Kind : Unsigned_8;
      Size : Unsigned_64;
      Modified_Ms : Unsigned_64;
   end record;
   package Entry_Vectors is new Ada.Containers.Vectors (Positive, Host_Entry);

   type Directory_Kind is (Closed, Host_Directory, Synthetic_Directory);
   type Directory_Slot is record
      Kind : Directory_Kind := Closed;
      Entries : Entry_Vectors.Vector;
      Synthetic_Count : Natural := 0;
      Next : Positive := 1;
      Generation : Unsigned_32 := 0;
      --  The path it was opened by (what Queue_Watch watches).
      Path : Unbounded_String;
   end record;
   type Directory_Table is array (1 .. MAXIMUM_OPEN_DIRECTORIES) of Directory_Slot;

   Hook : Wake_Hook := null with Atomic;
   procedure Set_Wake_Hook (Hook : Wake_Hook) is
   begin
      Files_Mock_Service.Hook := Hook;
   end Set_Wake_Hook;

   --  The held wake request, taken once by the service when answers wait.
   protected Wake_Request is
      procedure Arm;
      procedure Take (Armed : out Boolean);
   private
      Held : Boolean := False;
   end Wake_Request;
   protected body Wake_Request is
      procedure Arm is
      begin
         Held := True;
      end Arm;
      procedure Take (Armed : out Boolean) is
      begin
         Armed := Held;
         Held := False;
      end Take;
   end Wake_Request;

   protected Signal is
      procedure Reset;
      procedure Kick;
      procedure Stop;
      entry Wait (Stopping : out Boolean);
   private
      Pending : Boolean := False;
      Stopped : Boolean := False;
   end Signal;

   protected body Signal is
      procedure Reset is
      begin
         Pending := False;
         Stopped := False;
      end Reset;
      procedure Kick is
      begin
         Pending := True;
      end Kick;
      procedure Stop is
      begin
         Stopped := True;
         Pending := True;
      end Stop;
      entry Wait (Stopping : out Boolean) when Pending is
      begin
         Pending := False;
         Stopping := Stopped;
      end Wait;
   end Signal;

   ---------------------------------------------------------------------------
   --  Change watches (Queue_Watch): folders by path, and the event ring the
   --  client reads (records as CuBit.Filesystem_Events encodes them).
   ---------------------------------------------------------------------------
   --  A path without a trailing '/' except a place's root ("@scratch:0/").
   function Normal (Path : String) return String is
      Slashes : Natural := 0;
   begin
      for C of Path loop
         if C = '/' then
            Slashes := Slashes + 1;
         end if;
      end loop;
      if Path'Length > 1 and then Path (Path'Last) = '/' and then Slashes > 1 then
         return Path (Path'First .. Path'Last - 1);
      end if;
      return Path;
   end Normal;
   --  The folder a path is in, and its last component.
   function Folder_Of (Path : String) return String is
      P : constant String := Normal (Path);
   begin
      for K in reverse P'Range loop
         if P (K) = '/' then
            return Normal (P (P'First .. K));
         end if;
      end loop;
      return "";
   end Folder_Of;
   function Leaf_Of (Path : String) return String is
      P : constant String := Normal (Path);
   begin
      for K in reverse P'Range loop
         if P (K) = '/' then
            return P (K + 1 .. P'Last);
         end if;
      end loop;
      return P;
   end Leaf_Of;

   RING_RECORDS : constant := 256;
   type Ring_Record is record
      Bytes : FE.Record_Bytes := [others => 0];
      Used : Natural := 0;
   end record;
   type Ring_Table is array (1 .. RING_RECORDS) of Ring_Record;
   type Watch_Entry is record
      Used : Boolean := False;
      Path : Unbounded_String;
   end record;
   type Watch_Table is array (FE.Watch_Number) of Watch_Entry;

   protected Board is
      procedure Enable;
      procedure Disable;
      procedure Watch (Path : String; Number : out Natural);
      procedure Unwatch (Number : Natural; Found : out Boolean);
      --  An entry of a folder changed: its watch (if any) gets a record.
      procedure Changed (Kind : FE.Event_Kind; Path : String; Cookie : Unsigned_64 := 0);
      --  A folder went away (removed or moved): its watch, and those below
      --  it, end.
      procedure Gone (Path : String);
      procedure Take (Into : out Ring_Record; Got : out Boolean);
      function Waiting return Boolean;
   private
      Enabled : Boolean := False;
      Watches : Watch_Table;
      Ring : Ring_Table;
      Head, Count : Natural := 0;
      Stamp : Unsigned_64 := 0;
   end Board;

   protected body Board is
      procedure Enable is
      begin
         Enabled := True;
      end Enable;
      procedure Disable is
      begin
         Enabled := False;
         Watches := [others => <>];
         Head := 0;
         Count := 0;
      end Disable;

      procedure Push (Item : FE.Event; Name : String) is
         Bytes : FE.Name_Bytes := [others => 0];
         Encoded : Ring_Record;
      begin
         for K in Name'Range loop
            Bytes (K - Name'First + 1) := Character'Pos (Name (K));
         end loop;
         if Count = RING_RECORDS then
            --  No room: the newest record becomes this watch's rescan.
            FE.Encode ((Watch => Item.Watch, Kind => FE.Rescan_Needed, others => <>), Bytes, 0,
                       Ring ((Head + Count - 1) mod RING_RECORDS + 1).Bytes,
                       Ring ((Head + Count - 1) mod RING_RECORDS + 1).Used);
            return;
         end if;
         FE.Encode (Item, Bytes, Name'Length, Encoded.Bytes, Encoded.Used);
         Ring ((Head + Count) mod RING_RECORDS + 1) := Encoded;
         Count := Count + 1;
      end Push;

      procedure Watch (Path : String; Number : out Natural) is
      begin
         Number := 0;
         if not Enabled then
            return;
         end if;
         for N in Watches'Range loop
            if not Watches (N).Used then
               Watches (N) := (True, To_Unbounded_String (Normal (Path)));
               Number := N;
               return;
            end if;
         end loop;
      end Watch;

      procedure Unwatch (Number : Natural; Found : out Boolean) is
      begin
         Found := Number in Watches'Range and then Watches (Number).Used;
         if Found then
            Watches (Number).Used := False;
            Push ((Watch => Number, Kind => FE.Watch_Ended, others => <>), "");
         end if;
      end Unwatch;

      procedure Changed (Kind : FE.Event_Kind; Path : String; Cookie : Unsigned_64 := 0) is
         Folder : constant String := Folder_Of (Path);
         Name : constant String := Leaf_Of (Path);
      begin
         Stamp := Stamp + 1;
         for N in Watches'Range loop
            if Watches (N).Used and then To_String (Watches (N).Path) = Folder and then Name'Length > 0 then
               Push ((Watch => N, Kind => Kind, Cookie => Cookie, Stamp => Stamp, others => <>), Name);
            end if;
         end loop;
      end Changed;

      procedure Gone (Path : String) is
         Root : constant String := Normal (Path);
      begin
         for N in Watches'Range loop
            declare
               W : constant String := To_String (Watches (N).Path);
            begin
               if Watches (N).Used
                 and then (W = Root or else (W'Length > Root'Length
                                              and then W (W'First .. W'First + Root'Length - 1) = Root
                                              and then W (W'First + Root'Length) = '/'))
               then
                  Watches (N).Used := False;
                  Push ((Watch => N, Kind => FE.Watch_Ended, others => <>), "");
               end if;
            end;
         end loop;
      end Gone;

      procedure Take (Into : out Ring_Record; Got : out Boolean) is
      begin
         Got := Count > 0;
         if Got then
            Into := Ring (Head + 1);
            Head := (Head + 1) mod RING_RECORDS;
            Count := Count - 1;
         else
            Into := (others => <>);
         end if;
      end Take;

      function Waiting return Boolean is (Count > 0);
   end Board;

   procedure Enable_Events is
   begin
      Board.Enable;
   end Enable_Events;

   procedure Take_Record (Into : out FE.Record_Bytes; Used : out Natural; Got : out Boolean) is
      Item : Ring_Record;
   begin
      Board.Take (Item, Got);
      Into := Item.Bytes;
      Used := Item.Used;
   end Take_Record;

   --  Its granted scopes (Queue_List_Scopes), as the manifest would list
   --  them: rights, then prefix.
   SCOPE_READ : constant := 1;
   SCOPE_READ_WRITE_CREATE : constant := 1 + 2 + 8;
   type Scope is record
      Prefix : Unbounded_String;
      Rights : Unsigned_8;
   end record;
   Scopes : constant array (1 .. 3) of Scope :=
     [(To_Unbounded_String (HOST_PREFIX), SCOPE_READ),
      (To_Unbounded_String (SCRATCH_PREFIX), SCOPE_READ_WRITE_CREATE),
      (To_Unbounded_String (SYNTHETIC_PREFIX & "100000/"), SCOPE_READ)];

   task type Service is
      entry Begin_Serving (Client, Server, Arena : System.Address; Arena_Bytes : Unsigned_64);
   end Service;
   type Service_Access is access Service;
   Running : Service_Access;

   procedure Start (Client, Server, Arena : System.Address; Arena_Bytes : Unsigned_64) is
   begin
      Signal.Reset;
      Running := new Service;
      Running.Begin_Serving (Client, Server, Arena, Arena_Bytes);
   end Start;

   procedure Kick is
   begin
      Signal.Kick;
   end Kick;

   procedure Arm_Wake is
   begin
      Wake_Request.Arm;
      Signal.Kick;
   end Arm_Wake;

   procedure Stop is
   begin
      Board.Disable;
      Signal.Stop;
      --  Gone before another service may start on the same signal.
      while Running /= null and then not Running'Terminated loop
         delay 0.0001;
      end loop;
   end Stop;

   --  Where a path leads: a place and the rest (no leading slash).
   type Place is (No_Place, Host_Place, Scratch_Place, Synthetic_Place);
   type Resolved is record
      Where : Place := No_Place;
      Host_Path : Unbounded_String;
      Synthetic_Count : Natural := 0;
      Synthetic_Child : Boolean := False;
   end record;

   function Resolve (Path : String) return Resolved is
      Result : Resolved;
      Rest_First : Positive;
      function Starts (Prefix : String) return Boolean is
        (Path'Length >= Prefix'Length and then Path (Path'First .. Path'First + Prefix'Length - 1) = Prefix);
   begin
      if Path'Length = 0 or else Path'Length > CuBit.Directory_Paths.Maximum_Bytes then
         return Result;
      end if;
      if Starts (HOST_PREFIX) then
         Result.Where := Host_Place;
         Rest_First := Path'First + HOST_PREFIX'Length;
      elsif Starts (SCRATCH_PREFIX) then
         Result.Where := Scratch_Place;
         Rest_First := Path'First + SCRATCH_PREFIX'Length;
      elsif Starts (SYNTHETIC_PREFIX) then
         declare
            Slash : Natural := 0;
         begin
            for K in Path'First + SYNTHETIC_PREFIX'Length .. Path'Last loop
               if Path (K) = '/' then
                  Slash := K;
                  exit;
               end if;
            end loop;
            if Slash = 0 then
               return Result;
            end if;
            Result.Synthetic_Count := Natural'Value (Path (Path'First + SYNTHETIC_PREFIX'Length .. Slash - 1));
            Result.Where := Synthetic_Place;
            Rest_First := Slash + 1;
         exception
            when Constraint_Error =>
               return (others => <>);
         end;
      else
         return Result;
      end if;
      --  Every component a valid child name.
      declare
         Start : Positive := Rest_First;
      begin
         for K in Rest_First .. Path'Last + 1 loop
            if K > Path'Last or else Path (K) = '/' then
               if K > Start and then not CuBit.Directory_Paths.Valid_Child_Name (Path (Start .. K - 1)) then
                  return (others => <>);
               end if;
               Start := K + 1;
            end if;
         end loop;
      end;
      declare
         Rest : constant String := Path (Rest_First .. Path'Last);
      begin
         case Result.Where is
            when Host_Place => Result.Host_Path := Host_Root & "/" & Rest;
            when Scratch_Place => Result.Host_Path := Scratch_Root & "/" & Rest;
            when Synthetic_Place => Result.Synthetic_Child := Rest'Length > 0;
            when No_Place => null;
         end case;
      end;
      return Result;
   end Resolve;

   function Epoch_Ms (Time : Ada.Calendar.Time) return Unsigned_64 is
      use type Ada.Calendar.Time;
      Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1970, 1, 1);
      Since : constant Duration := Duration'Max (0.0, Time - Epoch);
      Rounded : constant Unsigned_64 := Unsigned_64 (Since);
      Seconds : constant Unsigned_64 := (if Duration (Rounded) > Since then Rounded - 1 else Rounded);
   begin
      --  Whole seconds first: milliseconds since 1970 overflow Duration.
      return Seconds * MILLISECONDS + Unsigned_64 ((Since - Duration (Seconds)) * MILLISECONDS);
   exception
      when others => return 0;
   end Epoch_Ms;

   procedure List_Host (Path : String; Into : in out Entry_Vectors.Vector; Status : out Unsigned_32) is
      use Ada.Directories;
      Search : Search_Type;
      Item : Directory_Entry_Type;
   begin
      Into.Clear;
      if not Exists (Path) then
         Status := REPLY_NOT_FOUND;
         return;
      elsif Kind (Path) /= Directory then
         Status := REPLY_WRONG_OBJECT_TYPE;
         return;
      end if;
      Start_Search (Search, Path, "");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         declare
            Name : constant String := Simple_Name (Item);
         begin
            if Name /= "." and then Name /= ".." then
               Into.Append
                 (Host_Entry'(Name => To_Unbounded_String (Name),
                   Kind => (case Kind (Item) is
                              when Directory => DIRECTORY_KIND_DIRECTORY,
                              when Ordinary_File => DIRECTORY_KIND_FILE,
                              when Special_File => DIRECTORY_KIND_UNKNOWN),
                   Size => (if Kind (Item) = Ordinary_File then Unsigned_64 (Size (Item)) else 0),
                   Modified_Ms => Epoch_Ms (Modification_Time (Item))));
            end if;
         exception
            when Name_Error | Use_Error => null;   --  vanished or unreadable: skipped
         end;
      end loop;
      End_Search (Search);
      Status := REPLY_OK;
   exception
      when Name_Error => Status := REPLY_NOT_FOUND;
      when Use_Error => Status := REPLY_ACCESS_DENIED;
   end List_Host;

   --  The synthetic folder's entry N: a deterministic name, kind, size and
   --  time.
   function Synthetic (N : Positive) return Host_Entry is
      Hash : constant Unsigned_64 := Unsigned_64 (N) * 16#9E37_79B9_7F4A_7C15#;
      Digits_Image : constant String := Natural'Image (N);
      Number : constant String := Digits_Image (Digits_Image'First + 1 .. Digits_Image'Last);
      EXTENSIONS : constant array (0 .. 3) of String (1 .. 3) := ["txt", "png", "adb", "dat"];
   begin
      if N mod SYNTHETIC_FOLDER_EVERY = 0 then
         return (To_Unbounded_String ("folder-" & Number), DIRECTORY_KIND_DIRECTORY, 0,
                 Hash mod 16#100_0000_0000#);
      end if;
      return (To_Unbounded_String ("entry-" & Number & "." & EXTENSIONS (Natural (Hash mod 4))),
              DIRECTORY_KIND_FILE, Shift_Right (Hash, 40), Hash mod 16#100_0000_0000#);
   end Synthetic;

   --  Names changed outside the service's task: it moves the namespace
   --  generation on its next turn.
   Namespace_Moved : Boolean := False with Atomic;

   Rename_Cookie : Unsigned_64 := 0;

   procedure Rename (Old_Path, New_Path : String; Status : out Unsigned_32) is
      use Ada.Directories;
      Old_Place : constant Resolved := Resolve (Old_Path);
      New_Place : constant Resolved := Resolve (New_Path);
   begin
      if Old_Place.Where = No_Place or else New_Place.Where = No_Place then
         Status := REPLY_ACCESS_DENIED;
      elsif Old_Place.Where /= Scratch_Place or else New_Place.Where /= Scratch_Place then
         Status := (if Old_Place.Where /= New_Place.Where then REPLY_CROSS_VOLUME else REPLY_READ_ONLY);
      elsif not Exists (To_String (Old_Place.Host_Path)) then
         Status := REPLY_NOT_FOUND;
      elsif Exists (To_String (New_Place.Host_Path)) then
         Status := REPLY_ALREADY_EXISTS;
      else
         Ada.Directories.Rename (To_String (Old_Place.Host_Path), To_String (New_Place.Host_Path));
         Status := REPLY_OK;
         Rename_Cookie := Rename_Cookie + 1;
         Board.Changed (FE.Renamed_From, Old_Path, Rename_Cookie);
         Board.Changed (FE.Renamed_To, New_Path, Rename_Cookie);
         Board.Gone (Old_Path);
         Namespace_Moved := True;
         Signal.Kick;
      end if;
   exception
      when Name_Error => Status := REPLY_NOT_FOUND;
      when Use_Error => Status := REPLY_ERR;
   end Rename;

   task body Service is
      Client_Base, Server_Base, Arena_Base : System.Address;
      Arena_Size : Unsigned_64;
      Server : Q.Server;
      Directories : Directory_Table;
      Namespace : Unsigned_32 := 1;
      Arming : Unsigned_32 := 0;
      Stopping : Boolean := False;

      function Client_Word (Offset : Natural) return System.Address is
        (Client_Base + Storage_Offset (Offset));
      function Server_Word (Offset : Natural) return System.Address is
        (Server_Base + Storage_Offset (Offset));

      function Arena_Text (Offset, Length : Unsigned_64) return String is
      begin
         if Length = 0 or else not FQ.In_Arena (Offset, Length, Arena_Size) then
            return "";
         end if;
         declare
            Text : constant String (1 .. Natural (Length))
              with Import, Address => Arena_Base + Storage_Offset (Offset);
         begin
            return Text;
         end;
      end Arena_Text;

      procedure Publish_Namespace is
         Word : Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Namespace_At);
      begin
         Word := Namespace;
      end Publish_Namespace;

      function Handle_Of (Slot : Positive) return Unsigned_64 is
        (Shift_Left (Unsigned_64 (Directories (Slot).Generation), SLOT_BITS) or Unsigned_64 (Slot));
      function Slot_Of (Handle : Unsigned_64) return Natural is
        (if Natural (Handle and 16#FFFF_FFFF#) in Directory_Table'Range
           and then Directories (Natural (Handle and 16#FFFF_FFFF#)).Kind /= Closed
           and then Unsigned_32 (Shift_Right (Handle, SLOT_BITS))
                    = Directories (Natural (Handle and 16#FFFF_FFFF#)).Generation
         then Natural (Handle and 16#FFFF_FFFF#) else 0);

      procedure Open_Directory (Path : String; Status : out Unsigned_32; Handle : out Unsigned_64) is
         Where : constant Resolved := Resolve (Path);
         Slot : Natural := 0;
      begin
         Handle := 0;
         if Where.Where = No_Place then
            Status := REPLY_ACCESS_DENIED;
            return;
         end if;
         for S in Directories'Range loop
            if Directories (S).Kind = Closed then
               Slot := S;
               exit;
            end if;
         end loop;
         if Slot = 0 then
            Status := REPLY_NO_SPACE;
            return;
         end if;
         declare
            D : Directory_Slot renames Directories (Slot);
         begin
            D.Next := 1;
            if Where.Where = Synthetic_Place then
               D.Kind := Synthetic_Directory;
               D.Synthetic_Count := (if Where.Synthetic_Child then SYNTHETIC_CHILD_ENTRIES else Where.Synthetic_Count);
               Status := REPLY_OK;
            else
               List_Host (To_String (Where.Host_Path), D.Entries, Status);
               if Status = REPLY_OK then
                  D.Kind := Host_Directory;
               end if;
            end if;
            if Status = REPLY_OK then
               D.Generation := D.Generation + 1;
               D.Path := To_Unbounded_String (Path);
               Handle := Handle_Of (Slot);
            end if;
         end;
      end Open_Directory;

      --  Fill up to Length / DIRECTORY_PAGE_BYTES Directory.Page.V2 pages at
      --  Offset (CuBit.Directory_Pages), with metadata when asked. Names the
      --  page format cannot hold ('.', '..', '/' or NUL in them) are left out,
      --  as the service never lists them.
      procedure Read_Directory
        (Handle : Unsigned_64; Offset, Length : Unsigned_64; Metadata : Boolean;
         Status : out Unsigned_32; Filled : out Unsigned_64)
      is
         package DP renames CuBit.Directory_Pages;
         Slot : constant Natural := Slot_Of (Handle);
         Stride : constant Unsigned_64 := DIRECTORY_PAGE_BYTES;
         Pages : constant Unsigned_64 := Length / Stride;
      begin
         Filled := 0;
         if Slot = 0 then
            Status := REPLY_ERR;
            return;
         elsif Pages = 0 or else not FQ.In_Arena (Offset, Pages * Stride, Arena_Size) then
            Status := REPLY_OUT_OF_RANGE;
            return;
         end if;
         declare
            D : Directory_Slot renames Directories (Slot);
            Total : constant Natural :=
              (if D.Kind = Synthetic_Directory then D.Synthetic_Count else Natural (D.Entries.Length));
         begin
            for Index in 0 .. Pages - 1 loop
               declare
                  Shared : DP.Page with Import, Address => Arena_Base + Storage_Offset (Offset + Index * Stride);
                  Page : DP.Page;
                  W : DP.Writer;
               begin
                  DP.Start (Page, W);
                  while D.Next <= Total loop
                     declare
                        E : constant Host_Entry :=
                          (if D.Kind = Synthetic_Directory then Synthetic (D.Next) else D.Entries (D.Next));
                        Name : constant String := To_String (E.Name);
                        Length : constant Natural := Natural'Min (Name'Length, DP.Maximum_Name_Bytes);
                        Bytes : DP.Name_Bytes := [others => 0];
                        Facts : DP.Facts :=
                          (Kind => E.Kind, Valid => DP.Valid_Object,
                           Object => Shift_Left (Unsigned_64 (Slot), SLOT_BITS) or Unsigned_64 (D.Next),
                           others => <>);
                     begin
                        for K in 1 .. Length loop
                           Bytes (K) := Character'Pos (Name (Name'First + K - 1));
                        end loop;
                        if DP.Valid_Name (Bytes, Length) then
                           exit when not DP.Fits (W, Length);
                           if Metadata then
                              Facts :=
                                (Kind => E.Kind,
                                 Valid => DP.Valid_Size or DP.Valid_Times or DP.Valid_Object or DP.Valid_Mode
                                          or DP.Valid_Owner,
                                 Mode => (if E.Kind = DIRECTORY_KIND_DIRECTORY then 8#40755# else 8#100644#),
                                 Size => E.Size, Modified => E.Modified_Ms, Changed => E.Modified_Ms,
                                 Accessed => E.Modified_Ms, Links => 1, Owner => 1000, Group => 1000,
                                 Object => Facts.Object);
                           end if;
                           DP.Append (Page, W, Facts, Bytes, Length);
                        end if;
                     end;
                     D.Next := D.Next + 1;
                  end loop;
                  DP.Finish (Page, W, Ended => D.Next > Total, Resume => Unsigned_64 (D.Next), Stamp => 0);
                  Shared := Page;
                  Filled := Filled + 1;
                  exit when D.Next > Total;
               end;
            end loop;
            Status := REPLY_OK;
         end;
      end Read_Directory;

      procedure Close_Directory (Handle : Unsigned_64; Status : out Unsigned_32) is
         Slot : constant Natural := Slot_Of (Handle);
      begin
         if Slot = 0 then
            Status := REPLY_ERR;
            return;
         end if;
         Directories (Slot).Kind := Closed;
         Directories (Slot).Entries.Clear;
         Status := REPLY_OK;
      end Close_Directory;

      --  Mkdir, rmdir and unlink: only in the scratch place.
      procedure Change (Operation : Unsigned_32; Path : String; Status : out Unsigned_32) is
         use Ada.Directories;
         Where : constant Resolved := Resolve (Path);
         Target : constant String := To_String (Where.Host_Path);
      begin
         if Where.Where in Host_Place | Synthetic_Place then
            Status := REPLY_READ_ONLY;
            return;
         elsif Where.Where = No_Place then
            Status := REPLY_ACCESS_DENIED;
            return;
         end if;
         --  Names resolve anew: before the change, as the service does.
         Namespace := Namespace + 1;
         Publish_Namespace;
         case Operation is
            when FQ.Queue_Mkdir =>
               if Exists (Target) then
                  Status := REPLY_ALREADY_EXISTS;
                  return;
               end if;
               Create_Directory (Target);
               Board.Changed (FE.Created, Path);
            when FQ.Queue_Rmdir =>
               if not Exists (Target) then
                  Status := REPLY_NOT_FOUND;
                  return;
               elsif Kind (Target) /= Directory then
                  Status := REPLY_WRONG_OBJECT_TYPE;
                  return;
               end if;
               declare
                  Contents : Entry_Vectors.Vector;
                  Listed : Unsigned_32;
               begin
                  List_Host (Target, Contents, Listed);
                  if not Contents.Is_Empty then
                     Status := REPLY_NOT_EMPTY;
                     return;
                  end if;
               end;
               Delete_Directory (Target);
               Board.Changed (FE.Removed, Path);
               Board.Gone (Path);
            when others =>
               if not Exists (Target) then
                  Status := REPLY_NOT_FOUND;
                  return;
               elsif Kind (Target) = Directory then
                  Status := REPLY_IS_DIRECTORY;
                  return;
               end if;
               Delete_File (Target);
               Board.Changed (FE.Removed, Path);
         end case;
         Status := REPLY_OK;
      exception
         when Name_Error => Status := REPLY_NOT_FOUND;
         when Use_Error => Status := REPLY_ERR;
      end Change;

      --  Open files: a host descriptor, or a synthetic file's generated text.
      type File_Kind is (No_File, Host_File, Synthetic_File);
      type File_Slot is record
         Kind : File_Kind := No_File;
         Descriptor : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
         Writable : Boolean := False;
         Synthetic_Size : Unsigned_64 := 0;
         Generation : Unsigned_32 := 0;
         Path : Unbounded_String;
         Written : Boolean := False;
      end record;
      MAXIMUM_OPEN_FILES : constant := 64;
      Files : array (1 .. MAXIMUM_OPEN_FILES) of File_Slot;

      function File_Slot_Of (Handle : Unsigned_64) return Natural is
        (if Natural (Handle and 16#FFFF_FFFF#) in Files'Range
           and then Files (Natural (Handle and 16#FFFF_FFFF#)).Kind /= No_File
           and then Unsigned_32 (Shift_Right (Handle, SLOT_BITS)) = Files (Natural (Handle and 16#FFFF_FFFF#)).Generation
         then Natural (Handle and 16#FFFF_FFFF#) else 0);

      procedure Open_File (Path : String; Options : Unsigned_32; Status : out Unsigned_32; Handle : out Unsigned_64) is
         use GNAT.OS_Lib;
         Where : constant Resolved := Resolve (Path);
         Mode : constant Open_Options := Open_Options (Options);
         Write : constant Boolean := Requests_Write (Mode);
         Slot : Natural := 0;
         Target : constant String := To_String (Where.Host_Path);
      begin
         Handle := 0;
         if Where.Where = No_Place or else not Valid_Open_Options (Mode) then
            Status := REPLY_ACCESS_DENIED;
            return;
         elsif Write and then Where.Where /= Scratch_Place then
            Status := REPLY_READ_ONLY;
            return;
         end if;
         for S in Files'Range loop
            if Files (S).Kind = No_File then
               Slot := S;
               exit;
            end if;
         end loop;
         if Slot = 0 then
            Status := REPLY_NO_SPACE;
            return;
         end if;
         declare
            F : File_Slot renames Files (Slot);
         begin
            if Where.Where = Synthetic_Place then
               F.Kind := Synthetic_File;
               F.Synthetic_Size := 4_096;
               F.Writable := False;
            else
               declare
                  Exists : constant Boolean := Is_Regular_File (Target);
               begin
                  if Is_Directory (Target) then
                     Status := REPLY_IS_DIRECTORY;
                     return;
                  elsif Exists and then (Mode and OPEN_EXCLUSIVE) /= 0 then
                     Status := REPLY_ALREADY_EXISTS;
                     return;
                  elsif not Exists and then (Mode and OPEN_CREATE) = 0 then
                     Status := REPLY_NOT_FOUND;
                     return;
                  end if;
                  if Write then
                     Namespace := Namespace + 1;
                     Publish_Namespace;
                     F.Descriptor :=
                       (if not Exists or else (Mode and OPEN_TRUNCATE) /= 0 then Create_File (Target, Binary)
                        else GNAT.OS_Lib.Open_Read_Write (Target, Binary));
                  else
                     F.Descriptor := Open_Read (Target, Binary);
                  end if;
                  if F.Descriptor = Invalid_FD then
                     Status := REPLY_ACCESS_DENIED;
                     return;
                  end if;
                  F.Kind := Host_File;
                  F.Writable := Write;
                  F.Written := False;
                  F.Path := To_Unbounded_String (Path);
                  if not Exists then
                     Board.Changed (FE.Created, Path);
                  end if;
               end;
            end if;
            F.Generation := F.Generation + 1;
            Handle := Shift_Left (Unsigned_64 (F.Generation), SLOT_BITS) or Unsigned_64 (Slot);
            Status := REPLY_OK;
         end;
      end Open_File;

      procedure Close_File (Handle : Unsigned_64; Status : out Unsigned_32) is
         Slot : constant Natural := File_Slot_Of (Handle);
      begin
         if Slot = 0 then
            Status := REPLY_ERR;
            return;
         end if;
         if Files (Slot).Kind = Host_File then
            GNAT.OS_Lib.Close (Files (Slot).Descriptor);
            if Files (Slot).Written then
               Board.Changed (FE.Modified, To_String (Files (Slot).Path));
            end if;
         end if;
         Files (Slot).Kind := No_File;
         Status := REPLY_OK;
      end Close_File;

      SYNTHETIC_LINE : constant String := "Synthetic file contents, line ";

      procedure Transfer
        (Write : Boolean; Handle, Position, Length, Offset : Unsigned_64; Status : out Unsigned_32;
         Done : out Unsigned_64)
      is
         use GNAT.OS_Lib;
         Slot : constant Natural := File_Slot_Of (Handle);
      begin
         Done := 0;
         if Slot = 0 then
            Status := REPLY_ERR;
            return;
         elsif not FQ.In_Arena (Offset, Length, Arena_Size) then
            Status := REPLY_OUT_OF_RANGE;
            return;
         elsif Write and then not Files (Slot).Writable then
            Status := REPLY_ACCESS_DENIED;
            return;
         end if;
         Status := REPLY_OK;
         if Length = 0 then
            return;
         end if;
         declare
            Data : String (1 .. Natural (Length)) with Import, Address => Arena_Base + Storage_Offset (Offset);
            F : File_Slot renames Files (Slot);
         begin
            if F.Kind = Synthetic_File then
               for K in Data'Range loop
                  exit when Position + Unsigned_64 (K) > F.Synthetic_Size;
                  Data (K) := SYNTHETIC_LINE ((Natural (Position) + K - 1) mod SYNTHETIC_LINE'Length + 1);
                  Done := Unsigned_64 (K);
               end loop;
               return;
            end if;
            Lseek (F.Descriptor, Long_Integer (Position), Seek_Set);
            Done := Unsigned_64 (Integer'Max (0, (if Write then GNAT.OS_Lib.Write (F.Descriptor, Data'Address, Data'Length)
                                                  else GNAT.OS_Lib.Read (F.Descriptor, Data'Address, Data'Length))));
            if Write and then Done < Length then
               Status := REPLY_NO_SPACE;
            end if;
            if Write and then Done > 0 then
               F.Written := True;
            end if;
         end;
      end Transfer;

      procedure List_Scopes (Offset, Length : Unsigned_64; Status : out Unsigned_32; Value : out Unsigned_64) is
         ENTRY_BYTES : constant := 264;   --  CuBit.File_Access.Wire_Entry_Bytes
         Needed : constant Unsigned_64 := Scopes'Length * ENTRY_BYTES;
      begin
         Value := Scopes'Length;
         if Length < Needed then
            Status := REPLY_NO_SPACE;
            return;
         elsif not FQ.In_Arena (Offset, Needed, Arena_Size) then
            Status := REPLY_OUT_OF_RANGE;
            return;
         end if;
         declare
            Bytes : array (1 .. Natural (Needed)) of Unsigned_8
              with Import, Address => Arena_Base + Storage_Offset (Offset);
         begin
            Bytes := [others => 0];
            for N in Scopes'Range loop
               declare
                  Base : constant Positive := (N - 1) * ENTRY_BYTES + 1;
                  Prefix : constant String := To_String (Scopes (N).Prefix);
               begin
                  Bytes (Base) := Scopes (N).Rights;
                  Bytes (Base + 1) := Unsigned_8 (Prefix'Length mod 256);
                  Bytes (Base + 2) := Unsigned_8 (Prefix'Length / 256);
                  for K in Prefix'Range loop
                     Bytes (Base + 8 + K - Prefix'First) := Character'Pos (Prefix (K));
                  end loop;
               end;
            end loop;
         end;
         Status := REPLY_OK;
      end List_Scopes;

      --  A made-up volume per place: 4 KiB blocks, a 256 GiB scratch volume
      --  a quarter used, the host place read-only.
      MOCK_BLOCK : constant := 4_096;
      MOCK_BLOCKS : constant := 67_108_864;
      procedure Describe_Volume (R : FQ.Request; Status : out Unsigned_32; Value : out Unsigned_64) is
         Path : constant String := Arena_Text (R.Arena_Offset, R.Position);
         Where : constant Resolved := Resolve (Path);
         Item : VD.Description;
         Name : constant String :=
           (case Where.Where is when Host_Place => "host", when Scratch_Place => "scratch",
                                when Synthetic_Place => "synthetic", when No_Place => "none");
      begin
         Value := 0;
         if Where.Where = No_Place then
            Status := REPLY_ACCESS_DENIED;
            return;
         elsif R.Length < VD.Record_Bytes or else not FQ.In_Arena (R.Arena_Offset, VD.Record_Bytes, Arena_Size) then
            Status := REPLY_OUT_OF_RANGE;
            return;
         end if;
         Item.Kind := VD.Ext2;
         Item.Block := MOCK_BLOCK;
         Item.Total_Blocks := MOCK_BLOCKS;
         Item.Free_Blocks := (if Where.Where = Scratch_Place then MOCK_BLOCKS / 4 * 3 else 0);
         Item.Flags := (if Where.Where = Scratch_Place then 0 else VD.Read_Only);
         Item.Total_Inodes := MOCK_BLOCKS / 4;
         Item.Free_Inodes := (if Where.Where = Scratch_Place then MOCK_BLOCKS / 8 else 0);
         for K in Name'Range loop
            Item.Name (K - Name'First + 1) := Character'Pos (Name (K));
         end loop;
         Item.Length := Name'Length;
         declare
            Image : VD.Record_Image with Import, Address => Arena_Base + Storage_Offset (R.Arena_Offset);
         begin
            VD.Encode (Item, Image);
         end;
         Value := VD.Record_Bytes;
         Status := REPLY_OK;
      end Describe_Volume;

      --  Server-side copies (Queue_Copy): one slice each per turn, progress
      --  in the service region's copy entries, answered when done.
      COPY_SLICE : constant := 524_288;
      type Copy_State is record
         Active, Cancel : Boolean := False;
         Tag : Q.Token := 0;
         Source, Target : Natural := 0;
         From, To, Remaining, Done : Unsigned_64 := 0;
         To_End : Boolean := False;
      end record;
      Copies : array (0 .. FQ.Maximum_Copies - 1) of Copy_State;
      Copy_Buffer : String (1 .. COPY_SLICE);

      procedure Publish_Copy (K : Natural) is
         Base : constant System.Address := Server_Word (FQ.Server_Copies_At + K * FQ.Copy_Entry_Bytes);
         Owner : Unsigned_64 with Import, Volatile, Address => Base + FQ.Copy_Token_At;
         Count : Unsigned_64 with Import, Volatile, Address => Base + FQ.Copy_Done_At;
      begin
         if Copies (K).Active then
            Count := Copies (K).Done;
            Owner := Unsigned_64 (Copies (K).Tag);
         else
            Owner := 0;
         end if;
      end Publish_Copy;

      procedure Start_Copy (Tag : Q.Token; R : FQ.Request; Status : out Unsigned_32) is
         Source : constant Natural := File_Slot_Of (R.Handle);
         Target : constant Natural := File_Slot_Of (R.Spare_1);
      begin
         Status := REPLY_OK;
         if Source = 0 or else Target = 0 or else Files (Source).Kind /= Host_File
           or else not Files (Target).Writable
         then
            Status := REPLY_ERR;
            return;
         end if;
         for K in Copies'Range loop
            if not Copies (K).Active then
               Copies (K) := (Active => True, Cancel => False, Tag => Tag, Source => Source, Target => Target,
                              From => R.Position, To => R.Arena_Offset, Remaining => R.Length, Done => 0,
                              To_End => R.Length = FQ.Copy_To_End);
               Publish_Copy (K);
               --  Answered when it ends (Advance_Copies).
               Status := 0;
               return;
            end if;
         end loop;
         Status := REPLY_BUSY;
      end Start_Copy;

      procedure Serve (Item : Q.Submission; Status : out Unsigned_32; Value : out Unsigned_64) is
         R : FQ.Request renames Item.Item;
      begin
         Value := 0;
         case R.Operation is
            when FQ.Queue_Open_Directory =>
               Open_Directory (Arena_Text (R.Arena_Offset, R.Length), Status, Value);
            when FQ.Queue_Read_Directory =>
               Read_Directory (R.Handle, R.Arena_Offset, R.Length, (R.Options and FQ.Directory_Metadata) /= 0,
                               Status, Value);
            when FQ.Queue_Close_Directory =>
               Close_Directory (R.Handle, Status);
            when FQ.Queue_Mkdir | FQ.Queue_Rmdir | FQ.Queue_Unlink =>
               Change (R.Operation, Arena_Text (R.Arena_Offset, R.Length), Status);
            when FQ.Queue_Rename =>
               declare
                  Both : constant String := Arena_Text (R.Arena_Offset, R.Length);
                  Split : constant Natural := Natural (Unsigned_64'Min (R.Position, Unsigned_64 (Both'Length)));
               begin
                  if Split = 0 or else Split >= Both'Length then
                     Status := REPLY_ERR;
                  else
                     Rename (Both (Both'First .. Both'First + Split - 1), Both (Both'First + Split .. Both'Last), Status);
                  end if;
               end;
            when FQ.Queue_Open =>
               Open_File (Arena_Text (R.Arena_Offset, R.Length), R.Options, Status, Value);
            when FQ.Queue_Close =>
               Close_File (R.Handle, Status);
            when FQ.Queue_Read_At | FQ.Queue_Write_At =>
               Transfer (R.Operation = FQ.Queue_Write_At, R.Handle, R.Position, R.Length, R.Arena_Offset,
                         Status, Value);
            when FQ.Queue_Watch =>
               declare
                  Slot : constant Natural := Slot_Of (R.Handle);
                  Number : Natural := 0;
               begin
                  if Slot = 0 then
                     Status := REPLY_ERR;
                  else
                     Board.Watch (To_String (Directories (Slot).Path), Number);
                     Status := (if Number = 0 then REPLY_NO_SPACE else REPLY_OK);
                     Value := Unsigned_64 (Number);
                  end if;
               end;
            when FQ.Queue_Unwatch =>
               declare
                  Found : Boolean;
               begin
                  Board.Unwatch (Natural (Unsigned_64'Min (R.Handle, Unsigned_64 (Natural'Last))), Found);
                  Status := (if Found then REPLY_OK else REPLY_ERR);
               end;
            when FQ.Queue_List_Scopes =>
               List_Scopes (R.Arena_Offset, R.Length, Status, Value);
            when FQ.Queue_Describe_Volume =>
               Describe_Volume (R, Status, Value);
            when FQ.Queue_Copy =>
               Start_Copy (Item.Tag, R, Status);
               Value := 0;
            when FQ.Queue_Cancel =>
               Status := REPLY_NOT_FOUND;
               for C of Copies loop
                  if C.Active and then C.Tag = Q.Token (R.Handle) then
                     C.Cancel := True;
                     Status := REPLY_OK;
                  end if;
               end loop;
            when others =>
               Status := REPLY_ERR;
         end case;
      end Serve;

      --  Serve every request that waits; False if there were none.
      function Serve_Waiting return Boolean is
         Requests : constant Q.Submissions.Ring with Import, Address => Client_Word (FQ.Client_Requests_At);
         Answers : Q.Completions.Ring with Import, Address => Server_Word (FQ.Server_Answers_At);
         Submitted : constant Unsigned_32 with Import, Volatile, Address => Client_Word (FQ.Client_Submitted_At);
         Reaped : constant Unsigned_32 with Import, Volatile, Address => Client_Word (FQ.Client_Reaped_At);
         Taken : Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Taken_At);
         Answered : Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Answered_At);
         Accepted : Boolean;
         Any : Boolean := False;
         Item : Q.Submission;
         Status : Unsigned_32;
         Value : Unsigned_64;
      begin
         loop
            Q.Submissions.Accept_Produced (Server.Requests, Q.Submissions.Index (Submitted), Accepted);
            Q.Accept_Reaped (Server, Q.Completions.Index (Reaped), Accepted);
            exit when not Q.Can_Take (Server);
            Q.Take (Server, Requests, Item);
            Taken := Unsigned_32 (Server.Requests.Consumed);
            if Latency > 0.0 then
               delay Latency;
            end if;
            Serve (Item, Status, Value);
            --  Status 0: a copy started; it is answered when it ends.
            if Status /= 0 then
               Q.Complete (Server, Answers, Item.Tag, (Status => Status, Value => Value, others => <>));
               Answered := Unsigned_32 (Server.Answers.Produced);
               Answered_Count := Answered_Count + 1;
            end if;
            Any := True;
         end loop;
         return Any;
      end Serve_Waiting;

      --  One slice of each running copy; True while any runs.
      function Advance_Copies return Boolean is
         use GNAT.OS_Lib;
         Answers : Q.Completions.Ring with Import, Address => Server_Word (FQ.Server_Answers_At);
         Answered : Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Answered_At);
         Any : Boolean := False;
      begin
         for K in Copies'Range loop
            declare
               C : Copy_State renames Copies (K);
               Status : Unsigned_32 := 0;
            begin
               if C.Active then
                  if C.Cancel then
                     Status := REPLY_CANCELLED;
                  else
                     declare
                        Want : constant Natural :=
                          Natural (Unsigned_64'Min (COPY_SLICE, (if C.To_End then COPY_SLICE else C.Remaining)));
                        Got, Put : Integer := 0;
                     begin
                        if Want > 0 then
                           Lseek (Files (C.Source).Descriptor, Long_Integer (C.From + C.Done), Seek_Set);
                           Got := Read (Files (C.Source).Descriptor, Copy_Buffer'Address, Want);
                           if Got > 0 then
                              Lseek (Files (C.Target).Descriptor, Long_Integer (C.To + C.Done), Seek_Set);
                              Put := Write (Files (C.Target).Descriptor, Copy_Buffer'Address, Got);
                              Files (C.Target).Written := True;
                           end if;
                        end if;
                        if Put < Got then
                           Status := REPLY_NO_SPACE;
                        elsif Got < 0 then
                           Status := REPLY_ERR;
                        else
                           C.Done := C.Done + Unsigned_64 (Got);
                           if not C.To_End then
                              C.Remaining := C.Remaining - Unsigned_64 (Got);
                           end if;
                           if Got = 0 or else (not C.To_End and then C.Remaining = 0) then
                              Status := REPLY_OK;
                           end if;
                        end if;
                     end;
                  end if;
                  if Status /= 0 then
                     C.Active := False;
                     Q.Complete (Server, Answers, C.Tag, (Status => Status, Value => C.Done, others => <>));
                     Answered := Unsigned_32 (Server.Answers.Produced);
                     Answered_Count := Answered_Count + 1;
                  end if;
                  Publish_Copy (K);
                  Any := Any or else C.Active or else Status /= 0;
               end if;
            end;
         end loop;
         return Any;
      end Advance_Copies;

   begin
      accept Begin_Serving (Client, Server, Arena : System.Address; Arena_Bytes : Unsigned_64) do
         Client_Base := Client;
         Server_Base := Server;
         Arena_Base := Arena;
         Arena_Size := Arena_Bytes;
      end Begin_Serving;
      Publish_Namespace;
      declare
         Wake : Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Wake_At);
         --  Answer a held wake request once answers wait.
         procedure Deliver_Wake is
            Answered : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Answered_At);
            Reaped : constant Unsigned_32 with Import, Volatile, Address => Client_Word (FQ.Client_Reaped_At);
            Armed : Boolean;
         begin
            if Answered /= Reaped or else Board.Waiting then
               Wake_Request.Take (Armed);
               if Armed and then Hook /= null then
                  Hook.all;
               end if;
            end if;
         end Deliver_Wake;
      begin
         while not Stopping loop
            if Namespace_Moved then
               Namespace_Moved := False;
               Namespace := Namespace + 1;
               Publish_Namespace;
            end if;
            Deliver_Wake;
            if not Serve_Waiting and then not Advance_Copies then
               --  Ask for a kick, look once more, then sleep until kicked.
               Arming := Arming + 1;
               Wake := Arming;
               if not Serve_Waiting then
                  Signal.Wait (Stopping);
               end if;
               Wake := 0;
            end if;
         end loop;
      end;
   end Service;
end Files_Mock_Service;
