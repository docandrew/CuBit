with Ada.Unchecked_Deallocation;
with CuBit.Directory_Paths;
with CuBit.Directory_Pages;
with CuBit.Filesystems;
with CuBit.Messages;
with Files_Limits;
with Files_Listing;
with Files_Pages;
with Files_Plan;

package body Files_Operations is
   package FS renames CuBit.Filesystems;
   package FQ renames Files_Queue.FQ;
   package DP renames CuBit.Directory_Paths;
   use type Files_Queue.Token;
   use type Files_Plan.Item_Kind, Files_Listing.Entry_Kind, FS.Open_Options;

   PLAN_PATH_BYTES : constant := MAXIMUM_ITEMS * 64;
   SCAN_ENTRIES : constant := SCAN_READ_PAGES * CuBit.Directory_Pages.Maximum_Entries;
   SCAN_NAME_BYTES : constant := SCAN_ENTRIES * Files_Limits.MAXIMUM_NAME_BYTES;

   type Plan_Access is access Files_Plan.Plan;
   type Scratch_Access is access Files_Listing.Listing;
   The_Plan : Plan_Access;
   Scratch : Scratch_Access;

   --  The steps a request can be.
   type Step_Kind is
     (No_Step, Open_Scan, Read_Scan, Close_Scan, Make_Dir, Open_Source, Open_Target, Server_Copy,
      Close_Target, Close_Source, Remove_File, Remove_Dir, Remove_Partial, Rename_Item);

   Current_Kind : Operation_Kind := Copy_Operation;
   --  Move's first pass: each source renamed; those across volumes marked
   --  for copying.
   type Crossing_Table is array (1 .. MAXIMUM_ITEMS) of Boolean with Pack;
   type Crossing_Access is access Crossing_Table;
   Crossing : Crossing_Access;
   --  Move falls back to copying then deleting: the plan runs twice.
   Deleting_After_Copy : Boolean := False;
   Policy_For_All : Conflict_Policy := Ask;
   Current_Phase : Phase_Kind := Idle;
   Final : Outcome := No_Outcome;
   Source, Target : DP.Path;
   Base : Unsigned_64 := 0;
   Changes : Unsigned_64 := 0;
   Status : Unsigned_32 := 0;
   Skip_Count : Natural := 0;
   Done_Items : Natural := 0;
   Done_Bytes : Unsigned_64 := 0;

   --  The step in flight.
   Step : Step_Kind := No_Step;
   Pending : Files_Queue.Token := Files_Queue.NO_TOKEN;
   Step_Deadline : Unsigned_64 := Unsigned_64'Last;
   --  A step that found the queue full: sent from Pump.
   Waiting_Send : Boolean := False;
   Pending_Request : FQ.Request;
   Clock_Us : Unsigned_64 := 0;

   --  Where the run is: the item, the scan's folder and handle, the open
   --  files and the position in the file.
   Index : Natural := 0;
   Scan_Index : Natural := 0;
   Scan_Handle, Source_Handle, Target_Handle : Unsigned_64 := 0;
   Scan_Pages : Files_Pages.Cursor_State;
   Position : Unsigned_64 := 0;
   --  The copy in flight: bytes counted so far, and when a busy one is
   --  asked again.
   Copied : Unsigned_64 := 0;
   Retry_At : Unsigned_64 := Unsigned_64'Last;
   Attempt : Files_Plan.Attempt_Number := 1;
   Item_Policy : Conflict_Policy := Ask;
   --  Late answers to abandoned steps: handles they open are closed.
   MAXIMUM_ORPHANS : constant := 8;
   type Orphan is record
      Tag : Files_Queue.Token := Files_Queue.NO_TOKEN;
      Opens : Boolean := False;
      Directory : Boolean := False;
   end record;
   Orphans : array (1 .. MAXIMUM_ORPHANS) of Orphan;

   procedure Changed is
   begin
      Changes := Changes + 1;
   end Changed;

   function Join (Directory, Relative : String) return String is
     (if Directory'Length > 0 and then Directory (Directory'Last) = '/' then Directory & Relative
      else Directory & "/" & Relative);

   --  The last component of a relative path, and what comes before it.
   function Leaf (Relative : String) return String is
   begin
      for K in reverse Relative'Range loop
         if Relative (K) = '/' then
            return Relative (K + 1 .. Relative'Last);
         end if;
      end loop;
      return Relative;
   end Leaf;
   function Parent (Relative : String) return String is
   begin
      for K in reverse Relative'Range loop
         if Relative (K) = '/' then
            return Relative (Relative'First .. K - 1);
         end if;
      end loop;
      return "";
   end Parent;

   function Item_Path return String is (Files_Plan.Relative (The_Plan.all, Index));
   function Source_Path return String is (Join (DP.Value (Source), Item_Path));
   --  The target for the current item: its leaf renamed on Keep_Both.
   function Target_Path return String is
      Relative : constant String := Item_Path;
      Name : constant String := Leaf (Relative);
      Fixed : constant String (1 .. Name'Length) := Name;
      Folder : constant String := Parent (Relative);
   begin
      if Fixed'Length = 0 then
         return DP.Value (Target);
      end if;
      declare
         Renamed : constant String := Files_Plan.Conflict_Name (Fixed, Attempt);
      begin
         return Join (DP.Value (Target), (if Folder'Length = 0 then Renamed else Folder & "/" & Renamed));
      end;
   end Target_Path;

   procedure Write_Path (Text : String; Offset : Unsigned_64) is
      Bytes : Files_Listing.Name_Bytes (1 .. Natural'Min (Text'Length, 2 * PATH_AREA));
   begin
      for K in Bytes'Range loop
         Bytes (K) := Character'Pos (Text (Text'First + K - 1));
      end loop;
      Files_Queue.Write_Arena (Offset, Bytes);
   end Write_Path;

   procedure Send (Kind : Step_Kind; Request : FQ.Request) is
   begin
      Step := Kind;
      Pending_Request := Request;
      Step_Deadline := Clock_Us + STEP_DEADLINE_US;
      Waiting_Send := not (Files_Queue.Ready and then Files_Queue.Can_Submit);
      if not Waiting_Send then
         Files_Queue.Submit (Request, Pending);
      end if;
   end Send;

   procedure Send_Path (Kind : Step_Kind; Operation : Unsigned_32; Path : String; Options : Unsigned_32 := 0) is
   begin
      if Path'Length > PATH_AREA then
         Status := FS.REPLY_OUT_OF_RANGE;
         Step := No_Step;
         Current_Phase := Finished;
         Final := Failed;
         Changed;
         return;
      end if;
      Write_Path (Path, Base);
      Send (Kind, (Operation => Operation, Options => Options, Length => Unsigned_64 (Path'Length),
                   Arena_Offset => Base, others => <>));
   end Send_Path;

   --  Queue_Rename: the old path then the new one in the arena (Length
   --  covers both, Position is the old path's length).
   procedure Send_Rename (Old_Path, New_Path : String) is
   begin
      if Old_Path'Length + New_Path'Length > 2 * PATH_AREA then
         Status := FS.REPLY_OUT_OF_RANGE;
         Step := No_Step;
         Current_Phase := Finished;
         Final := Failed;
         Changed;
         return;
      end if;
      Write_Path (Old_Path & New_Path, Base);
      Send (Rename_Item, (Operation => FQ.Queue_Rename, Position => Unsigned_64 (Old_Path'Length),
                          Length => Unsigned_64 (Old_Path'Length + New_Path'Length), Arena_Offset => Base,
                          others => <>));
   end Send_Rename;

   procedure Fire_And_Forget (Request : FQ.Request) is
      Tag : Files_Queue.Token;
   begin
      if Files_Queue.Ready and then Files_Queue.Can_Submit then
         Files_Queue.Submit (Request, Tag);
      end if;
   end Fire_And_Forget;

   procedure Finish (Result : Outcome) is
   begin
      --  A copy still running in the service stops at its next slice.
      if Step = Server_Copy and then Pending /= Files_Queue.NO_TOKEN then
         Fire_And_Forget ((Operation => FQ.Queue_Cancel, Handle => Unsigned_64 (Pending), others => <>));
      end if;
      Retry_At := Unsigned_64'Last;
      if Pending /= Files_Queue.NO_TOKEN then
         for O of Orphans loop
            if O.Tag = Files_Queue.NO_TOKEN then
               O := (Pending, Step in Open_Scan | Open_Source | Open_Target, Step = Open_Scan);
               exit;
            end if;
         end loop;
         Pending := Files_Queue.NO_TOKEN;
      end if;
      if Scan_Handle /= 0 then
         Fire_And_Forget ((Operation => FQ.Queue_Close_Directory, Handle => Scan_Handle, others => <>));
         Scan_Handle := 0;
      end if;
      if Source_Handle /= 0 then
         Fire_And_Forget ((Operation => FQ.Queue_Close, Handle => Source_Handle, others => <>));
         Source_Handle := 0;
      end if;
      if Target_Handle /= 0 then
         Fire_And_Forget ((Operation => FQ.Queue_Close, Handle => Target_Handle, others => <>));
         Target_Handle := 0;
      end if;
      Step := No_Step;
      Waiting_Send := False;
      Step_Deadline := Unsigned_64'Last;
      Current_Phase := Finished;
      Final := Result;
      Changed;
   end Finish;

   procedure Fail (Reply : Unsigned_32) is
   begin
      Status := Reply;
      Finish (Failed);
   end Fail;

   ---------------------------------------------------------------------------
   --  Scanning: every folder in the plan is listed and its entries added.
   ---------------------------------------------------------------------------
   procedure Begin_Run;

   procedure Scan_Next is
   begin
      --  The next folder not yet listed.
      while Scan_Index < The_Plan.Count loop
         Scan_Index := Scan_Index + 1;
         if Files_Plan.Kind (The_Plan.all, Scan_Index) = Files_Plan.Folder_Item then
            Index := Scan_Index;
            Files_Listing.Clear (Scratch.all);
            Scan_Pages := (others => <>);
            Send_Path (Open_Scan, FQ.Queue_Open_Directory, Source_Path);
            return;
         end if;
      end loop;
      Begin_Run;
   end Scan_Next;

   procedure Read_Scan is
   begin
      Send (Read_Scan, (Operation => FQ.Queue_Read_Directory, Options => FQ.Directory_Metadata,
                        Handle => Scan_Handle, Length => SCAN_READ_PAGES * Files_Pages.PAGE_BYTES,
                        Arena_Offset => Base + 2 * PATH_AREA, others => <>));
   end Read_Scan;

   --  A batch of pages: each entry joins the plan under its folder.
   procedure Take_Scan (Pages : Unsigned_64; Ended : out Boolean) is
      Page : Files_Pages.Page_Image;
      Result : Files_Pages.Page_Result;
      Folder : constant String := Item_Path;
   begin
      Ended := Pages = 0;
      Files_Listing.Clear (Scratch.all);
      for Index in 0 .. Unsigned_64'Min (Pages, SCAN_READ_PAGES) - 1 loop
         Files_Queue.Read_Arena (Base + 2 * PATH_AREA + Index * Files_Pages.PAGE_BYTES, Page);
         Files_Pages.Take_Page (Scratch.all, Page, Scan_Pages, Result);
         case Result is
            when Files_Pages.Page_Taken => null;
            when Files_Pages.Page_Last =>
               Ended := True;
               exit;
            when Files_Pages.Page_Malformed | Files_Pages.Listing_Full =>
               Ended := True;
               Status := FS.REPLY_MALFORMED_FILESYSTEM;
               exit;
         end case;
      end loop;
      for Id in 1 .. Scratch.Count loop
         declare
            Name : constant String := Files_Listing.Name (Scratch.all, Id);
            Relative : constant String := Folder & "/" & Name;
            Facts : constant Files_Listing.Entry_Facts := Files_Listing.Facts (Scratch.all, Id);
         begin
            if Relative'Length > Files_Plan.MAXIMUM_PATH
              or else not Files_Plan.Has_Room (The_Plan.all, Relative'Length)
            then
               Status := FS.REPLY_NO_SPACE;
               Ended := True;
               exit;
            end if;
            Files_Plan.Add
              (The_Plan.all,
               (if Facts.Kind = Files_Listing.Directory_Kind then Files_Plan.Folder_Item else Files_Plan.File_Item),
               Relative, (if Facts.Size_Known then Unsigned_64 (Facts.Size) else 0));
         end;
      end loop;
   end Take_Scan;

   ---------------------------------------------------------------------------
   --  Running.
   ---------------------------------------------------------------------------
   procedure Next_Item;

   procedure Begin_Run is
   begin
      Current_Phase := Running;
      Index := (if Current_Kind = Delete_Operation or else Deleting_After_Copy then The_Plan.Count + 1 else 0);
      Changed;
      Next_Item;
   end Begin_Run;

   procedure Start_Item is
   begin
      Attempt := 1;
      Item_Policy := Policy_For_All;
      Position := 0;
      if Current_Kind = Delete_Operation or else Deleting_After_Copy then
         if Files_Plan.Kind (The_Plan.all, Index) = Files_Plan.Folder_Item then
            Send_Path (Remove_Dir, FQ.Queue_Rmdir, Source_Path);
         else
            Send_Path (Remove_File, FQ.Queue_Unlink, Source_Path);
         end if;
      elsif Files_Plan.Kind (The_Plan.all, Index) = Files_Plan.Folder_Item then
         Send_Path (Make_Dir, FQ.Queue_Mkdir, Target_Path);
      else
         Send_Path (Open_Source, FQ.Queue_Open, Source_Path, Unsigned_32 (FS.OPEN_READ_ONLY));
      end if;
   end Start_Item;

   procedure Next_Item is
   begin
      if Current_Phase /= Running then
         return;
      end if;
      if Current_Kind = Delete_Operation or else Deleting_After_Copy then
         if Index <= 1 then
            Finish (Succeeded);
            return;
         end if;
         Index := Index - 1;
      else
         if Index >= The_Plan.Count then
            if Current_Kind = Move_Operation then
               --  Copied across volumes: now the sources go.
               Deleting_After_Copy := True;
               Begin_Run;
            else
               Finish (Succeeded);
            end if;
            return;
         end if;
         Index := Index + 1;
      end if;
      Start_Item;
   end Next_Item;

   procedure Item_Done is
   begin
      Done_Items := Done_Items + 1;
      Changed;
      Next_Item;
   end Item_Done;

   procedure Open_Target_File is
      Base_Options : constant FS.Open_Options := FS.OPEN_WRITE_ONLY or FS.OPEN_CREATE;
   begin
      Send_Path (Open_Target, FQ.Queue_Open, Target_Path,
                 Unsigned_32 (if Item_Policy = Overwrite_Existing then Base_Options or FS.OPEN_TRUNCATE
                              else Base_Options or FS.OPEN_EXCLUSIVE));
   end Open_Target_File;

   --  The whole file, by the service (no deadline of its own: ours follows
   --  its progress).
   procedure Copy_File is
   begin
      Copied := 0;
      Retry_At := Unsigned_64'Last;
      Send (Server_Copy, (Operation => FQ.Queue_Copy, Handle => Source_Handle, Spare_1 => Target_Handle,
                          Position => 0, Arena_Offset => 0, Length => FQ.Copy_To_End,
                          Spare_2 => CuBit.Messages.Wait_Forever, others => <>));
   end Copy_File;

   --  The target exists: what the policy says to do.
   procedure Resolve_Conflict is
   begin
      case Item_Policy is
         when Skip_Existing =>
            Skip_Count := Skip_Count + 1;
            Send (Close_Source, (Operation => FQ.Queue_Close, Handle => Source_Handle, others => <>));
         when Overwrite_Existing =>
            Open_Target_File;
         when Keep_Both =>
            if Attempt < Files_Plan.MAXIMUM_ATTEMPT then
               Attempt := Attempt + 1;
               Open_Target_File;
            else
               Fail (FS.REPLY_ALREADY_EXISTS);
            end if;
         when Ask =>
            Current_Phase := Asking;
            Step := No_Step;
            Step_Deadline := Unsigned_64'Last;
            Changed;
      end case;
   end Resolve_Conflict;

   --  Move's renames are done: what crossed volumes is copied (then
   --  deleted), the rest is finished.
   procedure Renamed_All is
      Kept : Natural := 0;
   begin
      for K in 1 .. The_Plan.Count loop
         if Crossing (K) then
            Kept := Kept + 1;
         end if;
      end loop;
      if Kept = 0 then
         Finish (Succeeded);
         return;
      end if;
      --  Keep only the crossing ones, in order (each earlier slot is free).
      declare
         type Saved is record
            Name : String (1 .. Files_Limits.MAXIMUM_NAME_BYTES) := [others => ' '];
            Length : Natural := 0;
            Folder : Boolean := False;
            Size : Unsigned_64 := 0;
         end record;
         type Saved_Table is array (Positive range <>) of Saved;
         type Saved_Access is access Saved_Table;
         procedure Free is new Ada.Unchecked_Deallocation (Saved_Table, Saved_Access);
         Rest : Saved_Access := new Saved_Table (1 .. Kept);
         Count : Natural := 0;
      begin
         for K in 1 .. The_Plan.Count loop
            if Crossing (K) then
               declare
                  Name : constant String := Files_Plan.Relative (The_Plan.all, K);
               begin
                  Count := Count + 1;
                  Rest (Count).Length := Natural'Min (Name'Length, Files_Limits.MAXIMUM_NAME_BYTES);
                  Rest (Count).Name (1 .. Rest (Count).Length) := Name (Name'First .. Name'First + Rest (Count).Length - 1);
                  Rest (Count).Folder := Files_Plan.Kind (The_Plan.all, K) = Files_Plan.Folder_Item;
                  Rest (Count).Size := Files_Plan.Size (The_Plan.all, K);
               end;
            end if;
         end loop;
         Files_Plan.Clear (The_Plan.all);
         for K in 1 .. Count loop
            Add_Source (Rest (K).Name (1 .. Rest (K).Length), Rest (K).Folder, Rest (K).Size);
         end loop;
         Free (Rest);
      end;
      Current_Phase := Scanning;
      Scan_Index := 0;
      Changed;
      Scan_Next;
   end Renamed_All;

   procedure Advance (Answer : Files_Queue.Answer) is
      OK : constant Boolean := Answer.Status = FS.REPLY_OK;
   begin
      case Step is
         when No_Step => null;
         when Open_Scan =>
            if not OK then
               Fail (Answer.Status);
            else
               Scan_Handle := Answer.Value;
               Read_Scan;
            end if;
         when Read_Scan =>
            if not OK then
               Fail (Answer.Status);
            else
               declare
                  Ended : Boolean;
               begin
                  Take_Scan (Answer.Value, Ended);
                  if Status /= 0 then
                     Fail (Status);
                  elsif Ended then
                     Send (Close_Scan, (Operation => FQ.Queue_Close_Directory, Handle => Scan_Handle, others => <>));
                     Scan_Handle := 0;
                  else
                     Read_Scan;
                  end if;
                  Changed;
               end;
            end if;
         when Close_Scan =>
            Scan_Next;
         when Make_Dir =>
            if OK or else Answer.Status = FS.REPLY_ALREADY_EXISTS then
               --  An existing folder merges.
               Item_Done;
            else
               Fail (Answer.Status);
            end if;
         when Open_Source =>
            if not OK then
               Fail (Answer.Status);
            else
               Source_Handle := Answer.Value;
               Open_Target_File;
            end if;
         when Open_Target =>
            if OK then
               Target_Handle := Answer.Value;
               Copy_File;
            elsif Answer.Status = FS.REPLY_ALREADY_EXISTS then
               Resolve_Conflict;
            else
               Fail (Answer.Status);
            end if;
         when Server_Copy =>
            if OK then
               Done_Bytes := Done_Bytes - Copied + Answer.Value;
               Copied := Answer.Value;
               Changed;
               Send (Close_Target, (Operation => FQ.Queue_Close, Handle => Target_Handle, others => <>));
            elsif Answer.Status = FS.REPLY_BUSY then
               --  Four copies of ours run already: ask again shortly.
               Step := Server_Copy;
               Retry_At := Clock_Us + BUSY_RETRY_US;
               Step_Deadline := Clock_Us + STEP_DEADLINE_US;
            else
               Fail (Answer.Status);
            end if;
         when Close_Target =>
            Target_Handle := 0;
            Send (Close_Source, (Operation => FQ.Queue_Close, Handle => Source_Handle, others => <>));
         when Close_Source =>
            Source_Handle := 0;
            Item_Done;
         when Remove_File | Remove_Dir =>
            if OK or else Answer.Status = FS.REPLY_NOT_FOUND then
               Item_Done;
            else
               Fail (Answer.Status);
            end if;
         when Remove_Partial =>
            Finish (Cancelled);
         when Rename_Item =>
            if Current_Kind = Rename_Operation then
               if OK then
                  Done_Items := 1;
                  Finish (Succeeded);
               else
                  Fail (Answer.Status);
               end if;
            elsif OK or else Answer.Status = FS.REPLY_CROSS_VOLUME then
               if OK then
                  Done_Items := Done_Items + 1;
               else
                  Crossing (Index) := True;
               end if;
               Changed;
               if Index < The_Plan.Count then
                  Index := Index + 1;
                  declare
                     Name : constant String := Files_Plan.Relative (The_Plan.all, Index);
                  begin
                     Send_Rename (Join (DP.Value (Source), Name), Join (DP.Value (Target), Name));
                  end;
               else
                  Renamed_All;
               end if;
            else
               Fail (Answer.Status);
            end if;
      end case;
   end Advance;

   ---------------------------------------------------------------------------
   --  The interface.
   ---------------------------------------------------------------------------
   procedure Prepare (Kind : Operation_Kind; Source_Dir, Target_Dir : String; Policy : Conflict_Policy) is
      Done : Boolean;
   begin
      if The_Plan = null then
         The_Plan := new Files_Plan.Plan (MAXIMUM_ITEMS, PLAN_PATH_BYTES);
         Scratch := new Files_Listing.Listing (SCAN_ENTRIES, SCAN_NAME_BYTES);
      end if;
      Files_Plan.Clear (The_Plan.all);
      Current_Kind := Kind;
      Deleting_After_Copy := False;
      Policy_For_All := Policy;
      DP.Set_Root (Source_Dir, Source, Done);
      DP.Set_Root (Target_Dir, Target, Done);
      Current_Phase := Idle;
      Final := No_Outcome;
      Status := 0;
      Skip_Count := 0;
      Done_Items := 0;
      Done_Bytes := 0;
      Index := 0;
      Scan_Index := 0;
      Changed;
   end Prepare;

   procedure Add_Source (Name : String; Folder : Boolean; Size : Unsigned_64) is
   begin
      if Files_Plan.Has_Room (The_Plan.all, Name'Length) then
         Files_Plan.Add (The_Plan.all, (if Folder then Files_Plan.Folder_Item else Files_Plan.File_Item), Name,
                         (if Folder then 0 else Size));
      end if;
   end Add_Source;

   procedure Start (Arena_Base : Unsigned_64; Now_Us : Unsigned_64) is
   begin
      Base := Arena_Base;
      Clock_Us := Now_Us;
      if Current_Kind = Move_Operation then
         --  A rename per source first (Queue_Rename); only what crosses
         --  volumes is copied, then deleted.
         if Crossing = null then
            Crossing := new Crossing_Table;
         end if;
         Crossing.all := [others => False];
         Current_Phase := Running;
         Index := 1;
         Changed;
         if The_Plan.Count = 0 then
            Finish (Succeeded);
         else
            declare
               Name : constant String := Files_Plan.Relative (The_Plan.all, 1);
            begin
               Send_Rename (Join (DP.Value (Source), Name), Join (DP.Value (Target), Name));
            end;
         end if;
         return;
      end if;
      Current_Phase := Scanning;
      Scan_Index := 0;
      Changed;
      Scan_Next;
   end Start;

   procedure Make_Folder (Directory, Name : String; Arena_Base, Now_Us : Unsigned_64) is
      Done : Boolean;
   begin
      if The_Plan = null then
         The_Plan := new Files_Plan.Plan (MAXIMUM_ITEMS, PLAN_PATH_BYTES);
         Scratch := new Files_Listing.Listing (SCAN_ENTRIES, SCAN_NAME_BYTES);
      end if;
      Files_Plan.Clear (The_Plan.all);
      Files_Plan.Add (The_Plan.all, Files_Plan.Folder_Item, Name, 0);
      Current_Kind := Make_Folder_Operation;
      Deleting_After_Copy := False;
      Policy_For_All := Ask;
      DP.Set_Root (Directory, Target, Done);
      DP.Set_Root (Directory, Source, Done);
      Base := Arena_Base;
      Clock_Us := Now_Us;
      Final := No_Outcome;
      Status := 0;
      Skip_Count := 0;
      Done_Items := 0;
      Done_Bytes := 0;
      Index := 1;
      Attempt := 1;
      Current_Phase := Running;
      Changed;
      if not DP.Valid_Child_Name (Name) then
         Fail (FS.REPLY_ERR);
         return;
      end if;
      Send_Path (Make_Dir, FQ.Queue_Mkdir, Target_Path);
   end Make_Folder;

   procedure Rename (Directory, Old_Name, New_Name : String; Arena_Base, Now_Us : Unsigned_64) is
      Done : Boolean;
   begin
      if The_Plan = null then
         The_Plan := new Files_Plan.Plan (MAXIMUM_ITEMS, PLAN_PATH_BYTES);
         Scratch := new Files_Listing.Listing (SCAN_ENTRIES, SCAN_NAME_BYTES);
      end if;
      Files_Plan.Clear (The_Plan.all);
      Files_Plan.Add (The_Plan.all, Files_Plan.File_Item, Old_Name, 0);
      Current_Kind := Rename_Operation;
      DP.Set_Root (Directory, Source, Done);
      DP.Set_Root (Directory, Target, Done);
      Base := Arena_Base;
      Clock_Us := Now_Us;
      Final := No_Outcome;
      Index := 1;
      Done_Items := 0;
      Done_Bytes := 0;
      Skip_Count := 0;
      Status := 0;
      Current_Phase := Running;
      Changed;
      if not DP.Valid_Child_Name (New_Name) then
         Fail (FS.REPLY_ERR);
         return;
      end if;
      Send_Rename (Join (Directory, Old_Name), Join (Directory, New_Name));
   end Rename;

   procedure Take (Answer : Files_Queue.Answer; Owned : out Boolean) is
   begin
      Owned := False;
      if Answer.Tag = Files_Queue.NO_TOKEN then
         return;
      end if;
      for O of Orphans loop
         if O.Tag = Answer.Tag then
            if O.Opens and then Answer.Status = FS.REPLY_OK then
               Fire_And_Forget ((Operation => (if O.Directory then FQ.Queue_Close_Directory else FQ.Queue_Close),
                                 Handle => Answer.Value, others => <>));
            end if;
            O := (others => <>);
            Owned := True;
            return;
         end if;
      end loop;
      if Answer.Tag /= Pending then
         return;
      end if;
      Owned := True;
      Pending := Files_Queue.NO_TOKEN;
      Step_Deadline := Unsigned_64'Last;
      if Current_Phase = Cancelling then
         --  The step in flight is over; a partly written file goes.
         case Step is
            when Open_Target =>
               if Answer.Status = FS.REPLY_OK then
                  Target_Handle := Answer.Value;
               end if;
            when Open_Source =>
               if Answer.Status = FS.REPLY_OK then
                  Source_Handle := Answer.Value;
               end if;
            when Open_Scan =>
               if Answer.Status = FS.REPLY_OK then
                  Scan_Handle := Answer.Value;
               end if;
            when Remove_Partial =>
               Finish (Cancelled);
               return;
            when others => null;
         end case;
         if Target_Handle /= 0 then
            Fire_And_Forget ((Operation => FQ.Queue_Close, Handle => Target_Handle, others => <>));
            Target_Handle := 0;
            Send_Path (Remove_Partial, FQ.Queue_Unlink, Target_Path);
         else
            Finish (Cancelled);
         end if;
         return;
      end if;
      Advance (Answer);
   end Take;

   procedure Pump (Now_Us : Unsigned_64) is
   begin
      Clock_Us := Now_Us;
      if Current_Phase = Running and then Step = Server_Copy then
         if Pending /= Files_Queue.NO_TOKEN then
            declare
               Now_Copied : constant Unsigned_64 := Files_Queue.Copy_Progress (Pending);
            begin
               --  Progress moves the deadline on: only a stalled copy times out.
               if Now_Copied > Copied then
                  Done_Bytes := Done_Bytes + (Now_Copied - Copied);
                  Copied := Now_Copied;
                  Step_Deadline := Now_Us + STEP_DEADLINE_US;
                  Changed;
               end if;
            end;
         elsif Now_Us >= Retry_At and then not Waiting_Send then
            Retry_At := Unsigned_64'Last;
            Send (Server_Copy, Pending_Request);
         end if;
      end if;
      if Current_Phase in Scanning | Running | Cancelling then
         if Now_Us > Step_Deadline then
            Status := 0;
            Finish (Timed_Out);
         elsif Waiting_Send and then Files_Queue.Ready and then Files_Queue.Can_Submit then
            Waiting_Send := False;
            Files_Queue.Submit (Pending_Request, Pending);
         end if;
      end if;
   end Pump;

   procedure Cancel is
   begin
      case Current_Phase is
         when Scanning | Running =>
            if Pending = Files_Queue.NO_TOKEN then
               Finish (Cancelled);
            else
               if Step = Server_Copy then
                  Fire_And_Forget ((Operation => FQ.Queue_Cancel, Handle => Unsigned_64 (Pending), others => <>));
               end if;
               Current_Phase := Cancelling;
               Changed;
            end if;
         when Asking =>
            Finish (Cancelled);
         when others => null;
      end case;
   end Cancel;

   procedure Decide (Policy : Conflict_Policy; For_All : Boolean) is
   begin
      if Current_Phase /= Asking then
         return;
      end if;
      if For_All then
         Policy_For_All := Policy;
      end if;
      Item_Policy := Policy;
      Current_Phase := Running;
      Changed;
      Resolve_Conflict;
   end Decide;

   procedure Acknowledge is
   begin
      if Current_Phase = Finished then
         Current_Phase := Idle;
         Changed;
      end if;
   end Acknowledge;

   function Kind return Operation_Kind is (Current_Kind);
   function Phase return Phase_Kind is (Current_Phase);
   function Result return Outcome is (Final);
   function Items_Done return Natural is (Done_Items);
   function Items_Total return Natural is (if The_Plan = null then 0 else The_Plan.Count);
   function Bytes_Done return Unsigned_64 is (Done_Bytes);
   function Bytes_Total return Unsigned_64 is (if The_Plan = null then 0 else The_Plan.Bytes);
   function Current return String is
     (if The_Plan = null or else Index = 0 or else Index > The_Plan.Count then "" else Item_Path);
   function Failure return Unsigned_32 is (Status);
   function Skipped return Natural is (Skip_Count);
   function Revision return Unsigned_64 is (Changes);
   --  The step's deadline; sooner while a copy runs (its progress is read
   --  then, for the bar) or waits to be asked again.
   function Deadline return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64'Min (Step_Deadline, Retry_At),
         (if Current_Phase = Running and then Step = Server_Copy and then Pending /= Files_Queue.NO_TOKEN
          then Clock_Us + PROGRESS_REFRESH_US else Unsigned_64'Last)));
end Files_Operations;
