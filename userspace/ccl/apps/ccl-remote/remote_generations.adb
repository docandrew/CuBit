pragma Ada_2022;
with CuBit.Launching;
with CuBit.Process_IDs;
with CuBit.Service_Replacement;
with CCL.Objects.Views;
with CCL.Typed_Values;
with Remote_Files;
with System_Generation_Records;
with Startup;

package body Remote_Generations is
   use Interfaces;
   use type W.Status;
   use type W.Digest;
   use type CuBit.Launching.Switch_Answer;
   use type CuBit.Service_Replacement.Outcome;
   package SR renames CuBit.Service_Replacement;
   package Views renames CCL.Objects.Views;
   package Records renames System_Generation_Records;
   use type Records.Digest;

   function Launcher return CuBit.Launching.Launcher is
     (CuBit.Launching.Using (Startup.Process_Manager));

   Executables : constant String := "executables";
   Profiles : constant String := "profiles";
   Profile : constant String := "profiles/system";
   Current_Name : constant String := "profiles/system/current";
   Incoming_Name : constant String := "executables/incoming";
   LF : constant Character := ASCII.LF;
   Record_Bytes : constant := 4_096;

   subtype Generation_Value is Records.Generation_Value;
   subtype Pin is Records.Pin;
   function Pin_Of (G : Generation_Value; Service : String) return Natural renames Records.Pin_Of;
   function Record_Text (G : Generation_Value) return String renames Records.Record_Text;
   function Hex (Item : Records.Digest) return String renames Records.Hex;

   function To_Wire (Item : Records.Digest) return W.Digest is
     ([for I in W.Digest_Index => Item (I)]);
   function From_Wire (Item : W.Digest) return Records.Digest is
     ([for I in Records.Digest_Index => Item (I)]);

   --  The checker's value (large: kept here, not on the stack).
   Object : Views.Snapshot;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Generation_Name (N : W.Generation) return String is
     (Profile & "/" & Decimal (Unsigned_64 (N)));
   function Program_Name (Item : Records.Digest) return String is (Executables & "/" & Hex (Item));

   --  Generation N's value, checked by the CCL type checker.
   procedure Read_Generation (N : W.Generation; G : out Generation_Value; Status : out W.Status) is
      Text : String (1 .. Record_Bytes);
      Length : Natural;
      Checked : CCL.Typed_Values.Check_Result;
   begin
      G := (others => <>);
      if N = W.No_Generation then
         Status := W.OK;
         return;
      end if;
      Remote_Files.Read_Store_Text (Generation_Name (N), Text, Length, Status);
      if Status /= W.OK then
         return;
      end if;
      while Length > 0 and then Text (Length) = LF loop
         Length := Length - 1;
      end loop;
      CCL.Typed_Values.Capture_Value (Records.Schema, Records.Root_Name, Text (1 .. Length), Object, Checked);
      if not Checked.Success then
         Status := W.Failed;
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Number : constant Unsigned_64 := Views.Scalar (Object, Views.Field (Object, Root, Records.Number_Field)).First;
         Previous : constant Unsigned_64 :=
           Views.Scalar (Object, Views.Field (Object, Root, Records.Previous_Field)).First;
         Pins : constant Views.Cursor := Views.Field (Object, Root, Records.Pins_Field);
         Total : constant Natural := Natural (Views.Length (Object, Pins));
         Valid : Boolean;
      begin
         if Number /= Unsigned_64 (N) or else Previous >= Number or else Total > Records.Maximum_Pins then
            Status := W.Failed;
            return;
         end if;
         G.Number := N;
         G.Previous := W.Generation (Previous);
         for I in 1 .. Total loop
            declare
               Element : constant Views.Cursor := Views.Element (Object, Pins, I);
               Service : constant String := Views.Text (Object, Views.Field (Object, Element, Records.Pin_Service_Field));
               Program : constant String := Views.Text (Object, Views.Field (Object, Element, Records.Pin_Executable_Field));
            begin
               if not W.Valid_Service_Name (Service) or else Pin_Of (G, Service) /= 0 then
                  Status := W.Failed;
                  return;
               end if;
               G.Count := G.Count + 1;
               G.Pins (G.Count).Length := Service'Length;
               G.Pins (G.Count).Service (1 .. Service'Length) := Service;
               Records.From_Hex (Program, G.Pins (G.Count).Program, Valid);
               if not Valid then
                  Status := W.Failed;
                  return;
               end if;
            end;
         end loop;
      end;
      Status := W.OK;
   end Read_Generation;

   --  The current generation's number (0: none recorded) and the newest.
   procedure Numbers (Current, Latest : out W.Generation; Status : out W.Status) is
      Text : String (1 .. 16);
      Length : Natural;
      Value : Unsigned_64 := 0;
      Size : Unsigned_64;
      Hash : W.Digest;
      Probe : W.Status;
   begin
      Current := W.No_Generation;
      Latest := W.No_Generation;
      Remote_Files.Read_Store_Text (Current_Name, Text, Length, Status);
      if Status = W.Not_Found then
         Status := W.OK;
      elsif Status = W.OK then
         while Length > 0 and then Text (Length) = LF loop
            Length := Length - 1;
         end loop;
         if Length not in 1 .. 9 or else (for some C of Text (1 .. Length) => C not in '0' .. '9') then
            Status := W.Failed;
            return;
         end if;
         for C of Text (1 .. Length) loop
            Value := Value * 10 + Unsigned_64 (Character'Pos (C) - Character'Pos ('0'));
         end loop;
         if Value > Maximum_Generations then
            Status := W.Failed;
            return;
         end if;
         Current := W.Generation (Value);
      else
         return;
      end if;
      --  Numbers only grow: the newest is the last one present.
      for N in 1 .. W.Generation (Maximum_Generations) loop
         Remote_Files.Hash_Store (Generation_Name (N), Size, Hash, Probe);
         exit when Probe /= W.OK;
         Latest := N;
      end loop;
      if Current > Latest then
         Status := W.Failed;
      end if;
   end Numbers;

   procedure Ensure_Directories (Status : out W.Status) is
   begin
      Remote_Files.Make_Store_Directory ("", Status);
      if Status = W.OK then
         Remote_Files.Make_Store_Directory (Executables, Status);
      end if;
      if Status = W.OK then
         Remote_Files.Make_Store_Directory (Profiles, Status);
      end if;
      if Status = W.OK then
         Remote_Files.Make_Store_Directory (Profile, Status);
      end if;
   end Ensure_Directories;

   function Valid_Deadlines (Grace, Health : Unsigned_32) return Boolean is
     (Unsigned_64 (Grace) <= SR.Maximum_Stop_Grace_Ms
      and then Unsigned_64 (Health) in SR.Minimum_Health_Window_Ms .. SR.Maximum_Health_Window_Ms);

   --  Switch from From to To (they differ in at most Maximum_Changed_Pins
   --  pins); on success, To becomes current.
   procedure Switch_To
     (From, To : Generation_Value; Grace, Health : Unsigned_32; Item : in out Report;
      Status : out W.Status)
   is
      Changed : Natural := 0;
      Service : Pin;
      In_To : Natural;
      Answer : CuBit.Launching.Switch_Result;
   begin
      Status := W.OK;
      Item.Target := To.Number;
      Item.Current := From.Number;
      --  The services whose pins differ: in To and not the same in From,
      --  or in From and gone from To.
      for I in 1 .. To.Count loop
         declare
            Before : constant Natural := Pin_Of (From, To.Pins (I).Service (1 .. To.Pins (I).Length));
         begin
            if Before = 0 or else From.Pins (Before).Program /= To.Pins (I).Program then
               Changed := Changed + 1;
               Service := To.Pins (I);
            end if;
         end;
      end loop;
      for I in 1 .. From.Count loop
         if Pin_Of (To, From.Pins (I).Service (1 .. From.Pins (I).Length)) = 0 then
            Changed := Changed + 1;
            Service := (Service => From.Pins (I).Service, Length => From.Pins (I).Length,
                        Program => [others => 0]);
         end if;
      end loop;
      if Changed > Maximum_Changed_Pins then
         Item.Result := W.Too_Many_Changes;
         return;
      end if;
      if Changed = 0 then
         --  Nothing runs differently: only the pointer moves.
         Remote_Files.Write_Store_Text (Current_Name, Decimal (Unsigned_64 (To.Number)) & LF, Status);
         Item.Result := W.Unchanged;
         Item.Current := (if Status = W.OK then To.Number else From.Number);
         return;
      end if;
      Item.Service_Length := Service.Length;
      Item.Service (1 .. Service.Length) := Service.Service (1 .. Service.Length);
      In_To := Pin_Of (To, Service.Service (1 .. Service.Length));
      Item.Pin := (if In_To = 0 then [others => 0] else To_Wire (To.Pins (In_To).Program));
      CuBit.Launching.Switch_Service
        (Launcher, Service.Service (1 .. Service.Length),
         (if In_To = 0 then "" else Remote_Files.Store_Path (Program_Name (To.Pins (In_To).Program))),
         SR.Stop_Grace_Ms (Grace), SR.Health_Window_Ms (Health), Answer);
      case Answer.Answer is
         when CuBit.Launching.Refused_Switch =>
            Item.Result := W.Refused;
            Item.Detail := Unsigned_8 (SR.Refusal'Enum_Rep (Answer.Why));
         when CuBit.Launching.Timed_Out | CuBit.Launching.No_Process_Manager_Switch =>
            Status := W.Unavailable;
         when CuBit.Launching.Answered =>
            Item.Running := CuBit.Process_IDs.To_Word (Answer.Running);
            Item.Detail := Unsigned_8 (SR.Health_Failure'Enum_Rep (Answer.Failure));
            case Answer.Outcome is
               when SR.Switched =>
                  Remote_Files.Write_Store_Text (Current_Name, Decimal (Unsigned_64 (To.Number)) & LF, Status);
                  Item.Result := W.Activated;
                  if Status = W.OK then
                     Item.Current := To.Number;
                  end if;
               when SR.Rolled_Back => Item.Result := W.Rolled_Back_Generation;
               when SR.Stop_Failed => Item.Result := W.Stop_Failed_Generation;
               when SR.Rollback_Failed => Item.Result := W.Rollback_Failed;
            end case;
      end case;
   end Switch_To;

   procedure Activate
     (Product, Service : String; Grace, Health : Unsigned_32;
      Item : out Report; Status : out W.Status)
   is
      Current, Latest : W.Generation;
      From, To : Generation_Value;
      Size : Unsigned_64;
      Hash : W.Digest;
      At_Pin : Natural;
   begin
      Item := (others => <>);
      Item.Service_Length := Service'Length;
      Item.Service (1 .. Service'Length) := Service;
      if not Valid_Deadlines (Grace, Health) then
         Status := W.Malformed;
         return;
      end if;
      Ensure_Directories (Status);
      if Status = W.OK then
         Numbers (Current, Latest, Status);
      end if;
      if Status = W.OK then
         Read_Generation (Current, From, Status);
      end if;
      if Status /= W.OK then
         return;
      end if;
      Item.Current := Current;
      if Latest >= Maximum_Generations then
         Status := W.Capacity;
         return;
      end if;
      --  Into the storehouse by its SHA-256 (the same bytes, the same name).
      Remote_Files.Copy_To_Store (W.Workspace, Product, Incoming_Name, Size, Hash, Status);
      if Status = W.OK then
         Remote_Files.Rename_Store (Incoming_Name, Program_Name (From_Wire (Hash)), Status);
      end if;
      if Status /= W.OK then
         return;
      end if;
      Item.Pin := Hash;
      To := From;
      To.Number := Latest + 1;
      To.Previous := Current;
      At_Pin := Pin_Of (To, Service);
      if At_Pin /= 0 and then To.Pins (At_Pin).Program = From_Wire (Hash) then
         Item.Result := W.Unchanged;
         Item.Target := Current;
         return;
      end if;
      if At_Pin = 0 then
         if To.Count = Records.Maximum_Pins then
            Status := W.Capacity;
            return;
         end if;
         To.Count := To.Count + 1;
         At_Pin := To.Count;
         To.Pins (At_Pin).Length := Service'Length;
         To.Pins (At_Pin).Service (1 .. Service'Length) := Service;
      end if;
      To.Pins (At_Pin).Program := From_Wire (Hash);
      --  The new generation exists (immutable) whether or not it becomes current.
      Remote_Files.Write_Store_Text (Generation_Name (To.Number), Record_Text (To) & LF, Status);
      if Status /= W.OK then
         return;
      end if;
      Switch_To (From, To, Grace, Health, Item, Status);
   end Activate;

   procedure Rollback
     (Grace, Health : Unsigned_32; Item : out Report; Status : out W.Status)
   is
      Current, Latest : W.Generation;
      From, To : Generation_Value;
   begin
      Item := (others => <>);
      if not Valid_Deadlines (Grace, Health) then
         Status := W.Malformed;
         return;
      end if;
      Numbers (Current, Latest, Status);
      if Status /= W.OK then
         return;
      end if;
      Item.Current := Current;
      if Current = W.No_Generation then
         Item.Result := W.No_Previous_Generation;
         return;
      end if;
      Read_Generation (Current, From, Status);
      if Status = W.OK then
         Read_Generation (From.Previous, To, Status);
      end if;
      if Status /= W.OK then
         return;
      end if;
      Switch_To (From, To, Grace, Health, Item, Status);
   end Rollback;

   procedure List (Output : out W.Byte_Array; Length : out Natural; Status : out W.Status) is
      Current, Latest : W.Generation;
      Text : String (1 .. Record_Bytes);
      Got : Natural;

      procedure Add (Line : String) is
         Kept : constant Natural := Natural'Min (Line'Length, Output'Length - Length);
      begin
         for I in 1 .. Kept loop
            Output (Length + I) := Character'Pos (Line (Line'First + I - 1));
         end loop;
         Length := Length + Kept;
      end Add;
   begin
      Output := [others => 0];
      Length := 0;
      Numbers (Current, Latest, Status);
      if Status /= W.OK then
         return;
      end if;
      for N in 1 .. Latest loop
         Remote_Files.Read_Store_Text (Generation_Name (N), Text, Got, Status);
         if Status /= W.OK then
            return;
         end if;
         Add (Text (1 .. Got));
      end loop;
      Add ("current " & Decimal (Unsigned_64 (Current)) & LF);
   end List;

   procedure Encode (Item : Report; Data : in out W.Byte_Array) is
   begin
      Data (1) := W.Generation_Result'Enum_Rep (Item.Result);
      Data (2) := Item.Detail;
      W.Put_U32 (Data, 3, Item.Current);
      W.Put_U32 (Data, 7, Item.Target);
      for I in W.Digest_Index loop
         Data (10 + I) := Item.Pin (I);
      end loop;
      W.Put_U64 (Data, 11 + W.Digest_Bytes, Item.Running);
   end Encode;

   function Image (Item : Report) return String is
      Service : constant String := Item.Service (1 .. Item.Service_Length);
      function Gen (N : W.Generation) return String is ("generation " & Decimal (Unsigned_64 (N)));
   begin
      return (if Service'Length > 0 then Service & ": " else "") &
        (case Item.Result is
            when W.Activated => Gen (Item.Current) & " active",
            when W.Rolled_Back_Generation => Gen (Item.Target) &
              " failed its health window and was rolled back; " & Gen (Item.Current) & " stays",
            when W.Refused => "procmgr refused the switch (" &
              (if Item.Detail = SR.Refusal'Enum_Rep (SR.Switch_Off)
               then "Allow app replacement is off on the device"
               elsif Item.Detail = SR.Refusal'Enum_Rep (SR.Not_Replaceable)
               then "its restart contract is reboot: not switched live"
               elsif Item.Detail = SR.Refusal'Enum_Rep (SR.Manifest_Changed)
               then "its manifest differs from the image's"
               else "refusal" & Item.Detail'Image) & ")",
            when W.Stop_Failed_Generation => "the running instance did not end; nothing changed",
            when W.Rollback_Failed => "FAILED: the new program failed and the one before did not start",
            when W.Unchanged => Gen (Item.Current) & " current; nothing to switch",
            when W.No_Previous_Generation => "the image's generation has none before it",
            when W.Too_Many_Changes => "the generations differ in more than one pin") &
        (if Item.Running /= 0 then " (pid" & Item.Running'Image & ")" else "");
   end Image;
end Remote_Generations;
