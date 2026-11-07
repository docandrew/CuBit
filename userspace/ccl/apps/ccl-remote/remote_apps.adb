pragma Ada_2022;
with Interfaces;
with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Messages;
with CuBit.Program_Descriptions;
with Remote_Files;
with Startup;

package body Remote_Apps is
   use Interfaces;
   use type W.Status;
   use type W.Digest;
   use type W.Instance_State;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Launching.Control_Result;
   use type CuBit.Launch_Arguments.Launch_Failure;
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;

   function Launcher return CuBit.Launching.Launcher is
     (CuBit.Launching.Using (Startup.Process_Manager));

   --  The volume the manifest's staging directory names.
   System_Prefix : constant String := "@system/";
   Generations_Directory : constant String := "generations";
   State_Name : constant String := "state";
   --  Between two looks at a stopping instance.
   Poll_Ms : constant := 20;

   Root_Bytes : constant := 64;
   Work_Root : String (1 .. Root_Bytes) := [others => ' '];
   Work_Length : Natural range 0 .. Root_Bytes := 0;

   subtype App_Name_Length is Natural range 0 .. W.Maximum_App_Name_Bytes;
   type App_Record is record
      Used   : Boolean := False;
      Name   : String (1 .. W.Maximum_App_Name_Bytes) := [others => ' '];
      Length : App_Name_Length := 0;
      --  The newest generation that ran (the next replace makes Latest + 1).
      Latest : W.Generation := W.No_Generation;
      State  : Report;
      Started : CuBit.Launching.Child;
   end record;
   subtype App_Count is Natural range 0 .. Maximum_Apps;
   subtype App_Index is Positive range 1 .. Maximum_Apps;
   Apps : array (App_Index) of App_Record;

   function Now return Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));
   procedure Pause is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, Poll_Ms);
   end Pause;

   procedure Initialize (Work : String) is
   begin
      Work_Length := Natural'Min (Work'Length, Root_Bytes);
      Work_Root (1 .. Work_Length) := Work (Work'First .. Work'First + Work_Length - 1);
   end Initialize;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Directory (App : String) return String is (Generations_Directory & "/" & App);
   function Generation_Name (App : String; N : W.Generation) return String is
     (Directory (App) & "/" & Decimal (Unsigned_64 (N)) & W.Product_Suffix);
   --  The name procmgr launches it by.
   function Program (App : String; N : W.Generation) return String is
     (Work_Root (1 .. Work_Length) & "/" & Generation_Name (App, N));

   ---------------------------------------------------------------------------
   --  The state file
   ---------------------------------------------------------------------------
   Hex_Digits : constant String (1 .. 16) := "0123456789abcdef";
   Hex_Length : constant := 2 * W.Digest_Bytes;

   function Hex (Item : W.Digest) return String is
      Text : String (1 .. Hex_Length);
   begin
      for I in W.Digest_Index loop
         Text (2 * I - 1) := Hex_Digits (Natural (Item (I) / 16) + 1);
         Text (2 * I) := Hex_Digits (Natural (Item (I) mod 16) + 1);
      end loop;
      return Text;
   end Hex;

   Latest_Key   : constant String := "latest ";
   Current_Key  : constant String := "current ";
   Previous_Key : constant String := "previous ";
   LF : constant Character := ASCII.LF;

   function State_Text (A : App_Record) return String is
     (Latest_Key & Decimal (Unsigned_64 (A.Latest)) & LF &
      Current_Key & Decimal (Unsigned_64 (A.State.Current)) & " " & Hex (A.State.Current_Hash) & LF &
      Previous_Key & Decimal (Unsigned_64 (A.State.Previous)) & " " & Hex (A.State.Previous_Hash) & LF);

   --  Reads State_Text's three lines back; Valid only for exactly that form.
   procedure Parse_State (Text : String; A : in out App_Record; Valid : out Boolean) is
      Position : Natural := Text'First;

      procedure Number (Value : out W.Generation; Ok : in out Boolean) is
         Taken : Unsigned_64 := 0;
         Start : constant Natural := Position;
      begin
         Value := W.No_Generation;
         while Ok and then Position <= Text'Last and then Text (Position) in '0' .. '9' loop
            if Taken > (Unsigned_64 (Unsigned_32'Last) - 9) / 10 then
               Ok := False;
            else
               Taken := Taken * 10 + Unsigned_64 (Character'Pos (Text (Position)) - Character'Pos ('0'));
               Position := Position + 1;
            end if;
         end loop;
         Ok := Ok and then Position > Start;
         if Ok then
            Value := W.Generation (Taken);
         end if;
      end Number;

      procedure Expect (Word : String; Ok : in out Boolean) is
      begin
         if Ok and then Position <= Text'Last and then Text'Last - Position + 1 >= Word'Length
           and then Text (Position .. Position + Word'Length - 1) = Word
         then
            Position := Position + Word'Length;
         else
            Ok := False;
         end if;
      end Expect;

      procedure Digest (Value : out W.Digest; Ok : in out Boolean) is
         function Nibble (C : Character) return Unsigned_8 is
           (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
            else Character'Pos (C) - Character'Pos ('a') + 10);
      begin
         Value := [others => 0];
         if not Ok or else Text'Last - Position + 1 < Hex_Length then
            Ok := False;
            return;
         end if;
         for I in W.Digest_Index loop
            declare
               High : constant Character := Text (Position + 2 * I - 2);
               Low : constant Character := Text (Position + 2 * I - 1);
            begin
               if High not in '0' .. '9' | 'a' .. 'f' or else Low not in '0' .. '9' | 'a' .. 'f' then
                  Ok := False;
                  return;
               end if;
               Value (I) := Nibble (High) * 16 + Nibble (Low);
            end;
         end loop;
         Position := Position + Hex_Length;
      end Digest;

      Ok : Boolean := True;
      Latest, Current, Previous : W.Generation;
      Current_Hash, Previous_Hash : W.Digest;
   begin
      Expect (Latest_Key, Ok);
      Number (Latest, Ok);
      Expect ([LF], Ok);
      Expect (Current_Key, Ok);
      Number (Current, Ok);
      Expect (" ", Ok);
      Digest (Current_Hash, Ok);
      Expect ([LF], Ok);
      Expect (Previous_Key, Ok);
      Number (Previous, Ok);
      Expect (" ", Ok);
      Digest (Previous_Hash, Ok);
      Expect ([LF], Ok);
      Valid := Ok and then Position = Text'Last + 1
        and then Current <= Latest and then Previous <= Latest
        and then (Current /= W.No_Generation or else Previous = W.No_Generation);
      if Valid then
         A.Latest := Latest;
         A.State.Current := Current;
         A.State.Previous := Previous;
         A.State.Current_Hash := Current_Hash;
         A.State.Previous_Hash := Previous_Hash;
      end if;
   end Parse_State;

   State_Bytes : constant := 256;

   procedure Save (A : App_Record; Status : out W.Status) is
      App : constant String := A.Name (1 .. A.Length);
   begin
      Remote_Files.Write_Work_Text (Directory (App) & "/" & State_Name, State_Text (A), Status);
   end Save;

   --  The slot of App: the one in use, else a fresh one read back from its
   --  state file (Known: the file existed). Status Capacity when every
   --  slot is in use; Failed for a state file not in State_Text's form.
   procedure Find (App : String; Index : out App_Count; Known : out Boolean; Status : out W.Status) is
      Text : String (1 .. State_Bytes);
      Length : Natural;
      Read : W.Status;
      Valid : Boolean;
   begin
      Index := 0;
      Known := False;
      Status := W.OK;
      for I in App_Index loop
         if Apps (I).Used and then Apps (I).Name (1 .. Apps (I).Length) = App then
            Index := I;
            Known := True;
            return;
         end if;
      end loop;
      for I in App_Index loop
         if not Apps (I).Used then
            Index := I;
            exit;
         end if;
      end loop;
      if Index = 0 then
         Status := W.Capacity;
         return;
      end if;
      Remote_Files.Read_Work_Text (Directory (App) & "/" & State_Name, Text, Length, Read);
      declare
         Fresh : App_Record;
      begin
         Fresh.Length := App'Length;
         Fresh.Name (1 .. App'Length) := App;
         if Read = W.OK then
            Parse_State (Text (1 .. Length), Fresh, Valid);
            if not Valid then
               Status := W.Failed;
               Index := 0;
               return;
            end if;
            Known := True;
         elsif Read /= W.Not_Found then
            Status := Read;
            Index := 0;
            return;
         end if;
         Fresh.Used := True;
         Apps (Index) := Fresh;
      end;
   end Find;

   ---------------------------------------------------------------------------
   --  Instances
   ---------------------------------------------------------------------------
   --  Whether A's instance has ended on its own since it was last looked at.
   procedure Refresh (A : in out App_Record) is
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean;
   begin
      if A.State.Instance = W.Running then
         CuBit.Launching.Poll_Exit (A.Started, Has_Ended, Ended);
         if Has_Ended then
            A.State.Instance := W.Exited;
         end if;
      end if;
   end Refresh;

   --  Stopping the running instance, as an explicit machine: Asking (Stop
   --  sent, again while the child is Busy), Waiting (for its exit until
   --  the stop deadline), Ending (procmgr asked to end it), Waiting_End
   --  (for its exit until the end deadline); then Gone or Stuck.
   type Stop_Phase is (Asking, Waiting, Ending, Waiting_End, Gone, Stuck);

   procedure Stop (A : in out App_Record; How : out W.Stop_Kind; Stopped : out Boolean) is
      Phase : Stop_Phase := Asking;
      Stop_Deadline : constant Unsigned_64 := Now + Stop_Deadline_Ms;
      End_Deadline : Unsigned_64 := 0;
      Kind : W.Stop_Kind := W.Stopped;
      Sent : CuBit.Launching.Control_Result;
      Ended_Child : CuBit.Launching.Launch_Result;
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean;
   begin
      Refresh (A);
      if A.State.Instance /= W.Running then
         How := W.None_Running;
         Stopped := True;
         return;
      end if;
      while Phase not in Gone | Stuck loop
         case Phase is
            when Asking =>
               CuBit.Launching.Send_Control (A.Started, CuBit.Control_Events.Stop, Sent);
               case Sent is
                  when CuBit.Launching.Sent => Phase := Waiting;
                  when CuBit.Launching.Busy =>
                     if Now >= Stop_Deadline then
                        Phase := Ending;
                     else
                        Pause;
                     end if;
                  when CuBit.Launching.Refused =>
                     --  It ended meanwhile, or no longer takes control.
                     Phase := Waiting;
               end case;
            when Waiting | Waiting_End =>
               CuBit.Launching.Poll_Exit (A.Started, Has_Ended, Ended);
               if Has_Ended then
                  Phase := Gone;
               elsif Phase = Waiting and then Now >= Stop_Deadline then
                  Phase := Ending;
               elsif Phase = Waiting_End and then Now >= End_Deadline then
                  Phase := Stuck;
               else
                  Pause;
               end if;
            when Ending =>
               Kind := W.Ended;
               CuBit.Launching.End_Child (Launcher, A.Started, Ended_Child);
               End_Deadline := Now + End_Deadline_Ms;
               Phase := (if Ended_Child = CuBit.Launching.Launched then Waiting_End else Stuck);
               if Phase = Stuck then
                  --  Refused: it may have ended just now.
                  CuBit.Launching.Poll_Exit (A.Started, Has_Ended, Ended);
                  Phase := (if Has_Ended then Gone else Stuck);
               end if;
            when Gone | Stuck =>
               null;
         end case;
      end loop;
      Stopped := Phase = Gone;
      How := (if Stopped then Kind else W.None_Running);
      if Stopped then
         A.State.Instance := W.Exited;
      end if;
   end Stop;

   --  Start generation N of A; Started reports whether procmgr did.
   procedure Start (A : in out App_Record; N : W.Generation; Started : out Boolean;
                    Failure : out LA.Launch_Failure) is
      App : constant String := A.Name (1 .. A.Length);
      Name : constant String := Program (App, N);
      Block : LA.Builder;
      Length : LA.Present_Length;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Added : Boolean;
      Launched : CuBit.Launching.Launch_Result;
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, Name, Added);
      LA.Finish (Block, Length, Added);
      LG.Start (Places);
      LG.Finish (Places, Region, Region_Length);
      CuBit.Launching.Launch
        (Launcher, Name, Block.Data (1 .. Length), Region (1 .. Region_Length),
         A.Started, Launched, Failure);
      Started := Launched = CuBit.Launching.Launched;
      A.State.Instance := (if Started then W.Running else W.Exited);
   end Start;

   --  Whether procmgr would admit generation N (OP_DESCRIBE runs the
   --  launch's admission, the Desktop's switch included): Switched_Off when
   --  it refused as not granted.
   procedure Admitted (App : String; N : W.Generation; Yes, Switched_Off : out Boolean) is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
   begin
      CuBit.Launching.Describe (Launcher, Program (App, N), Descriptor, Length, Result, Failure);
      Yes := Result = CuBit.Launching.Launched;
      Switched_Off := Result = CuBit.Launching.Refused and then Failure = LA.Not_Granted;
   end Admitted;

   --  Swap A's running instance for generation N (Hash its SHA-256), the
   --  common part of Replace and Rollback. Success: N runs.
   procedure Swap (A : in out App_Record; N : W.Generation; Item : out Report; Success : out Boolean) is
      App : constant String := A.Name (1 .. A.Length);
      Yes, Switched_Off, Stopped, Started, Restarted : Boolean;
      How : W.Stop_Kind;
      Ignore : LA.Launch_Failure;
      Was_Running : Boolean;
   begin
      Success := False;
      Refresh (A);
      Item := A.State;
      Admitted (App, N, Yes, Switched_Off);
      if not Yes then
         Item.Result := (if Switched_Off then W.Switch_Off else W.Start_Failed);
         return;
      end if;
      Was_Running := A.State.Instance = W.Running;
      Stop (A, How, Stopped);
      if not Stopped then
         Item := A.State;
         Item.Result := W.Stop_Failed;
         return;
      end if;
      Start (A, N, Started, Ignore);
      if not Started then
         if Was_Running then
            --  Its predecessor again, as it was.
            Start (A, A.State.Current, Restarted, Ignore);
         end if;
         Item := A.State;
         Item.Result := W.Start_Failed;
         Item.Stop := How;
         return;
      end if;
      Item := A.State;
      Item.Stop := How;
      Success := True;
   end Swap;

   ---------------------------------------------------------------------------
   --  Operations
   ---------------------------------------------------------------------------
   function Usable return Boolean is
     (Work_Length > System_Prefix'Length
      and then Work_Root (1 .. System_Prefix'Length) = System_Prefix);

   procedure Replace (Product : String; Item : out Report; Status : out W.Status) is
      App : constant String := W.App_Of_Product (Product);
      Index : App_Count;
      Known, Success : Boolean;
      N : W.Generation;
      Size : Unsigned_64;
      Hash : W.Digest;
   begin
      Item := (others => <>);
      if not Usable then
         Status := W.Unavailable;
         return;
      elsif App'Length = 0 then
         Status := W.Malformed;
         return;
      end if;
      Find (App, Index, Known, Status);
      if Status /= W.OK then
         return;
      end if;
      declare
         A : App_Record renames Apps (Index);
      begin
         if A.Latest = W.Generation'Last then
            Status := W.Capacity;
            return;
         end if;
         N := A.Latest + 1;
         Remote_Files.Make_Work_Directory (Generations_Directory, Status);
         if Status = W.OK then
            Remote_Files.Make_Work_Directory (Directory (App), Status);
         end if;
         if Status = W.OK then
            Remote_Files.Copy_To_Work (W.Workspace, Product, Generation_Name (App, N), Size, Hash, Status);
         end if;
         if Status /= W.OK then
            if not Known then
               A.Used := False;
            end if;
            return;
         end if;
         Swap (A, N, Item, Success);
         if Success then
            A.Latest := N;
            declare
               How : constant W.Stop_Kind := Item.Stop;
            begin
               A.State.Previous := A.State.Current;
               A.State.Previous_Hash := A.State.Current_Hash;
               A.State.Current := N;
               A.State.Current_Hash := Hash;
               Item := A.State;
               Item.Stop := How;
               Item.Result := W.Replaced;
            end;
         end if;
         --  Generation N counts once it ran; a failed attempt's copy is
         --  overwritten by the next one.
         if Success then
            Save (A, Status);
         end if;
      end;
   end Replace;

   procedure Rollback (App : String; Item : out Report; Status : out W.Status) is
      Index : App_Count;
      Known, Success : Boolean;
      Size : Unsigned_64;
      Hash : W.Digest;
      Back : W.Generation;
      Back_Hash : W.Digest;
   begin
      Item := (others => <>);
      if not Usable then
         Status := W.Unavailable;
         return;
      end if;
      Find (App, Index, Known, Status);
      if Status /= W.OK then
         return;
      elsif not Known then
         Apps (Index).Used := False;
         Status := W.Not_Found;
         return;
      end if;
      declare
         A : App_Record renames Apps (Index);
      begin
         Refresh (A);
         Item := A.State;
         if A.State.Previous = W.No_Generation then
            Item.Result := W.No_Previous;
            return;
         end if;
         Back := A.State.Previous;
         Back_Hash := A.State.Previous_Hash;
         Remote_Files.Hash_Work (Generation_Name (App, Back), Size, Hash, Status);
         if Status = W.OK and then Hash /= Back_Hash then
            Status := W.Integrity_Mismatch;
         end if;
         if Status /= W.OK then
            return;
         end if;
         Swap (A, Back, Item, Success);
         if Success then
            A.State.Previous := A.State.Current;
            A.State.Previous_Hash := A.State.Current_Hash;
            A.State.Current := Back;
            A.State.Current_Hash := Back_Hash;
            declare
               How : constant W.Stop_Kind := Item.Stop;
            begin
               Item := A.State;
               Item.Stop := How;
               Item.Result := W.Rolled_Back;
            end;
            Save (A, Status);
         end if;
      end;
   end Rollback;

   procedure Query (App : String; Item : out Report; Status : out W.Status) is
      Index : App_Count;
      Known : Boolean;
   begin
      Item := (others => <>);
      Find (App, Index, Known, Status);
      if Status /= W.OK then
         return;
      elsif not Known then
         Apps (Index).Used := False;
         Status := W.Not_Found;
         return;
      end if;
      Refresh (Apps (Index));
      Item := Apps (Index).State;
      Item.Result := W.Reported;
      Item.Stop := W.None_Running;
   end Query;

   procedure Encode (Item : Report; Data : in out W.Byte_Array) is
   begin
      Data (1 .. W.App_Response_Bytes) := W.Encode_App_Response
        (Item.Result, Item.Current, Item.Previous, Item.Instance, Item.Stop,
         Item.Current_Hash, Item.Previous_Hash);
   end Encode;

   function Image (App : String; Item : Report) return String is
      function Gen (N : W.Generation) return String is ("generation " & Decimal (Unsigned_64 (N)));
      Instance : constant String :=
        (case Item.Instance is
            when W.Not_Started => "not started", when W.Running => "running",
            when W.Exited => "exited");
      Previous : constant String :=
        (if Item.Previous = W.No_Generation then ""
         else "; previous " & Gen (Item.Previous) &
              (case Item.Stop is
                  when W.None_Running => "",
                  when W.Stopped => " stopped",
                  when W.Ended => " ended after the stop deadline"));
   begin
      return App & " " &
        (case Item.Result is
            when W.Replaced | W.Rolled_Back | W.Reported =>
              (if Item.Current = W.No_Generation then "no generation"
               else Gen (Item.Current) & " " & Instance) & Previous,
            when W.Switch_Off => "refused: Allow app replacement is off on the device",
            when W.Start_Failed => "the new generation did not start; " &
              (if Item.Current = W.No_Generation then "nothing runs"
               else Gen (Item.Current) & " " & Instance),
            when W.Stop_Failed => Gen (Item.Current) & " did not stop; nothing started",
            when W.No_Previous => "has no previous generation");
   end Image;
end Remote_Apps;
