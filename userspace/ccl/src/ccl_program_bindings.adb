with Ada.Unchecked_Conversion;
with CCL.Interfaces.Programs;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Streams;
with CCL_Launcher;
with CuBit.Failures;
with CuBit.Program_Descriptions;

package body CCL_Program_Bindings is
   use Interfaces;
   --  A process identity as a CCL Integer: the same 64 bits (it is
   --  opaque; only compared, never computed with).
   function Pid_Value is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type CCL.Streams.Handle;
   use type CCL_Launcher.Start_Result;
   package Programs renames CCL.Interfaces.Programs;
   package PD renames CuBit.Program_Descriptions;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;
   use type PD.Kind;
   use type PD.Connector_Direction;
   use type PD.Element_Kind;
   use type Programs.Program_State;

   --  The programs published, by binding program index. An index keeps
   --  its program across refreshes while the program is still there, and a
   --  program that went away keeps its index (not published) while a run
   --  this session follows refers to it.
   type Program_Slot is record
      Item : CCL_Launcher.Program;     --  Length 0: free
      Published : Boolean := False;
   end record;
   Slots : array (Programs.Program_Index) of Program_Slot;
   Bound : array (Programs.Program_Index) of Programs.Contracts;
   Directory : Programs.Directory_Contracts;

   --  The last look at what this process may launch (programs.refresh, or
   --  the first Install), each with the state programs.refresh reports.
   Found : CCL_Launcher.Program_Array;
   Found_Count : CCL_Launcher.Program_Count := 0;
   Found_More : Boolean := False;
   Found_State : array (1 .. CCL_Launcher.MAXIMUM_PROGRAMS) of Programs.Program_State :=
     [others => Programs.Ready];
   --  Install publishes Found as it is instead of looking again.
   Pending_Refresh : Boolean := False;

   --  A run this session follows: its streams by connector, and its
   --  outcome (a task).
   type Handle_Array is array (PD.Connector_Index) of CCL.Streams.Handle;
   --  A run that ended keeps its streams and outcome (pinned, readable)
   --  until its slot is needed for a new run: the oldest ended run goes
   --  first. Slots are this session's, not the launcher's, which reuses
   --  its own as soon as a run is over.
   type Followed is record
      Active : Boolean := False;
      Ended : Boolean := False;
      --  Start order, to find the oldest.
      Sequence : Interfaces.Unsigned_64 := 0;
      Program : Programs.Program_Index := 0;
      Started : CCL_Launcher.Run;
      Outlet_Streams : Handle_Array := [others => CCL.Streams.No_Handle];
      Outcome : CCL.Streams.Handle := CCL.Streams.No_Handle;
   end record;
   --  Each run pins a stream per outlet and its outcome in the session's
   --  table (CCL_Stream_Table.MAX_STREAMS), so a few are kept, not all
   --  the launcher can run at once.
   MAXIMUM_FOLLOWED : constant := 8;
   subtype Followed_Index is Positive range 1 .. MAXIMUM_FOLLOWED;
   Runs : array (Followed_Index) of Followed;
   Next_Sequence : Interfaces.Unsigned_64 := 1;

   --  A slot for a new run: a free one, else the oldest that ended. Every
   --  one still running: the oldest, which the caller must not take.
   function Free_Slot return Followed_Index is
      Best : Followed_Index := Followed_Index'First;
      function Rank (F : Followed) return Natural is
        (if not F.Active then 0 elsif F.Ended then 1 else 2);
   begin
      for I in Runs'Range loop
         if Rank (Runs (I)) < Rank (Runs (Best))
           or else (Rank (Runs (I)) = Rank (Runs (Best)) and then Runs (I).Sequence < Runs (Best).Sequence)
         then
            Best := I;
         end if;
      end loop;
      return Best;
   end Free_Slot;

   Started : Started_Array;
   Started_Count : Natural range 0 .. MAXIMUM_STARTED := 0;

   procedure Note_Started
     (Kind : Started_Kind; Launched : String; Run : CCL_Launcher.Run; Outlet : String;
      Integers : Boolean; Stream : CCL.Streams.Handle)
   is
      Program : constant String := Programs.Interface_Name (Launched);
   begin
      if Started_Count < MAXIMUM_STARTED and then Launched'Length <= MAXIMUM_NAME
        and then Outlet'Length <= MAXIMUM_NAME
      then
         Started_Count := Started_Count + 1;
         Started (Started_Count) :=
           (Kind => Kind, Program_Length => Program'Length, Launched_Length => Launched'Length,
            Pid => Pid_Value (Run.Process),
            Outlet_Length => Outlet'Length, Integers => Integers, Stream => Integer_64 (Stream),
            others => <>);
         Started (Started_Count).Program (1 .. Program'Length) := Program;
         Started (Started_Count).Launched (1 .. Launched'Length) := Launched;
         Started (Started_Count).Outlet (1 .. Outlet'Length) := Outlet;
      end if;
   end Note_Started;

   procedure Take_Started (Items : out Started_Array; Count : out Natural) is
   begin
      Items := Started;
      Count := Started_Count;
      Started_Count := 0;
   end Take_Started;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in Programs.REFRESH_BINDING | Programs.CHECK_BINDING
      or else (Programs.Handles (Binding) and then Slots (Programs.Program_Of (Binding)).Published));

   function Refresh_Requested return Boolean is (Pending_Refresh);

   procedure Set_Why (Item : in out CCL_Launcher.Program; Text : String) is
      Kept : constant Natural := Natural'Min (Text'Length, CCL_Launcher.MAXIMUM_WHY);
   begin
      Item.Why := [others => ' '];
      Item.Why (1 .. Kept) := Text (Text'First .. Text'First + Kept - 1);
      Item.Why_Length := Kept;
   end Set_Why;

   function Name_Of (Item : CCL_Launcher.Program) return String is (Item.Name (1 .. Item.Length));

   --  Look again: the launch table's programs and the staged ones.
   procedure Scan is
      use type CCL_Launcher.Program_State;
   begin
      CCL_Launcher.Programs (Found, Found_Count, Found_More);
      for I in 1 .. Found_Count loop
         Found_State (I) :=
           (case Found (I).State is
               when CCL_Launcher.Ready => Programs.Ready,
               when CCL_Launcher.Refused => Programs.Refused,
               when CCL_Launcher.Not_Described => Programs.Not_Described);
         --  Two programs of one name (a table program and a build of it,
         --  or two builds): the first keeps it, as Install would.
         if Found (I).State = CCL_Launcher.Ready then
            declare
               Name : constant String := Programs.Interface_Name (Name_Of (Found (I)));
            begin
               if Name = Programs.DIRECTORY_INTERFACE then
                  Found_State (I) := Programs.Not_Published;
               end if;
               for J in 1 .. I - 1 loop
                  if Found_State (J) = Programs.Ready
                    and then Programs.Interface_Name (Name_Of (Found (J))) = Name
                  then
                     Found_State (I) := Programs.Not_Published;
                  end if;
               end loop;
               if Found_State (I) = Programs.Not_Published then
                  Set_Why (Found (I), "another program is published as " & Name);
               end if;
            end;
         end if;
      end loop;
   end Scan;

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Taken : array (Programs.Program_Index) of Boolean := [others => False];
      Placed : array (1 .. CCL_Launcher.MAXIMUM_PROGRAMS) of Boolean := [others => False];

      --  A run this session follows refers to the program at Index.
      function Referenced (Index : Programs.Program_Index) return Boolean is
        (for some F of Runs => F.Active and then F.Program = Index);

      --  Publish Found (I) at Index. One whose interface does not publish
      --  (a name that is not a CCL name, a clash) is left out; the others
      --  still appear.
      procedure Place (Index : Programs.Program_Index; I : Positive) is
         Kept : constant Programs.Contracts := Bound (Index);
      begin
         Taken (Index) := True;
         Placed (I) := True;
         Programs.Publish
           (Catalog, Grants, Index, Name_Of (Found (I)), Found (I).Description, Bound (Index), Error);
         if Error = CCL.Catalog.Catalog_Valid then
            Slots (Index) := (Item => Found (I), Published => True);
         else
            Bound (Index) := Kept;
            Taken (Index) := False;
            Found_State (I) := Programs.Not_Published;
            Set_Why (Found (I), "its interface " & Programs.Interface_Name (Name_Of (Found (I))) &
                     " does not publish (" & Error'Image & "): not a CCL name, or one another " &
                     "interface holds");
         end if;
      end Place;
   begin
      Programs.Publish_Directory (Catalog, Grants, Directory, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      if not Pending_Refresh then
         Scan;
      end if;
      Pending_Refresh := False;
      for S of Slots loop
         S.Published := False;
      end loop;
      --  A program still there keeps its index.
      for I in 1 .. Found_Count loop
         if Found_State (I) = Programs.Ready then
            for Index in Programs.Program_Index loop
               if not Taken (Index) and then Slots (Index).Item.Length > 0
                 and then Name_Of (Slots (Index).Item) = Name_Of (Found (I))
               then
                  Place (Index, I);
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      --  A new one takes a free index, or one whose program went away and
      --  no followed run refers to.
      for I in 1 .. Found_Count loop
         if Found_State (I) = Programs.Ready and then not Placed (I) then
            for Index in Programs.Program_Index loop
               if not Taken (Index)
                 and then (Slots (Index).Item.Length = 0 or else not Referenced (Index))
               then
                  Place (Index, I);
                  exit;
               end if;
            end loop;
            if not Placed (I) then
               Found_State (I) := Programs.Not_Published;
               Set_Why (Found (I), "every program index is taken");
            end if;
         end if;
      end loop;
      for Index in Programs.Program_Index loop
         if not Taken (Index) and then not Referenced (Index) then
            Slots (Index) := (others => <>);
         end if;
      end loop;
   end Install;

   function Program_At (Index : Programs.Program_Index) return CCL_Launcher.Program is
     (Slots (Index).Item);

   --  The call's values from a captured parameters record: its fields are
   --  the parameters in order.
   procedure Read_Values
     (Object : Views.Snapshot; Description : PD.Signature; V : out PD.Values;
      Complete : out Boolean)
   is
      Added : Boolean := True;
      procedure Put (P : PD.Parameter_Index; Text : String) is
      begin
         if Added then
            PD.Add (V, P, Text, Added);
         end if;
      end Put;
      --  A file value is a record holding its name; text is a String.
      function Value_Text (Kind : PD.Kind; At_Cursor : Views.Cursor) return String is
        (if Kind = PD.Text then Views.Text (Object, At_Cursor)
         else Views.Text (Object, Views.Field (Object, At_Cursor, Programs.FILE_FIELDS)));
   begin
      PD.Clear (V);
      for P in 0 .. Description.Parameter_Total - 1 loop
         declare
            Param : PD.Parameter renames Description.Parameters (P);
            Field : constant Views.Cursor := Views.Field (Object, Views.Root (Object), P + 1);
         begin
            if Param.Of_Kind = PD.Flag then
               if Views.Scalar (Object, Field).First = 1 then
                  Put (P, "");
               end if;
            elsif Param.Many or else Param.Optional then
               for E in 1 .. Views.Length (Object, Field) loop
                  Put (P, Value_Text (Param.Of_Kind, Views.Element (Object, Field, E)));
               end loop;
            else
               Put (P, Value_Text (Param.Of_Kind, Field));
            end if;
         end;
      end loop;
      Complete := Added;
   end Read_Values;

   procedure Run_Program
     (Index : Programs.Program_Index; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table; Reply : in out CCL.Host_Values.Call_Result)
   is
      Item : constant CCL_Launcher.Program := Program_At (Index);
      Name : constant String := Item.Name (1 .. Item.Length);
      Object : Views.Snapshot;
      Captured, Complete : Boolean := False;
      V : PD.Values;
      Started : CCL_Launcher.Run;
      Result : CCL_Launcher.Start_Result;
      Why : String (1 .. 160);
      Why_Length : Natural;
      F : Followed;
      Image : CCL.Objects.Image;
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      Slot : constant Followed_Index := Free_Slot;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound (Index).Parameters, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument,
           "the parameters are not " & Programs.Parameters_Type (Name));
         return;
      end if;
      Read_Values (Object, Item.Description, V, Complete);
      if not Complete then
         Reply.Why := Failures.Failed (Failures.Exhausted, "the parameters do not fit one launch");
         return;
      end if;
      --  A run this session cannot follow is not started.
      if Runs (Slot).Active and then not Runs (Slot).Ended then
         Reply.Why := Failures.Failed
           (Failures.Exhausted, "this session already follows" & Natural'Image (MAXIMUM_FOLLOWED) &
            " running programs", "wait for one to end");
         return;
      end if;
      CCL_Launcher.Start (Name, Item.Description, V, Started, Result, Why, Why_Length);
      if Result /= CCL_Launcher.Started then
         Reply.Why := Failures.Failed
           ((if Result = CCL_Launcher.Refused then Failures.Not_Granted else Failures.Unavailable),
            "starting " & Name & ": " & Why (1 .. Why_Length));
         return;
      end if;
      --  Its streams, one per outlet, and its outcome; pinned until the
      --  slot starts another run, so they are there when the console
      --  binds them.
      F := (Active => True, Program => Index, Started => Started, others => <>);
      for P in 0 .. Item.Description.Connector_Total - 1 loop
         if Item.Description.Connectors (P).Direction = PD.Outlet then
            CCL_Stream_Table.Open_Outlet
              (Streams,
               (if Item.Description.Connectors (P).Element = PD.Integers
                then CCL_Stream_Table.Integer_Elements else CCL_Stream_Table.Text_Elements),
               F.Outlet_Streams (P), Pinned => True);
         end if;
      end loop;
      CCL_Stream_Table.Open_Task (Streams, F.Outcome, Pinned => True);
      for P in 0 .. Item.Description.Connector_Total - 1 loop
         if F.Outlet_Streams (P) /= CCL.Streams.No_Handle then
            Note_Started
              (Outlet_Started, Name, Started,
               Item.Description.Connectors (P).Name (1 .. Item.Description.Connectors (P).Name_Length),
               Item.Description.Connectors (P).Element = PD.Integers, F.Outlet_Streams (P));
         end if;
      end loop;
      if F.Outcome /= CCL.Streams.No_Handle then
         Note_Started (Outcome_Started, Name, Started, "", False, F.Outcome);
      end if;
      --  The run this slot followed before lets go of its streams: Retain
      --  closes them unless a name holds them.
      if Runs (Slot).Active then
         for H of Runs (Slot).Outlet_Streams loop
            if H /= CCL.Streams.No_Handle then
               CCL_Stream_Table.Unpin (Streams, H);
            end if;
         end loop;
         CCL_Stream_Table.Unpin (Streams, Runs (Slot).Outcome);
      end if;
      F.Sequence := Next_Sequence;
      Next_Sequence := Next_Sequence + 1;
      Runs (Slot) := F;
      --  The Run value: its program and incarnation.
      Image := CCL.Objects.Empty (Bound (Index).Run);
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (Programs.RUN_FIELDS), Step);
      if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Name, Step); end if;
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (Pid_Value (Started.Process)), Step);
      end if;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Bound (Index).Run) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := Failures.Failed (Failures.Exhausted, "the run does not fit one result");
      end if;
   end Run_Program;

   --  An outlet accessor or ld.outcome: the stream or task of the run named.
   procedure Run_Stream
     (Index : Programs.Program_Index; Number : Natural; Argument : CCL.Host_Values.Value;
      Reply : in out CCL.Host_Values.Call_Result)
   is
      Item : constant CCL_Launcher.Program := Program_At (Index);
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound (Index).Run, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "that is not a Run");
         return;
      end if;
      declare
         Pid : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 2)));
      begin
         for F of Runs loop
            if F.Active and then F.Program = Index
              and then Pid_Value (F.Started.Process) = Pid
            then
               declare
                  Handle : constant CCL.Streams.Handle :=
                    (if Number = Programs.OUTCOME_NUMBER then F.Outcome
                     elsif Number in 1 .. Item.Description.Connector_Total then F.Outlet_Streams (Number - 1)
                     else CCL.Streams.No_Handle);
               begin
                  if Handle /= CCL.Streams.No_Handle then
                     Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Handle)),
                               Success => True, Why => <>);
                     return;
                  end if;
               end;
            end if;
         end loop;
      end;
      Reply.Why := Failures.Failed
        (Failures.Not_Found, "this session does not follow that run (it ended and was released, " &
         "or another session started it)");
   end Run_Stream;

   --  ld.outlets: the run's outlets with what arrived, what was lost, and
   --  whether they ended.
   procedure Run_Outlets
     (Index : Programs.Program_Index; Argument : CCL.Host_Values.Value;
      Streams : CCL_Stream_Table.Table; Reply : in out CCL.Host_Values.Call_Result)
   is
      Item : constant CCL_Launcher.Program := Program_At (Index);
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Image : CCL.Objects.Image;
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (C : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, C, Step); end if;
      end Put;
      function Total (H : CCL.Streams.Handle; View : CCL.Streams.View_Kind) return Integer_64 is
         Reply_View : CCL.Streams.View_Reply;
      begin
         CCL_Stream_Table.Read (Streams, (Stream => H, View => View, Count => 1), Reply_View);
         return Integer_64 (Reply_View.Total);
      end Total;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound (Index).Run, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "that is not a Run");
         return;
      end if;
      declare
         Pid : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 2)));
      begin
         for F of Runs loop
            if F.Active and then F.Program = Index
              and then Pid_Value (F.Started.Process) = Pid
            then
               declare
                  Count : Natural := 0;
               begin
                  for H of F.Outlet_Streams loop
                     if H /= CCL.Streams.No_Handle then Count := Count + 1; end if;
                  end loop;
                  Image := CCL.Objects.Empty (Bound (Index).Outlet_States);
                  Put (CCL.Objects.Sequence_Cell (Count));
                  for P in 0 .. Item.Description.Connector_Total - 1 loop
                     declare
                        H : constant CCL.Streams.Handle := F.Outlet_Streams (P);
                        Name : constant String :=
                          Item.Description.Connectors (P).Name
                            (1 .. Item.Description.Connectors (P).Name_Length);
                        Signal : constant PD.Signal_Kind := Item.Description.Connectors (P).Signal;
                        --  The type its accessor gives, as code writes it.
                        Stream_Type : constant String :=
                          (if Item.Description.Connectors (P).Element = PD.Integers
                           then "Stream<Integer>" else "Stream<String>");
                     begin
                        if H /= CCL.Streams.No_Handle then
                           Put (CCL.Objects.Product_Cell (Programs.OUTLET_STATE_FIELDS));
                           if Step = CCL.Objects.Added then
                              CCL.Objects.Append_Text (Image, Name, Step);
                           end if;
                           if Step = CCL.Objects.Added then
                              CCL.Objects.Append_Text (Image, Stream_Type, Step);
                           end if;
                           Put (CCL.Objects.Variant_Cell (PD.Signal_Kind'Pos (Signal) + 1));
                           Put (CCL.Objects.Unit_Cell);
                           Put (CCL.Objects.Integer_Cell (Total (H, CCL.Streams.Arrived_View)));
                           Put (CCL.Objects.Integer_Cell (Total (H, CCL.Streams.Lost_View)));
                           Put (CCL.Objects.Boolean_Cell (CCL_Stream_Table.Ended (Streams, H)));
                        end if;
                     end;
                  end loop;
                  if Step = CCL.Objects.Added
                    and then CCL.Objects.Validate (Image, Bound (Index).Outlet_States)
                  then
                     Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True,
                               Why => <>);
                  else
                     Reply.Why := Failures.Failed (Failures.Exhausted, "the outlets do not fit one result");
                  end if;
                  return;
               end;
            end if;
         end loop;
      end;
      Reply.Why := Failures.Failed (Failures.Not_Found, "this session does not follow that run");
   end Run_Outlets;

   --  One Program_Entry's cells: its interface name, its launch name,
   --  whether it is staged, its state and why.
   procedure Put_Entry
     (Image : in out CCL.Objects.Image; Item : CCL_Launcher.Program; State : Programs.Program_State;
      Step : in out CCL.Objects.Build_Result)
   is
      procedure Put (C : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, C, Step); end if;
      end Put;
      procedure Put_Text (Text : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Text, Step); end if;
      end Put_Text;
   begin
      Put (CCL.Objects.Product_Cell (Programs.PROGRAM_ENTRY_FIELDS));
      Put_Text (Programs.Interface_Name (Name_Of (Item)));
      Put_Text (Name_Of (Item));
      Put (CCL.Objects.Boolean_Cell (Item.Staged));
      Put (CCL.Objects.Variant_Cell (Programs.Program_State'Pos (State) + 1));
      Put (CCL.Objects.Unit_Cell);
      Put_Text (Item.Why (1 .. Item.Why_Length));
   end Put_Entry;

   --  programs.refresh: look again, answer what was found, and have the
   --  front end publish it once this entry has ended.
   procedure Refresh (Reply : in out CCL.Host_Values.Call_Result) is
      Image : CCL.Objects.Image := CCL.Objects.Empty (Directory.Program_Entries);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Scan;
      Pending_Refresh := True;
      CCL.Objects.Append (Image, CCL.Objects.Sequence_Cell (Found_Count), Step);
      for I in 1 .. Found_Count loop
         Put_Entry (Image, Found (I), Found_State (I), Step);
      end loop;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Directory.Program_Entries) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := Failures.Failed (Failures.Exhausted, "the programs do not fit one result");
      end if;
   end Refresh;

   --  programs.check: whether this process may start the program named.
   procedure Check (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      use type CCL_Launcher.Program_State;
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Item : CCL_Launcher.Program;
      State : Programs.Program_State;
      Image : CCL.Objects.Image := CCL.Objects.Empty (Directory.Program_Entry);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Directory.Input_File, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "that is not an Input_File");
         return;
      end if;
      CCL_Launcher.Check
        (Views.Text (Object, Views.Field (Object, Views.Root (Object), Programs.FILE_FIELDS)), Item);
      State :=
        (case Item.State is
            when CCL_Launcher.Ready => Programs.Ready,
            when CCL_Launcher.Refused => Programs.Refused,
            when CCL_Launcher.Not_Described => Programs.Not_Described);
      Put_Entry (Image, Item, State, Step);
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Directory.Program_Entry) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := Failures.Failed (Failures.Exhausted, "the answer does not fit one result");
      end if;
   end Check;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table;
      Reply : out CCL.Host_Values.Call_Result) is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      if Binding = Programs.REFRESH_BINDING then
         Refresh (Reply);
      elsif Binding = Programs.CHECK_BINDING then
         Check (Argument, Reply);
      elsif Programs.Number_Of (Binding) = 0 then
         Run_Program (Programs.Program_Of (Binding), Argument, Streams, Reply);
      elsif Programs.Number_Of (Binding) = Programs.OUTLETS_NUMBER then
         Run_Outlets (Programs.Program_Of (Binding), Argument, Streams, Reply);
      else
         Run_Stream (Programs.Program_Of (Binding), Programs.Number_Of (Binding), Argument, Reply);
      end if;
   end Invoke;

   --  The run's outcome task completes with its Run_Outcome:
   --  Finished (Unix_Exit code) or Stopped.
   procedure Complete_Outcome
     (Streams : in out CCL_Stream_Table.Table; F : Followed; How : CCL_Launcher.Ending)
   is
      use type CCL_Launcher.Ending_Kind;
      Image : CCL.Objects.Image := CCL.Objects.Empty (Bound (F.Program).Run_Outcome);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      Completed : Boolean;
      procedure Put (C : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, C, Step); end if;
      end Put;
      function Alternative (A : Programs.Outcome_Alternative) return CCL.Objects.Cell is
        (CCL.Objects.Variant_Cell (Programs.Outcome_Alternative'Pos (A) + 1));
   begin
      if F.Outcome = CCL.Streams.No_Handle then return; end if;
      if How.Kind = CCL_Launcher.Exited then
         Put (Alternative (Programs.Finished));
         Put (CCL.Objects.Product_Cell (Programs.UNIX_EXIT_FIELDS));
         Put (CCL.Objects.Integer_Cell (How.Code));
      else
         Put (Alternative (Programs.Stopped));
         Put (CCL.Objects.Unit_Cell);
      end if;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Bound (F.Program).Run_Outcome) then
         CCL_Stream_Table.Complete_Task (Streams, F.Outcome, Image, Completed);
      end if;
   end Complete_Outcome;

   procedure Pump (Streams : in out CCL_Stream_Table.Table; Arrived : out Boolean) is
   begin
      Arrived := False;
      for F of Runs loop
         if F.Active and then not F.Ended then
            declare
               procedure Deliver (Port : PD.Connector_Index; Line : String) is
                  Pushed : Boolean;
               begin
                  if F.Outlet_Streams (Port) /= CCL.Streams.No_Handle then
                     CCL_Stream_Table.Push_Text (Streams, F.Outlet_Streams (Port), Line, Pushed);
                     Arrived := Arrived or else Pushed;
                  end if;
               end Deliver;
               procedure Poll is new CCL_Launcher.Poll (Deliver);
               Ended : Boolean;
               How : CCL_Launcher.Ending;
            begin
               Poll (F.Started, Ended, How);
               if Ended then
                  Complete_Outcome (Streams, F, How);
                  Arrived := True;
                  for H of F.Outlet_Streams loop
                     if H /= CCL.Streams.No_Handle then
                        CCL_Stream_Table.End_Stream (Streams, H);
                     end if;
                  end loop;
                  CCL_Launcher.Release (F.Started);
                  F.Ended := True;
               end if;
            end;
         end if;
      end loop;
   end Pump;
end CCL_Program_Bindings;
