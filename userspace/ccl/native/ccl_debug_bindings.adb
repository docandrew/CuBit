with CCL.Interfaces.Debug;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Native_Bindings;
with CCL_Places;
with CCL_Processes;
with CuBit.Debug;
with CuBit.Debug_Target;
with CuBit.Failures;
with CuBit.Handles;
with CuBit.Handles.Debug;
with CuBit.Handles.IPC;
with CuBit.Kernel_ABI;
with CuBit.Launch_Handles;
with CuBit.Messages;
with Debug_ABI;

--  debug.* on CuBit (DBG-001): a session is Debug_Observe, which procmgr
--  mints through the console's "debug-session" authority within the user
--  session's debug envelope, read by the debugger engine (CuBit.Debug)
--  through the kernel: threads, frames unwound from saved registers, locals,
--  values and memory through read-only views. The target needs no stub.
package body CCL_Debug_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type CuBit.Debug.Outcome;
   use type CuBit.Debug.Thread_Id;
   use type CuBit.Launch_Handles.Outcome;
   use type CCL.Interfaces.Debug.Operation;
   package DI renames CCL.Interfaces.Debug;
   package D renames CuBit.Debug;
   package DT renames CuBit.Debug_Target;
   package HD renames CuBit.Handles.Debug;
   package IPC renames CuBit.Handles.IPC;
   package LH renames CuBit.Launch_Handles;
   package Objects renames CCL.Objects;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;

   Contracts : DI.Contracts;

   --  The console holds its one filesystem queue in its places (CCL_Places):
   --  the engine reads debug files and sources through it.
   procedure Read_File
     (Path : String; Offset : Natural; Into : out DT.Byte_Buffer; Got : out Natural; Found : out Boolean)
   is
      use type CCL_Places.Result_Kind;
      Text : String (1 .. Into'Length);
      Last : Natural;
      Result : CCL_Places.Result_Kind;
   begin
      Into := [others => 0];
      CCL_Places.Read_At (Path, Offset, Text, Last, Result);
      Found := Result = CCL_Places.Listed_All;
      Got := (if Found then Last - Text'First + 1 else 0);
      for I in 1 .. Got loop
         Into (Into'First + I - 1) := Character'Pos (Text (I));
      end loop;
   end Read_File;

   ---------------------------------------------------------------------------
   --  Sessions: the console's, numbered from 1 (D.Session_Ceiling of them).
   --  A session keeps its latest frames (debug.locals reads one) and the
   --  values it handed out by ref (debug.children, debug.graph), at most
   --  Ref_Ceiling: an older ref is stale, never another value.
   ---------------------------------------------------------------------------
   Ref_Ceiling : constant := 1_024;
   type Ref_Slot is record
      Id  : Unsigned_64 := 0;
      Ref : D.Value_Ref;
   end record;
   type Ref_Table is array (1 .. Ref_Ceiling) of Ref_Slot;
   type Ref_Table_Access is access Ref_Table;

   type Console_Session is record
      Live     : Boolean := False;
      Engine   : D.Session;
      Refs     : Ref_Table_Access;
      Next_Ref : Unsigned_64 := 1;
      Thread   : D.Thread_Id := 0;          --  whose frames Frames holds
      Frames   : D.Frame_List;
   end record;
   subtype Session_Number is Positive range 1 .. D.Session_Ceiling;
   Sessions : array (Session_Number) of Console_Session;

   function Session_Of (Id : Integer_64) return Natural is
     (if Id in 1 .. D.Session_Ceiling and then Sessions (Natural (Id)).Live then Natural (Id) else 0);

   function Keep (S : in out Console_Session; Ref : D.Value_Ref) return Integer_64 is
      Id : constant Unsigned_64 := S.Next_Ref;
      Slot : constant Positive := Positive ((Id - 1) mod Ref_Ceiling + 1);
   begin
      if S.Refs = null then
         S.Refs := new Ref_Table;
      end if;
      S.Refs (Slot) := (Id => Id, Ref => Ref);
      S.Next_Ref := S.Next_Ref + 1;
      return Integer_64 (Id);
   end Keep;

   procedure Find_Ref (S : Console_Session; Id : Integer_64; Ref : out D.Value_Ref; Found : out Boolean) is
   begin
      Ref := D.Null_Ref;
      Found := False;
      if S.Refs = null or else Id < 1 then
         return;
      end if;
      declare
         Slot : constant Positive := Positive ((Unsigned_64 (Id) - 1) mod Ref_Ceiling + 1);
      begin
         if S.Refs (Slot).Id = Unsigned_64 (Id) then
            Ref := S.Refs (Slot).Ref;
            Found := True;
         end if;
      end;
   end Find_Ref;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in DI.FIRST_BINDING .. DI.Binding_Of (DI.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      DI.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      DT.Use_File_Reader (Read_File'Access);
      CCL.Catalog.Resolve_Schema (Catalog, DI.SESSION_KEY, Contracts.Session);
      CCL.Catalog.Resolve_Schema (Catalog, DI.ATTACH_QUERY_KEY, Contracts.Attach_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.THREADS_KEY, Contracts.Threads);
      CCL.Catalog.Resolve_Schema (Catalog, DI.FRAME_QUERY_KEY, Contracts.Frame_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.FRAMES_KEY, Contracts.Frames);
      CCL.Catalog.Resolve_Schema (Catalog, DI.LOCALS_QUERY_KEY, Contracts.Locals_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.BINDING_KEY, Contracts.Binding);
      CCL.Catalog.Resolve_Schema (Catalog, DI.BINDINGS_KEY, Contracts.Bindings);
      CCL.Catalog.Resolve_Schema (Catalog, DI.VALUE_QUERY_KEY, Contracts.Value_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.CHILDREN_QUERY_KEY, Contracts.Children_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.GRAPH_QUERY_KEY, Contracts.Graph_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.GRAPH_KEY, Contracts.Graph);
      CCL.Catalog.Resolve_Schema (Catalog, DI.MEMORY_QUERY_KEY, Contracts.Memory_Query);
      CCL.Catalog.Resolve_Schema (Catalog, DI.MEMORY_KEY, Contracts.Memory);
      Success := Objects.Is_Bound (Contracts.Session) and then Objects.Is_Bound (Contracts.Attach_Query)
        and then Objects.Is_Bound (Contracts.Threads) and then Objects.Is_Bound (Contracts.Frame_Query)
        and then Objects.Is_Bound (Contracts.Frames) and then Objects.Is_Bound (Contracts.Locals_Query)
        and then Objects.Is_Bound (Contracts.Binding) and then Objects.Is_Bound (Contracts.Bindings)
        and then Objects.Is_Bound (Contracts.Value_Query) and then Objects.Is_Bound (Contracts.Children_Query)
        and then Objects.Is_Bound (Contracts.Graph_Query) and then Objects.Is_Bound (Contracts.Graph)
        and then Objects.Is_Bound (Contracts.Memory_Query) and then Objects.Is_Bound (Contracts.Memory);
      for Op in DI.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "debug." & DI.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, DI.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------
   type Builder is record
      Image : Objects.Image;
      Step  : Objects.Build_Result := Objects.Added;
   end record;

   procedure Put (B : in out Builder; Cell : Objects.Cell) is
   begin
      if B.Step = Objects.Added then Objects.Append (B.Image, Cell, B.Step); end if;
   end Put;
   procedure Put_Text (B : in out Builder; Value : String) is
   begin
      if B.Step = Objects.Added then Objects.Append_Text (B.Image, Value, B.Step); end if;
   end Put_Text;
   procedure Put_Number (B : in out Builder; Value : Integer_64) is
   begin
      Put (B, Objects.Integer_Cell (Value));
   end Put_Number;
   --  An address or other unsigned word, as CCL's Integer holds it.
   function Signed (Value : Unsigned_64) return Integer_64 is
     (if Value > Unsigned_64 (Integer_64'Last) then -1 else Integer_64 (Value));

   procedure Finish (B : Builder; Contract : Objects.Binding; Reply : out CCL.Host_Values.Call_Result) is
   begin
      if B.Step = Objects.Added and then Objects.Validate (B.Image, Contract) then
         Reply := (Value => CCL.Host_Values.Object_Constant (B.Image), Success => True, Why => <>);
      else
         Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False,
                   Why => Failures.Failed (Failures.Exhausted, "the result does not fit one CCL value",
                                           "ask for fewer (a smaller count, depth or length)"));
      end if;
   end Finish;

   procedure Refuse (Reply : out CCL.Host_Values.Call_Result; Why : Failures.Reason;
                     Detail : String; Remedy : String := "") is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False,
                Why => Failures.Failed (Why, Detail, Remedy));
   end Refuse;

   --  The engine's outcome as a refusal.
   procedure Refuse_Outcome (Reply : out CCL.Host_Values.Call_Result; R : D.Outcome; What : String) is
   begin
      case R is
         when D.Not_Attached =>
            Refuse (Reply, Failures.Not_Found, What & ": no such debug session", "attach first (debug.attach)");
         when D.Exited =>
            Refuse (Reply, Failures.Unavailable, What & ": the target has ended");
         when D.Not_Stopped =>
            Refuse (Reply, Failures.Unavailable, What & ": the thread is on a CPU now", "ask again");
         when D.Stale =>
            Refuse (Reply, Failures.Invalid_Argument, What & ": from an earlier stop",
                    "read the frames again (debug.frames)");
         when D.No_Debug_Info =>
            Refuse (Reply, Failures.Not_Found, What & ": no debug information for the target's build-id",
                    "install its debug file at @system/Debug/.build-id/");
         when D.Unreadable =>
            Refuse (Reply, Failures.Outside_Scope, What & ": that memory is not viewable");
         when D.Timed_Out =>
            Refuse (Reply, Failures.Unavailable, What & ": the deadline passed");
         when D.Not_Found =>
            Refuse (Reply, Failures.Not_Found, What & ": no such thread, frame or name");
         when others =>
            Refuse (Reply, Failures.Refused, What & ": the engine refused (" & D.Outcome'Image (R) & ")");
      end case;
   end Refuse_Outcome;

   function Wait_Member (W : DT.Wait_Code) return DI.Wait_Member is
     (case W is
        when DT.Not_Known => DI.Unknown, when DT.Running => DI.Running, when DT.Ready => DI.Ready,
        when DT.In_Call => DI.In_Call, when DT.Receiving => DI.Receiving,
        when DT.Completion_Wait => DI.Completion_Wait, when DT.Futex => DI.Futex,
        when DT.Sleeping => DI.Sleeping, when DT.Not_Started => DI.Not_Started, when DT.Other => DI.Other);

   --  A value's binding (its ref kept for children and graph).
   Name_Shown : constant := 48;
   Value_Shown : constant := 96;
   function Cut (S : String; Limit : Positive) return String is
     (if S'Length <= Limit then S else S (S'First .. S'First + Limit - 2) & "~");

   procedure Put_Binding (B : in out Builder; S : in out Console_Session; Node : D.Value_Node) is
   begin
      Put (B, Objects.Product_Cell (DI.BINDING_FIELDS));
      Put_Number (B, Keep (S, Node.Self));
      Put_Text (B, Cut (D.Image (Node.Name), Name_Shown));
      Put_Text (B, Cut (D.Image (Node.Type_Name), Name_Shown));
      Put_Text (B, Cut (D.Image (Node.Display), Value_Shown));
      Put_Number (B, Signed (Unsigned_64 (Node.Address)));
      Put_Number (B, Integer_64 (Node.Child_Count));
   end Put_Binding;

   ---------------------------------------------------------------------------
   --  Operations
   ---------------------------------------------------------------------------
   function Deadline_Valid (Ms : Integer_64) return Boolean is (Ms in 1 .. DI.MAXIMUM_DEADLINE_MS);
   DEADLINE_REMEDY : constant String := "give a deadline of 1 .. 60000 ms; there is no default";

   --  The process named Program (its kernel name, "clock.svc", or its
   --  manifest identity, "com.cubit.clock"): its identity word, or 0.
   function Process_Named (Program : String) return Unsigned_64 is
      Found : Unsigned_64 := 0;
      procedure Visit (Item : CCL_Processes.Listed) is
      begin
         if Found = 0
           and then (Item.Name (1 .. Item.Name_Length) = Program
                     or else Item.Identity (1 .. Item.Identity_Length) = Program)
         then
            Found := Item.Process;
         end if;
      end Visit;
      procedure Each is new CCL_Processes.Each (Visit);
      Seen, Total : Natural;
      Result : CCL_Processes.Result_Kind;
   begin
      Each (Seen, Total, Result);
      return Found;
   end Process_Named;

   procedure Attach_Of (Program : String; Deadline_Ms : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      Free : Natural := 0;
      Target : Unsigned_64;
      Badge : constant CuBit.Handles.Endpoint := CCL_Native_Bindings.Debug_Session;
      Request : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Received : IPC.Carried;
      Answer : CuBit.Messages.MessageTag;
      Minted : LH.Outcome;
      Engine : D.Session;
      R : D.Outcome;
      Info : D.Program_Info;
      B : Builder;
   begin
      if not Deadline_Valid (Deadline_Ms) then
         Refuse (Reply, Failures.Invalid_Argument, "debug.attach needs an explicit deadline", DEADLINE_REMEDY);
         return;
      elsif not CuBit.Handles.Valid (Badge) then
         Refuse (Reply, Failures.Not_Granted, "debugging needs procmgr's debug-session authority",
                 "the program's manifest must request (service ""debug-session"" ""debug-session"")");
         return;
      end if;
      for K in Sessions'Range loop
         if not Sessions (K).Live then
            Free := K;
            exit;
         end if;
      end loop;
      if Free = 0 then
         Refuse (Reply, Failures.Exhausted, "every debug session is in use",
                 "detach one first (debug.detach)");
         return;
      end if;
      Target := Process_Named (Program);
      if Target = 0 then
         Refuse (Reply, Failures.Not_Found, "no process named " & Cut (Program, Name_Shown) & " runs",
                 "name it as (proc.list) does: clock.svc, or com.cubit.clock");
         return;
      end if;
      --  procmgr decides, under the session's debug envelope.
      Request.tag := (label => LH.Session_Debug_Operation, length => LH.Session_Debug_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words := [Target, Debug_ABI.Kind_Word (Debug_ABI.Observe), 0, 0];
      Answer := IPC.Call_Accepting
        (Badge, Request, CuBit.Messages.Deadline_After (Unsigned_64 (Deadline_Ms)), Received);
      Minted := (if Answer.label = CuBit.Kernel_ABI.Reply_OK and then LH.Is_Outcome_Word (Request.words (0))
                 then LH.To_Outcome (Request.words (0)) else LH.Failed);
      if Minted /= LH.Done then
         declare
            Ignore : IPC.Close_Result := IPC.Close (Received);
         begin
            case Minted is
               when LH.Never_Debuggable =>
                  Refuse (Reply, Failures.Refused, Cut (Program, Name_Shown) &
                          " is never debuggable (secrets.svc, the trusted path, procmgr, devmgr)");
               when LH.Not_Held =>
                  Refuse (Reply, Failures.Refused, "procmgr holds no control of " & Cut (Program, Name_Shown) &
                          " (it did not start it, or it has not run)");
               when LH.Kind_Withheld =>
                  Refuse (Reply, Failures.Refused, "the session's debug envelope withholds that kind");
               when others =>
                  Refuse (Reply, Failures.Unavailable, "procmgr did not mint a debug session (" &
                          LH.Outcome'Image (Minted) & ")");
            end case;
         end;
         return;
      end if;
      declare
         Taken : constant HD.Accepted_Session := HD.Accept_Session (Received, Debug_ABI.Observe);
      begin
         if not Taken.OK then
            Refuse (Reply, Failures.Unavailable, "procmgr's debug session could not be taken");
            return;
         end if;
         D.Attach ((Target => CuBit.Handles.No_Endpoint, Session => Taken.Value,
                    Filesystem => CCL_Native_Bindings.Filesystem),
                   D.Observe, CuBit.Messages.Deadline_After (Unsigned_64 (Deadline_Ms)), Engine, R);
      end;
      if R /= D.Ok then
         Refuse_Outcome (Reply, R, "debug.attach");
         return;
      end if;
      Sessions (Free) := (Live => True, Engine => Engine, Refs => Sessions (Free).Refs,
                          Next_Ref => Sessions (Free).Next_Ref, Thread => 0, Frames => <>);
      D.Describe (Engine, Info, R);
      B.Image := Objects.Empty (Contracts.Session);
      Put (B, Objects.Product_Cell (DI.SESSION_FIELDS));
      Put_Number (B, Integer_64 (Free));
      Put_Text (B, D.Image (Info.Program));
      Put_Number (B, Signed (Info.Process));
      Put_Text (B, Info.Build_Id);
      Put_Text (B, D.Image (Info.Attach_Mode));
      Finish (B, Contracts.Session, Reply);
   end Attach_Of;

   procedure Threads_Of (Id : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      List : D.Thread_List;
      R : D.Outcome;
      B : Builder;
      Shown : Natural;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.threads");
         return;
      end if;
      D.Threads (Sessions (K).Engine, List, R);
      if R not in D.Ok | D.More then
         Refuse_Outcome (Reply, R, "debug.threads");
         return;
      end if;
      Shown := Natural'Min (List.Count, DI.MAXIMUM_THREADS);
      B.Image := Objects.Empty (Contracts.Threads);
      Put (B, Objects.Sequence_Cell (Shown));
      for T of List.Items (1 .. Shown) loop
         Put (B, Objects.Product_Cell (DI.THREAD_FIELDS));
         Put_Number (B, Integer_64 (T.Id));
         Put_Text (B, D.Image (T.Name));
         Put (B, Objects.Variant_Cell (DI.Wait_Member'Pos (Wait_Member (T.Wait)) + 1));
         Put (B, Objects.Unit_Cell);
         Put_Number (B, Integer_64 (T.CPU));
         Put_Number (B, Signed (T.Peer));
         Put_Number (B, Signed (T.Deadline));
         Put_Number (B, Signed (Unsigned_64 (T.PC)));
      end loop;
      Finish (B, Contracts.Threads, Reply);
   end Threads_Of;

   procedure Frames_Of (Id, Thread, Deadline_Ms : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      R : D.Outcome;
      B : Builder;
      Shown : Natural;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.frames");
         return;
      elsif not Deadline_Valid (Deadline_Ms) then
         Refuse (Reply, Failures.Invalid_Argument, "debug.frames needs an explicit deadline", DEADLINE_REMEDY);
         return;
      elsif Thread not in 1 .. Integer_64 (Unsigned_32'Last) then
         Refuse_Outcome (Reply, D.Not_Found, "debug.frames");
         return;
      end if;
      declare
         S : Console_Session renames Sessions (K);
      begin
         D.Frames (S.Engine, D.Thread_Id (Thread), S.Frames, R);
         if R /= D.Ok then
            S.Thread := 0;
            Refuse_Outcome (Reply, R, "debug.frames");
            return;
         end if;
         S.Thread := D.Thread_Id (Thread);
         Shown := Natural'Min (S.Frames.Count, DI.MAXIMUM_FRAMES);
         B.Image := Objects.Empty (Contracts.Frames);
         Put (B, Objects.Sequence_Cell (Shown));
         for I in 0 .. Shown - 1 loop
            declare
               F : D.Frame renames S.Frames.Items (I);
            begin
               Put (B, Objects.Product_Cell (DI.FRAME_FIELDS));
               Put_Number (B, Integer_64 (F.Index));
               Put_Text (B, Cut (D.Image (F.Function_Name), D.Name_Capacity));
               Put_Text (B, Cut (D.Image (F.File), D.Name_Capacity));
               Put_Number (B, Integer_64 (F.Line));
               Put_Number (B, Signed (Unsigned_64 (F.PC)));
               Put (B, Objects.Boolean_Cell (F.Has_Debug_Info));
            end;
         end loop;
         Finish (B, Contracts.Frames, Reply);
      end;
   end Frames_Of;

   --  A page of value nodes as a List<Debug_Binding>.
   procedure Bindings_Of (S : in out Console_Session; Refs : D.Ref_List; Reply : out CCL.Host_Values.Call_Result) is
      B : Builder;
      Node : D.Value_Node;
      R : D.Outcome;
      Shown : constant Natural := Natural'Min (Refs.Count, DI.MAXIMUM_BINDINGS);
   begin
      B.Image := Objects.Empty (Contracts.Bindings);
      Put (B, Objects.Sequence_Cell (Shown));
      for I in 1 .. Shown loop
         D.Value (S.Engine, Refs.Items (I), Node, R);
         if R /= D.Ok then
            Node := (Self => Refs.Items (I), Name => Refs.Items (I).Name,
                     Display => D.To_Display ("<" & D.Outcome'Image (R) & ">"), others => <>);
         end if;
         Put_Binding (B, S, Node);
      end loop;
      Finish (B, Contracts.Bindings, Reply);
   end Bindings_Of;

   procedure Locals_Of (Id, Thread, Frame : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      Refs : D.Ref_List;
      R : D.Outcome;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.locals");
         return;
      end if;
      declare
         S : Console_Session renames Sessions (K);
      begin
         if S.Thread = 0 or else Integer_64 (S.Thread) /= Thread
           or else Frame not in 0 .. Integer_64 (S.Frames.Count) - 1
         then
            Refuse (Reply, Failures.Invalid_Argument, "debug.locals reads a frame of the latest debug.frames",
                    "read that thread's frames first (debug.frames)");
            return;
         end if;
         D.Locals (S.Engine, S.Frames.Items (Natural (Frame)), Refs, R);
         if R not in D.Ok | D.More then
            Refuse_Outcome (Reply, R, "debug.locals");
            return;
         end if;
         Bindings_Of (S, Refs, Reply);
      end;
   end Locals_Of;

   procedure Value_Of (Id : Integer_64; Name : String; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      Globals : D.Ref_List;
      R : D.Outcome;
      Node : D.Value_Node;
      B : Builder;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.value");
         return;
      end if;
      D.Globals (Sessions (K).Engine, "", Globals, R);
      for G of Globals.Items (1 .. Globals.Count) loop
         if D.Image (G.Name) = Name then
            D.Value (Sessions (K).Engine, G, Node, R);
            if R /= D.Ok then
               Refuse_Outcome (Reply, R, "debug.value");
               return;
            end if;
            B.Image := Objects.Empty (Contracts.Binding);
            Put_Binding (B, Sessions (K), Node);
            Finish (B, Contracts.Binding, Reply);
            return;
         end if;
      end loop;
      Refuse (Reply, Failures.Not_Found, "no global named " & Cut (Name, Name_Shown),
              "name it as the source does: Package.Object");
   end Value_Of;

   procedure Children_Of (Id, Ref_Id, First, Count : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      Ref : D.Value_Ref;
      Found : Boolean;
      Kids : D.Node_List;
      R : D.Outcome;
      B : Builder;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.children");
         return;
      end if;
      Find_Ref (Sessions (K), Ref_Id, Ref, Found);
      if not Found then
         Refuse (Reply, Failures.Invalid_Argument, "no such value ref in this session (or an old one)",
                 "take refs from the latest debug.locals, debug.value or debug.children");
         return;
      elsif First not in 0 .. Integer_64 (Natural'Last)
        or else Count not in 1 .. Integer_64 (Natural'Min (D.Children_Page_Ceiling, DI.MAXIMUM_BINDINGS))
      then
         Refuse (Reply, Failures.Invalid_Argument, "debug.children takes first >= 0 and a count of 1 .. 48");
         return;
      end if;
      D.Children (Sessions (K).Engine, Ref, Natural (First), Natural (Count), Kids, R);
      if R not in D.Ok | D.More then
         Refuse_Outcome (Reply, R, "debug.children");
         return;
      end if;
      B.Image := Objects.Empty (Contracts.Bindings);
      Put (B, Objects.Sequence_Cell (Kids.Count));
      for I in 1 .. Kids.Count loop
         Put_Binding (B, Sessions (K), Kids.Items (I));
      end loop;
      Finish (B, Contracts.Bindings, Reply);
   end Children_Of;

   type Graph_Access is access D.Graph_Result;
   Graph_Area : Graph_Access;

   procedure Graph_Of (Id, Ref_Id, Depth, Nodes : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      Ref : D.Value_Ref;
      Found : Boolean;
      Roots : D.Ref_List;
      R : D.Outcome;
      B : Builder;
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.graph");
         return;
      end if;
      Find_Ref (Sessions (K), Ref_Id, Ref, Found);
      if not Found then
         Refuse (Reply, Failures.Invalid_Argument, "no such value ref in this session (or an old one)");
         return;
      elsif Depth not in 0 .. D.Graph_Depth_Ceiling or else Nodes not in 1 .. DI.MAXIMUM_GRAPH_NODES then
         Refuse (Reply, Failures.Invalid_Argument, "debug.graph takes a depth of 0 .. 16 and 1 .. 32 nodes");
         return;
      end if;
      if Graph_Area = null then
         Graph_Area := new D.Graph_Result;
      end if;
      Roots.Count := 1;
      Roots.Items (1) := Ref;
      D.Graph (Sessions (K).Engine, Roots, Natural (Depth), Natural (Nodes), Graph_Area.all, R);
      if R not in D.Ok | D.More then
         Refuse_Outcome (Reply, R, "debug.graph");
         return;
      end if;
      declare
         Edges : constant Natural := Natural'Min (Graph_Area.Edge_Total, DI.MAXIMUM_GRAPH_EDGES);
      begin
         B.Image := Objects.Empty (Contracts.Graph);
         Put (B, Objects.Product_Cell (DI.GRAPH_FIELDS));
         Put (B, Objects.Sequence_Cell (Graph_Area.Node_Total));
         for I in 1 .. Graph_Area.Node_Total loop
            Put_Binding (B, Sessions (K), Graph_Area.Nodes (I).Value);
         end loop;
         Put (B, Objects.Sequence_Cell (Edges));
         for I in 1 .. Edges loop
            Put (B, Objects.Product_Cell (DI.EDGE_FIELDS));
            Put_Number (B, Integer_64 (Graph_Area.Edges (I).From));
            Put_Number (B, Integer_64 (Graph_Area.Edges (I).To));
            Put_Text (B, D.Image (Graph_Area.Edges (I).Label));
         end loop;
         Put (B, Objects.Boolean_Cell
                (Graph_Area.Truncated or else R = D.More or else Edges < Graph_Area.Edge_Total));
         Finish (B, Contracts.Graph, Reply);
      end;
   end Graph_Of;

   procedure Memory_Of (Id, Address, Length : Integer_64; Reply : out CCL.Host_Values.Call_Result) is
      K : constant Natural := Session_Of (Id);
      Bytes : D.Byte_Array (1 .. DI.MAXIMUM_MEMORY);
      Got : D.Memory_Length;
      R : D.Outcome;
      B : Builder;
      Hex : constant String := "0123456789abcdef";
   begin
      if K = 0 then
         Refuse_Outcome (Reply, D.Not_Attached, "debug.memory");
         return;
      elsif Address < 0 or else Length not in 1 .. DI.MAXIMUM_MEMORY then
         Refuse (Reply, Failures.Invalid_Argument, "debug.memory takes an address and 1 .. 1024 bytes");
         return;
      end if;
      D.Memory (Sessions (K).Engine, D.Target_Address (Address), Natural (Length), Bytes, Got, R);
      if R not in D.Ok | D.Unreadable then
         Refuse_Outcome (Reply, R, "debug.memory");
         return;
      end if;
      declare
         Text : String (1 .. 2 * Got);
      begin
         for I in 1 .. Got loop
            Text (2 * I - 1) := Hex (Natural (Bytes (I) / 16) + 1);
            Text (2 * I) := Hex (Natural (Bytes (I) mod 16) + 1);
         end loop;
         B.Image := Objects.Empty (Contracts.Memory);
         Put (B, Objects.Product_Cell (DI.MEMORY_FIELDS));
         Put_Number (B, Address);
         Put_Text (B, Text);
         Put_Number (B, Integer_64 (Got));
         Finish (B, Contracts.Memory, Reply);
      end;
   end Memory_Of;

   ---------------------------------------------------------------------------
   --  Dispatch
   ---------------------------------------------------------------------------
   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : DI.Operation;
      Object : Views.Snapshot;
      Captured : Boolean := False;
      function Root return Views.Cursor is (Views.Root (Object));
      function Field (Index : Positive) return Views.Cursor is (Views.Field (Object, Root, Index));
      function Number (Index : Positive) return Integer_64 is
        (Objects.Integer_Of (Views.Scalar (Object, Field (Index))));
      procedure Capture (Contract : Objects.Binding) is
      begin
         if Argument.Kind = CCL.Host_Values.Object_Value then
            Views.Capture (Object, Contract, Argument.Object, Captured);
         end if;
      end Capture;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      Op := DI.Operation'Val (Binding - DI.FIRST_BINDING);
      case Op is
         when DI.Threads | DI.Detach =>
            if Argument.Kind /= CCL.Host_Values.Integer_Value then return; end if;
            if Op = DI.Threads then
               Threads_Of (Argument.Integer, Reply);
            else
               declare
                  K : constant Natural := Session_Of (Argument.Integer);
               begin
                  if K /= 0 then
                     D.Detach (Sessions (K).Engine);
                     Sessions (K).Live := False;
                     Sessions (K).Thread := 0;
                  end if;
                  Reply := (Value => (Kind => CCL.Host_Values.Boolean_Value, Boolean => K /= 0),
                            Success => True, Why => <>);
               end;
            end if;
         when DI.Attach =>
            Capture (Contracts.Attach_Query);
            if not Captured then return; end if;
            Attach_Of (Views.Text (Object, Field (1)), Number (2), Reply);
         when DI.Frames =>
            Capture (Contracts.Frame_Query);
            if not Captured then return; end if;
            Frames_Of (Number (1), Number (2), Number (3), Reply);
         when DI.Locals =>
            Capture (Contracts.Locals_Query);
            if not Captured then return; end if;
            Locals_Of (Number (1), Number (2), Number (3), Reply);
         when DI.Value =>
            Capture (Contracts.Value_Query);
            if not Captured then return; end if;
            Value_Of (Number (1), Views.Text (Object, Field (2)), Reply);
         when DI.Children =>
            Capture (Contracts.Children_Query);
            if not Captured then return; end if;
            Children_Of (Number (1), Number (2), Number (3), Number (4), Reply);
         when DI.Graph =>
            Capture (Contracts.Graph_Query);
            if not Captured then return; end if;
            Graph_Of (Number (1), Number (2), Number (3), Number (4), Reply);
         when DI.Memory =>
            Capture (Contracts.Memory_Query);
            if not Captured then return; end if;
            Memory_Of (Number (1), Number (2), Number (3), Reply);
      end case;
   end Invoke;
end CCL_Debug_Bindings;
