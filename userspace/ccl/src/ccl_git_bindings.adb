------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git.* (see the spec).
------------------------------------------------------------------------------
with CCL.Interfaces.Git;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Streams;
with CCL_Git;
with CCL_Net_Bindings;
with Git_Clone_Reports;
with CuBit.Failures;

package body CCL_Git_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Host_Values.Value_Kind;
   package G renames CCL.Interfaces.Git;
   package R renames Git_Clone_Reports;
   use type G.Operation;
   use type R.Start_Result;
   use type R.Outcome;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;

   Contracts : G.Contracts;

   --  What one reply image's text holds of a URL and a detail.
   REPLY_URL_BYTES : constant := 4_096;
   REPLY_DETAIL_BYTES : constant := 1_024;

   --  The clones this session follows: the reply task, pinned while the
   --  clone runs and after, until the row is needed (the oldest finished
   --  goes first); the last report, for git.status after the end.
   MAXIMUM_FOLLOWED : constant := 8;
   type Followed is record
      Used : Boolean := False;
      Id : Unsigned_64 := 0;
      Reply : CCL.Streams.Handle := CCL.Streams.No_Handle;
      Finished : Boolean := False;
      Started : Unsigned_64 := 0;
      Last : R.Report;
   end record;
   Table : array (1 .. MAXIMUM_FOLLOWED) of Followed;
   Starts : Unsigned_64 := 0;
   Due : Unsigned_64 := Unsigned_64'Last;

   function Handles (Binding : Unsigned_32) return Boolean is
     (for some Op in G.Operation => Binding = G.Binding_Of (Op));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      G.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, G.REQUEST_KEY, Contracts.Request);
      CCL.Catalog.Resolve_Schema (Catalog, G.CLONE_KEY, Contracts.Clone);
      CCL.Catalog.Resolve_Schema (Catalog, G.REPLY_KEY, Contracts.Reply);
      Success := CCL.Objects.Is_Bound (Contracts.Request) and then CCL.Objects.Is_Bound (Contracts.Clone)
        and then CCL.Objects.Is_Bound (Contracts.Reply);
      for Op in G.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "git." & G.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, G.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   --  CCL's Git_Outcome: the engine's outcome, grouped (the reply's reason
   --  names the engine's own).
   function Outcome_Of (O : R.Outcome) return G.Outcome is
     (case O is
         when R.Running => G.Running,
         when R.Cloned => G.Cloned,
         when R.Not_Granted => G.Not_Granted,
         when R.Target_Exists => G.Exists,
         when R.Bad_Url => G.Bad_Url,
         when R.Too_Many_Redirects => G.Too_Many_Redirects,
         when R.Connect_Failed => G.Connect_Failed,
         when R.Tls_Failed => G.Tls_Failed,
         when R.Http_Status => G.Http_Status,
         when R.Bad_Response | R.Truncated => G.Bad_Response,
         when R.Deadline => G.Deadline,
         when R.Cancelled => G.Cancelled,
         when R.Write_Failed => G.Write_Failed,
         when R.Bad_Directory | R.Not_Smart_Http | R.Bad_Refs | R.Bad_Fetch | R.Remote_Error | R.Bad_Pack
            | R.Bad_Delta | R.Missing_Object | R.Bad_Object | R.Path_Too_Long => G.Refused,
         when R.Too_Many_Objects | R.Too_Large => G.Too_Large,
         when R.No_Refs => G.Empty);

   --  The engine's outcome by name (native builds discard enumeration images).
   function Reason (O : R.Outcome) return String is
     (case O is
         when R.Running => "Running", when R.Cloned => "Cloned", when R.Not_Granted => "Not_Granted",
         when R.Target_Exists => "Target_Exists", when R.Bad_Url => "Bad_Url",
         when R.Bad_Directory => "Bad_Directory", when R.Too_Many_Redirects => "Too_Many_Redirects",
         when R.Connect_Failed => "Connect_Failed", when R.Tls_Failed => "Tls_Failed",
         when R.Http_Status => "Http_Status", when R.Bad_Response => "Bad_Response",
         when R.Truncated => "Truncated", when R.Deadline => "Deadline", when R.Cancelled => "Cancelled",
         when R.Write_Failed => "Write_Failed", when R.Not_Smart_Http => "Not_Smart_Http",
         when R.Bad_Refs => "Bad_Refs", when R.Bad_Fetch => "Bad_Fetch", when R.Remote_Error => "Remote_Error",
         when R.Bad_Pack => "Bad_Pack", when R.Bad_Delta => "Bad_Delta",
         when R.Too_Many_Objects => "Too_Many_Objects", when R.Too_Large => "Too_Large",
         when R.Missing_Object => "Missing_Object", when R.Bad_Object => "Bad_Object",
         when R.Path_Too_Long => "Path_Too_Long", when R.No_Refs => "No_Refs");

   function Count_Cell (Value : Unsigned_64) return CCL.Objects.Cell is
     (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Value, Unsigned_64 (Integer_64'Last)))));

   function Clone_Image (Id : Unsigned_64) return CCL.Host_Values.Value is
      Image : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Clone);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (G.CLONE_FIELDS), Step);
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Image, Count_Cell (Id), Step);
      end if;
      return CCL.Host_Values.Object_Constant (Image);
   end Clone_Image;

   procedure Reply_Image (S : R.Report; Image : out CCL.Objects.Image; Built : out Boolean) is
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Text (Value : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Value, Step); end if;
      end Text;
   begin
      Image := CCL.Objects.Empty (Contracts.Reply);
      Put (CCL.Objects.Product_Cell (G.REPLY_FIELDS));
      Put (CCL.Objects.Variant_Cell (G.Outcome'Pos (Outcome_Of (S.Ended_As)) + 1));
      Put (CCL.Objects.Unit_Cell);
      Text (Reason (S.Ended_As));
      Put (Count_Cell (Unsigned_64 (S.Status)));
      Put (Count_Cell (Unsigned_64 (S.Refs)));
      Put (Count_Cell (Unsigned_64 (S.Objects)));
      Put (Count_Cell (Unsigned_64 (S.Files)));
      Put (Count_Cell (S.Bytes));
      Put (CCL.Objects.Variant_Cell (R.Object_Format'Pos (S.Format) + 1));
      Put (CCL.Objects.Unit_Cell);
      Put (Count_Cell (Unsigned_64 (S.Redirects)));
      Text (S.Url (1 .. Natural'Min (S.Url_Length, REPLY_URL_BYTES)));
      Text (S.Detail.Data (1 .. Natural'Min (S.Detail.Length, REPLY_DETAIL_BYTES)));
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.Reply);
   end Reply_Image;

   function Find (Id : Unsigned_64) return Natural is
   begin
      for K in Table'Range loop
         if Table (K).Used and then Table (K).Id = Id then
            return K;
         end if;
      end loop;
      return 0;
   end Find;

   function Free_Row (Streams : in out CCL_Stream_Table.Table) return Natural is
      Oldest : Natural := 0;
   begin
      for K in Table'Range loop
         if not Table (K).Used then
            return K;
         elsif Table (K).Finished and then (Oldest = 0 or else Table (K).Started < Table (Oldest).Started) then
            Oldest := K;
         end if;
      end loop;
      if Oldest /= 0 then
         CCL_Stream_Table.Unpin (Streams, Table (Oldest).Reply);
         Table (Oldest) := (others => <>);
      end if;
      return Oldest;
   end Free_Row;

   procedure Run_Clone
     (Argument : CCL.Host_Values.Value; Now_Ms : Unsigned_64; Streams : in out CCL_Stream_Table.Table;
      Reply : in out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Contracts.Request, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "git.clone takes a Clone_Request");
         return;
      end if;
      declare
         Root_Cursor : constant Views.Cursor := Views.Root (Object);
         function Field (K : Positive) return Views.Cursor is (Views.Field (Object, Root_Cursor, K));
         function Limit (K : Positive) return Integer_64 is
           (CCL.Objects.Integer_Of (Views.Scalar (Object, Field (K))));
         Into : constant Views.Cursor := Field (G.REQUEST_INTO);
         Url : constant String := Views.Text (Object, Field (G.REQUEST_URL));
         Root : constant String := Views.Text (Object, Views.Field (Object, Into, G.PLACE_ROOT));
         Path : constant String := Views.Text (Object, Views.Field (Object, Into, G.PLACE_PATH));
         Directory : constant String := Views.Text (Object, Field (G.REQUEST_DIRECTORY));
         Refs : constant Integer_64 := Limit (G.REQUEST_MAX_REFS);
         Objects : constant Integer_64 := Limit (G.REQUEST_MAX_OBJECTS);
         Bytes : constant Integer_64 := Limit (G.REQUEST_MAX_BYTES);
         Redirects : constant Integer_64 := Limit (G.REQUEST_MAX_REDIRECTS);
         Deadline : constant Integer_64 := Limit (G.REQUEST_DEADLINE);
         Id : Unsigned_64;
         Result : R.Start_Result;
         Why : R.Short_Text;
         Row : Natural;
      begin
         --  The CCL range types check the limits; checked again here.
         if Refs not in 1 .. G.MAXIMUM_REFS or else Objects not in 1 .. G.MAXIMUM_OBJECTS
           or else Bytes not in 1 .. G.MAXIMUM_BYTES or else Redirects not in 0 .. G.MAXIMUM_REDIRECTS
           or else Deadline not in 1 .. G.MAXIMUM_DEADLINE_MS
         then
            Reply.Why := Failures.Failed (Failures.Invalid_Argument, "a Clone_Request limit is out of range");
            return;
         elsif not CCL_Net_Bindings.Valid_Root (Root) or else not CCL_Net_Bindings.Valid_Below (Path) then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument,
               """" & Root & """ """ & Path & """ is not a place: a root such as ""@system"" and names below it",
               "no '.', '..', '\' or empty names; the filesystem checks the place against the manifest");
            return;
         elsif not CCL_Net_Bindings.Valid_Name (Directory) then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, """" & Directory & """ is not one entry's name (no '/', '\', '.' or '..')");
            return;
         end if;
         Row := Free_Row (Streams);
         if Row = 0 then
            Reply.Why := Failures.Failed
              (Failures.Exhausted, "this session follows" & Natural'Image (MAXIMUM_FOLLOWED) &
               " clones that are still running");
            return;
         end if;
         CCL_Git.Start
           (Url, Root, Path, Directory,
            (Maximum_Refs => Natural (Refs), Maximum_Objects => Natural (Objects),
             Maximum_Bytes => Unsigned_64 (Bytes), Maximum_Redirects => Natural (Redirects),
             Deadline_Ms => Positive (Deadline)),
            Now_Ms, Id, Result, Why);
         if Result /= R.Started then
            Reply.Why := Failures.Failed
              ((case Result is
                  when R.Not_Granted => Failures.Not_Granted,
                  when R.Unavailable => Failures.Unavailable,
                  when others => Failures.Refused),
               "git.clone: " & Why.Data (1 .. Why.Length),
               (if Result = R.Not_Granted
                then "the manifest's (filesystem ...) scope must grant write and create there, and its " &
                     "(tls ""HOST:PORT"") scope the host"
                else ""));
            return;
         end if;
         Starts := Starts + 1;
         Table (Row) := (Used => True, Id => Id, Started => Starts, others => <>);
         CCL_Stream_Table.Open_Task (Streams, Table (Row).Reply, Pinned => True);
         Reply := (Value => Clone_Image (Id), Success => True, Why => <>);
      end;
   end Run_Clone;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value; Now_Ms : Unsigned_64;
      Streams : in out CCL_Stream_Table.Table; Reply : out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then
         return;
      end if;
      declare
         Op : constant G.Operation := G.Operation'Val (Binding - G.FIRST_BINDING);
      begin
         if Op = G.Clone then
            Run_Clone (Argument, Now_Ms, Streams, Reply);
            return;
         end if;
         if Argument.Kind = CCL.Host_Values.Object_Value then
            Views.Capture (Object, Contracts.Clone, Argument.Object, Captured);
         end if;
         if not Captured then
            Reply.Why := Failures.Failed (Failures.Invalid_Argument, "git: takes a Git_Clone");
            return;
         end if;
         declare
            Id : constant Integer_64 :=
              CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 1)));
            Row : constant Natural := (if Id < 0 then 0 else Find (Unsigned_64 (Id)));
         begin
            if Row = 0 then
               Reply.Why := Failures.Failed (Failures.Invalid_Argument, "git: this session does not follow that clone");
               return;
            end if;
            case Op is
               when G.Reply =>
                  Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Table (Row).Reply)),
                            Success => True, Why => <>);
               when G.Status =>
                  declare
                     Now : R.Report := Table (Row).Last;
                     Found, Built : Boolean;
                     Image : CCL.Objects.Image;
                  begin
                     if not Table (Row).Finished then
                        CCL_Git.Poll (Table (Row).Id, Now, Found);
                     end if;
                     Reply_Image (Now, Image, Built);
                     if Built then
                        Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
                     end if;
                  end;
               when G.Cancel =>
                  if not Table (Row).Finished then
                     CCL_Git.Cancel (Table (Row).Id);
                  end if;
                  Reply := (Value => Clone_Image (Table (Row).Id), Success => True, Why => <>);
               when G.Clone =>
                  null;
            end case;
         end;
      end;
   end Invoke;

   procedure Pump
     (Streams : in out CCL_Stream_Table.Table; Now_Ms : Unsigned_64; Arrived : out Boolean)
   is
   begin
      Arrived := False;
      CCL_Git.Pump (Now_Ms, Due);
      for K in Table'Range loop
         if Table (K).Used and then not Table (K).Finished then
            declare
               S : R.Report;
               Found, Built, Completed : Boolean;
               Image : CCL.Objects.Image;
            begin
               CCL_Git.Poll (Table (K).Id, S, Found);
               if not Found or else S.Ended_As /= R.Running then
                  Table (K).Finished := True;
                  if not Found then
                     S.Ended_As := R.Cancelled;
                     S.Detail := R.Short ("the clone was lost");
                  end if;
                  Table (K).Last := S;
                  Reply_Image (S, Image, Built);
                  if Built then
                     CCL_Stream_Table.Complete_Task (Streams, Table (K).Reply, Image, Completed);
                  end if;
                  CCL_Git.Forget (Table (K).Id);
                  Arrived := True;
               end if;
            end;
         end if;
      end loop;
   end Pump;

   function Running return Boolean is (for some F of Table => F.Used and then not F.Finished);
   function Next_Due return Unsigned_64 is (Due);
end CCL_Git_Bindings;
