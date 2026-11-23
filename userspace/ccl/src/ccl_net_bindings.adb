------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.* and archive.* (see the spec).
------------------------------------------------------------------------------
with CCL.Interfaces.Net;
with CCL.Interfaces.Archive;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Streams;
with CCL_Fetch;
with CCL_Archive;
with CuBit.Failures;

package body CCL_Net_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL_Fetch.Start_Result;
   use type CCL_Fetch.Outcome;
   package N renames CCL.Interfaces.Net;
   package A renames CCL.Interfaces.Archive;
   use type N.Operation;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;

   Net_Contracts : N.Contracts;
   Archive_Contracts : A.Contracts;

   PATH_SEPARATOR : constant Character := '/';
   VOLUME_MARK : constant Character := '@';
   MAXIMUM_NAME : constant := 255;
   MAXIMUM_PLACE : constant := 4_096;
   --  What one reply image's text holds of a URL, an entry and a detail.
   REPLY_URL_BYTES : constant := 4_096;
   REPLY_DETAIL_BYTES : constant := 1_024;

   --  The fetches this session follows: the reply task, pinned while the
   --  fetch runs and after, until the row is needed (the oldest finished
   --  goes first); the last report, for net.status after the end.
   MAXIMUM_FOLLOWED : constant := 16;
   type Followed is record
      Used : Boolean := False;
      Id : Unsigned_64 := 0;
      Reply : CCL.Streams.Handle := CCL.Streams.No_Handle;
      Finished : Boolean := False;
      Started : Unsigned_64 := 0;
      Last : CCL_Fetch.Report;
   end record;
   Table : array (1 .. MAXIMUM_FOLLOWED) of Followed;
   Starts : Unsigned_64 := 0;
   Due : Unsigned_64 := Unsigned_64'Last;

   function Handles (Binding : Unsigned_32) return Boolean is
     ((for some Op in N.Operation => Binding = N.Binding_Of (Op)) or else
      (for some Op in A.Operation => Binding = A.Binding_Of (Op)));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      N.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if Success then
         A.Publish (Catalog, Error);
         Success := Error = CCL.Catalog.Catalog_Valid;
      end if;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, N.REQUEST_KEY, Net_Contracts.Request);
      CCL.Catalog.Resolve_Schema (Catalog, N.FETCH_KEY, Net_Contracts.Fetch);
      CCL.Catalog.Resolve_Schema (Catalog, N.REPLY_KEY, Net_Contracts.Reply);
      CCL.Catalog.Resolve_Schema (Catalog, A.REQUEST_KEY, Archive_Contracts.Request);
      CCL.Catalog.Resolve_Schema (Catalog, A.REPLY_KEY, Archive_Contracts.Reply);
      Success := CCL.Objects.Is_Bound (Net_Contracts.Request) and then CCL.Objects.Is_Bound (Net_Contracts.Fetch)
        and then CCL.Objects.Is_Bound (Net_Contracts.Reply) and then CCL.Objects.Is_Bound (Archive_Contracts.Request)
        and then CCL.Objects.Is_Bound (Archive_Contracts.Reply);
      for Op in N.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "net." & N.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, N.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
      for Op in A.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "archive." & A.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, A.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. MAXIMUM_NAME and then Name /= "." and then Name /= ".." and then
      (for all C of Name => C in ' ' .. '~' and then C /= PATH_SEPARATOR and then C /= '\'));

   function Valid_Below (Path : String) return Boolean is
      First : Positive := Path'First;
   begin
      if Path'Length = 0 then
         return True;
      elsif Path'Length > MAXIMUM_PLACE then
         return False;
      end if;
      for I in Path'Range loop
         if Path (I) = PATH_SEPARATOR or else I = Path'Last then
            declare
               Last : constant Natural := (if Path (I) = PATH_SEPARATOR then I - 1 else I);
            begin
               if Last < First or else not Valid_Name (Path (First .. Last)) then
                  return False;
               end if;
            end;
            First := I + 1;
         end if;
      end loop;
      return Path (Path'Last) /= PATH_SEPARATOR;
   end Valid_Below;

   function Valid_Root (Root : String) return Boolean is
     (Root'Length >= 2 and then Root (Root'First) = VOLUME_MARK and then
      Valid_Below (Root (Root'First + 1 .. Root'Last)));

   function Bad_Place (What : String) return Failures.Failure is
     (Failures.Failed
        (Failures.Invalid_Argument, What & " is not a place: a root such as ""@system"" and names below it",
         "no '.', '..', '\' or empty names; the filesystem checks the place against the manifest"));

   function Count_Cell (Value : Unsigned_64) return CCL.Objects.Cell is
     (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Value, Unsigned_64 (Integer_64'Last)))));

   function Fetch_Image (Id : Unsigned_64) return CCL.Host_Values.Value is
      Image : CCL.Objects.Image := CCL.Objects.Empty (Net_Contracts.Fetch);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (N.FETCH_FIELDS), Step);
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Image, Count_Cell (Id), Step);
      end if;
      return CCL.Host_Values.Object_Constant (Image);
   end Fetch_Image;

   procedure Reply_Image (S : CCL_Fetch.Report; Image : out CCL.Objects.Image; Built : out Boolean) is
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
      Image := CCL.Objects.Empty (Net_Contracts.Reply);
      Put (CCL.Objects.Product_Cell (N.REPLY_FIELDS));
      Put (CCL.Objects.Variant_Cell (CCL_Fetch.Outcome'Pos (S.Ended_As) + 1));
      Put (CCL.Objects.Unit_Cell);
      Put (Count_Cell (Unsigned_64 (S.Status)));
      Put (Count_Cell (S.Bytes));
      if S.Expected_Known then
         Put (CCL.Objects.Variant_Cell (N.EXPECTED_BYTES));
         Put (Count_Cell (S.Expected));
      else
         Put (CCL.Objects.Variant_Cell (N.EXPECTED_UNKNOWN));
         Put (CCL.Objects.Unit_Cell);
      end if;
      Put (Count_Cell (Unsigned_64 (S.Redirects)));
      Text (if S.Sha256_Known then S.Sha256 else "");
      Text (S.Url (1 .. Natural'Min (S.Url_Length, REPLY_URL_BYTES)));
      Text (S.Detail.Data (1 .. Natural'Min (S.Detail.Length, REPLY_DETAIL_BYTES)));
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Net_Contracts.Reply);
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

   procedure Run_Fetch
     (Argument : CCL.Host_Values.Value; Now_Ms : Unsigned_64; Streams : in out CCL_Stream_Table.Table;
      Reply : in out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Net_Contracts.Request, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "net.fetch takes a Fetch_Request");
         return;
      end if;
      declare
         Root_Cursor : constant Views.Cursor := Views.Root (Object);
         function Field (K : Positive) return Views.Cursor is (Views.Field (Object, Root_Cursor, K));
         Into : constant Views.Cursor := Field (N.REQUEST_INTO);
         Url : constant String := Views.Text (Object, Field (N.REQUEST_URL));
         Root : constant String := Views.Text (Object, Views.Field (Object, Into, N.PLACE_ROOT));
         Path : constant String := Views.Text (Object, Views.Field (Object, Into, N.PLACE_PATH));
         Name : constant String := Views.Text (Object, Field (N.REQUEST_NAME));
         Pin_Field : constant Views.Cursor := Field (N.REQUEST_PIN);
         Pinned : constant Boolean := Natural (Views.Alternative (Object, Pin_Field)) = N.PIN_SHA256;
         Pin : constant String := (if Pinned then Views.Text (Object, Views.Payload (Object, Pin_Field)) else "");
         Replace : constant Boolean :=
           Natural (Views.Alternative (Object, Field (N.REQUEST_EXISTING))) = N.EXISTING_REPLACE;
         Max_Bytes : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Field (N.REQUEST_MAX_BYTES)));
         Redirects : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Field (N.REQUEST_MAX_REDIRECTS)));
         Deadline : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Field (N.REQUEST_DEADLINE)));
         Id : Unsigned_64;
         Result : CCL_Fetch.Start_Result;
         Why : CCL_Fetch.Short_Text;
         Row : Natural;
      begin
         --  The CCL range types check the limits; checked again here.
         if Max_Bytes not in 1 .. N.MAXIMUM_BYTES or else Redirects not in 0 .. N.MAXIMUM_REDIRECTS
           or else Deadline not in 1 .. N.MAXIMUM_DEADLINE_MS
         then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "a Fetch_Request limit is out of range",
               "max_bytes 1 B-1 TiB, max_redirects 0-10, deadline_ms 1-86400000");
            return;
         elsif not Valid_Root (Root) or else not Valid_Below (Path) then
            Reply.Why := Bad_Place ("""" & Root & """ """ & Path & """");
            return;
         elsif not Valid_Name (Name) then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, """" & Name & """ is not one entry's name (no '/', '\', '.' or '..')");
            return;
         end if;
         Row := Free_Row (Streams);
         if Row = 0 then
            Reply.Why := Failures.Failed
              (Failures.Exhausted, "this session follows" & Natural'Image (MAXIMUM_FOLLOWED) &
               " fetches that are still running");
            return;
         end if;
         CCL_Fetch.Start
           (Url, Root, Path, Name, Pinned, Pin, (if Replace then CCL_Fetch.Replace else CCL_Fetch.Refuse),
            Unsigned_64 (Max_Bytes), Natural (Redirects), Positive (Deadline), Now_Ms, Id, Result, Why);
         if Result /= CCL_Fetch.Started then
            Reply.Why := Failures.Failed
              ((case Result is
                  when CCL_Fetch.Not_Granted => Failures.Not_Granted,
                  when CCL_Fetch.Unavailable => Failures.Unavailable,
                  when others => Failures.Refused),
               "net.fetch: " & Why.Data (1 .. Why.Length),
               (if Result = CCL_Fetch.Not_Granted
                then "the manifest's (filesystem ...) scope must grant write and create there, and its " &
                     "(tls ""HOST:PORT"") scope the host"
                else ""));
            return;
         end if;
         Starts := Starts + 1;
         Table (Row) := (Used => True, Id => Id, Started => Starts, others => <>);
         CCL_Stream_Table.Open_Task (Streams, Table (Row).Reply, Pinned => True);
         Reply := (Value => Fetch_Image (Id), Success => True, Why => <>);
      end;
   end Run_Fetch;

   procedure Unpack_Image (S : CCL_Archive.Report; Image : out CCL.Objects.Image; Built : out Boolean) is
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Text (Value : CCL_Archive.Text; Most : Natural) is
      begin
         if Step = CCL.Objects.Added then
            CCL.Objects.Append_Text (Image, Value.Data (1 .. Natural'Min (Value.Length, Most)), Step);
         end if;
      end Text;
   begin
      Image := CCL.Objects.Empty (Archive_Contracts.Reply);
      Put (CCL.Objects.Product_Cell (A.REPLY_FIELDS));
      Put (CCL.Objects.Variant_Cell (CCL_Archive.Outcome'Pos (S.Ended_As) + 1));
      Put (CCL.Objects.Unit_Cell);
      Put (Count_Cell (S.Entries));
      Put (Count_Cell (S.Bytes));
      Put (Count_Cell (Unsigned_64 (S.Skipped)));
      Text (S.Reason, REPLY_DETAIL_BYTES);
      Text (S.Entry_Name, REPLY_URL_BYTES);
      Text (S.Detail, REPLY_DETAIL_BYTES);
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Archive_Contracts.Reply);
   end Unpack_Image;

   procedure Run_Unpack (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Archive_Contracts.Request, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "archive.unpack takes an Unpack_Request");
         return;
      end if;
      declare
         Root_Cursor : constant Views.Cursor := Views.Root (Object);
         function Field (K : Positive) return Views.Cursor is (Views.Field (Object, Root_Cursor, K));
         From : constant Views.Cursor := Field (A.REQUEST_FROM);
         Into : constant Views.Cursor := Field (A.REQUEST_INTO);
         From_Root : constant String := Views.Text (Object, Views.Field (Object, From, A.PLACE_ROOT));
         From_Path : constant String := Views.Text (Object, Views.Field (Object, From, A.PLACE_PATH));
         Into_Root : constant String := Views.Text (Object, Views.Field (Object, Into, A.PLACE_ROOT));
         Into_Path : constant String := Views.Text (Object, Views.Field (Object, Into, A.PLACE_PATH));
         Name : constant String := Views.Text (Object, Field (A.REQUEST_NAME));
         Directory : constant String := Views.Text (Object, Field (A.REQUEST_DIRECTORY));
         Skip : constant Boolean := Natural (Views.Alternative (Object, Field (A.REQUEST_SPECIALS))) = A.SPECIALS_SKIP;
         Max_Entries : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Field (A.REQUEST_MAX_ENTRIES)));
         Max_Bytes : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Field (A.REQUEST_MAX_BYTES)));
         Result : CCL_Archive.Report;
         Available, Built : Boolean;
         Image : CCL.Objects.Image;
      begin
         if Max_Entries not in 1 .. A.MAXIMUM_ENTRIES or else Max_Bytes not in 1 .. A.MAXIMUM_BYTES then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "an Unpack_Request limit is out of range",
               "max_entries 1-10000000, max_bytes 1 B-1 TiB");
            return;
         elsif not Valid_Root (From_Root) or else not Valid_Below (From_Path) then
            Reply.Why := Bad_Place ("""" & From_Root & """ """ & From_Path & """");
            return;
         elsif not Valid_Root (Into_Root) or else not Valid_Below (Into_Path) then
            Reply.Why := Bad_Place ("""" & Into_Root & """ """ & Into_Path & """");
            return;
         elsif not Valid_Name (Name) or else not Valid_Name (Directory) then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "name and directory are each one entry's name (no '/', '\', '.' or '..')");
            return;
         end if;
         CCL_Archive.Unpack
           (From_Root, From_Path, Name, Into_Root, Into_Path, Directory, Skip, Natural (Max_Entries),
            Unsigned_64 (Max_Bytes), Result, Available);
         if not Available then
            Reply.Why := Failures.Failed (Failures.Unavailable, "archive.unpack: " &
                                          Result.Detail.Data (1 .. Result.Detail.Length));
            return;
         end if;
         Unpack_Image (Result, Image, Built);
         if Built then
            Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
         else
            Reply.Why := Failures.Failed (Failures.Exhausted, "archive.unpack: the reply does not fit one value");
         end if;
      end;
   end Run_Unpack;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value; Now_Ms : Unsigned_64;
      Streams : in out CCL_Stream_Table.Table; Reply : out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if Binding = A.Binding_Of (A.Unpack) then
         Run_Unpack (Argument, Reply);
         return;
      elsif not (for some Op in N.Operation => Binding = N.Binding_Of (Op)) then
         return;
      end if;
      declare
         Op : constant N.Operation := N.Operation'Val (Binding - N.FIRST_BINDING);
      begin
         if Op = N.Fetch then
            Run_Fetch (Argument, Now_Ms, Streams, Reply);
            return;
         end if;
         if Argument.Kind = CCL.Host_Values.Object_Value then
            Views.Capture (Object, Net_Contracts.Fetch, Argument.Object, Captured);
         end if;
         if not Captured then
            Reply.Why := Failures.Failed (Failures.Invalid_Argument, "net: takes a Fetch");
            return;
         end if;
         declare
            Id : constant Integer_64 :=
              CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 1)));
            Row : constant Natural := (if Id < 0 then 0 else Find (Unsigned_64 (Id)));
         begin
            if Row = 0 then
               Reply.Why := Failures.Failed (Failures.Invalid_Argument, "net: this session does not follow that fetch");
               return;
            end if;
            case Op is
               when N.Reply =>
                  Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Table (Row).Reply)),
                            Success => True, Why => <>);
               when N.Status =>
                  declare
                     Now : CCL_Fetch.Report := Table (Row).Last;
                     Found, Built : Boolean;
                     Image : CCL.Objects.Image;
                  begin
                     if not Table (Row).Finished then
                        CCL_Fetch.Poll (Table (Row).Id, Now, Found);
                     end if;
                     Reply_Image (Now, Image, Built);
                     if Built then
                        Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
                     end if;
                  end;
               when N.Cancel =>
                  if not Table (Row).Finished then
                     CCL_Fetch.Cancel (Table (Row).Id);
                  end if;
                  Reply := (Value => Fetch_Image (Table (Row).Id), Success => True, Why => <>);
               when N.Fetch =>
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
      CCL_Fetch.Pump (Now_Ms, Due);
      for K in Table'Range loop
         if Table (K).Used and then not Table (K).Finished then
            declare
               S : CCL_Fetch.Report;
               Found, Built, Completed : Boolean;
               Image : CCL.Objects.Image;
            begin
               CCL_Fetch.Poll (Table (K).Id, S, Found);
               if not Found or else S.Ended_As /= CCL_Fetch.Running then
                  Table (K).Finished := True;
                  if not Found then
                     S.Ended_As := CCL_Fetch.Cancelled;
                     S.Detail := CCL_Fetch.Short ("the fetch was lost");
                  end if;
                  Table (K).Last := S;
                  Reply_Image (S, Image, Built);
                  if Built then
                     CCL_Stream_Table.Complete_Task (Streams, Table (K).Reply, Image, Completed);
                  end if;
                  CCL_Fetch.Forget (Table (K).Id);
                  Arrived := True;
               end if;
            end;
         end if;
      end loop;
   end Pump;

   function Running return Boolean is (for some F of Table => F.Used and then not F.Finished);
   function Next_Due return Unsigned_64 is (Due);
end CCL_Net_Bindings;
