------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ccl-remote: the LAN remote session (tools/cubit-remote is its host
--  client). Off unless its launch parameters say listen => LAN; then it
--  makes a pairing token, shows it at the device, listens on every
--  configured address, and serves one framed request per connection
--  (Remote_Wire) for the paired principal: Diagnose always, Files, Builds
--  and Mail only as the parameters grant.
--
--  @description
--  Plaintext on the LAN: the token authenticates every request but nothing
--  is encrypted (TLS is the next step: tls.svc is client-only today; see
--  tests/ccl-remote/README.md). What the host does is shown to the person
--  at the device and logged ("ccl-remote: host <op> ..."): the operation,
--  its CCL source or place and name (cut to one short line) and how it
--  ended (Remote_Activity). Never shown or logged: the token, file
--  contents, evaluation results.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CCL_Native_Bindings;
with Parameters;
with Startup;
pragma Unreferenced (Startup);   --  its body checks the startup table first
with Remote_Wire;
with Remote_Transport;
with Remote_Pairing;
with Remote_Host;
with Remote_Files;
with Remote_Jobs;
with Remote_Activity;

procedure Main is
   package W renames Remote_Wire;
   use type W.Status;
   use type W.Job_State;
   use type W.Operation;
   use type W.Place;
   use type Remote_Activity.Kind;
   use type Parameters.Remote_Listen_Value;
   use type Parameters.Pairing_Display_Value;

   P : constant Parameters.Remote_Parameters_Value := Parameters.Value;
   --  The one port the manifest's listen scope names.
   LAN_Port : constant Unsigned_16 := 8445;

   --  Deadlines (monotonic milliseconds from now).
   Request_Read_Ms : constant := 10_000;
   Response_Write_Ms : constant := 30_000;
   --  netstack's interface events go to netmgr only: addresses are asked
   --  for again on this period (sooner while there are none yet).
   Address_Refresh_Ms : constant := 10_000;
   Address_Wait_Ms : constant := 1_000;
   --  While a build runs its outlets are drained this often; while the
   --  pairing window is open its input is polled this often.
   Job_Drain_Ms : constant := 50;
   Window_Poll_Ms : constant := 100;
   --  Wrong tokens before the endpoint stops answering until it restarts
   --  (the token is 128 bits; this only bounds a misbehaving client).
   Maximum_Failed_Attempts : constant := 32;

   Grants : constant W.Grant_Set :=
     [W.Diagnose => True, W.Files => P.files, W.Builds => P.builds, W.Mail => P.mail];
   Failed_Attempts : Natural := 0;
   Paired : Boolean := False;
   OK : Boolean;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));
   function After (Milliseconds : Unsigned_64) return Unsigned_64 is
     (if Now > Unsigned_64'Last - Milliseconds then Unsigned_64'Last else Now + Milliseconds);

   procedure Log (Text : String) is
      Ignore : Boolean;
   begin
      debugPrint ("ccl-remote: " & Text & ASCII.LF);
      if CCL_Native_Bindings.Logstore.Present then
         CuBit.Logging.Announce ("ccl-remote: " & Text, Ignore, CCL_Native_Bindings.Logstore.Value);
      end if;
   end Log;

   function Grant_Names return String is
     ("diagnose" & (if P.files then ", files" else "") & (if P.builds then ", builds" else "") &
      (if P.mail then ", mail" else ""));

   function Addresses return String is
      function Join (From : Positive) return String is
        (if From > Remote_Transport.Listening then ""
         elsif From = Remote_Transport.Listening then Remote_Transport.Address (From)
         else Remote_Transport.Address (From) & " " & Join (From + 1));
   begin
      return Join (1);
   end Addresses;

   --  For activity lines (this runtime keeps no enumeration images).
   function Verb (Op : W.Operation) return String is
     (case Op is
         when W.Hello => "hello", when W.Evaluate => "evaluate", when W.List => "list",
         when W.Stat => "stat", when W.Get => "get", when W.Put | W.Commit => "put",
         when W.Remove => "rm", when W.Build_Start => "build", when W.Build_Poll => "build poll",
         when W.Build_Cancel => "build cancel");
   function Place_Name (Where : W.Place) return String is
     (case Where is
         when W.None => "", when W.Workspace => "workspace",
         when W.Mail_To_Device => "mail/to-device", when W.Mail_To_Host => "mail/to-host");
   --  What the person reads: a mailbox operation is mail, whatever its verb.
   function Activity_Kind (Op : W.Operation; Where : W.Place) return Remote_Activity.Kind is
     (if Where in W.Mail_To_Device | W.Mail_To_Host and then Op in W.List .. W.Remove
      then Remote_Activity.Mail
      else (case Op is
               when W.Hello => Remote_Activity.Hello, when W.Evaluate => Remote_Activity.Evaluate,
               when W.List => Remote_Activity.List, when W.Stat => Remote_Activity.Stat,
               when W.Get => Remote_Activity.Get, when W.Put | W.Commit => Remote_Activity.Put,
               when W.Remove => Remote_Activity.Remove,
               when W.Build_Start | W.Build_Poll | W.Build_Cancel => Remote_Activity.Build));
   function Outcome_Of (Result : W.Status) return Remote_Activity.Outcome is
     (case Result is
         when W.OK => Remote_Activity.OK,
         when W.Unauthorized | W.Not_Granted | W.Locked => Remote_Activity.Refused,
         when others => Remote_Activity.Failed);

   --  One operation of the host: kept for the window and logged.
   procedure Note (Op : W.Operation; Where : W.Place; Argument : String; Result : Remote_Activity.Outcome) is
      Kind : constant Remote_Activity.Kind := Activity_Kind (Op, Where);
      --  A mailbox verb says what was done in it ("mail get mail/to-host/x").
      Shown : constant String :=
        (if Kind = Remote_Activity.Mail then Verb (Op) & (if Argument'Length > 0 then " " else "") & Argument
         else Argument);
   begin
      Remote_Activity.Add (Now, Kind, Shown, Result);
      Log ("host " & Remote_Activity.Line (Remote_Activity.Entry_At (Remote_Activity.Count)));
      Remote_Pairing.Activity_Changed;
   end Note;
   --  "workspace/upload/blob.bin", "mail/to-host" (no name).
   function Path_Of (Where : W.Place; Name : String) return String is
     (Place_Name (Where) & (if Name'Length > 0 then "/" & Name else ""));

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   ---------------------------------------------------------------------------
   --  One request
   ---------------------------------------------------------------------------
   Header_Text : String (1 .. W.Request_Header_Bytes);
   Body_Text : String (1 .. W.Maximum_Request_Body);
   Reply : W.Byte_Array (1 .. W.Maximum_Response_Body);

   procedure Respond (Result : W.Status; Length : W.Response_Body_Length) is
      Header : constant W.Response_Header := W.Encode_Response_Header (Result, Length);
      Text : String (1 .. W.Response_Header_Bytes + Length);
      Sent : Boolean;
   begin
      for I in Header'Range loop
         Text (I) := Character'Val (Header (I));
      end loop;
      for I in 1 .. Length loop
         Text (W.Response_Header_Bytes + I) := Character'Val (Reply (I));
      end loop;
      Remote_Transport.Write_All (Text, After (Response_Write_Ms), Sent);
   end Respond;

   procedure Put_Text (Text : String; At_Index : Positive; Length : out Natural) is
   begin
      Length := Natural'Min (Text'Length, Reply'Last - At_Index + 1);
      for I in 1 .. Length loop
         Reply (At_Index + I - 1) := Character'Pos (Text (Text'First + I - 1));
      end loop;
   end Put_Text;

   procedure Serve is
      Header : W.Request_Header;
      Item : W.Request;
      Result : W.Status;
      Read : Boolean;
      Length : Natural := 0;
      Deadline : constant Unsigned_64 := After (Request_Read_Ms);
   begin
      Remote_Transport.Read_Exact (Header_Text, Deadline, Read);
      if not Read then
         return;
      end if;
      declare
         Bytes : W.Byte_Array (1 .. W.Request_Header_Bytes);
      begin
         for I in Bytes'Range loop
            Bytes (I) := Character'Pos (Header_Text (I));
         end loop;
         W.Decode_Header (Bytes, Header, Result);
      end;
      if Result /= W.OK then
         Respond (Result, 0);
         return;
      end if;
      --  The token first: nothing else is looked at for a stranger.
      if Failed_Attempts >= Maximum_Failed_Attempts then
         Respond (W.Locked, 0);
         return;
      elsif not Remote_Pairing.Matches (Header.Credential) then
         Failed_Attempts := Failed_Attempts + 1;
         Note (Header.Op, W.None, "with a wrong pairing token (" & Decimal (Unsigned_64 (Failed_Attempts)) &
               " of" & Natural'Image (Maximum_Failed_Attempts) & ")", Remote_Activity.Refused);
         if Failed_Attempts = Maximum_Failed_Attempts then
            Log ("too many wrong tokens: locked until ccl-remote.app restarts");
         end if;
         Respond (W.Unauthorized, 0);
         return;
      end if;
      if not W.Admits (Grants, Header.Op, Header.Where) then
         Note (Header.Op, Header.Where, Path_Of (Header.Where, "") & " (not granted)", Remote_Activity.Refused);
         Respond (W.Not_Granted, 0);
         return;
      end if;
      Remote_Transport.Read_Exact (Body_Text (1 .. Header.Length), Deadline, Read);
      if not Read then
         return;
      end if;
      declare
         Data : W.Byte_Array (1 .. Header.Length);
      begin
         for I in Data'Range loop
            Data (I) := Character'Pos (Body_Text (I));
         end loop;
         W.Decode_Body (Header, Data, Item, Result);
         if Result /= W.OK then
            Note (Header.Op, Header.Where, "(a malformed request)", Remote_Activity.Failed);
            Respond (Result, 0);
            return;
         end if;
         Reply := [others => 0];
         declare
            Name : constant String := Item.Name (1 .. Item.Name_Last);
         begin
            case Item.Op is
               when W.Hello =>
                  Reply (1) := W.Grant_Bits (Grants);
                  W.Put_U32 (Reply, 2, W.Maximum_Chunk_Bytes);
                  Put_Text ("ccl-remote 0.1.0", 6, Length);
                  Length := Length + 5;
                  if not Paired then
                     Paired := True;
                     Remote_Pairing.Paired (Remote_Transport.Peer);
                     Log ("a host paired" & (if Remote_Transport.Peer = "" then ""
                                             else " from " & Remote_Transport.Peer));
                  end if;
                  Note (Item.Op, Item.Where, "", Remote_Activity.OK);

               when W.Evaluate =>
                  declare
                     Source : String (1 .. Item.Data_Length);
                     Text : String (1 .. W.Maximum_Response_Body - 1);
                     Text_Length : Natural;
                     Succeeded : Boolean;
                  begin
                     for I in Source'Range loop
                        Source (I) := Character'Val (Data (Item.Data_First + I - 1));
                     end loop;
                     Remote_Host.Evaluate (Source, Succeeded, Text, Text_Length);
                     Note (Item.Op, Item.Where, Source,
                           (if Succeeded then Remote_Activity.OK else Remote_Activity.Failed));
                     Reply (1) := (if Succeeded then 1 else 0);
                     Put_Text (Text (1 .. Text_Length), 2, Length);
                     Length := Length + 1;
                  end;

               when W.List =>
                  Remote_Files.List (Item.Where, Name, Reply, Length, Result);

               when W.Stat =>
                  declare
                     Size : Unsigned_64;
                     Hash : W.Digest;
                  begin
                     Remote_Files.Stat (Item.Where, Name, Size, Hash, Result);
                     if Result = W.OK then
                        W.Put_U64 (Reply, 1, Size);
                        for I in W.Digest_Index loop
                           Reply (W.Offset_Bytes + I) := Hash (I);
                        end loop;
                        Length := W.Offset_Bytes + W.Digest_Bytes;
                     end if;
                  end;

               when W.Get =>
                  Remote_Files.Get (Item.Where, Name, Item.Offset, Reply, Length, Result);

               when W.Put =>
                  Remote_Files.Put
                    (Item.Where, Name, Item.Offset,
                     Data (Item.Data_First .. Item.Data_First + Item.Data_Length - 1), Result);

               when W.Commit =>
                  Remote_Files.Commit (Item.Where, Name, Item.Size, Item.Hash, Result);
                  Note (Item.Op, Item.Where, Path_Of (Item.Where, Name) & " (" & Decimal (Item.Size) & " bytes" &
                        (if Result = W.OK then ", SHA-256 checked)" else ")"), Outcome_Of (Result));

               when W.Remove =>
                  Remote_Files.Remove (Item.Where, Name, Result);

               when W.Build_Start =>
                  declare
                     Job : Unsigned_32;
                  begin
                     Remote_Jobs.Start (Name, Job, Result);
                     if Result = W.OK then
                        W.Put_U32 (Reply, 1, Job);
                        Length := W.Job_Bytes;
                     end if;
                     Note (Item.Op, Item.Where,
                           Name & (if Result = W.OK then " (job" & Job'Image & ")" else ""), Outcome_Of (Result));
                  end;

               when W.Build_Poll =>
                  declare
                     State : W.Job_State;
                     Code : Unsigned_32;
                     Total, First_Kept : Unsigned_64;
                     Fixed : constant := 1 + 4 + 8 + 8;
                     Output_Length : Natural;
                  begin
                     Remote_Jobs.Poll (Item.Job, Item.Offset, State, Code, Total, First_Kept,
                                       Reply (Fixed + 1 .. Reply'Last), Output_Length);
                     Reply (1) := W.Job_State'Enum_Rep (State);
                     W.Put_U32 (Reply, 2, Code);
                     W.Put_U64 (Reply, 6, Total);
                     W.Put_U64 (Reply, 14, First_Kept);
                     Length := Fixed + Output_Length;
                     Result := (if State = W.Unknown_Job then W.Not_Found else W.OK);
                  end;

               when W.Build_Cancel =>
                  declare
                     State : W.Job_State;
                  begin
                     Remote_Jobs.Cancel (Item.Job, State);
                     Reply (1) := W.Job_State'Enum_Rep (State);
                     Length := 1;
                     Result := (if State = W.Unknown_Job then W.Not_Found else W.OK);
                     Note (Item.Op, Item.Where, "job" & Item.Job'Image, Outcome_Of (Result));
                  end;
            end case;
         end;
      end;
      --  File operations, once each: a download at its first chunk, an
      --  upload when it is committed (above), or a chunk that failed.
      if Item.Op in W.List | W.Stat | W.Remove
        or else (Item.Op = W.Get and then (Item.Offset = 0 or else Result /= W.OK))
        or else (Item.Op = W.Put and then Result /= W.OK)
      then
         Note (Item.Op, Item.Where, Path_Of (Item.Where, Item.Name (1 .. Item.Name_Last)), Outcome_Of (Result));
      end if;
      if Result /= W.OK then
         Length := (if Item.Op = W.Build_Poll then Length else 0);
      end if;
      Respond (Result, Length);
   end Serve;

   Port : Unsigned_16;
   Changed : Boolean;
   Next_Refresh : Unsigned_64 := 0;
   Shown : Boolean;
begin
   if P.listen = Parameters.Off then
      Log ("off (launch parameter listen => Remote_Listen.Off)");
      return;
   end if;
   Port := LAN_Port;
   if P.work.Length = 0 or else P.work.Length > Remote_Files.Maximum_Root_Bytes then
      Log ("the work root parameter is empty or too long; not listening");
      return;
   end if;
   Log ("DEVELOPMENT PLAINTEXT on the LAN, port" & Port'Image & "; grants: " & Grant_Names);
   Remote_Pairing.Generate (OK);
   if not OK then
      Log ("the CPU gave no random words for a pairing token; not listening");
      return;
   end if;
   Remote_Host.Initialize (OK);
   if not OK then
      Log ("the CCL environment did not start; not listening");
      return;
   end if;
   Remote_Files.Initialize (P.work.Data (1 .. P.work.Length), OK);
   if not OK then
      Log ("the work places under " & P.work.Data (1 .. P.work.Length) & " could not be made; not listening");
      return;
   end if;
   Remote_Jobs.Initialize (P.work.Data (1 .. P.work.Length));
   Remote_Transport.Initialize (OK);
   if not OK then
      Log ("no network authority (the startup entry needs (network approve-declared)); not listening");
      return;
   end if;
   Remote_Transport.Refresh (Port, Changed);
   Next_Refresh := After (if Remote_Transport.Listening = 0 then Address_Wait_Ms else Address_Refresh_Ms);
   Remote_Pairing.Show
     ((if P.display = Parameters.Window then Remote_Pairing.Window else Remote_Pairing.Kernel_Console),
      Addresses, Grant_Names, Shown);
   if not Shown then
      Log ("the pairing code could not be shown (no desktop?); not listening");
      Remote_Transport.Close;
      return;
   end if;
   Log ("ready");
   loop
      if Now >= Next_Refresh then
         Remote_Transport.Refresh (Port, Changed);
         if Changed then
            Log ("listening on " & Addresses);
            Remote_Pairing.Update (Addresses);
         end if;
         Next_Refresh := After (if Remote_Transport.Listening = 0 then Address_Wait_Ms else Address_Refresh_Ms);
      end if;
      Remote_Jobs.Pump;
      Remote_Pairing.Service;
      Remote_Transport.Accept_Connection
        (Unsigned_64'Min
           (Next_Refresh,
            (if Remote_Jobs.Running then After (Job_Drain_Ms)
             elsif Remote_Pairing.Window_Open then After (Window_Poll_Ms)
             else Unsigned_64'Last)),
         OK);
      if OK then
         Serve;
         Remote_Transport.Close_Connection;
      end if;
   end loop;
end Main;
