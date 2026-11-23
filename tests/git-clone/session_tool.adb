------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted driver of Git_Clone_Sessions for tests/git-clone/check.py: each
--  exchange the session asks for is answered by the host's git
--  (`git upload-pack --stateless-rpc`, protocol version 2 unless
--  PROTOCOL is "0"), the session's own request body as its input, and the
--  answer is given back in CHUNK-byte pieces.
--    session_tool REPO WORK PARENT DIRECTORY URL MAX_REFS MAX_OBJECTS MAX_BYTES CHUNK PROTOCOL
--  prints "CLONED refs objects files format" or "OUTCOME detail".
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Inflate_Streams;
with Git_Protocol;
with Git_Clone_Sessions;
with Host_Stores; use Host_Stores;

procedure Session_Tool is
   package Sessions is new Git_Clone_Sessions
     (Host_Store, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory, Rename, Exists);
   use type Sessions.Exchange;
   use type Sessions.Outcome;
   package IO renames Ada.Streams.Stream_IO;

   Repo : constant String := Argument (1);
   Work : constant String := Argument (2);
   Chunk : constant Positive := Positive'Value (Argument (9));
   Version_2 : constant Boolean := Argument (10) /= "0";
   S : Host_Store;
   C : Sessions.Session;

   procedure Put_File (Path, Text : String) is
      F : IO.File_Type;
   begin
      IO.Create (F, IO.Out_File, Path);
      String'Write (IO.Stream (F), Text);
      IO.Close (F);
   end Put_File;

   --  Run git upload-pack for the exchange; its answer's path.
   function Answer (E : Sessions.Exchange) return String is
      Request : constant String := Work & "/request";
      Output : constant String := Work & "/answer";
      Command : constant String :=
        (if Version_2 then "GIT_PROTOCOL=version=2 " else "")
        & "git upload-pack --stateless-rpc "
        & (if E = Sessions.Advertisement then "--advertise-refs '" & Repo & "' < /dev/null"
           else "'" & Repo & "' < '" & Request & "'")
        & " > '" & Output & "'";
      Args : GNAT.OS_Lib.Argument_List :=
        [new String'("-c"), new String'(Command)];
      Ok : Boolean;
   begin
      if E /= Sessions.Advertisement then
         Put_File (Request, Sessions.Request_Body (C));
      end if;
      GNAT.OS_Lib.Spawn ("/bin/sh", Args, Ok);
      for A of Args loop
         GNAT.OS_Lib.Free (A);
      end loop;
      return Output;
   end Answer;

   procedure Feed (Path : String) is
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Data : Inflate_Streams.Byte_Array (1 .. Size);
      F : IO.File_Type;
      Lo : Positive := 1;
   begin
      IO.Open (F, IO.In_File, Path);
      Inflate_Streams.Byte_Array'Read (IO.Stream (F), Data);
      IO.Close (F);
      while Lo <= Size loop
         Sessions.Take (C, S, Data (Lo .. Natural'Min (Lo + Chunk - 1, Size)));
         Lo := Lo + Chunk;
      end loop;
      Sessions.Response_Ended (C, S);
   end Feed;
begin
   Ada.Directories.Create_Path (Work);
   Sessions.Start (C, S, Argument (3), Argument (4), Argument (5),
                   (Maximum_Refs => Natural'Value (Argument (6)), Maximum_Objects => Natural'Value (Argument (7)),
                    Maximum_Bytes => Inflate_Streams.Byte_Count'Value (Argument (8))));
   while Sessions.Next (C) /= Sessions.Finished loop
      Feed (Answer (Sessions.Next (C)));
   end loop;
   if Sessions.Result (C) = Sessions.Cloned then
      Put_Line ("CLONED" & Sessions.Refs (C)'Image & Sessions.Objects (C)'Image & Sessions.Files (C)'Image & " "
                & Git_Protocol.Format_Name (Sessions.Format (C)));
   else
      Put_Line (Sessions.Name (Sessions.Result (C)) & " " & Sessions.Detail (C));
   end if;
end Session_Tool;
