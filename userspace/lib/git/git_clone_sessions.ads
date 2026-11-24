------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A clone over git's smart HTTP, protocol version 2 (SELF-003 step 2): the
--  conversation, without the transport. A plain typed Ada API, so the same
--  engine serves git.clone in CCL, a future service a jj front end calls,
--  and the hosted tests (which answer with the host's git upload-pack).
--
--  @description
--  gitprotocol-http and gitprotocol-v2; facts only. A clone is three
--  exchanges with the repository URL U, each one request and its response
--  body (the caller does HTTP, TLS and redirects):
--  1. Advertisement: GET U/info/refs?service=git-upload-pack with
--     "Git-Protocol: version=2". The capability advertisement: ls-refs and
--     fetch, and the object format (SHA-1 unless "object-format=sha256").
--  2. List_Refs: POST U/git-upload-pack, Request_Body "command=ls-refs"
--     with peel, symrefs and the prefixes HEAD, refs/heads/, refs/tags/.
--  3. Fetch_Pack: POST U/git-upload-pack, Request_Body "command=fetch"
--     with ofs-delta, no-progress, a want for every ref's object, done.
--     Skipped when the repository has no refs (No_Refs).
--  Every byte of a response goes through the proved readers (Git_Packets,
--  Git_Protocol) and the pack into Git_Clones, which writes the clone.
--
--  Use: Start; then while Next is not Finished, send the request Next
--  names (Request_Body for a POST), give the response body to Take as it
--  arrives and call Response_Ended at its end. A transport failure is
--  Abandon. Result says how it ended.
------------------------------------------------------------------------------
pragma Ada_2022;
with Inflate_Streams; use Inflate_Streams;
with Git_Protocol;
with Git_Clones;
private with Git_Packets;
private with Git_Lists;
private with Git_Strings;

generic
   type Store is limited private;
   with procedure Make_Directory (S : in out Store; Path : String; Ok : out Boolean);
   with procedure Create_File (S : in out Store; Path : String; Ok : out Boolean);
   with procedure Write (S : in out Store; Offset : Byte_Count; Data : Byte_Array; Ok : out Boolean);
   with procedure Close_File (S : in out Store);
   with procedure Remove_File (S : in out Store; Path : String);
   with procedure Remove_Directory (S : in out Store; Path : String);
   with procedure Rename (S : in out Store; From, To : String; Ok : out Boolean);
   with function Exists (S : in out Store; Path : String) return Boolean;
package Git_Clone_Sessions is
   package Clones is new Git_Clones
     (Store, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory, Rename, Exists);

   type Exchange is (Advertisement, List_Refs, Fetch_Pack, Finished);

   --  The service's paths below the repository URL (gitprotocol-http).
   ADVERTISEMENT_PATH : constant String := "/info/refs?service=git-upload-pack";
   UPLOAD_PACK_PATH : constant String := "/git-upload-pack";
   --  The request and response bodies' media types, and the header that
   --  asks for protocol version 2.
   REQUEST_TYPE : constant String := "application/x-git-upload-pack-request";
   RESULT_TYPE : constant String := "application/x-git-upload-pack-result";
   PROTOCOL_HEADER : constant String := "Git-Protocol: version=2";

   --  Every ceiling is the caller's (no defaults).
   type Limits is record
      Maximum_Refs : Git_Protocol.Ref_Count := 0;
      Maximum_Objects : Natural := 0;
      --  The pack, and every object inflated, together.
      Maximum_Bytes : Byte_Count := 0;
   end record;

   type Outcome is
     (In_Progress,
      Cloned,
      Target_Exists,      --  the directory, or its temporary name, is there already
      Bad_Directory,      --  the directory is not one safe name
      Bad_Url,            --  the URL has a byte git's config could not hold
      Write_Failed,       --  the store refused a write; what was made is gone
      Not_Smart_Http,     --  the advertisement was refused (not a protocol v2 server)
      Bad_Refs,           --  the ref list was refused (a bad name or id, too many)
      Bad_Fetch,          --  the fetch response was refused
      Remote_Error,       --  the server's error message (Detail)
      Bad_Pack,           --  the pack reader refused the pack
      Bad_Delta,          --  a delta was refused, or its base is not in the pack
      Too_Many_Objects,   --  past the ceiling
      Too_Large,          --  past the byte ceiling
      Missing_Object,     --  a ref, commit or tree names an object not in the pack
      Bad_Object,         --  a commit or tree the readers refused
      Path_Too_Long,      --  a checked-out path past 4096 bytes
      No_Refs,            --  an empty repository
      Abandoned);         --  the caller abandoned it (a transport failure)

   function Name (O : Outcome) return String is
     (case O is
         when In_Progress => "In_Progress",
         when Cloned => "Cloned",
         when Target_Exists => "Target_Exists",
         when Bad_Directory => "Bad_Directory",
         when Bad_Url => "Bad_Url",
         when Write_Failed => "Write_Failed",
         when Not_Smart_Http => "Not_Smart_Http",
         when Bad_Refs => "Bad_Refs",
         when Bad_Fetch => "Bad_Fetch",
         when Remote_Error => "Remote_Error",
         when Bad_Pack => "Bad_Pack",
         when Bad_Delta => "Bad_Delta",
         when Too_Many_Objects => "Too_Many_Objects",
         when Too_Large => "Too_Large",
         when Missing_Object => "Missing_Object",
         when Bad_Object => "Bad_Object",
         when Path_Too_Long => "Path_Too_Long",
         when No_Refs => "No_Refs",
         when Abandoned => "Abandoned");

   type Session is limited private;

   --  Make ".DIRECTORY.cloning" under Parent (before anything is fetched,
   --  so a place that may not be written is refused first). Url is the
   --  repository's URL, recorded as origin.
   procedure Start (C : in out Session; S : in out Store; Parent, Directory, Url : String; Ceiling : Limits);

   function Next (C : Session) return Exchange;
   function Result (C : Session) return Outcome;
   function Detail (C : Session) return String;
   function Format (C : Session) return Git_Protocol.Object_Format;
   function Refs (C : Session) return Natural;
   function Objects (C : Session) return Natural;
   function Files (C : Session) return Natural;

   --  The POST body of List_Refs or Fetch_Pack: pkt-lines.
   function Request_Body (C : Session) return String
   with Pre => Next (C) in List_Refs | Fetch_Pack;

   --  The next bytes of the current response's body (decoded from its
   --  HTTP framing).
   procedure Take (C : in out Session; S : in out Store; Data : Byte_Array)
   with Pre => Data'Last < Positive'Last - 128;

   --  The current response's body ended: on to the next exchange, or (after
   --  the fetch) the clone is written and renamed into place.
   procedure Response_Ended (C : in out Session; S : in out Store);

   --  The transport failed (or was cancelled): everything made is removed.
   procedure Abandon (C : in out Session; S : in out Store; Detail : String);
private
   use Git_Strings;
   package Id_Lists is new Git_Lists (Git_Protocol.Object_Id);
   type Packet_Reader_Access is access Git_Packets.Reader;

   type Session is limited record
      Next : Exchange := Advertisement;
      Result : Outcome := In_Progress;
      Detail : Text;
      Ceiling : Limits;
      Clone : Clones.Clone;
      Packets : Packet_Reader_Access;
      Advert : Git_Protocol.Advertisement_Reader;
      Ref_List : Git_Protocol.Refs_Reader;
      Fetch : Git_Protocol.Fetch_Reader;
      Format_Named : Boolean := False;
      Ended : Boolean := False;
      Wants : Id_Lists.List;
      Ref_Total : Natural := 0;
   end record;
end Git_Clone_Sessions;
