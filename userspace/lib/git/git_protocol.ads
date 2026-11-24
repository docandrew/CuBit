------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git protocol version 2 over smart HTTP, the read side (SELF-003 step
--  2): object names, ref names, and readers for the three answers a clone
--  gets from git-upload-pack: the capability advertisement, the ls-refs
--  list and the fetch response.
--
--  @description
--  From git's documentation (gitprotocol-v2, gitprotocol-http,
--  git-check-ref-format); facts only, no code. The readers take packets
--  from Git_Packets, one at a time, and say what each means.
--
--  * Advertisement: an optional "# service=git-upload-pack" line and
--    flush (the smart-HTTP preamble of older servers), then "version 2",
--    then capability lines "key[=value]" up to a flush. ls-refs and fetch
--    must be offered; the object format is SHA-1 (git's default, also when
--    none is named) or SHA-256 (gitformat-... hash-function-transition:
--    "object-format=sha256"), and the other readers take that format.
--  * ls-refs: lines "<id> <refname>[ symref-target:<ref>][ peeled:<id>]"
--    up to a flush. Every name handed out is Safe_Ref_Name: "HEAD" or
--    "refs/..." by git's ref-name rules, printable ASCII only, so that as
--    a path under .git it names a file inside .git/refs (or .git/HEAD).
--  * fetch: sections; for a clone (wants and "done", nothing else asked
--    for) only "packfile", whose packets are side-band: channel 1 is pack
--    data, 2 progress text, 3 a fatal error from the server; then a flush.
--
--  Proved at level 2 (tests/git-codecs): every ref name and symref target
--  handed out is Safe_Ref_Name and every id is the format's lowercase hex
--  digits (40 or 64) decoded; pack data runs lie inside the packet; refusals are final.
------------------------------------------------------------------------------
pragma Ada_2022;
with Git_Packets; use Git_Packets;

package Git_Protocol with SPARK_Mode, Pure is
   use type Byte;
   --  git's object formats: object names are the SHA-1 (20 bytes, "sha1",
   --  the default) or the SHA-256 (32 bytes, "sha256") of the object.
   type Object_Format is (Sha1_Format, Sha256_Format);
   SHA1_ID_BYTES : constant := 20;
   SHA256_ID_BYTES : constant := 32;
   MAXIMUM_ID_BYTES : constant := SHA256_ID_BYTES;
   MAXIMUM_HEX_DIGITS : constant := 2 * MAXIMUM_ID_BYTES;
   subtype Id_Length is Positive range SHA1_ID_BYTES .. MAXIMUM_ID_BYTES;
   function Id_Bytes (F : Object_Format) return Id_Length is
     (case F is when Sha1_Format => SHA1_ID_BYTES, when Sha256_Format => SHA256_ID_BYTES);
   function Hex_Digits (F : Object_Format) return Positive is (2 * Id_Bytes (F));
   --  The format's name in the protocol and in a repository's config.
   function Format_Name (F : Object_Format) return String is
     (case F is when Sha1_Format => "sha1", when Sha256_Format => "sha256");

   --  An object name: Id_Bytes of its format, then zeros.
   type Object_Id is array (1 .. MAXIMUM_ID_BYTES) of Byte;
   NO_ID : constant Object_Id := [others => 0];

   function Lower_Hex (C : Character) return Boolean is (C in '0' .. '9' | 'a' .. 'f');
   function Is_Hex_Id (S : String; F : Object_Format) return Boolean is
     (S'Length = Hex_Digits (F) and then (for all C of S => Lower_Hex (C)));
   function To_Id (S : String; F : Object_Format) return Object_Id
   with Pre => Is_Hex_Id (S, F),
        Post => (for all K in Id_Bytes (F) + 1 .. MAXIMUM_ID_BYTES => To_Id'Result (K) = 0);
   function To_Hex (Id : Object_Id; F : Object_Format) return String
   with Post => To_Hex'Result'First = 1 and then Is_Hex_Id (To_Hex'Result, F);

   --  Named ceiling: a ref name (git has none; CuBit's paths are 4096).
   MAXIMUM_REF_NAME_BYTES : constant := 1_024;
   subtype Ref_Name_Length is Natural range 0 .. MAXIMUM_REF_NAME_BYTES;

   --  git-check-ref-format, plus printable ASCII: no control bytes, space,
   --  '~', '^', ':', '?', '*', '[', '\', or DEL.
   function Ref_Character (C : Character) return Boolean is
     (C in '!' .. '~' and then C not in '~' | '^' | ':' | '?' | '*' | '[' | '\');

   --  "HEAD", or "refs/" and more, where no component is empty or begins
   --  with '.', none ends with ".lock", there is no ".." and no "@{", and
   --  the name does not end with '/' or '.'.
   function Safe_Ref_Name (S : String) return Boolean is
     (S'Length in 1 .. MAXIMUM_REF_NAME_BYTES
      and then S'Last < Positive'Last - 5
      and then (S = "HEAD"
                or else (S'Length > 5 and then S (S'First .. S'First + 4) = "refs/"
                         and then (for all C of S => Ref_Character (C))
                         and then S (S'Last) /= '/' and then S (S'Last) /= '.'
                         and then (for all I in S'First .. S'Last - 1 =>
                                     not (S (I) = '/' and then S (I + 1) = '/')
                                     and then not (S (I) = '.' and then S (I + 1) = '.')
                                     and then not (S (I) = '@' and then S (I + 1) = '{')
                                     and then not (S (I) = '/' and then S (I + 1) = '.'))
                         and then (for all I in S'First .. S'Last - 4 =>
                                     (if S (I .. I + 4) = ".lock" then I + 4 < S'Last and then S (I + 5) /= '/')))));

   ---------------------------------------------------------------------------
   --  The capability advertisement (GET /info/refs?service=git-upload-pack).

   type Advertisement_Refusal is
     (No_Refusal,
      Not_Version_2,             --  a version 0/1 ref advertisement, or no "version 2"
      Bad_Capability,            --  a line that is not "key[=value]"
      No_Ls_Refs,                --  ls-refs is not offered
      No_Fetch,                  --  fetch is not offered
      Unsupported_Object_Format, --  an object format other than sha1 or sha256
      Unexpected_Packet);        --  a delimiter or response end, or data after the flush

   function Name (Why : Advertisement_Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Not_Version_2 => "Not_Version_2",
         when Bad_Capability => "Bad_Capability",
         when No_Ls_Refs => "No_Ls_Refs",
         when No_Fetch => "No_Fetch",
         when Unsupported_Object_Format => "Unsupported_Object_Format",
         when Unexpected_Packet => "Unexpected_Packet");

   type Reply_Kind is (Need_More, Ended, Refused);

   type Advertisement_Reader is private;
   function Valid (A : Advertisement_Reader) return Boolean;
   function Failed (A : Advertisement_Reader) return Boolean;
   function Ended (A : Advertisement_Reader) return Boolean;
   function Failure (A : Advertisement_Reader) return Advertisement_Refusal;
   --  What the server offered (meaningful once Ended).
   function Offers_Agent (A : Advertisement_Reader) return Boolean;
   function Offers_Object_Format (A : Advertisement_Reader) return Boolean;
   --  The repository's object format (SHA-1 when none is named).
   function Format (A : Advertisement_Reader) return Object_Format;

   procedure Start (A : out Advertisement_Reader)
   with Post => Valid (A) and then not Failed (A) and then not Ended (A);

   procedure Feed (A : in out Advertisement_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Reply_Kind)
   with Pre => Valid (A) and then Payload'First = 1 and then Payload'Last in 0 .. Positive'Last - 1,
        Post => Valid (A) and then (Reply = Refused) = Failed (A) and then (Reply = Ended) = Ended (A)
          and then (if Failed (A'Old) then Reply = Refused)
          and then (if Failed (A) then Failure (A) /= No_Refusal);

   ---------------------------------------------------------------------------
   --  ls-refs (POST /git-upload-pack, command=ls-refs, peel and symrefs).

   type Refs_Refusal is
     (No_Refusal,
      Bad_Ref_Line,      --  not "<id> <name>[ attributes]"
      Bad_Object_Id,     --  not the format's lowercase hex digits (40 or 64)
      Bad_Ref_Name,      --  not Safe_Ref_Name
      Bad_Attribute,     --  an attribute not asked for, or given twice
      Too_Many_Refs,     --  past the caller's ceiling
      Unexpected_Packet);

   function Name (Why : Refs_Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Ref_Line => "Bad_Ref_Line",
         when Bad_Object_Id => "Bad_Object_Id",
         when Bad_Ref_Name => "Bad_Ref_Name",
         when Bad_Attribute => "Bad_Attribute",
         when Too_Many_Refs => "Too_Many_Refs",
         when Unexpected_Packet => "Unexpected_Packet");

   type Ref_Reply_Kind is (Ref_Read, Refs_Ended, Refs_Refused);
   subtype Ref_Count is Natural;

   type Refs_Reader is private;
   function Valid (L : Refs_Reader) return Boolean;
   function Failed (L : Refs_Reader) return Boolean;
   function Ended (L : Refs_Reader) return Boolean;
   function Failure (L : Refs_Reader) return Refs_Refusal;
   function Count (L : Refs_Reader) return Ref_Count;
   function Maximum_Refs (L : Refs_Reader) return Ref_Count;
   function Format (L : Refs_Reader) return Object_Format;
   --  The last ref read.
   function Has_Ref (L : Refs_Reader) return Boolean;
   function Ref_Id (L : Refs_Reader) return Object_Id with Pre => Has_Ref (L);
   function Ref_Name (L : Refs_Reader) return String
   with Pre => Valid (L) and then Has_Ref (L), Post => Safe_Ref_Name (Ref_Name'Result);
   function Has_Symref (L : Refs_Reader) return Boolean;
   function Symref_Target (L : Refs_Reader) return String
   with Pre => Valid (L) and then Has_Symref (L), Post => Safe_Ref_Name (Symref_Target'Result);
   function Has_Peeled (L : Refs_Reader) return Boolean;
   function Peeled_Id (L : Refs_Reader) return Object_Id with Pre => Has_Peeled (L);

   procedure Start (L : out Refs_Reader; Maximum : Ref_Count; Format : Object_Format)
   with Post => Valid (L) and then not Failed (L) and then not Ended (L) and then Count (L) = 0
                  and then Maximum_Refs (L) = Maximum and then Git_Protocol.Format (L) = Format;

   procedure Feed (L : in out Refs_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Ref_Reply_Kind)
   with Pre => Valid (L) and then Payload'First = 1 and then Payload'Last in 0 .. Positive'Last - 1,
        Post => Valid (L) and then Maximum_Refs (L) = Maximum_Refs (L'Old)
          and then Format (L) = Format (L'Old)
          and then Count (L) <= Maximum_Refs (L)
          and then (Reply = Refs_Refused) = Failed (L) and then (Reply = Refs_Ended) = Ended (L)
          and then (if Failed (L'Old) then Reply = Refs_Refused)
          and then (if Failed (L) then Failure (L) /= No_Refusal)
          and then Has_Ref (L) = (Reply = Ref_Read)
          and then (if Reply = Ref_Read then Count (L) = Count (L'Old) + 1);

   ---------------------------------------------------------------------------
   --  The fetch response (POST /git-upload-pack, command=fetch, wants and
   --  "done").

   type Fetch_Refusal is
     (No_Refusal,
      Unexpected_Section,   --  a section a clone did not ask for
      Bad_Sideband,         --  an empty packet or a channel other than 1, 2, 3
      Remote_Error,         --  the server's channel 3 message (Message)
      Unexpected_Packet);

   function Name (Why : Fetch_Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Unexpected_Section => "Unexpected_Section",
         when Bad_Sideband => "Bad_Sideband",
         when Remote_Error => "Remote_Error",
         when Unexpected_Packet => "Unexpected_Packet");

   type Fetch_Reply_Kind is
     (Nothing,      --  a packet that carries nothing for the caller
      Pack_Data,    --  Payload (First .. Last) is pack data
      Progress,     --  Payload (First .. Last) is progress text
      Fetch_Ended,
      Fetch_Refused);

   type Fetch_Reply is record
      Kind : Fetch_Reply_Kind := Nothing;
      First : Positive := 1;
      Last : Natural := 0;
   end record;

   type Fetch_Reader is private;
   function Valid (F : Fetch_Reader) return Boolean;
   function Failed (F : Fetch_Reader) return Boolean;
   function Ended (F : Fetch_Reader) return Boolean;
   function Failure (F : Fetch_Reader) return Fetch_Refusal;
   --  The server's error text, or the unexpected section's name.
   function Message (F : Fetch_Reader) return String;

   procedure Start (F : out Fetch_Reader)
   with Post => Valid (F) and then not Failed (F) and then not Ended (F);

   procedure Feed (F : in out Fetch_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Fetch_Reply)
   with Pre => Valid (F) and then Payload'First = 1 and then Payload'Last in 0 .. Positive'Last - 1,
        Post => Valid (F)
          and then (Reply.Kind = Fetch_Refused) = Failed (F) and then (Reply.Kind = Fetch_Ended) = Ended (F)
          and then (if Failed (F'Old) then Reply.Kind = Fetch_Refused)
          and then (if Failed (F) then Failure (F) /= No_Refusal)
          and then (if Reply.Kind in Pack_Data | Progress then
                      Reply.First in Payload'Range and then Reply.Last in Reply.First .. Payload'Last);

   ---------------------------------------------------------------------------
   --  Requests: one pkt-line of Text.
   function Packet (Text : String) return String
   with Pre => Text'Length <= MAXIMUM_PAYLOAD_BYTES,
        Post => Packet'Result'Length = Text'Length + LENGTH_DIGITS;
   FLUSH : constant String := "0000";
   DELIMITER : constant String := "0001";

private
   subtype Ref_Buffer is String (1 .. MAXIMUM_REF_NAME_BYTES);
   MAXIMUM_MESSAGE_BYTES : constant := 256;
   subtype Message_Length is Natural range 0 .. MAXIMUM_MESSAGE_BYTES;

   type Advertisement_Phase is (Expect_Version, Expect_Service_Flush, In_Capabilities, Done_Phase, Failed_Phase);

   type Advertisement_Reader is record
      Phase : Advertisement_Phase := Expect_Version;
      Why : Advertisement_Refusal := No_Refusal;
      Service_Seen : Boolean := False;
      Ls_Refs : Boolean := False;
      Fetch : Boolean := False;
      Agent : Boolean := False;
      Object_Format : Boolean := False;
      Format : Git_Protocol.Object_Format := Sha1_Format;
   end record;

   function Valid (A : Advertisement_Reader) return Boolean is
     ((A.Phase = Failed_Phase) = (A.Why /= No_Refusal)
      and then (if A.Phase = Done_Phase then A.Ls_Refs and then A.Fetch));
   function Failed (A : Advertisement_Reader) return Boolean is (A.Phase = Failed_Phase);
   function Ended (A : Advertisement_Reader) return Boolean is (A.Phase = Done_Phase);
   function Failure (A : Advertisement_Reader) return Advertisement_Refusal is (A.Why);
   function Offers_Agent (A : Advertisement_Reader) return Boolean is (A.Agent);
   function Offers_Object_Format (A : Advertisement_Reader) return Boolean is (A.Object_Format);
   function Format (A : Advertisement_Reader) return Object_Format is (A.Format);

   type Refs_Phase is (In_Refs, Done_Phase, Failed_Phase);

   type Refs_Reader is record
      Phase : Refs_Phase := In_Refs;
      Why : Refs_Refusal := No_Refusal;
      Maximum : Ref_Count := 0;
      Format : Object_Format := Sha1_Format;
      Count : Ref_Count := 0;
      Has_Ref : Boolean := False;
      Id : Object_Id := [others => 0];
      Name : Ref_Buffer := [others => ' '];
      Name_Length : Ref_Name_Length := 0;
      Has_Symref : Boolean := False;
      Target : Ref_Buffer := [others => ' '];
      Target_Length : Ref_Name_Length := 0;
      Has_Peeled : Boolean := False;
      Peeled : Object_Id := [others => 0];
   end record;

   function Valid (L : Refs_Reader) return Boolean is
     ((L.Phase = Failed_Phase) = (L.Why /= No_Refusal)
      and then L.Count <= L.Maximum
      and then (if L.Has_Ref then Safe_Ref_Name (L.Name (1 .. L.Name_Length)))
      and then (if L.Has_Symref then Safe_Ref_Name (L.Target (1 .. L.Target_Length))));
   function Failed (L : Refs_Reader) return Boolean is (L.Phase = Failed_Phase);
   function Ended (L : Refs_Reader) return Boolean is (L.Phase = Done_Phase);
   function Failure (L : Refs_Reader) return Refs_Refusal is (L.Why);
   function Count (L : Refs_Reader) return Ref_Count is (L.Count);
   function Maximum_Refs (L : Refs_Reader) return Ref_Count is (L.Maximum);
   function Format (L : Refs_Reader) return Object_Format is (L.Format);
   function Has_Ref (L : Refs_Reader) return Boolean is (L.Has_Ref);
   function Ref_Id (L : Refs_Reader) return Object_Id is (L.Id);
   function Ref_Name (L : Refs_Reader) return String is (L.Name (1 .. L.Name_Length));
   function Has_Symref (L : Refs_Reader) return Boolean is (L.Has_Ref and then L.Has_Symref);
   function Symref_Target (L : Refs_Reader) return String is (L.Target (1 .. L.Target_Length));
   function Has_Peeled (L : Refs_Reader) return Boolean is (L.Has_Ref and then L.Has_Peeled);
   function Peeled_Id (L : Refs_Reader) return Object_Id is (L.Peeled);

   type Fetch_Phase is (Expect_Section, In_Packfile, Done_Phase, Failed_Phase);

   type Fetch_Reader is record
      Phase : Fetch_Phase := Expect_Section;
      Why : Fetch_Refusal := No_Refusal;
      Text : String (1 .. MAXIMUM_MESSAGE_BYTES) := [others => ' '];
      Text_Length : Message_Length := 0;
   end record;

   function Valid (F : Fetch_Reader) return Boolean is ((F.Phase = Failed_Phase) = (F.Why /= No_Refusal));
   function Failed (F : Fetch_Reader) return Boolean is (F.Phase = Failed_Phase);
   function Ended (F : Fetch_Reader) return Boolean is (F.Phase = Done_Phase);
   function Failure (F : Fetch_Reader) return Fetch_Refusal is (F.Why);
   function Message (F : Fetch_Reader) return String is (F.Text (1 .. F.Text_Length));
end Git_Protocol;
