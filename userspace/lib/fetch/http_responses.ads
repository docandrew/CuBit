------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch's HTTP/1.1 response reader (SELF-003 step 1): the status line
--  and header fields, then the body by its framing, from untrusted bytes
--  as tls.svc hands them over, in pieces of any size.
--
--  @description
--  RFC 9112 (HTTP/1.1 message syntax) and RFC 9110 (semantics), strictly:
--  * the head ends at the first empty line; CRLF only (no bare LF), no
--    obsolete line folding, field names are tokens, values have no
--    control bytes but HTAB; the head is at most MAXIMUM_HEAD_BYTES;
--  * 1xx interim responses are passed over (101 is refused: nothing asked
--    to switch protocols);
--  * the body is length-delimited (Content-Length: digits only, one value),
--    chunked (Transfer-Encoding: exactly "chunked", hex sizes, extensions
--    and trailers read past), or delimited by the connection's close
--    (tls.svc reports EOF only after the peer's close_notify);
--  * Content-Length with Transfer-Encoding, two different lengths, any
--    other transfer coding, or a content coding other than identity is
--    refused rather than guessed at (request smuggling, RFC 9112 6.3);
--  * 204 and 304 have no body.
--  Proved at level 2 (tests/fetch-codecs): every body run lies inside the
--  bytes given; the body delivered never passes the caller's ceiling; a
--  length-delimited body completes with exactly Content-Length bytes and
--  any byte after it is refused; a refusal is final.
------------------------------------------------------------------------------
pragma Ada_2022;

package Http_Responses with SPARK_Mode, Pure is

   --  Named ceilings: one response's status line and header fields
   --  together; one Location; a chunk's size.
   MAXIMUM_HEAD_BYTES : constant := 65_536;
   MAXIMUM_LOCATION_BYTES : constant := 8_192;
   MAXIMUM_CHUNK_BYTES : constant := 2 ** 60;

   type Body_Count is range 0 .. 2 ** 62 - 1;
   subtype Status_Code is Natural range 100 .. 599;
   subtype Head_Length is Natural range 0 .. MAXIMUM_HEAD_BYTES;
   subtype Location_Length is Natural range 0 .. MAXIMUM_LOCATION_BYTES;

   type Framing is (Length_Delimited, Chunked, Close_Delimited);

   type Refusal is
     (No_Refusal,
      Head_Too_Large,              --  beyond MAXIMUM_HEAD_BYTES
      Bad_Status_Line,             --  not "HTTP/1.x NNN reason"
      Bad_Field,                   --  a field line that is not "token: value"
      Folded_Field,                --  obsolete line folding
      Bad_Content_Length,          --  not digits, or beyond Body_Count
      Conflicting_Lengths,         --  two Content-Length fields that differ
      Length_And_Chunked,          --  Content-Length and Transfer-Encoding both
      Unsupported_Transfer_Coding, --  a transfer coding other than chunked
      Unsupported_Content_Coding,  --  a content coding other than identity
      Switching_Protocols,         --  101: nothing asked for it
      Location_Too_Long,           --  beyond MAXIMUM_LOCATION_BYTES
      Bad_Chunk,                   --  a chunk size or its framing
      Body_Too_Large,              --  beyond the caller's ceiling
      Extra_Data,                  --  bytes after a complete body
      Truncated);                  --  the connection ended before the body did

   --  The reason's name (native builds discard enumeration images).
   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Head_Too_Large => "Head_Too_Large",
         when Bad_Status_Line => "Bad_Status_Line",
         when Bad_Field => "Bad_Field",
         when Folded_Field => "Folded_Field",
         when Bad_Content_Length => "Bad_Content_Length",
         when Conflicting_Lengths => "Conflicting_Lengths",
         when Length_And_Chunked => "Length_And_Chunked",
         when Unsupported_Transfer_Coding => "Unsupported_Transfer_Coding",
         when Unsupported_Content_Coding => "Unsupported_Content_Coding",
         when Switching_Protocols => "Switching_Protocols",
         when Location_Too_Long => "Location_Too_Long",
         when Bad_Chunk => "Bad_Chunk",
         when Body_Too_Large => "Body_Too_Large",
         when Extra_Data => "Extra_Data",
         when Truncated => "Truncated");

   type Event_Kind is
     (Need_More,   --  every byte given was taken; give more
      Head_Ready,  --  the final head was read: Status, Framing, Location
      Body_Run,    --  Input (First .. Last) is body
      Complete,    --  the body ended well
      Refused);    --  Why says why; final

   type Event is record
      Kind : Event_Kind := Need_More;
      Why : Refusal := No_Refusal;
      First : Positive := 1;
      Last : Natural := 0;
   end record;

   type Reader is private;

   function Valid (R : Reader) return Boolean;
   function Failed (R : Reader) return Boolean;
   function Done (R : Reader) return Boolean;
   function Head_Read (R : Reader) return Boolean;
   function Delivered (R : Reader) return Body_Count;
   function Maximum_Body (R : Reader) return Body_Count;
   function Status (R : Reader) return Status_Code with Pre => Head_Read (R);
   function Body_Framing (R : Reader) return Framing with Pre => Head_Read (R);
   function Content_Length (R : Reader) return Body_Count with Pre => Head_Read (R);
   function Location (R : Reader) return String with Pre => Head_Read (R);

   procedure Start (R : out Reader; Ceiling : Body_Count)
   with Post => Valid (R) and then not Failed (R) and then not Done (R) and then not Head_Read (R)
                  and then Delivered (R) = 0 and then Maximum_Body (R) = Ceiling;

   --  Take bytes of Input from Next on, up to one event: Next moves past
   --  what was taken. Call again while Next <= Input'Last.
   procedure Step (R : in out Reader; Input : String; Next : in out Positive; E : out Event)
   with
     Pre => Valid (R) and then Input'Last < Positive'Last and then Next in Input'First .. Input'Last + 1,
     Post =>
       Valid (R)
       and then Maximum_Body (R) = Maximum_Body (R'Old)
       and then Next in Next'Old .. Input'Last + 1
       and then Delivered (R) <= Maximum_Body (R)
       and then (if Failed (R'Old) then E.Kind = Refused)
       and then (E.Kind = Refused) = Failed (R)
       and then (if E.Kind = Refused then E.Why /= No_Refusal)
       and then (if E.Kind = Complete then Done (R) and then Head_Read (R)
                   and then (if Body_Framing (R) = Length_Delimited then Delivered (R) = Content_Length (R)))
       and then (if E.Kind = Head_Ready then Head_Read (R) and then Status (R) not in 100 .. 199)
       and then (if E.Kind = Body_Run then
                   E.First >= Next'Old and then E.First <= E.Last and then E.Last = Next - 1
                   and then E.Last <= Input'Last and then Head_Read (R)
                   and then Delivered (R) = Delivered (R'Old) + Body_Count (E.Last - E.First + 1)
                 else Delivered (R) = Delivered (R'Old));

   --  The connection ended (an authenticated EOF): Complete for a body
   --  delimited by the close, or one that is done; else Truncated.
   procedure Input_Ended (R : in out Reader; E : out Event)
   with Pre => Valid (R),
        Post => Valid (R) and then E.Kind in Complete | Refused
          and then (E.Kind = Refused) = Failed (R)
          and then (if E.Kind = Complete then Head_Read (R)
                      and then (if Body_Framing (R) = Length_Delimited then Delivered (R) = Content_Length (R)));
private
   type Phase_Kind is
     (In_Head, In_Length_Body, In_Close_Body, In_Chunk_Size, In_Chunk_Extension, In_Chunk_Size_LF,
      In_Chunk_Data, In_Chunk_Data_CR, In_Chunk_Data_LF, In_Trailer, In_Trailer_LF, Done_Phase,
      Failed_Phase);

   subtype Head_Buffer is String (1 .. MAXIMUM_HEAD_BYTES);
   subtype Location_Buffer is String (1 .. MAXIMUM_LOCATION_BYTES);

   type Reader is record
      Phase : Phase_Kind := In_Head;
      Why : Refusal := No_Refusal;
      Ceiling : Body_Count := 0;
      Delivered : Body_Count := 0;
      Head : Head_Buffer := [others => ' '];
      Head_Last : Head_Length := 0;
      Head_Read : Boolean := False;
      Status : Status_Code := 200;
      Framing : Http_Responses.Framing := Close_Delimited;
      Length : Body_Count := 0;
      Location : Location_Buffer := [others => ' '];
      Location_Last : Location_Length := 0;
      --  Of the current length-delimited body or chunk.
      Remaining : Body_Count := 0;
      Chunk_Digits : Natural range 0 .. 16 := 0;
      --  Bytes of the trailer section read, and of its current line.
      Trailer_Bytes : Head_Length := 0;
      Line_Bytes : Head_Length := 0;
   end record;

   function Valid (R : Reader) return Boolean is
     ((R.Phase = Failed_Phase) = (R.Why /= No_Refusal)
      and then R.Delivered <= R.Ceiling
      and then (if R.Phase = In_Head then R.Delivered = 0 and then not R.Head_Read)
      and then (if R.Phase /= In_Head and then R.Phase /= Failed_Phase then R.Head_Read)
      and then (if R.Phase = In_Length_Body then
                  R.Framing = Length_Delimited and then R.Remaining >= 1
                  and then R.Delivered + R.Remaining = R.Length)
      and then (if R.Phase = Done_Phase and then R.Framing = Length_Delimited then R.Delivered = R.Length)
      and then (if R.Phase = In_Close_Body then R.Framing = Close_Delimited)
      and then (if R.Phase in In_Chunk_Size .. In_Trailer_LF then R.Framing = Chunked)
      and then (if R.Phase = In_Chunk_Data then R.Remaining >= 1));

   function Failed (R : Reader) return Boolean is (R.Phase = Failed_Phase);
   function Done (R : Reader) return Boolean is (R.Phase = Done_Phase);
   function Head_Read (R : Reader) return Boolean is (R.Head_Read);
   function Delivered (R : Reader) return Body_Count is (R.Delivered);
   function Maximum_Body (R : Reader) return Body_Count is (R.Ceiling);
   function Status (R : Reader) return Status_Code is (R.Status);
   function Body_Framing (R : Reader) return Framing is (R.Framing);
   function Content_Length (R : Reader) return Body_Count is (R.Length);
   function Location (R : Reader) return String is (R.Location (1 .. R.Location_Last));
end Http_Responses;
