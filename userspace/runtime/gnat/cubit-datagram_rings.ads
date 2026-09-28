------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Datagrams through a CuBit.Channel_Rings byte ring (connected UDP
--  channels; docs/netstack-redesign.md, "Async channels"). A record is a
--  4-byte header (payload length, kind; little endian) and the payload,
--  padded to a multiple of 4. Positions stay 4-byte aligned (checked where
--  a header is read or written), so a header never straddles the end of
--  the ring, and a record never wraps: when it
--  does not fit before the end, a pad record fills the end and the record
--  starts the ring. The pad and the record are published together.
--
--  The peer writes the records a consumer reads, so every header is
--  checked: a malformed one (unknown kind, a record running past the end
--  or past what was produced) is not consumed and is reported.
--
--  Proved (tests/channel-rings): every access stays in the ring and the
--  caller's buffers; the indices stay valid; a datagram is put
--  whole or not at all; a take never returns more than the caller's buffer
--  and reports truncation. Tested, not proved: payloads arrive intact.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;

package CuBit.Datagram_Rings with Pure, SPARK_Mode is

   Header_Bytes : constant := 4;
   Maximum_Payload : constant := 65_535;
   Kind_Data : constant := 0;
   Kind_Pad  : constant := 1;

   subtype Payload_Length is Natural range 0 .. Maximum_Payload;

   --  The ring bytes a record of Length payload bytes takes.
   function Record_Bytes (Length : Payload_Length) return Positive is
     (Header_Bytes + (Length + 3) / 4 * 4);

   type Put_Result is (Put, No_Room, Too_Large);

   procedure Put
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Result : out Put_Result)
   with
     Pre  => Valid (P) and then
             Ring'First = 0 and then Ring'Last = P.Size - 1 and then
             Data'Length <= Maximum_Payload and then Data'Last < Natural'Last,
     Post => Valid (P) and then P.Size = P'Old.Size and then
             (if Result /= Put then P = P'Old) and then
             (if Result = Put then P.Fill > P'Old.Fill);

   type Take_Result is (Taken, Empty, Malformed);

   --  The oldest datagram into Into (as much as fits: Truncated if not
   --  all of it). Length is the bytes copied.
   procedure Take
     (C : in out Consumer; Ring : Bytes; Into : in out Bytes;
      Length : out Natural; Truncated : out Boolean;
      Result : out Take_Result)
   with
     Pre  => Valid (C) and then
             Ring'First = 0 and then Ring'Last = C.Size - 1 and then
             Into'Last < Natural'Last,
     Post => Valid (C) and then C.Size = C'Old.Size and then
             Length <= Into'Length and then
             (if Result /= Taken then Length = 0 and then not Truncated);

end CuBit.Datagram_Rings;
