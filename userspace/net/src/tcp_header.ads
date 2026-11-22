------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The TCP header on the wire (RFC 9293 3.1): parsing and writing the fixed
--  20 bytes in place, and writing the options our SYNs carry.
--
--  The data path's codec. Well_Formed is RFC 9293 3.1's rule for the
--  header; option kinds it does not know are accepted here and skipped by
--  TCP_Wire, as 3.1 requires.
--
--  Proved (tests/net-tcp): every field parsed is its bytes on the wire,
--  big-endian; writing then parsing gives back each field written, and
--  writing touches only the header's bytes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package TCP_Header with SPARK_Mode is

   type Bytes is array (Natural range <>) of Unsigned_8;

   Fixed_Size   : constant := 20;
   Maximum_Size : constant := 60;   --  data offset 15
   subtype Header_Size is Natural range Fixed_Size .. Maximum_Size
     with Dynamic_Predicate => Header_Size mod 4 = 0;

   --  Header byte offsets.
   Offset_Byte : constant := 12;   --  data offset (high 4 bits), reserved, NS
   Flags_Byte  : constant := 13;

   type Header is record
      Source_Port, Destination_Port : Unsigned_16 := 0;
      Seq_No, Ack_No : Unsigned_32 := 0;
      Size           : Header_Size := Fixed_Size;   --  data offset * 4
      NS, CWR, ECE, URG, ACK, PSH, RST, SYN, FIN : Boolean := False;
      Window, Checksum, Urgent_Pointer : Unsigned_16 := 0;
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function U32 (B : Bytes; I : Natural) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (B (I)), 24) or Shift_Left (Unsigned_32 (B (I + 1)), 16) or
      Shift_Left (Unsigned_32 (B (I + 2)), 8) or Unsigned_32 (B (I + 3)))
   with Pre => I >= B'First and then B'Last >= 3 and then I <= B'Last - 3;

   function Bit (V : Unsigned_8; N : Natural) return Boolean is
     ((Shift_Right (V, N) and 1) = 1)
   with Pre => N < 8;

   --  The data offset field in bytes.
   function Stated_Size (B : Bytes) return Natural is
     (Natural (Shift_Right (B (B'First + Offset_Byte), 4)) * 4)
   with Pre => B'Length >= Fixed_Size;

   --  A segment (header and data) whose header is well formed.
   function Well_Formed (B : Bytes) return Boolean is
     (B'Length >= Fixed_Size and then B'Length <= 2 ** 16 and then
      Stated_Size (B) >= Fixed_Size and then Stated_Size (B) <= B'Length and then
      --  Reserved: bits 3 .. 1 of byte 12.
      (B (B'First + Offset_Byte) and 16#0E#) = 0 and then
      --  No urgent pointer without URG.
      (Bit (B (B'First + Flags_Byte), 5) or else U16 (B, B'First + 18) = 0));

   procedure Parse (B : Bytes; H : out Header) with
     Pre  => B'First = 0 and then Well_Formed (B),
     Post => H.Source_Port = U16 (B, 0) and then H.Destination_Port = U16 (B, 2) and then
             H.Seq_No = U32 (B, 4) and then H.Ack_No = U32 (B, 8) and then
             H.Size = Stated_Size (B) and then
             H.NS = Bit (B (12), 0) and then
             H.CWR = Bit (B (13), 7) and then H.ECE = Bit (B (13), 6) and then
             H.URG = Bit (B (13), 5) and then H.ACK = Bit (B (13), 4) and then
             H.PSH = Bit (B (13), 3) and then H.RST = Bit (B (13), 2) and then
             H.SYN = Bit (B (13), 1) and then H.FIN = Bit (B (13), 0) and then
             H.Window = U16 (B, 14) and then H.Checksum = U16 (B, 16) and then
             H.Urgent_Pointer = U16 (B, 18);

   --  Write the fixed header (options, if H.Size says so, are written
   --  separately); then it parses back to H.
   procedure Write (B : in out Bytes; H : Header) with
     Pre  => B'First = 0 and then B'Length <= 2 ** 16 and then B'Length >= H.Size and then
             (H.URG or else H.Urgent_Pointer = 0),
     Post => U16 (B, 0) = H.Source_Port and then U16 (B, 2) = H.Destination_Port and then
             U32 (B, 4) = H.Seq_No and then U32 (B, 8) = H.Ack_No and then
             Stated_Size (B) = H.Size and then (B (12) and 16#0E#) = 0 and then
             Bit (B (12), 0) = H.NS and then
             Bit (B (13), 7) = H.CWR and then Bit (B (13), 6) = H.ECE and then
             Bit (B (13), 5) = H.URG and then Bit (B (13), 4) = H.ACK and then
             Bit (B (13), 3) = H.PSH and then Bit (B (13), 2) = H.RST and then
             Bit (B (13), 1) = H.SYN and then Bit (B (13), 0) = H.FIN and then
             U16 (B, 14) = H.Window and then U16 (B, 16) = H.Checksum and then
             U16 (B, 18) = H.Urgent_Pointer and then
             Well_Formed (B) and then
             (for all I in Fixed_Size .. B'Last => B (I) = B'Old (I));

   --  Our SYN options at B (Fixed_Size ..): MSS (RFC 9293 3.7.1) and, when
   --  Shift is given, NOP and window scale (RFC 7323 2.2).
   SYN_Options_Size_MSS   : constant := 4;
   SYN_Options_Size_Scale : constant := 8;

   procedure Write_SYN_Options (B : in out Bytes; MSS : Unsigned_16; Scale : Boolean;
                                Shift : Unsigned_8)
   with
     Pre  => B'First = 0 and then B'Length >= Fixed_Size + SYN_Options_Size_Scale,
     Post => B (20) = 2 and then B (21) = 4 and then U16 (B, 22) = MSS and then
             (if Scale then B (24) = 1 and then B (25) = 3 and then B (26) = 3 and then
                            B (27) = Shift) and then
             (for all I in B'Range =>
                (if I < 20 or else I >= (if Scale then 28 else 24) then B (I) = B'Old (I)));

end TCP_Header;
