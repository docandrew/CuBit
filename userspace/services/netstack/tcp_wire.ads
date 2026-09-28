------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP option bytes the RecordFlux parser has already framed: the
--  options area of a segment whose header it validated.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with TCP_Options;

package TCP_Wire with SPARK_Mode is

   Maximum_Options : constant := 40;   --  data offset 15, less the fixed header

   --  Option kinds and lengths (RFC 9293 3.2, RFC 7323, RFC 2018).
   End_Of_List_Kind    : constant := 0;
   No_Operation_Kind   : constant := 1;
   MSS_Kind            : constant := 2;
   MSS_Length          : constant := 4;
   Window_Scale_Kind   : constant := 3;
   Window_Scale_Length : constant := 3;
   SACK_Permitted_Kind : constant := 4;
   SACK_Permitted_Length : constant := 2;
   Timestamps_Kind     : constant := 8;
   Timestamps_Length   : constant := 10;

   --  Header flag bits (the low byte of the flags field).
   Flag_FIN : constant Unsigned_8 := 16#01#;
   Flag_SYN : constant Unsigned_8 := 16#02#;
   Flag_RST : constant Unsigned_8 := 16#04#;
   Flag_PSH : constant Unsigned_8 := 16#08#;
   Flag_ACK : constant Unsigned_8 := 16#10#;

   type Bytes is array (Natural range <>) of Unsigned_8;

   --  The options a segment carries (MSS, window scale, SACK-permitted,
   --  timestamps), each only if well formed. Unknown kinds are skipped by
   --  their length; parsing stops at End of Option List or at the first
   --  malformed length.
   procedure Parse (Options : Bytes; Found : out TCP_Options.Received)
   with Pre => Options'First = 0 and then Options'Length <= Maximum_Options;

end TCP_Wire;
