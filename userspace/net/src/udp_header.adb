------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body UDP_Header with SPARK_Mode is

   procedure Parse (B : Bytes; H : out Header) is
   begin
      H := (Source_Port      => U16 (B, 0),
            Destination_Port => U16 (B, 2),
            Length           => Natural (U16 (B, 4)),
            Checksum         => U16 (B, 6));
   end Parse;

end UDP_Header;
