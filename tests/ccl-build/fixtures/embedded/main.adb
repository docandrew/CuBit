------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ccl-build's embedded-blob fixture: reports the size and first byte of
--  the firmware ccl-build linked into it (package Embedded, generated).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Embedded;

procedure main is
begin
   debugPrint ("embed-check:" & Natural'Image (Embedded.Guc_Firmware_Bytes) & " bytes, first" &
               Unsigned_8'Image (Embedded.Guc_Firmware (1)) & ASCII.LF);
end main;
