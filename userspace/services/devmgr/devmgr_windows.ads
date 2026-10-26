------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The device-memory window the generic PCI launch grants for one manifest
--  request (docs/device-matching.md "Generic PCI launch").
--
--  @description
--  A request's size is an upper bound: the window is never larger than the
--  BAR decodes, so a driver asking for more than its device has (the UFS
--  driver once asked 16 KiB of the IdeaPad's 4 KiB BAR) never receives the
--  neighbouring device's registers. devmgr logs every clamp.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Devmgr_Windows with SPARK_Mode, Pure is
   type Window_Grant is record
      Bytes   : Unsigned_64 := 0;
      --  The request was larger than the BAR: Bytes is the BAR's size.
      Clamped : Boolean := False;
   end record;

   --  Requested: the manifest's max_bytes. Decoded: the BAR's decoded size
   --  (zero when it is not a usable memory BAR, which the caller refuses).
   function Window (Requested, Decoded : Unsigned_64) return Window_Grant is
     ((Bytes   => Unsigned_64'Min (Requested, Decoded),
       Clamped => Requested > Decoded))
     with Post => Window'Result.Bytes <= Decoded
                  and then Window'Result.Bytes <= Requested
                  and then Window'Result.Clamped = (Requested > Decoded)
                  and then (if not Window'Result.Clamped
                            then Window'Result.Bytes = Requested
                            else Window'Result.Bytes = Decoded);
end Devmgr_Windows;
