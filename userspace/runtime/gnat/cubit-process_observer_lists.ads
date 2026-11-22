------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The process list through procmgr's process-observer role
--  (CuBit.Process_Observer), as CuBit.Process_List entries.
--
--  @description
--  Only procmgr holds the kernel's process-list object (Process_List_ABI);
--  a program that needs to know what runs declares (service
--  "process-observer" ...) and reads procmgr's pages here. procmgr's
--  records carry more (launcher, start time, manifest identity); Read keeps
--  what CuBit.Process_List entries hold. One caller at a time: the page
--  procmgr fills is this package's.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;

package CuBit.Process_Observer_Lists is

   --  Fill Into (Bytes long) with CuBit.Process_List entries, one per
   --  process procmgr lists: the count written, or Unsigned_64'Last when
   --  procmgr did not answer (no observer endpoint, or refused).
   function Read
     (Observer : CuBit.Handles.Endpoint; Into : System.Address; Bytes : Unsigned_64)
      return Unsigned_64;

end CuBit.Process_Observer_Lists;
