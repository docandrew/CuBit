------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's view of this process's startup table (CAP-002): the handle
--  words its launcher placed, by name, for the libc's own system calls
--  (files, processes, network). The libc has no Ada run-time library, so it
--  reads the table itself (CuBit.Launch_Envelopes, CuBit.Startup_Tables)
--  instead of through CuBit.Handles.Startup.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Startup_Tables;
with CuBit.Handles;
with CuBit.Handles.C_ABI;
with Interfaces.C;

package CuBit.Libc_Startup is

   --  Decode the table appended to the launch block (Block_Length bytes in
   --  all), once, at program start. A block without one, or a malformed
   --  one, leaves no handles.
   procedure Load (Block_Length : Unsigned_64);

   --  The handle granted under Name with Kind, or 0 (none).
   function Granted
     (Name : String; Kind : CuBit.Startup_Tables.Capability_Kind) return Unsigned_64;

   --  The table's entries, for kinds a program may hold several of
   --  (network scopes): Handle_At is 0 unless entry I is Granted with Kind.
   function Entry_Count return Natural;
   function Handle_At
     (I : Positive; Kind : CuBit.Startup_Tables.Capability_Kind) return Unsigned_64;

   --  The Endpoint-kind entry named Name, or the null handle.
   function Endpoint_Named (Name : String) return CuBit.Handles.Endpoint;

   --  The same for C (cubit_handles.h): Name is NUL-terminated and at most
   --  Startup_Tables.Maximum_Name_Bytes long; a longer or null one finds
   --  nothing.
   function Startup_Endpoint
     (Name : System.Address) return CuBit.Handles.C_ABI.C_Endpoint
   with Export, Convention => C, External_Name => "cubit_startup_endpoint";

   --  The startup check of a C program (CAP-003): its generated startup.h
   --  defines __cubit_startup_check, which the shared start code
   --  (userspace/runtime/start/cubit_start.S) calls before the libc's start
   --  and which calls this with the manifest's declared table (Count
   --  struct cubit_startup_expectation entries at Expected) and identity
   --  (NUL-terminated). When the startup table falls short (an entry
   --  missing, of another kind or need, or a required one not granted) or
   --  the launch block carried none, it says which and why on the console
   --  and stops the process with EX_CONFIG (CuBit.Startup_Check's
   --  Startup_Refused). It needs no elaboration: it runs before the libc's.
   type C_Expectation is record
      Name : System.Address;
      Kind : Unsigned_8;
      Need : Unsigned_8;
   end record
   with Convention => C;
   procedure Require
     (Expected : System.Address; Count : Interfaces.C.unsigned_long; Program : System.Address)
   with Export, Convention => C, External_Name => "cubit_startup_require";

end CuBit.Libc_Startup;
