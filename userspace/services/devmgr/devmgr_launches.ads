------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The children devmgr launches, by handle (CAP-002, PLAN §9 item 8).
--
--  @description
--  A launch is: Spawn (suspended child, its CSPACE, process and endpoint
--  handles), then devmgr decides what the child gets, then Install (the
--  startup table built from the child's manifest, appended after its typed
--  launch configuration, as one launch block), then Resume.
--
--  Each entry of the child's declared startup table is answered, in order:
--  * by what devmgr prepared for that binding name (Give: a derivation of
--    one of devmgr's own capabilities, installed into the child; or
--    Give_Placed: a handle the kernel already placed in the child, such as
--    an allocated vector block's interrupt capability);
--  * else, for a service request, a copy of the endpoint the role
--    resolves to (Provide) with the requested rights; for a notification
--    request, the notification root narrowed to that one role;
--  * else it is not available: an optional entry is recorded Absent, a
--    required one fails the launch (the child is stopped).
--  Every install goes through the child's own CSPACE, before it first runs;
--  nothing is ever placed into a running process.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with Driver_Manifests;
with Launch_Configuration;

package Devmgr_Launches is

   package H renames CuBit.Handles;
   subtype Process_ID is CuBit.Process_IDs.Process_ID;

   --  devmgr's spawning authority and its notification root.
   procedure Configure (Process_Root : H.Process; Notifications : H.Notification);

   --  A refused launch step is printed on the serial console and kept
   --  (bounded) until devmgr forwards it to its warnings, which reach
   --  logstore (the Desktop bootstrap panel and the Logs app) once it
   --  runs. Each names the child and, for a startup entry, the entry.
   --  Forward_Refusals hands every kept one to Warn, oldest first, then
   --  one line counting any that did not fit.
   generic
      with procedure Warn (Text : String);
   procedure Forward_Refusals;

   --  The endpoint a (service ...) request for Role resolves to: untagged,
   --  with Grant (a child gets a copy with the rights it asked for).
   procedure Provide (Role : Unsigned_32; Service : H.Endpoint);
   function Service_Of (Role : Unsigned_32) return H.Endpoint;

   Capacity : constant := 32;
   type Child is new Natural range 0 .. Capacity;
   No_Child : constant Child := 0;

   --  Spawn the ELF image at Image (Bytes long) suspended. No_Child when
   --  refused or when no launch record is free.
   function Spawn
     (Name : String; Image : System.Address; Bytes, Priority : Unsigned_64) return Child;

   function PID (Item : Child) return Process_ID
   with Pre => Item /= No_Child;
   --  devmgr's handles to it: process control (all rights), its untagged
   --  endpoint (Grant: copies may be passed on), its CSPACE until it runs.
   function Control (Item : Child) return H.Process
   with Pre => Item /= No_Child;
   function Mailbox (Item : Child) return H.Endpoint
   with Pre => Item /= No_Child;
   function Space (Item : Child) return H.Cspace
   with Pre => Item /= No_Child;
   --  The handles the kernel placed in the child for itself.
   function Child_Self (Item : Child) return H.Foreign_Handle
   with Pre => Item /= No_Child;
   function Child_Self_Process (Item : Child) return H.Foreign_Handle
   with Pre => Item /= No_Child;

   --  What its manifest asks for.
   function Interrupt_Of (Item : Child) return Driver_Manifests.Interrupt_Request
   with Pre => Item /= No_Child;
   --  Whether its manifest declares a binding Name.
   function Declares (Item : Child; Name : String) return Boolean
   with Pre => Item /= No_Child;

   --  Answer the binding Name with a copy of one of devmgr's capabilities,
   --  or with a handle already placed in the child. A name the child's
   --  manifest does not declare answers nothing and is reported.
   procedure Give (Item : Child; Name : String; Copy : H.Derivation)
   with Pre => Item /= No_Child;
   procedure Give_Placed (Item : Child; Name : String; Placed : H.Foreign_Handle)
   with Pre => Item /= No_Child;

   --  Answer the binding Name with nothing: an optional one is recorded
   --  Absent, a required one fails the launch.
   procedure Withhold (Item : Child; Name : String)
   with Pre => Item /= No_Child;

   --  The same for the first declared binding of Kind (a driver's one
   --  interrupt or register window, whatever its manifest calls it). False
   --  when it declares none.
   function Give_First
     (Item : Child; Kind : CuBit.Startup_Tables.Capability_Kind; Copy : H.Derivation)
      return Boolean
   with Pre => Item /= No_Child;
   function Give_First_Placed
     (Item : Child; Kind : CuBit.Startup_Tables.Capability_Kind; Placed : H.Foreign_Handle)
      return Boolean
   with Pre => Item /= No_Child;

   --  Install one of devmgr's capabilities in the child outside its table
   --  (procmgr's typed handles): the placed handle's word, or 0.
   function Place (Item : Child; Copy : H.Derivation) return Unsigned_64
   with Pre => Item /= No_Child;

   --  The typed launch configuration it reads first (none by default).
   procedure Set_Payload (Item : Child; Block : Launch_Configuration.Encoded)
   with Pre => Item /= No_Child;

   --  Build its startup table and install the launch block. False when a
   --  required capability was refused or the kernel refused the block; the
   --  caller then stops the child (Abandon).
   function Install (Item : Child) return Boolean
   with Pre => Item /= No_Child;

   --  Start it (unlimited frames and CPU).
   function Resume (Item : Child) return Boolean
   with Pre => Item /= No_Child;

   --  Install then Resume; on failure the child is stopped.
   function Start (Item : Child) return Boolean
   with Pre => Item /= No_Child;

   --  Stop a child that will not be started.
   procedure Abandon (Item : Child)
   with Pre => Item /= No_Child;

end Devmgr_Launches;
