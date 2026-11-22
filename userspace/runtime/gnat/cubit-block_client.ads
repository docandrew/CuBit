------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A Block.Device.V1 client over one endpoint: Describe, then block reads,
--  writes and flushes through one shared-memory grant (the installer's disk
--  access, SYS-001; HW-001 step 4).
--
--  @description
--  Open describes the device and grants the provider Pages of this
--  process's memory, which every transfer moves data through. Transfers are
--  synchronous calls (the provider's front end is asynchronous; a client
--  that wants several in flight uses several sessions). Each result says
--  exactly what the provider answered: a transfer succeeded only when every
--  requested byte moved.
--
--  Every call has the session's explicit deadline (Within_Ms, IPC-000
--  rule 6: never Wait_Forever toward a driver). A call past it ends
--  Timed_Out: the provider's outcome is then unknown, and the session
--  should not be trusted for writes (the installer stops). Choose it above
--  the provider's own command timeout (UFS and NVMe: 30 s, as Linux's
--  SCSI default), so a provider that answers late but answers is heard.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Device_Identities;

package CuBit.Block_Client is

   --  The shared buffer bounds one transfer: at most this many pages.
   Maximum_Pages : constant := 256;
   subtype Page_Count is Positive range 1 .. Maximum_Pages;
   Page_Bytes : constant := 4096;

   --  A named ceiling on a session's request deadline.
   Maximum_Request_Ms : constant := 600_000;
   subtype Request_Ms is Unsigned_64 range 1 .. Maximum_Request_Ms;

   type Session is record
      Open    : Boolean := False;
      Device  : Device_Session;
      Pages   : Page_Count := Page_Count'First;
      Within  : Request_Ms := Request_Ms'Last;
   end record;

   type Open_Result is
     (Opened, Describe_Failed, No_Memory, Grant_Refused, Describe_Timed_Out);

   procedure Open
     (Endpoint  : CuBit.Handles.Endpoint;
      Pages     : Page_Count;
      Within_Ms : Request_Ms;
      Item      : out Session;
      Result    : out Open_Result)
     with Post => Item.Open = (Result = Opened);

   --  The device as the provider describes it to this session (a holder
   --  without the write grant sees FEATURE_READ_ONLY).
   function Description (Item : Session) return Device_Description is
     (Item.Device.description);

   --  Blocks one transfer can move: the buffer's and the device's limit.
   function Transfer_Limit (Item : Session) return Unsigned_64
     with Pre => Item.Open;

   --  The shared buffer: a read's data lands here, a write's comes from here.
   function Buffer (Item : Session) return System.Address
     with Pre => Item.Open;

   type Transfer_Result is (Transferred, Refused, Short_Transfer, Timed_Out);

   --  Moves Count blocks at First between the device and Buffer. Moved: the
   --  bytes the provider reports.
   procedure Read
     (Item : Session; First : Unsigned_64; Count : Unsigned_64;
      Moved : out Unsigned_64; Result : out Transfer_Result)
     with Pre => Item.Open and then Count in 1 .. Transfer_Limit (Item);

   procedure Write
     (Item : Session; First : Unsigned_64; Count : Unsigned_64;
      Force_Unit_Access : Boolean;
      Moved : out Unsigned_64; Result : out Transfer_Result)
     with Pre => Item.Open and then Count in 1 .. Transfer_Limit (Item)
                 and then (if Force_Unit_Access then
                             Supports_FUA (Description (Item)));

   --  The device's model and serial (OP_IDENTIFY_DEVICE). Known False:
   --  the driver reports no identity, or its record was malformed.
   procedure Identify
     (Item : Session; Identity : out CuBit.Device_Identities.Identity;
      Known : out Boolean)
     with Pre => Item.Open;

   --  Revokes the grant and closes the endpoint (its buffer's pages stay
   --  with the process).
   procedure Close (Item : in out Session)
     with Post => not Item.Open;

   --  Makes every completed write durable (OP_FLUSH_DEVICE). Durable False:
   --  refused, failed or Timed_Out.
   procedure Flush (Item : Session; Durable : out Boolean)
     with Pre => Item.Open;
   procedure Flush (Item : Session; Durable : out Boolean; Timed_Out : out Boolean)
     with Pre => Item.Open;

end CuBit.Block_Client;
