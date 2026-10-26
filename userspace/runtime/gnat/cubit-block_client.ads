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
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Block_Devices; use CuBit.Block_Devices;

package CuBit.Block_Client is

   --  The shared buffer bounds one transfer: at most this many pages.
   Maximum_Pages : constant := 256;
   subtype Page_Count is Positive range 1 .. Maximum_Pages;
   Page_Bytes : constant := 4096;

   type Session is record
      Open    : Boolean := False;
      Device  : Device_Session;
      Pages   : Page_Count := Page_Count'First;
   end record;

   type Open_Result is
     (Opened, Describe_Failed, No_Memory, Grant_Refused);

   procedure Open
     (Endpoint : CuBit.Handles.Endpoint;
      Pages    : Page_Count;
      Item     : out Session;
      Result   : out Open_Result)
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

   type Transfer_Result is (Transferred, Refused, Short_Transfer);

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

   --  Makes every completed write durable (OP_FLUSH_DEVICE).
   procedure Flush (Item : Session; Durable : out Boolean)
     with Pre => Item.Open;

end CuBit.Block_Client;
