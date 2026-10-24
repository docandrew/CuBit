------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block.Device.V1 front end shared by every block driver: request checks,
--  grant acquisition and return, and replies (HW-001 step 1).
--
--  Decode and the reply builders are pure and proved (tests/block-requests).
--  Admit and Settle are the only calls into the kernel grant API; their
--  contracts state what the kernel promises, they are not proved here.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Grant_References;

package CuBit.Block_Requests
  with SPARK_Mode, Abstract_State => Kernel_Grants
is
   use type CuBit.Messages.Protocol_Flags;

   --  Describe word 2 carries the largest transfer in 32 bits.
   subtype Transfer_Limit is Unsigned_64 range 1 .. Unsigned_64 (Unsigned_32'Last);
   subtype Device_Blocks is Unsigned_64 range 1 .. Unsigned_64'Last;
   subtype Block_Address is Unsigned_64;
   subtype Block_Count is Unsigned_64 range 1 .. Transfer_Limit'Last;

   --  No_Device: this provider offers nothing (Describe answers
   --  REPLY_NO_DEVICE). Faulted: the device exists but identification or
   --  bring-up failed; every request is an error.
   type Availability is (Available, No_Device, Faulted);

   type Device_State (Presence : Availability := Faulted) is record
      case Presence is
         when Available =>
            Blocks        : Device_Blocks := 1;
            Logical_Size  : Logical_Block_Size := Logical_Block_Size'First;
            Physical_Size : Logical_Block_Size := Logical_Block_Size'First;
            Max_Transfer  : Transfer_Limit := 1;
            Features      : Device_Features := 0;
            Media         : Media_Kind := Fixed_Media;
         when No_Device | Faulted =>
            null;
      end case;
   end record;

   function Offers (State : Device_State; Feature : Device_Features)
     return Boolean is
     (State.Presence = Available and then (State.Features and Feature) /= 0);

   --  OP_FLUSH_DEVICE is accepted.
   function Flushes (State : Device_State) return Boolean is
     (Offers (State, FEATURE_FLUSH));

   type Transfer_Direction is (Device_Read, Device_Write);

   type Transfer_Request is record
      Direction   : Transfer_Direction := Device_Read;
      First_Block : Block_Address := 0;
      Count       : Block_Count := 1;
      Block_Bytes : Logical_Block_Size := Logical_Block_Size'First;
      Grant       : CuBit.Grant_References.Reference;
      --  Write only, and only when the device offers FEATURE_FUA.
      Force_Unit_Access : Boolean := False;
   end record;

   --  The transfer's size. Never wraps: at most 2**32 - 1 blocks of 64 KiB.
   Largest_Transfer_Bytes : constant Unsigned_64 :=
     Block_Count'Last * Unsigned_64 (Logical_Block_Size'Last);
   function Bytes (Item : Transfer_Request) return Unsigned_64 is
     (Item.Count * Unsigned_64 (Item.Block_Bytes))
     with Post => Bytes'Result in
       Unsigned_64 (Logical_Block_Size'First) .. Largest_Transfer_Bytes;

   --  Everything a driver may rely on once a transfer is decoded.
   function Valid_Transfer
     (State : Device_State; Item : Transfer_Request) return Boolean is
     (State.Presence = Available
      and then Item.Count <= State.Max_Transfer
      and then Item.First_Block < State.Blocks
      and then Item.Count <= State.Blocks - Item.First_Block
      and then Item.Block_Bytes = State.Logical_Size
      and then (if Item.Direction = Device_Write then
                  not Offers (State, FEATURE_READ_ONLY))
      and then (if Item.Force_Unit_Access then
                  Item.Direction = Device_Write
                  and then Offers (State, FEATURE_FUA)));

   type Request_Kind is (Describe, Transfer, Flush, Rejected);

   type Decoded_Request (Kind : Request_Kind := Rejected) is record
      case Kind is
         when Transfer =>
            Data : Transfer_Request;
         when Describe | Flush | Rejected =>
            null;
      end case;
   end record;

   --  The tag carries no protocol flag but the ones the operation and the
   --  device both allow, and nothing in its reserved field.
   function Clean_Tag
     (Request : CuBit.Messages.Message; Length : Unsigned_8) return Boolean is
     (Request.tag.length = Length
      and then Request.tag.protocol = 0
      and then Request.tag.reserved = 0);

   --  Requests a device does not advertise support for are refused here:
   --  writes to read-only media, flushes without FEATURE_FLUSH, FUA without
   --  FEATURE_FUA. Ranges are checked without overflow.
   function Decode
     (Request : CuBit.Messages.Message; State : Device_State)
      return Decoded_Request
     with Post =>
       (case Decode'Result.Kind is
          when Describe =>
            Request.tag.label = OP_DESCRIBE_DEVICE
            and then Clean_Tag (Request, 0),
          when Flush =>
            Request.tag.label = OP_FLUSH_DEVICE
            and then Clean_Tag (Request, 0)
            and then Flushes (State),
          when Transfer =>
            Valid_Transfer (State, Decode'Result.Data)
            and then Request.tag.length = 4
            and then Request.tag.reserved = 0
            and then Decode'Result.Data.First_Block = Request.words (0)
            and then Decode'Result.Data.Count = Request.words (2)
            and then Decode'Result.Data.Grant.slot = Request.words (1)
            and then Decode'Result.Data.Grant.generation = Request.words (3)
            and then
              (if Request.tag.label = OP_READ_BLOCKS then
                 Decode'Result.Data.Direction = Device_Read
               elsif Request.tag.label = OP_WRITE_BLOCKS then
                 Decode'Result.Data.Direction = Device_Write
               else False),
          when Rejected => True);

   --  Every reply is one word except a successful Describe (four).
   function Status_Reply (Success : Boolean; Moved : Unsigned_64)
     return CuBit.Messages.Message
     with Post =>
       Status_Reply'Result.tag.length = 1
       and then Status_Reply'Result.tag.label =
         (if Success then REPLY_OK else REPLY_ERROR)
       and then Status_Reply'Result.words (0) = Moved;

   function Describe_Reply (State : Device_State) return CuBit.Messages.Message
     with Post =>
       (case State.Presence is
          when Available =>
            Describe_Reply'Result.tag.label = REPLY_OK
            and then Describe_Reply'Result.tag.length = 4
            and then Describe_Reply'Result.words (0) = State.Blocks
            and then Describe_Reply'Result.words (2) = State.Max_Transfer,
          when No_Device =>
            Describe_Reply'Result.tag.label = REPLY_NO_DEVICE
            and then Describe_Reply'Result.tag.length = 1
            and then Describe_Reply'Result.words (0) = 0,
          when Faulted =>
            Describe_Reply'Result.tag.label = REPLY_ERROR
            and then Describe_Reply'Result.tag.length = 1);

   --  Success needs the grant back and every requested byte moved. A failed
   --  return makes the outcome uncertain: it reports zero bytes.
   function Transfer_Reply
     (Item : Transfer_Request; Returned : Boolean; Moved : Unsigned_64)
      return CuBit.Messages.Message
     with Post =>
       Transfer_Reply'Result.tag.length = 1
       and then
         (Transfer_Reply'Result.tag.label = REPLY_OK) =
           (Returned and then Moved = Bytes (Item))
       and then Transfer_Reply'Result.words (0) =
         (if Returned then Moved else 0);

   --  A transfer whose caller's grant is held: Buffer maps Bytes (Item).
   type Loan is record
      Item   : Transfer_Request;
      Buffer : System.Address := System.Null_Address;
   end record;

   --  Acquires the caller's grant for the transfer: a device read needs
   --  write access to it, a device write read access. The kernel checks the
   --  owner, generation, range and access (assumed, not proved here).
   procedure Admit
     (Sender  : CuBit.Messages.Process_ID;
      Item    : Transfer_Request;
      Granted : out Loan;
      Success : out Boolean)
     with Global => (In_Out => Kernel_Grants),
          Post => (if Success then Granted.Item = Item);

   --  Returns the grant and builds the reply (Transfer_Reply's rules).
   procedure Settle
     (Granted  : Loan;
      Moved    : Unsigned_64;
      Response : out CuBit.Messages.Message)
     with Global => (In_Out => Kernel_Grants),
          Post =>
            Response.tag.length = 1
            and then (if Response.tag.label = REPLY_OK then
                        Moved = Bytes (Granted.Item));
end CuBit.Block_Requests;
