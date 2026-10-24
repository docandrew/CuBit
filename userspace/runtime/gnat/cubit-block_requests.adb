------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Memory_Grants;

package body CuBit.Block_Requests
  with SPARK_Mode, Refined_State => (Kernel_Grants => null)
is

   No_Request : constant Decoded_Request := (Kind => Rejected);

   --  The read and write branch of Decode.
   function Decode_Transfer
     (Request : CuBit.Messages.Message; State : Device_State)
      return Decoded_Request
     with Pre =>
            Request.tag.label in OP_READ_BLOCKS | OP_WRITE_BLOCKS
            and then State.Presence = Available
            and then Request.tag.length = 4
            and then Request.tag.reserved = 0,
          Post =>
            (if Decode_Transfer'Result.Kind /= Rejected then
               Decode_Transfer'Result.Kind = Transfer
               and then Valid_Transfer (State, Decode_Transfer'Result.Data)
               and then Decode_Transfer'Result.Data.First_Block =
                 Request.words (0)
               and then Decode_Transfer'Result.Data.Count = Request.words (2)
               and then Decode_Transfer'Result.Data.Grant.slot =
                 Request.words (1)
               and then Decode_Transfer'Result.Data.Grant.generation =
                 Request.words (3)
               and then Decode_Transfer'Result.Data.Direction =
                 (if Request.tag.label = OP_READ_BLOCKS then Device_Read
                  else Device_Write));

   function Decode_Transfer
     (Request : CuBit.Messages.Message; State : Device_State)
      return Decoded_Request
   is
      Write : constant Boolean := Request.tag.label = OP_WRITE_BLOCKS;
      --  WRITE_FLAG_FUA is the only request flag in the protocol.
      FUA   : constant Boolean :=
        Write and then Request.tag.protocol = WRITE_FLAG_FUA;
      First : constant Unsigned_64 := Request.words (0);
      Slot  : constant Unsigned_64 := Request.words (1);
      Count : constant Unsigned_64 := Request.words (2);
      Generation : constant Unsigned_64 := Request.words (3);
   begin
      if (Request.tag.protocol /= 0
          and then not (FUA and then Offers (State, FEATURE_FUA)))
        or else (Write and then Offers (State, FEATURE_READ_ONLY))
        or else Count = 0
        or else Count > State.Max_Transfer
        or else First >= State.Blocks
        or else Count > State.Blocks - First
        or else Slot > CuBit.Grant_References.Maximum_Slot
        or else Generation = 0
        or else Generation > CuBit.Grant_References.Maximum_Generation
      then
         return No_Request;
      end if;
      return
        (Kind => Transfer,
         Data =>
           (Direction   => (if Write then Device_Write else Device_Read),
            First_Block => First,
            Count       => Count,
            Block_Bytes => State.Logical_Size,
            Grant       => (slot => Slot, generation => Generation),
            Force_Unit_Access => FUA));
   end Decode_Transfer;

   function Decode
     (Request : CuBit.Messages.Message; State : Device_State)
      return Decoded_Request
   is
      Label : constant Unsigned_32 := Request.tag.label;
   begin
      if Label = OP_DESCRIBE_DEVICE then
         if Clean_Tag (Request, 0) then
            return (Kind => Describe);
         end if;
      elsif Label = OP_FLUSH_DEVICE then
         if Clean_Tag (Request, 0) and then Flushes (State) then
            return (Kind => Flush);
         end if;
      elsif Label in OP_READ_BLOCKS | OP_WRITE_BLOCKS
        and then State.Presence = Available
        and then Request.tag.length = 4
        and then Request.tag.reserved = 0
      then
         return Decode_Transfer (Request, State);
      end if;
      return No_Request;
   end Decode;

   function Reply_Of
     (Label : Unsigned_32; Length : Unsigned_8;
      Words : CuBit.Messages.MessageWords) return CuBit.Messages.Message;

   function Reply_Of
     (Label : Unsigned_32; Length : Unsigned_8;
      Words : CuBit.Messages.MessageWords) return CuBit.Messages.Message is
     ((tag => (label => Label, length => Length, protocol => 0, ipc => <>,
               reserved => 0),
       authorityTag => 0, words => Words));

   function Status_Reply (Success : Boolean; Moved : Unsigned_64)
     return CuBit.Messages.Message is
     (Reply_Of ((if Success then REPLY_OK else REPLY_ERROR), 1,
                [0 => Moved, others => 0]));

   function Describe_Reply (State : Device_State) return CuBit.Messages.Message
   is
   begin
      case State.Presence is
         when Available =>
            return Reply_Of
              (REPLY_OK, 4,
               [0 => State.Blocks,
                1 => Pack_Sizes (State.Logical_Size, State.Physical_Size),
                2 => State.Max_Transfer,
                3 => Pack_Properties (State.Features, State.Media)]);
         when No_Device =>
            return Reply_Of (REPLY_NO_DEVICE, 1, [others => 0]);
         when Faulted =>
            return Status_Reply (False, 0);
      end case;
   end Describe_Reply;

   function Transfer_Reply
     (Item : Transfer_Request; Returned : Boolean; Moved : Unsigned_64)
      return CuBit.Messages.Message is
     (Status_Reply (Returned and then Moved = Bytes (Item),
                    (if Returned then Moved else 0)));

   --  The kernel grant calls. Their effects live outside this model.
   procedure Acquire_Grant
     (Sender  : CuBit.Messages.Process_ID;
      Item    : Transfer_Request;
      Buffer  : out System.Address;
      Success : out Boolean)
     with Global => null;

   procedure Return_Grant
     (Item : Transfer_Request; Returned : out Boolean)
     with Global => null;

   procedure Acquire_Grant
     (Sender  : CuBit.Messages.Process_ID;
      Item    : Transfer_Request;
      Buffer  : out System.Address;
      Success : out Boolean)
     with SPARK_Mode => Off
   is
      use CuBit.Memory_Grants;
   begin
      Acquire
        (reference      => Item.Grant,
         expectedOwner  => Sender,
         byteOffset     => 0,
         byteLength     => Bytes (Item),
         --  A device read fills the caller's grant; a write reads it.
         requiredAccess =>
           (if Item.Direction = Device_Read then Write_Access
            else Read_Access),
         mappedAddress  => Buffer,
         success        => Success);
   end Acquire_Grant;

   procedure Return_Grant
     (Item : Transfer_Request; Returned : out Boolean)
     with SPARK_Mode => Off
   is
   begin
      CuBit.Memory_Grants.Return_Acquisition (Item.Grant, Returned);
   end Return_Grant;

   procedure Admit
     (Sender  : CuBit.Messages.Process_ID;
      Item    : Transfer_Request;
      Granted : out Loan;
      Success : out Boolean)
     with Refined_Global => null
   is
      Buffer : System.Address;
   begin
      Acquire_Grant (Sender, Item, Buffer, Success);
      Granted := (Item => Item, Buffer => Buffer);
   end Admit;

   procedure Settle
     (Granted  : Loan;
      Moved    : Unsigned_64;
      Response : out CuBit.Messages.Message)
     with Refined_Global => null
   is
      Returned : Boolean;
   begin
      Return_Grant (Granted.Item, Returned);
      Response := Transfer_Reply (Granted.Item, Returned, Moved);
   end Settle;
end CuBit.Block_Requests;
