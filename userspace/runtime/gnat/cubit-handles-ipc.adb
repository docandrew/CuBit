------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.IPC - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;
with CuBit.Handles.Kernel;

package body CuBit.Handles.IPC is

   package K renames CuBit.Handles.Kernel;
   use type ABI.Refusal;
   use type ABI.Kind;

   function Tag_Word is new Ada.Unchecked_Conversion (MessageTag, Unsigned_64);
   function Word_Tag is new Ada.Unchecked_Conversion (Unsigned_64, MessageTag);

   function Address_Word (A : System.Address) return Unsigned_64;
   function Closing (Word : Unsigned_64) return Close_Result;
   function Describing (Word : Unsigned_64) return Description;
   function Accepting (Msg : Message) return Message;
   function Kind_Of (Item : Carried) return Description;

   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   function Send (Target : Endpoint; Msg : Message; Deadline : Unsigned_64)
     return MessageTag is
     (Word_Tag (K.Call (K.Send_Via_Endpoint, K.Word (Target), Tag_Word (Msg.tag),
                        Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3),
                        Deadline)));

   function Call (Target : Endpoint; Msg : in out Message; Deadline : Unsigned_64)
     return MessageTag is
   begin
      return Word_Tag (K.Call (K.Call_Via_Endpoint, K.Word (Target),
                               Address_Word (Msg'Address), Deadline));
   end Call;

   function Submit (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean is
     (K.Call (K.Submit_Via_Endpoint, K.Word (Target), Tag_Word (Msg.tag),
              Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3), Token) = 1);

   function Save_Reply return Saved_Reply is
      Result : constant Unsigned_64 := K.Call (K.Move_Reply);
   begin
      if ABI.Is_Handle_Word (Result) then
         return (Saved => True, Handle => K.To_Reply (Result));
      end if;
      return (Saved => False, Why => K.Refusal_Of (Result));
   end Save_Reply;

   function Answer (Item : Reply; Msg : Message) return Boolean is
     (K.Call (K.Reply_And_Consume, K.Word (Item), Tag_Word (Msg.tag),
              Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)) /= 0);

   function Closing (Word : Unsigned_64) return Close_Result is
      Result : constant Unsigned_64 := K.Call (K.Close_Handle, Word);
   begin
      if Result = 0 then
         return (Closed => True);
      end if;
      return (Closed => False, Why => K.Refusal_Of (Result));
   end Closing;

   function Close (Item : Endpoint) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Reply) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Process) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Cspace) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Device_Memory) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : IO_Ports) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Interrupt) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Notification) return Close_Result is (Closing (K.Word (Item)));
   function Close (Item : Scheduling) return Close_Result is (Closing (K.Word (Item)));

   function Describing (Word : Unsigned_64) return Description is
      Info   : aliased ABI.Inspection := [others => 0];
      Result : constant Unsigned_64 :=
        K.Call (K.Inspect, Word, Address_Word (Info'Address));
      Kind_Word : Unsigned_64;
   begin
      if Result /= 0 then   --  INSPECT_CAPABILITY: 0, or a refusal code
         return (Current => False, Why => K.Refusal_Of (Result));
      end if;
      Kind_Word := Info (ABI.Capability_Kind);
      if Kind_Word > ABI.Kind'Enum_Rep (ABI.Kind'Last) then
         return (Current => False, Why => ABI.Invalid_Argument);
      end if;
      return
        (Current       => True,
         Kind          => ABI.Kind'Enum_Val (Kind_Word),
         Held          =>
           (Read    => (Info (ABI.Rights_Mask) and ABI.Right_Read) /= 0,
            Write   => (Info (ABI.Rights_Mask) and ABI.Right_Write) /= 0,
            Execute => (Info (ABI.Rights_Mask) and ABI.Right_Execute) /= 0,
            Grant   => (Info (ABI.Rights_Mask) and ABI.Right_Grant) /= 0,
            Revoke  => (Info (ABI.Rights_Mask) and ABI.Right_Revoke) /= 0),
         Authority_Tag => Info (ABI.Authority_Tag),
         Object        => Info (ABI.Object),
         Parameter     => Info (ABI.Parameter));
   end Describing;

   function Describe (Item : Endpoint) return Description is (Describing (K.Word (Item)));
   function Describe (Item : Process) return Description is (Describing (K.Word (Item)));
   function Describe (Item : Device_Memory) return Description is
     (Describing (K.Word (Item)));
   function Describe (Item : IO_Ports) return Description is (Describing (K.Word (Item)));
   function Describe (Item : Interrupt) return Description is (Describing (K.Word (Item)));
   function Describe (Item : Notification) return Description is
     (Describing (K.Word (Item)));
   function Describe (Item : Scheduling) return Description is (Describing (K.Word (Item)));

   function Carried_By (Reply_Message : Message) return Carried is
   begin
      if Reply_Message.tag.ipc.Capability_Transferred
        and then ABI.Is_Handle_Word (Reply_Message.authorityTag)
      then
         return (Kind => Present, Word => Reply_Message.authorityTag);
      elsif Reply_Message.tag.ipc.Capability_Refused then
         return (Kind => Refused, Why => K.Refusal_Of (Reply_Message.authorityTag));
      end if;
      return (Kind => Absent);
   end Carried_By;

   function Accepting (Msg : Message) return Message is
     ((tag => (label    => Msg.tag.label,
               length   => Msg.tag.length,
               protocol => Msg.tag.protocol,
               ipc      => (Msg.tag.ipc with delta Accepts_Capability => True),
               reserved => Msg.tag.reserved),
       authorityTag => Msg.authorityTag,
       words => Msg.words));

   function Call_Accepting
     (Target   : Endpoint;
      Msg      : in out Message;
      Deadline : Unsigned_64;
      Received : out Carried) return MessageTag
   is
      Result : MessageTag;
   begin
      Msg := Accepting (Msg);
      Result := Call (Target, Msg, Deadline);
      Received := Carried_By (Msg);
      return Result;
   end Call_Accepting;

   function Submit_Accepting (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean is (Submit (Target, Accepting (Msg), Token));

   --  The kind of a carried word, checked with the kernel.
   function Kind_Of (Item : Carried) return Description is
     (if Item.Kind = Present then Describing (Item.Word)
      else (Current => False,
            Why => (if Item.Kind = Refused then Item.Why else ABI.Invalid_Handle)));

   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint is
      D : constant Description := Kind_Of (Item);
   begin
      if not D.Current then
         return (OK => False, Why => D.Why);
      elsif D.Kind /= ABI.Endpoint then
         return (OK => False, Why => ABI.Wrong_Kind);
      end if;
      return (OK => True, Value => K.To_Endpoint (Item.Word));
   end Accept_Endpoint;

   function Accept_Device_Memory (Item : Carried) return Accepted_Device_Memory is
      D : constant Description := Kind_Of (Item);
   begin
      if not D.Current then
         return (OK => False, Why => D.Why);
      elsif D.Kind /= ABI.Device_Memory then
         return (OK => False, Why => ABI.Wrong_Kind);
      end if;
      return (OK => True, Value => K.To_Device_Memory (Item.Word));
   end Accept_Device_Memory;

   function Accept_Interrupt (Item : Carried) return Accepted_Interrupt is
      D : constant Description := Kind_Of (Item);
   begin
      if not D.Current then
         return (OK => False, Why => D.Why);
      elsif D.Kind /= ABI.Interrupt then
         return (OK => False, Why => ABI.Wrong_Kind);
      end if;
      return (OK => True, Value => K.To_Interrupt (Item.Word));
   end Accept_Interrupt;

   function Accept_Notification (Item : Carried) return Accepted_Notification is
      D : constant Description := Kind_Of (Item);
   begin
      if not D.Current then
         return (OK => False, Why => D.Why);
      elsif D.Kind /= ABI.Notification then
         return (OK => False, Why => ABI.Wrong_Kind);
      end if;
      return (OK => True, Value => K.To_Notification (Item.Word));
   end Accept_Notification;

   function Reply_With
     (Requester : CuBit.Messages.Process_ID;
      Msg       : Message;
      Copy      : Derivation) return Reply_Result
   is
      Words  : aliased ABI.Derivation := K.Derivation_Words (Copy);
      Answer : aliased Message := Msg;
      Result : constant Unsigned_64 :=
        K.Call (K.Reply_With_Capability, CuBit.Messages.To_Word (Requester),
                Address_Word (Answer'Address), Address_Word (Words'Address));
      Why    : Refusal;
   begin
      if ABI.Is_Handle_Word (Result) then
         return (Outcome => Transferred);
      end if;
      Why := K.Refusal_Of (Result);
      if Why in ABI.Limit_Reached | ABI.Table_Full | ABI.Out_Of_Memory then
         return (Outcome => Delivered_Refused, Why => Why);
      end if;
      return (Outcome => Refused, Why => Why);
   end Reply_With;

   function Reply_Saved_With
     (Item : Reply; Msg : Message; Copy : Derivation) return Reply_Result
   is
      Words  : aliased ABI.Derivation := K.Derivation_Words (Copy);
      Answer : aliased Message := Msg;
      Result : constant Unsigned_64 :=
        K.Call (K.Reply_Saved_With_Capability, K.Word (Item),
                Address_Word (Answer'Address), Address_Word (Words'Address));
      Why    : Refusal;
   begin
      if ABI.Is_Handle_Word (Result) then
         return (Outcome => Transferred);
      end if;
      Why := K.Refusal_Of (Result);
      if Why in ABI.Limit_Reached | ABI.Table_Full | ABI.Out_Of_Memory then
         return (Outcome => Delivered_Refused, Why => Why);
      end if;
      return (Outcome => Refused, Why => Why);
   end Reply_Saved_With;

   function Call_Offering
     (Target   : Endpoint;
      Msg      : in out Message;
      Deadline : Unsigned_64;
      Offer    : Derivation) return MessageTag
   is
      Words : aliased ABI.Derivation := K.Derivation_Words (Offer);
   begin
      return Word_Tag (K.Call (K.Call_Offering_Capability, K.Word (Target),
                               Address_Word (Msg'Address), Address_Word (Words'Address),
                               Deadline));
   end Call_Offering;

   function Offers (Request : Message) return Boolean is
     (Request.tag.ipc.Capability_Offered);

   function Accept_Offered (Requester : CuBit.Messages.Process_ID) return Carried is
      Result : constant Unsigned_64 :=
        K.Call (K.Accept_Offered_Capability, CuBit.Messages.To_Word (Requester));
   begin
      if ABI.Is_Handle_Word (Result) then
         return (Kind => Present, Word => Result);
      end if;
      return (Kind => Refused, Why => K.Refusal_Of (Result));
   end Accept_Offered;

end CuBit.Handles.IPC;
