------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  IPC by handle (CAP-002): calls through endpoints, saved replies, giving
--  a handle up, and capabilities carried by replies.
--
--  @description
--  A capability enters a running process's table only in a reply to its own
--  request that said it accepts one (Call_Accepting): the server answers
--  with Reply_With, which copies (and may narrow) one of its capabilities
--  into the requester's table. Accept_* then checks the kind of what
--  arrived. A request that did not accept one is never given one.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages;

package CuBit.Handles.IPC is

   subtype Message is CuBit.Messages.Message;
   subtype MessageTag is CuBit.Messages.MessageTag;

   ---------------------------------------------------------------------------
   --  Endpoints. The deadline is an absolute monotonic millisecond or
   --  CuBit.Messages.Wait_Forever, spelled out.
   ---------------------------------------------------------------------------
   function Send (Target : Endpoint; Msg : Message; Deadline : Unsigned_64)
     return MessageTag;
   function Call (Target : Endpoint; Msg : in out Message; Deadline : Unsigned_64)
     return MessageTag;
   function Submit (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Saved replies (answer a request later).
   ---------------------------------------------------------------------------
   type Saved_Reply (Saved : Boolean := False) is record
      case Saved is
         when True  => Handle : Reply;
         when False => Why    : Refusal := ABI.Invalid_Argument;
      end case;
   end record;

   --  The calling thread's current reply authority becomes a handle. On a
   --  refusal the thread keeps it (and can still answer now).
   function Save_Reply return Saved_Reply;

   --  Answer a saved request; the saved reply is used up either way.
   --  True when the answer was delivered.
   function Answer (Item : Reply; Msg : Message) return Boolean;

   ---------------------------------------------------------------------------
   --  Giving a handle up. Closing a saved reply unanswered wakes its caller
   --  with an empty reply.
   ---------------------------------------------------------------------------
   type Close_Result (Closed : Boolean := False) is record
      case Closed is
         when True  => null;
         when False => Why : Refusal := ABI.Invalid_Handle;
      end case;
   end record;
   function Close (Item : Endpoint) return Close_Result;
   function Close (Item : Reply) return Close_Result;
   function Close (Item : Process) return Close_Result;
   function Close (Item : Cspace) return Close_Result;
   function Close (Item : Device_Memory) return Close_Result;
   function Close (Item : IO_Ports) return Close_Result;
   function Close (Item : Interrupt) return Close_Result;
   function Close (Item : Notification) return Close_Result;
   function Close (Item : Scheduling) return Close_Result;

   ---------------------------------------------------------------------------
   --  What a handle names (INSPECT_CAPABILITY on this process's table).
   ---------------------------------------------------------------------------
   type Description (Current : Boolean := False) is record
      case Current is
         when True =>
            Kind          : ABI.Kind := ABI.No_Capability;
            Held          : Rights;
            Authority_Tag : Unsigned_64 := 0;
            --  A process identity for endpoint, process and CSPACE kinds;
            --  the first address, port, vector or role for ranges.
            Object        : Unsigned_64 := 0;
            Parameter     : Unsigned_64 := 0;
         when False =>
            Why           : Refusal := ABI.Invalid_Handle;
      end case;
   end record;
   function Describe (Item : Endpoint) return Description;
   function Describe (Item : Process) return Description;
   function Describe (Item : Device_Memory) return Description;
   function Describe (Item : IO_Ports) return Description;
   function Describe (Item : Interrupt) return Description;
   function Describe (Item : Notification) return Description;
   function Describe (Item : Scheduling) return Description;

   ---------------------------------------------------------------------------
   --  Capabilities in replies.
   ---------------------------------------------------------------------------
   --  What a reply carried. Absent: nothing (the server answered without
   --  one). Present: a handle in this process's table. Refused: the server sent one and this process's table could not
   --  take it (its limit or memory).
   type Carried_Kind is (Absent, Present, Refused);
   type Carried (Kind : Carried_Kind := Absent) is record
      case Kind is
         when Absent  => null;
         when Present => Word : Unsigned_64 := 0;
         when Refused => Why  : Refusal := ABI.Limit_Reached;
      end case;
   end record;

   --  What a delivered reply (or an asynchronous completion's message)
   --  carries, from the flags only the kernel sets.
   function Carried_By (Reply_Message : Message) return Carried;

   --  A request that accepts one capability in its reply: Call with the
   --  Accepts_Capability flag, then Carried_By on the reply.
   function Call_Accepting
     (Target   : Endpoint;
      Msg      : in out Message;
      Deadline : Unsigned_64;
      Received : out Carried) return MessageTag;

   --  The same for an asynchronous request: the completion's message says
   --  (Carried_By) what came with the answer.
   function Submit_Accepting (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean;

   --  Typed acceptance: the carried handle, if it is of this kind (checked
   --  with the kernel); otherwise the null handle, and Why says.
   type Accepted_Endpoint (OK : Boolean := False) is record
      case OK is
         when True  => Value : Endpoint;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   type Accepted_Device_Memory (OK : Boolean := False) is record
      case OK is
         when True  => Value : Device_Memory;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   type Accepted_Interrupt (OK : Boolean := False) is record
      case OK is
         when True  => Value : Interrupt;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   type Accepted_Notification (OK : Boolean := False) is record
      case OK is
         when True  => Value : Notification;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint;
   function Accept_Device_Memory (Item : Carried) return Accepted_Device_Memory;
   function Accept_Interrupt (Item : Carried) return Accepted_Interrupt;
   function Accept_Notification (Item : Carried) return Accepted_Notification;

   --  Answer the request just received from Requester with a copy of one of
   --  this process's capabilities. Refused when the request did not accept
   --  one (Not_Accepted: the reply authority is kept, answer normally), or
   --  the copy would amplify. Transferred when the copy was placed in the
   --  requester's table and the reply delivered; Delivered_Refused when the
   --  requester's table refused it (the reply still went, saying why).
   type Reply_Outcome is (Transferred, Delivered_Refused, Refused);
   type Reply_Result (Outcome : Reply_Outcome := Refused) is record
      case Outcome is
         when Transferred => null;
         when Delivered_Refused | Refused => Why : Refusal := ABI.Not_Accepted;
      end case;
   end record;
   function Reply_With
     (Requester : CuBit.Messages.Process_ID;
      Msg       : Message;
      Copy      : Derivation) return Reply_Result;

   --  The same for a request set aside with Save_Reply. Refused (the saved
   --  reply kept) when it did not accept one or the copy would amplify.
   function Reply_Saved_With
     (Item : Reply; Msg : Message; Copy : Derivation) return Reply_Result;

   ---------------------------------------------------------------------------
   --  Capabilities offered with requests. A call may offer one copy of a
   --  capability this process holds (Grant needed); the server takes it
   --  explicitly while answering that call, or the offer is dropped when
   --  the call ends. Nothing enters a table unasked.
   ---------------------------------------------------------------------------
   function Call_Offering
     (Target   : Endpoint;
      Msg      : in out Message;
      Deadline : Unsigned_64;
      Offer    : Derivation) return MessageTag;

   --  Whether a received request carries an offer.
   function Offers (Request : Message) return Boolean;

   --  Take what Requester's call (the one this thread is answering)
   --  offers: Present with its handle, or Refused with why (Not_Accepted:
   --  nothing was offered).
   function Accept_Offered (Requester : CuBit.Messages.Process_ID) return Carried;

end CuBit.Handles.IPC;
