------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Rings;
with CuBit.Child_Exits;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Control_Events;
with CuBit.Protocols;
with CuBit.Stream_Regions;
with CuBit.Stream_Rings;

package body CuBit.Libc_Streams is

   package K renames CuBit.Kernel_ABI;
   package Rings renames CuBit.Channel_Rings;
   package Regions renames CuBit.Stream_Regions;
   package SR renames CuBit.Stream_Rings;
   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   use type CC.Channel_Kind;
   use type CC.Channel_Policy;

   use type Interfaces.C.int;
   use type Interfaces.C.long;

   --  CuBit.Streams' limits (tests/libc-ada checks them). A reader opens a
   --  channel on an outlet's connector (CuBit.Outlet_Channels) and gets its
   --  own read-only grant of the region, with grant events; the ring is
   --  CuBit.Stream_Regions'.
   Maximum_Streams : constant := 4;
   Maximum_Subscribers : constant := 8;

   subtype Subscriber_Slot is Natural range 0 .. Maximum_Subscribers - 1;
   type Subscriber is record
      Process : Unsigned_64 := 0;          --  0: free
      Grant   : Unsigned_64 := 0;          --  its slot
      Generation : Unsigned_64 := 0;
   end record;
   type Subscriber_Array is array (Subscriber_Slot) of Subscriber;

   type Stream_State is record
      Active : Boolean := False;
      Id     : Unsigned_16 := 0;
      Pages  : Natural := 0;              --  declared
      Base   : Unsigned_64 := 0;          --  the region
      Writer : Rings.Producer;
      Element : Unsigned_16 := 0;         --  CuBit.Stream_Rings.ELEMENT_*
      Subscribers : Subscriber_Array;
   end record;
   Streams : array (0 .. Maximum_Streams - 1) of Stream_State;
   pragma Suppress_Initialization (Streams);

   function Find (Id : Unsigned_16) return Integer;
   function Find (Id : Unsigned_16) return Integer is
   begin
      for I in Streams'Range loop
         if Streams (I).Active and then Streams (I).Id = Id then
            return I;
         end if;
      end loop;
      return -1;
   end Find;

   function Free_Stream return Integer;
   function Free_Stream return Integer is
   begin
      for I in Streams'Range loop
         if not Streams (I).Active then
            return I;
         end if;
      end loop;
      return -1;
   end Free_Stream;

   procedure Adopt (Stream : Unsigned_16; Pages : Interfaces.C.unsigned; Base : System.Address) is
      Address : constant Unsigned_64 := Unsigned_64 (To_Integer (Base));
      Slot : constant Integer := Free_Stream;
   begin
      if Find (Stream) >= 0 or else Slot < 0 then
         return;
      end if;
      Streams (Slot) := (Active => True, Id => Stream, Pages => Natural (Pages),
                         Base => Address,
                         Writer => Regions.Writer_Of (Address, Natural (Pages)),
                         Element => Regions.Element (Address),
                         Subscribers => [others => <>]);
   end Adopt;

   procedure Create (Stream : Unsigned_16; Pages : Interfaces.C.unsigned;
                     Type_Tag : Unsigned_16)
   is
      Slot : constant Integer := Free_Stream;
      Bytes : constant Unsigned_64 := Regions.Region_Bytes (Natural (Pages));
      Break, Base : Unsigned_64;
   begin
      if Find (Stream) >= 0 or else Slot < 0 then   --  two descriptors may share a port
         return;
      end if;
      --  A page more than the region: grants are whole pages, and the break
      --  need not be page aligned.
      Break := CuBit.Kernel_Calls.Call (K.Grow_Heap, Bytes + SR.PAGE_BYTES);
      if Break = K.Failed then
         return;
      end if;
      Base := (Break + SR.PAGE_BYTES - 1) / SR.PAGE_BYTES * SR.PAGE_BYTES;
      Regions.Initialize (Base, Type_Tag);
      Streams (Slot) := (Active => True, Id => Stream, Pages => Natural (Pages), Base => Base,
                         Writer => SR.New_Writer (SR.Ring_Bytes (Regions.Declared (Natural (Pages)))),
                         Element => Type_Tag,
                         Subscribers => [others => <>]);
   end Create;

   --  SYSCALL_REPLY: the tag (label, as the message's first word) and words.
   procedure Reply (To : Interfaces.C.long; Label : Unsigned_32;
                    W0, W1, W2, W3 : Unsigned_64 := 0);
   procedure Reply (To : Interfaces.C.long; Label : Unsigned_32;
                    W0, W1, W2, W3 : Unsigned_64 := 0)
   is
      Ignore : constant Unsigned_64 := CuBit.Kernel_Calls.Call
        (K.Reply, Unsigned_64'Mod (To), Unsigned_64 (Label), W0, W1, W2, W3);
   begin
      null;
   end Reply;

   function Number_Of (Stream : Natural; Slot : Subscriber_Slot) return Unsigned_64 is
     (Unsigned_64 (Stream) * Maximum_Subscribers + Unsigned_64 (Slot) + 1);

   function Element_Contract (Element : Unsigned_16) return CuBit.Protocols.Schema_Contract is
     (if Element = SR.ELEMENT_TEXT_LINE then CuBit.Protocols.TEXT_LINE_CONTRACT
      else CuBit.Protocols.RAW_BYTES_CONTRACT);

   procedure Reply_Refused (To : Interfaces.C.long; Why : CP.Open_Refusal);
   procedure Reply_Refused (To : Interfaces.C.long; Why : CP.Open_Refusal) is
   begin
      Reply (To, K.Reply_Error, CP.Open_Refusal'Enum_Rep (Why));
   end Reply_Refused;

   --  A reader opens a channel on an outlet (CuBit.Outlet_Channels): it gets
   --  its own read-only grant of the region, with grant events. The reply:
   --  its number here, the grant, the ring's pages.
   procedure Open_Reader (From : Interfaces.C.long; M : K.Message);
   procedure Open_Reader (From : Interfaces.C.long; M : K.Message) is
      Offered : CC.Contract;
      Decoded : Boolean;
      Index : constant Integer := Find (M.Reserved);
      Who : constant Unsigned_64 := Unsigned_64'Mod (From);
   begin
      CC.Decode ([M.Words (0), M.Words (1), M.Words (2)], Offered, Decoded);
      if not Decoded or else Index < 0 or else M.Length /= CP.Open_Words
        or else Offered.Kind /= CC.Queue or else Offered.Policy /= CC.Drop_Oldest
        or else not CuBit.Protocols.Compatible
                      (Element_Contract (Streams (Index).Element), Offered.Element)
      then
         Reply_Refused (From, CP.Unknown_Type);
         return;
      end if;
      declare
         S : Stream_State renames Streams (Index);
         Ring_Pages : constant Positive := SR.Ring_Pages (Regions.Declared (S.Pages));
         Slot, Generation : Unsigned_64;
      begin
         for Reader in Subscriber_Slot loop
            if S.Subscribers (Reader).Process = 0 then
               Slot := CuBit.Kernel_Calls.Call
                 (K.Create_Shared_Memory_Grant_For_Process_Id, Who, S.Base,
                  Unsigned_64 (SR.Region_Pages (Regions.Declared (S.Pages))),
                  K.Grant_Read_Only + K.Grant_Notify);
               Generation := (if Slot = K.Failed then 0
                              else CuBit.Kernel_Calls.Call
                                     (K.Get_Owned_Shared_Memory_Grant_Generation, Slot));
               if Slot = K.Failed or else Generation = K.Failed or else Generation = 0 then
                  Reply_Refused (From, CP.No_Room);
                  return;
               end if;
               S.Subscribers (Reader) := (Process => Who, Grant => Slot, Generation => Generation);
               Reply (From, K.Reply_OK, Number_Of (Index, Reader),
                      Shift_Left (Generation, K.Generation_Shift) or Slot,
                      Unsigned_64 (Ring_Pages));
               return;
            end if;
         end loop;
         Reply_Refused (From, CP.No_Room);
      end;
   end Open_Reader;

   procedure Let_Go (Item : in out Subscriber);
   procedure Let_Go (Item : in out Subscriber) is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (K.Revoke_Shared_Memory_Grant_Reference, Item.Grant, Item.Generation);
      Item := (others => <>);
   end Let_Go;

   function Handle_Message (From : Interfaces.C.long; Message : System.Address)
     return Interfaces.C.int
   is
      M : constant K.Message with Import, Address => Message;
      Number : constant Unsigned_64 := M.Words (0) - 1;
   begin
      if M.Label = CP.OP_OPEN_CONSUMING and then From /= 0 then
         Open_Reader (From, M);
         return 1;
      elsif M.Label = CP.OP_CLOSE and then From /= 0 then
         --  One-way: a reader let go of its channel.
         if M.Words (0) in 1 .. Maximum_Streams * Maximum_Subscribers then
            declare
               Item : Subscriber renames Streams (Natural (Number / Maximum_Subscribers))
                                          .Subscribers (Natural (Number mod Maximum_Subscribers));
            begin
               --  Word 1 names the reader's grant: a slot reused since
               --  (the kernel's event freed it first) is another channel.
               if Item.Process /= 0 and then Item.Process = Unsigned_64'Mod (From)
                 and then M.Length = CP.Close_Words
                 and then M.Words (1) = (Shift_Left (Item.Generation, K.Generation_Shift) or Item.Grant)
               then
                  Let_Go (Item);
               end if;
            end;
         end if;
         return 1;
      elsif M.Label = CuBit.Control_Events.Grant_Returned_Label and then From = 0 then
         --  The kernel's event (no sender): a client's call with this label
         --  is not one.
         --  A reader's grant came back (it closed, or died): its slot is free.
         for S of Streams loop
            for Item of S.Subscribers loop
               if Item.Process /= 0 and then Item.Grant = M.Words (0)
                 and then Item.Generation = M.Words (1)
               then
                  Item := (others => <>);
               end if;
            end loop;
         end loop;
         return 1;
      end if;
      return 0;                         --  not a stream request
   end Handle_Message;

   --  One pending request, if any (a program without a mailbox owner).
   procedure Poll_Subscription;
   procedure Note_Event (Item : System.Address)
   with Import, Convention => C, External_Name => "__cubit_note_event";

   --  POLL_ANY_IPC answers the sender, and an event's sender is 0, the
   --  same as "nothing waiting": the message (zeroed first) tells them
   --  apart, since every message has a label.
   procedure Poll_Subscription is
      M : aliased K.Message := (others => <>);
      From : constant Unsigned_64 := CuBit.Kernel_Calls.Call
        (K.Poll_Any_IPC, Unsigned_64 (To_Integer (M'Address)));
      Ignore : Interfaces.C.int;
   begin
      if From = K.Failed or else (From = 0 and then M.Label = 0) then
         return;
      elsif From = 0 and then M.Label = CuBit.Child_Exits.Event_Label then
         --  A child's exit belongs to waitpid's bookkeeping, not here.
         Note_Event (M'Address);
      else
         Ignore := Handle_Message (Interfaces.C.long (From), M'Address);
      end if;
   end Poll_Subscription;

   function Write (Stream : Unsigned_16; Data : System.Address; Length : Unsigned_32;
                   Type_Tag : Unsigned_16) return Unsigned_32
   is
      pragma Unreferenced (Type_Tag);   --  the stream's, in the region
      Index : constant Integer := Find (Stream);
   begin
      if Index < 0 or else Length = 0 or else Length > Unsigned_32 (Rings.Maximum_Size) then
         return 0;
      end if;
      if Poll_On_Write /= 0 then
         Poll_Subscription;
      end if;
      --  As records the ring takes (at most half of it each).
      declare
         S : Stream_State renames Streams (Index);
         Largest : constant Positive := SR.Largest_Payload (S.Writer.Size);
         Left : Natural := Natural (Length);
         Position : Unsigned_64 := Unsigned_64 (To_Integer (Data));
         Chunk : Positive;
      begin
         while Left > 0 loop
            Chunk := Natural'Min (Left, Largest);
            if not Regions.Write (S.Base, S.Writer, To_Address (Integer_Address (Position)), Chunk)
            then
               return Length - Unsigned_32 (Left);
            end if;
            Position := Position + Unsigned_64 (Chunk);
            Left := Left - Chunk;
         end loop;
      end;
      return Length;
   end Write;

end CuBit.Libc_Streams;
