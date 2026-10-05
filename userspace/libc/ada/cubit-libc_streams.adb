------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_Stream_Rings; use CuBit.Libc_Stream_Rings;

package body CuBit.Libc_Streams is

   package K renames CuBit.Kernel_ABI;

   use type Interfaces.C.int;
   use type Interfaces.C.long;

   --  CuBit.Streams' protocol (the ring layout is CuBit.Libc_Stream_Rings';
   --  tests/libc-ada checks both).
   Maximum_Streams : constant := 4;
   Maximum_Subscribers : constant := 8;
   OP_STREAM_SUBSCRIBE   : constant := 16#0700#;
   OP_STREAM_UNSUBSCRIBE : constant := 16#0701#;
   Grant_Read_Only : constant := 0;

   type Grant_Ids is array (0 .. Maximum_Subscribers - 1) of Unsigned_64;
   type Stream_State is record
      Active : Boolean := False;
      Id     : Unsigned_16 := 0;
      Pages  : Unsigned_64 := 0;
      Base   : Unsigned_64 := 0;
      Size   : Natural := 0;            --  ring capacity
      Grants : Grant_Ids := [others => 0];
   end record;
   Streams : array (0 .. Maximum_Streams - 1) of Stream_State;
   pragma Suppress_Initialization (Streams);

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));

   function Read_32 (At_Byte : Unsigned_64) return Unsigned_32;
   function Read_32 (At_Byte : Unsigned_64) return Unsigned_32 is
      W : constant Unsigned_32 with Import, Volatile, Address => To_Address (At_Byte);
   begin
      return W;
   end Read_32;
   procedure Write_32 (At_Byte : Unsigned_64; Value : Unsigned_32);
   procedure Write_32 (At_Byte : Unsigned_64; Value : Unsigned_32) is
      W : Unsigned_32 with Import, Volatile, Address => To_Address (At_Byte);
   begin
      W := Value;
   end Write_32;
   procedure Write_16 (At_Byte : Unsigned_64; Value : Unsigned_16);
   procedure Write_16 (At_Byte : Unsigned_64; Value : Unsigned_16) is
      W : Unsigned_16 with Import, Volatile, Address => To_Address (At_Byte);
   begin
      W := Value;
   end Write_16;
   function Read_8 (At_Byte : Unsigned_64) return Unsigned_8;
   function Read_8 (At_Byte : Unsigned_64) return Unsigned_8 is
      W : constant Unsigned_8 with Import, Volatile, Address => To_Address (At_Byte);
   begin
      return W;
   end Read_8;
   procedure Write_8 (At_Byte : Unsigned_64; Value : Unsigned_8);
   procedure Write_8 (At_Byte : Unsigned_64; Value : Unsigned_8) is
      W : Unsigned_8 with Import, Volatile, Address => To_Address (At_Byte);
   begin
      W := Value;
   end Write_8;

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

   function Subscriber (Base : Unsigned_64; Slot : Natural) return Unsigned_64 is
     (Base + SUBSCRIBER_TABLE_OFF + Unsigned_64 (Slot * Subscriber_Entry_Size));

   procedure Adopt (Stream : Unsigned_16; Pages : Interfaces.C.unsigned; Base : System.Address) is
      Address : constant Unsigned_64 := Unsigned_64 (System.Storage_Elements.To_Integer (Base));
      Capacity : constant Unsigned_32 := Read_32 (Address + HDR_CAPACITY);
   begin
      if Find (Stream) >= 0
        or else Read_32 (Address + HDR_MAGIC) /= Stream_Magic
        or else Unsigned_64 (Capacity) + Header_Size /= Unsigned_64 (Pages) * K.Page_Bytes
      then
         return;
      end if;
      for I in Streams'Range loop
         if not Streams (I).Active then
            Streams (I) := (Active => True, Id => Stream, Pages => Unsigned_64 (Pages),
                            Base => Address, Size => Natural (Capacity),
                            Grants => [others => 0]);
            return;
         end if;
      end loop;
   end Adopt;

   procedure Create (Stream : Unsigned_16; Pages : Interfaces.C.unsigned;
                     Type_Tag : Unsigned_16)
   is
      Total : constant Unsigned_64 := Unsigned_64 (Pages) * K.Page_Bytes;
      Base : Unsigned_64;
   begin
      if Total <= Header_Size or else Total - Header_Size > Maximum_Capacity
        or else Find (Stream) >= 0    --  two descriptors may share a port
      then
         return;
      end if;
      for I in Streams'Range loop
         if not Streams (I).Active then
            Base := CuBit.Kernel_Calls.Call (K.Grow_Heap, Total);
            if Base = K.Failed then
               return;
            end if;
            declare
               Header : Storage_Array (1 .. Header_Size) with Import, Address => To_Address (Base);
            begin
               Header := [others => 0];
            end;
            Write_32 (Base + HDR_MAGIC, Stream_Magic);
            Write_16 (Base + HDR_VERSION, Stream_Version);
            Write_32 (Base + HDR_CAPACITY, Unsigned_32 (Total - Header_Size));
            Write_16 (Base + HDR_DEFAULT_TYPE_TAG, Type_Tag);
            Write_8 (Base + HDR_OVERFLOW_POLICY, Drop_Oldest);
            Write_16 (Base + HDR_STREAM_ID, Stream);
            Streams (I) := (Active => True, Id => Stream, Pages => Unsigned_64 (Pages),
                            Base => Base, Size => Natural (Total - Header_Size),
                            Grants => [others => 0]);
            return;
         end if;
      end loop;
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

   function Handle_Message (From : Interfaces.C.long; Message : System.Address)
     return Interfaces.C.int
   is
      M : constant K.Message with Import, Address => Message;
      Id : constant Unsigned_16 := Unsigned_16 (M.Words (0) and 16#FFFF#);
      Index : constant Integer := Find (Id);
   begin
      if M.Label = OP_STREAM_SUBSCRIBE then
         if Index < 0 then
            Reply (From, K.Reply_Error);
            return 1;
         end if;
         declare
            S : Stream_State renames Streams (Index);
            Count : constant Natural := Natural (Read_8 (S.Base + HDR_SUBSCRIBER_COUNT));
            Grant : Unsigned_64;
            Producer : Unsigned_32;
         begin
            if Count >= Maximum_Subscribers then
               Reply (From, K.Reply_Error);
               return 1;
            end if;
            Grant := CuBit.Kernel_Calls.Call
              (K.Create_Shared_Memory_Grant_For_Process_Id, Unsigned_64'Mod (From),
               S.Base, S.Pages, Grant_Read_Only);
            if Grant = K.Failed then
               Reply (From, K.Reply_Error);
               return 1;
            end if;
            S.Grants (Count) := Grant;
            Producer := Read_32 (S.Base + HDR_PRODUCER_IDX);
            Write_32 (Subscriber (S.Base, Count) + SUB_OFF_PID, Unsigned_32'Mod (From));
            Write_32 (Subscriber (S.Base, Count) + SUB_OFF_CURSOR, Producer);
            Write_16 (Subscriber (S.Base, Count) + SUB_OFF_FLAGS, 0);
            Write_8 (S.Base + HDR_SUBSCRIBER_COUNT, Unsigned_8 (Count + 1));
            Reply (From, K.Reply_OK, Grant, Unsigned_64 (Count), Unsigned_64 (S.Size),
                   Unsigned_64 (Producer));
            return 1;
         end;
      elsif M.Label = OP_STREAM_UNSUBSCRIBE then
         if Index < 0 or else M.Words (1) >= Maximum_Subscribers then
            Reply (From, K.Reply_Error);
            return 1;
         end if;
         declare
            S : Stream_State renames Streams (Index);
            Slot : constant Natural := Natural (M.Words (1));
            Entry_At : constant Unsigned_64 := Subscriber (S.Base, Slot);
            Count : constant Unsigned_8 := Read_8 (S.Base + HDR_SUBSCRIBER_COUNT);
            Ignore : Unsigned_64;
         begin
            if Read_32 (Entry_At + SUB_OFF_PID) /= Unsigned_32'Mod (From) then
               Reply (From, K.Reply_Error);
               return 1;
            end if;
            if S.Grants (Slot) /= 0 then
               Ignore := CuBit.Kernel_Calls.Call (K.Revoke_Shared_Memory_Grant, S.Grants (Slot));
               S.Grants (Slot) := 0;
            end if;
            Write_32 (Entry_At + SUB_OFF_PID, 0);
            Write_32 (Entry_At + SUB_OFF_CURSOR, 0);
            Write_16 (Entry_At + SUB_OFF_FLAGS, 0);
            if Count > 0 then
               Write_8 (S.Base + HDR_SUBSCRIBER_COUNT, Count - 1);
            end if;
            Reply (From, K.Reply_OK);
            return 1;
         end;
      end if;
      return 0;                         --  not a stream request
   end Handle_Message;

   --  One pending request, if any (a program without a mailbox owner).
   procedure Poll_Subscription;
   procedure Poll_Subscription is
      M : aliased K.Message;
      From : constant Unsigned_64 := CuBit.Kernel_Calls.Call
        (K.Poll_Any_IPC, Unsigned_64 (To_Integer (M'Address)));
      Ignore : Interfaces.C.int;
   begin
      if From /= 0 and then From /= K.Failed then
         Ignore := Handle_Message (Interfaces.C.long (From), M'Address);
      end if;
   end Poll_Subscription;

   function Write (Stream : Unsigned_16; Data : System.Address; Length : Unsigned_32;
                   Type_Tag : Unsigned_16) return Unsigned_32
   is
      Index : constant Integer := Find (Stream);
   begin
      --  An entry's length is 16 bits, all ones being the sentinel.
      if Index < 0 or else Length = 0 or else Length >= Sentinel_Length then
         return 0;
      end if;
      if Poll_On_Write /= 0 then
         Poll_Subscription;
      end if;
      declare
         S : Stream_State renames Streams (Index);
         Data_Base : constant Unsigned_64 := S.Base + Data_Offset;
         Bytes : constant Unsigned_32 := Entry_Bytes (Length);
         Offset, Sentinel_Offset : Unsigned_32;
         Sentinel : Boolean;
         Producer : Unsigned_32;
         Count : constant Natural :=
           Natural'Min (Natural (Read_8 (S.Base + HDR_SUBSCRIBER_COUNT)), Maximum_Subscribers);
      begin
         if S.Size < Entry_Alignment or else Bytes > Unsigned_32 (S.Size) then
            return 0;
         end if;
         Place (Read_32 (S.Base + HDR_PRODUCER_IDX), S.Size, Bytes, Offset, Sentinel,
                Sentinel_Offset, Producer);
         if Sentinel then
            Write_16 (Data_Base + Unsigned_64 (Sentinel_Offset), Sentinel_Length);
         end if;
         --  Flow control: drop-oldest advances lagging subscribers.
         if Count > 0 then
            declare
               Minimum : Unsigned_32 := Producer;
               Used, Room : Unsigned_32;
            begin
               for I in 0 .. Count - 1 loop
                  if Read_32 (Subscriber (S.Base, I) + SUB_OFF_PID) /= 0 then
                     declare
                        Cursor : constant Unsigned_32 := Read_32 (Subscriber (S.Base, I) + SUB_OFF_CURSOR);
                     begin
                        if I = 0 or else Cursor < Minimum then
                           Minimum := Cursor;
                        end if;
                     end;
                  end if;
               end loop;
               Used := Unsigned_32'Min (Producer - Minimum, Unsigned_32 (S.Size));
               Room := Unsigned_32 (S.Size) - Used;
               if Room < Bytes then
                  for I in 0 .. Count - 1 loop
                     if Read_32 (Subscriber (S.Base, I) + SUB_OFF_PID) /= 0 then
                        Write_32 (Subscriber (S.Base, I) + SUB_OFF_CURSOR,
                                  Advanced (Producer,
                                            Read_32 (Subscriber (S.Base, I) + SUB_OFF_CURSOR),
                                            Bytes - Room, S.Size));
                     end if;
                  end loop;
               end if;
            end;
         end if;
         Write_16 (Data_Base + Unsigned_64 (Offset), Unsigned_16 (Length));
         Write_16 (Data_Base + Unsigned_64 (Offset) + 2, Type_Tag);
         declare
            Source : constant Storage_Array (1 .. Storage_Offset (Length)) with Import, Address => Data;
            Target : Storage_Array (1 .. Storage_Offset (Length)) with Import,
              Address => To_Address (Data_Base + Unsigned_64 (Offset) + Entry_Header_Bytes);
         begin
            Target := Source;
         end;
         Write_32 (S.Base + HDR_PRODUCER_IDX, Producer + Bytes);
      end;
      return Length;
   end Write;

end CuBit.Libc_Streams;
