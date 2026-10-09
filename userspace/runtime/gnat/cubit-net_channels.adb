------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Datagram_Rings;

package body CuBit.Net_Channels is

   use type System.Address;

   OP_NET_OPEN   : constant Unsigned_32 := 16#0420#;
   OP_NET_SHUT   : constant Unsigned_32 := 16#0423#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   Page_Bytes    : constant := 4_096;

   procedure Full_Fence with Inline;
   procedure Compiler_Fence with Inline;
   function Word (S : Stream; Offset : Natural) return Unsigned_32;
   procedure Set_Word (S : Stream; Offset : Natural; Value : Unsigned_32);
   procedure Kick (S : Stream; Slot : CapabilitySlot);
   function Accept_Receive (S : in out Stream) return Boolean;
   function Accept_Send (S : in out Stream) return Boolean;

   procedure Full_Fence is
   begin
      System.Machine_Code.Asm
        ("mfence", Clobber => "memory", Volatile => True);
   end Full_Fence;

   procedure Compiler_Fence is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Compiler_Fence;

   function Word (S : Stream; Offset : Natural) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile,
        Address => S.Base + Storage_Offset (Offset);
   begin
      return Value;
   end Word;

   procedure Set_Word (S : Stream; Offset : Natural; Value : Unsigned_32) is
      Target : Unsigned_32 with Import, Volatile,
        Address => S.Base + Storage_Offset (Offset);
   begin
      Target := Value;
   end Set_Word;

   function Send_Ring (S : Stream) return System.Address is
     (S.Base + Storage_Offset (Layout.Header_Bytes));

   function Receive_Ring (S : Stream) return System.Address is
     (Send_Ring (S) + Storage_Offset (S.Tx.Size));

   procedure Kick (S : Stream; Slot : CapabilitySlot) is
      Msg : Message := NULL_MESSAGE;
      Ignore : Boolean;
   begin
      Msg.tag := (label => Layout.OP_NET_KICK, length => 2, flags => 0,
                  reserved => 0);
      Msg.words (0) := Shift_Left (Unsigned_64'(1), S.Bit);
      Ignore := capSubmit (Slot, Msg, NO_COMPLETION_TOKEN);
   end Kick;

   --  Kicks go to the endpoint the channel was opened through.
   Kick_Slot : array (Wait_Bit) of CapabilitySlot := [others => 0];

   procedure Reset (S : in out Stream) is
      Header : array (0 .. Layout.Header_Bytes / 4 - 1) of Unsigned_32
        with Import, Volatile, Address => S.Base;
   begin
      for I in Header'Range loop
         Header (I) := 0;
      end loop;
      Set_Word (S, Layout.Tx_Size_At, Unsigned_32 (S.Tx.Size));
      Set_Word (S, Layout.Rx_Size_At, Unsigned_32 (S.Rx.Size));
      Set_Word (S, Layout.Wait_Bit_At, Unsigned_32 (S.Bit));
      S.Tx := Rings.New_Producer (S.Tx.Size);
      S.Rx := Rings.New_Consumer (S.Rx.Size);
      S.Handle := 0;
      S.Broken := False;
   end Reset;

   procedure Create_Arena
     (A       : out Arena;
      Slot    : CapabilitySlot;
      Memory  : System.Address;
      Tx_Size : Rings.Ring_Size;
      Rx_Size : Rings.Ring_Size;
      Count   : Positive;
      OK      : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Revoked : Boolean;
   begin
      A := (Base => Memory, Grant => <>, Handle => 0, Tx_Size => Tx_Size,
            Rx_Size => Rx_Size, Count => 0);
      CuBit.Memory_Grants.Create_Via_Capability
        (Slot, Memory, Count * Buffer_Bytes (Tx_Size, Rx_Size) / Page_Bytes,
         True, A.Grant, OK);
      if not OK then
         return;
      end if;
      Msg.tag := (label => Layout.OP_NET_ARENA, length => 4, flags => 0,
                  reserved => 0);
      Msg.words :=
        [A.Grant.slot, A.Grant.generation,
         Unsigned_64 (Tx_Size) or Shift_Left (Unsigned_64 (Rx_Size), 32),
         Unsigned_64 (Count)];
      if capCall (Slot, Msg, CuBit.Messages.Wait_Forever).label /= REPLY_OK then
         CuBit.Memory_Grants.Revoke (A.Grant, Revoked);
         OK := False;
         return;
      end if;
      A.Handle := Msg.words (0);
      A.Count := Count;
   end Create_Arena;

   procedure Release_Arena
     (A : in out Arena; Slot : CapabilitySlot; OK : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => Layout.OP_NET_ARENA_RELEASE, length => 1,
                  flags => 0, reserved => 0);
      Msg.words (0) := A.Handle;
      OK := capCall (Slot, Msg, CuBit.Messages.Wait_Forever).label = REPLY_OK;
      if OK then
         CuBit.Memory_Grants.Revoke (A.Grant, OK);
         A.Handle := 0;
         A.Count := 0;
      end if;
   end Release_Arena;

   procedure Prepare
     (S      : in out Stream;
      A      : Arena;
      Buffer : Natural;
      Bit    : Wait_Bit;
      Slot   : CapabilitySlot)
   is
   begin
      S.Base := A.Base +
        Storage_Offset (Buffer * Buffer_Bytes (A.Tx_Size, A.Rx_Size));
      S.Arena := A.Handle;
      S.Buffer := Buffer;
      S.Tx := Rings.New_Producer (A.Tx_Size);
      S.Rx := Rings.New_Consumer (A.Rx_Size);
      S.Bit := Bit;
      Reset (S);
      Kick_Slot (Bit) := Slot;
   end Prepare;

   procedure Submit_Open
     (S : in out Stream; Slot : CapabilitySlot; Target : String;
      Token : Unsigned_64; Submitted : out Boolean)
   is
      Text : String (1 .. Target'Length) with Import,
        Address => S.Base + Storage_Offset (Layout.Target_At);
      Msg : Message := NULL_MESSAGE;
   begin
      Text := Target;
      Msg.tag := (label => OP_NET_OPEN, length => Unsigned_8 (Target'Length),
                  flags => 0, reserved => 0);
      Msg.words := [S.Arena, Unsigned_64 (S.Buffer), 0, 0];
      Kick_Slot (S.Bit) := Slot;
      Submitted := capSubmit (Slot, Msg, Token);
   end Submit_Open;

   function Opened (S : in out Stream; Reply : Message) return Boolean is
   begin
      if Reply.tag.label /= REPLY_OK then
         return False;
      end if;
      S.Handle := Reply.words (0);
      return True;
   end Opened;

   function Status (S : Stream) return Unsigned_32 is
     (if S.Broken then Layout.Status_Protocol_Error
      else Word (S, Layout.Status_At));

   --  Take netstack's produced index; False (and Broken) if it broke the
   --  ring rules.
   function Accept_Receive (S : in out Stream) return Boolean is
      OK : Boolean;
   begin
      if S.Broken then
         return False;
      end if;
      Rings.Accept_Produced
        (S.Rx, Rings.Index (Word (S, Layout.Rx_Produced_At)), OK);
      Compiler_Fence;
      S.Broken := not OK;
      return OK;
   end Accept_Receive;

   function Accept_Send (S : in out Stream) return Boolean is
      OK : Boolean;
   begin
      if S.Broken then
         return False;
      end if;
      Rings.Accept_Consumed
        (S.Tx, Rings.Index (Word (S, Layout.Tx_Consumed_At)), OK);
      S.Broken := not OK;
      return OK;
   end Accept_Send;

   procedure Readable
     (S : in out Stream; First : out System.Address; Length : out Natural)
   is
      At_1, L1, L2 : Natural;
   begin
      First := Receive_Ring (S);
      Length := 0;
      if Accept_Receive (S) then
         Rings.Data_Slices (S.Rx, At_1, L1, L2);
         First := Receive_Ring (S) + Storage_Offset (At_1);
         Length := L1;
      end if;
   end Readable;

   procedure Consume (S : in out Stream; Count : Natural) is
   begin
      if Count = 0 or else Count > S.Rx.Available then
         return;
      end if;
      Rings.Consume (S.Rx, Count);
      Compiler_Fence;
      Set_Word (S, Layout.Rx_Consumed_At, Unsigned_32 (S.Rx.Consumed));
      Full_Fence;
      if (Word (S, Layout.Kick_Wanted_At) and Layout.Kick_On_Receive) /= 0
      then
         Kick (S, Kick_Slot (S.Bit));
      end if;
   end Consume;

   procedure Read
     (S : in out Stream; Into : System.Address; Length : Natural;
      Got : out Natural)
   is
      At_1, L1, L2, N1, N2 : Natural;
   begin
      Got := 0;
      if not Accept_Receive (S) then
         return;
      end if;
      Rings.Data_Slices (S.Rx, At_1, L1, L2);
      Got := Natural'Min (Length, L1 + L2);
      N1 := Natural'Min (Got, L1);
      N2 := Got - N1;
      if N1 > 0 then
         declare
            Source : Rings.Bytes (1 .. N1) with Import,
              Address => Receive_Ring (S) + Storage_Offset (At_1);
            Target : Rings.Bytes (1 .. N1) with Import, Address => Into;
         begin
            Target := Source;
         end;
      end if;
      if N2 > 0 then
         declare
            Source : Rings.Bytes (1 .. N2) with Import,
              Address => Receive_Ring (S);
            Target : Rings.Bytes (1 .. N2) with Import,
              Address => Into + Storage_Offset (N1);
         begin
            Target := Source;
         end;
      end if;
      Consume (S, Got);
   end Read;

   procedure Writable
     (S : in out Stream; First : out System.Address; Length : out Natural)
   is
      At_1, L1, L2 : Natural;
   begin
      First := Send_Ring (S);
      Length := 0;
      if Accept_Send (S) then
         Rings.Free_Slices (S.Tx, At_1, L1, L2);
         First := Send_Ring (S) + Storage_Offset (At_1);
         Length := L1;
      end if;
   end Writable;

   procedure Commit (S : in out Stream; Count : Natural) is
   begin
      if Count = 0 or else Count > Rings.Space (S.Tx) then
         return;
      end if;
      Rings.Commit (S.Tx, Count);
      Compiler_Fence;
      Set_Word (S, Layout.Tx_Produced_At, Unsigned_32 (S.Tx.Produced));
      Full_Fence;
      if (Word (S, Layout.Kick_Wanted_At) and Layout.Kick_On_Send) /= 0 then
         Kick (S, Kick_Slot (S.Bit));
      end if;
   end Commit;

   procedure Write
     (S : in out Stream; From : System.Address; Length : Natural;
      Put : out Natural)
   is
      At_1, L1, L2, N1, N2 : Natural;
   begin
      Put := 0;
      if not Accept_Send (S) then
         return;
      end if;
      Rings.Free_Slices (S.Tx, At_1, L1, L2);
      Put := Natural'Min (Length, L1 + L2);
      N1 := Natural'Min (Put, L1);
      N2 := Put - N1;
      if N1 > 0 then
         declare
            Source : Rings.Bytes (1 .. N1) with Import, Address => From;
            Target : Rings.Bytes (1 .. N1) with Import,
              Address => Send_Ring (S) + Storage_Offset (At_1);
         begin
            Target := Source;
         end;
      end if;
      if N2 > 0 then
         declare
            Source : Rings.Bytes (1 .. N2) with Import,
              Address => From + Storage_Offset (N1);
            Target : Rings.Bytes (1 .. N2) with Import,
              Address => Send_Ring (S);
         begin
            Target := Source;
         end;
      end if;
      Commit (S, Put);
   end Write;

   procedure Send_Datagram
     (S : in out Stream; From : System.Address; Length : Natural;
      Sent : out Boolean)
   is
      use type Datagram_Rings.Put_Result;
      Ring : Rings.Bytes (0 .. S.Tx.Size - 1)
        with Import, Address => Send_Ring (S);
      Data : Rings.Bytes (1 .. Length) with Import, Address => From;
      Result : Datagram_Rings.Put_Result;
   begin
      Sent := False;
      if Length > Layout.Datagram_Maximum or else not Accept_Send (S) then
         return;
      end if;
      Datagram_Rings.Put (S.Tx, Ring, Data, Result);
      if Result = Datagram_Rings.Put then
         Compiler_Fence;
         Set_Word (S, Layout.Tx_Produced_At, Unsigned_32 (S.Tx.Produced));
         Full_Fence;
         if (Word (S, Layout.Kick_Wanted_At) and Layout.Kick_On_Send) /= 0
         then
            Kick (S, Kick_Slot (S.Bit));
         end if;
         Sent := True;
      end if;
   end Send_Datagram;

   procedure Receive_Datagram
     (S : in out Stream; Into : System.Address; Length : Natural;
      Got : out Natural; Truncated : out Boolean; Found : out Boolean)
   is
      use type Datagram_Rings.Take_Result;
      Ring : Rings.Bytes (0 .. S.Rx.Size - 1)
        with Import, Address => Receive_Ring (S);
      Data : Rings.Bytes (1 .. Length) with Import, Address => Into;
      Result : Datagram_Rings.Take_Result;
   begin
      Got := 0;
      Truncated := False;
      Found := False;
      if not Accept_Receive (S) then
         return;
      end if;
      Datagram_Rings.Take (S.Rx, Ring, Data, Got, Truncated, Result);
      if Result = Datagram_Rings.Malformed then
         S.Broken := True;
      elsif Result = Datagram_Rings.Taken then
         Compiler_Fence;
         Set_Word (S, Layout.Rx_Consumed_At, Unsigned_32 (S.Rx.Consumed));
         Found := True;
      end if;
   end Receive_Datagram;

   procedure Offer
     (Listener : in out Stream; A : Arena; Buffer : Natural;
      Offered  : out Boolean)
   is
      Item : Rings.Bytes (0 .. Layout.Offer_Bytes - 1);
   begin
      for K in 0 .. 7 loop
         Item (Layout.Offer_Arena_At + K) :=
           Unsigned_8 (Shift_Right (A.Handle, 8 * K) and 16#FF#);
      end loop;
      for K in 0 .. 3 loop
         Item (Layout.Offer_Buffer_At + K) :=
           Unsigned_8 (Shift_Right (Unsigned_64 (Buffer), 8 * K) and 16#FF#);
      end loop;
      Send_Datagram (Listener, Item'Address, Item'Length, Offered);
   end Offer;

   procedure Take_Arrival
     (Listener : in out Stream; Item : out Arrival; Found : out Boolean)
   is
      Data : Rings.Bytes (0 .. Layout.Arrival_Bytes - 1) := [others => 0];
      Got : Natural;
      Truncated : Boolean;
      function Field (Offset : Natural; Bytes : Positive) return Unsigned_64;
      function Field (Offset : Natural; Bytes : Positive) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         for K in reverse 0 .. Bytes - 1 loop
            V := Shift_Left (V, 8) or Unsigned_64 (Data (Offset + K));
         end loop;
         return V;
      end Field;
   begin
      Item := (others => <>);
      Receive_Datagram (Listener, Data'Address, Data'Length, Got, Truncated,
                        Found);
      Found := Found and then Got = Layout.Arrival_Bytes and then
        not Truncated;
      if Found then
         Item :=
           (Channel => Field (Layout.Arrival_Channel_At, 8),
            Arena   => Field (Layout.Arrival_Arena_At, 8),
            Buffer  => Natural (Field (Layout.Arrival_Buffer_At, 4)),
            Address => [for K in CuBit.Net_Address.Byte_Index =>
                          Data (Layout.Arrival_Address_At + K)],
            Port    => Unsigned_16 (Field (Layout.Arrival_Port_At, 2)));
      end if;
   end Take_Arrival;

   procedure Want (S : Stream; Flags : Unsigned_32) is
   begin
      if Word (S, Layout.Want_At) /= Flags then
         Set_Word (S, Layout.Want_At, Flags);
      end if;
      Full_Fence;
   end Want;

   procedure Shut_Write (S : in out Stream; Slot : CapabilitySlot) is
   begin
      Set_Word (S, Layout.Shut_Write_At, 1);
      Full_Fence;
      Kick (S, Slot);
   end Shut_Write;

   procedure Close (S : in out Stream; Slot : CapabilitySlot) is
      Msg : Message := NULL_MESSAGE;
      Ignore : MessageTag;
   begin
      if S.Handle /= 0 then
         Msg.tag := (label => OP_NET_SHUT, length => 1, flags => 0,
                     reserved => 0);
         Msg.words (0) := S.Handle;
         Ignore := capCall (Slot, Msg, CuBit.Messages.Wait_Forever);
         S.Handle := 0;
      end if;
   end Close;

   procedure Submit_Wait
     (Slot : CapabilitySlot; Kicks, Interest : Unsigned_64;
      Deadline : Unsigned_64; Token : Unsigned_64; Submitted : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => Layout.OP_NET_WAIT, length => 3, flags => 0,
                  reserved => 0);
      Msg.words := [Kicks, Deadline, Interest, 0];
      Submitted := capSubmit (Slot, Msg, Token);
   end Submit_Wait;

   function Has_Input (S : in out Stream) return Boolean is
     (Accept_Receive (S) and then
      (S.Rx.Available > 0 or else Final (S)));

   function Has_Room (S : in out Stream) return Boolean is
     (S.Broken or else Failed (S) or else
      (Accept_Send (S) and then Rings.Space (S.Tx) > 0));

   procedure Await
     (S : in out Stream; Slot : CapabilitySlot; Flags : Unsigned_32;
      Deadline : Unsigned_64; Token : Unsigned_64; Ready : out Boolean)
   is
      function Now_Ready return Boolean is
        (S.Broken or else
         ((Flags and Layout.Want_Readable) /= 0 and then Has_Input (S))
         or else
         ((Flags and Layout.Want_Writable) /= 0 and then Has_Room (S)));
      Submitted, OK : Boolean;
      Reply : Message;
   begin
      loop
         Ready := Now_Ready;
         exit when Ready or else syscall (SYSCALL_GETTIME) >= Deadline;
         Want (S, Flags);
         Ready := Now_Ready;
         exit when Ready;
         Submit_Wait (Slot, 0, Mask (S), Deadline, Token, Submitted);
         exit when not Submitted;
         Wait_For (Token, Reply, OK);
         exit when not OK;
      end loop;
   end Await;

   procedure Wait_For (Token : Unsigned_64; Reply : out Message;
                       OK : out Boolean)
   is
      Entry_Buffer : CompletionEntry;
      Count : Unsigned_64;
   begin
      loop
         Count := waitCompletion (Entry_Buffer'Address, 1, 1);
         if Count = 1 and then Entry_Buffer.token = Token then
            Reply := Entry_Buffer.msg;
            OK := Entry_Buffer.status = COMPLETION_OK;
            return;
         end if;
      end loop;
   end Wait_For;

end CuBit.Net_Channels;
