with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Channels;
with CuBit.Net_Channel_Layout;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Net_Address;
with CuBit.Published_Clock;
with CCL_Native_Bindings;

package body Remote_Transport is
   package Channels renames CuBit.Net_Channels;
   package Layout renames CuBit.Net_Channel_Layout;
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   use type NL.Link_Condition;

   REPLY_OK  : constant Unsigned_32 := 16#F000#;

   Ring_Size : constant := 16_384;
   Buffer_Bytes : constant := Layout.Header_Bytes + 2 * Ring_Size;
   --  Listener I: arena buffer (and wait bit) 2 (I - 1); its connection
   --  buffer the next one.
   Arena_Buffers : constant := 2 * Maximum_Listeners;
   Page_Bytes : constant := 4_096;

   type Listener_Slot is record
      Open : Boolean := False;
      Offered : Boolean := False;
      Word : NM.IPv4_Word := NM.No_IPv4;
      Port : Unsigned_16 := 0;
      Stream : Channels.Stream;
      --  Its connection buffer's stream, laid out before it is offered.
      Connection : Channels.Stream;
   end record;
   Listeners : array (Listener_Index) of Listener_Slot;
   Connected : Boolean := False;
   Connection_Owner : Listener_Index := Listener_Index'First;
   Arena : Channels.Arena;
   Granted : Boolean := False;
   --  The connected peer, as netstack reported its arrival.
   Peer_Address : CuBit.Net_Address.Address := CuBit.Net_Address.Unspecified;

   Wait_Token : Unsigned_64 := 16#CC20_0000_0000_0000#;
   function Next_Wait return Unsigned_64 is
   begin
      Wait_Token := Wait_Token + 1;
      return Wait_Token;
   end Next_Wait;

   function Listener_Buffer (I : Listener_Index) return Natural is (2 * (I - 1));
   function Connection_Buffer (I : Listener_Index) return Natural is (2 * (I - 1) + 1);

   function Decimal (Value : Unsigned_32) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   --  a.b.c.d from the word a | b << 8 | c << 16 | d << 24.
   function Dotted (Word : NM.IPv4_Word) return String is
     (Decimal (Word and 16#FF#) & "." & Decimal (Shift_Right (Word, 8) and 16#FF#) & "." &
      Decimal (Shift_Right (Word, 16) and 16#FF#) & "." & Decimal (Shift_Right (Word, 24) and 16#FF#));

   procedure Initialize (Success : out Boolean) is
      Raw : Unsigned_64;
      Memory : Unsigned_64;
   begin
      Success := False;
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Network) then
         return;
      end if;
      Raw := syscall (SYSCALL_SBRK, Arena_Buffers * Buffer_Bytes + Page_Bytes);
      if Raw = Unsigned_64'Last then
         return;
      end if;
      Memory := (Raw + Page_Bytes - 1) and not Unsigned_64 (Page_Bytes - 1);
      Channels.Create_Arena
        (Arena, CCL_Native_Bindings.Network, To_Address (Integer_Address (Memory)), Ring_Size, Ring_Size,
         Arena_Buffers, Granted);
      Success := Granted;
   end Initialize;

   --  The configured IPv4 addresses of interfaces whose link is up.
   type Word_List is array (Listener_Index) of NM.IPv4_Word;
   procedure Configured (Words : out Word_List; Count : out Listener_Count) is
      Netstack : constant CuBit.Handles.Endpoint := CCL_Native_Bindings.Network_Stack;
      Previous : Unsigned_64 := 0;
      M : Message;
      Tag : MessageTag;
      function Request (Op : NM.Operation; W0 : Unsigned_64) return Message is
         R : Message := NULL_MESSAGE;
      begin
         R.tag := (label => NM.Label (Op), length => Unsigned_8 (NM.Request_Words (Op)),
                   protocol => 0, ipc => <>, reserved => 0);
         R.words (0) := W0;
         return R;
      end Request;
   begin
      Words := [others => NM.No_IPv4];
      Count := 0;
      if not CuBit.Handles.Valid (Netstack) then
         return;
      end if;
      loop
         M := Request (NM.Interface_Next, Previous);
         Tag := capCall (Netstack, M, CuBit.Messages.Wait_Forever);
         exit when Tag.label /= REPLY_OK;   --  REPLY_ERR: no more interfaces
         exit when not NL.Valid_Handle_Word (M.words (0)) or else M.words (0) = Previous;
         Previous := M.words (0);
         M := Request (NM.Interface_Detail, Previous);
         Tag := capCall (Netstack, M, CuBit.Messages.Wait_Forever);
         if Tag.label = REPLY_OK then
            declare
               S : constant NM.Decoded_Summary :=
                 NM.Decode_Summary ([M.words (0), M.words (1), M.words (2), M.words (3)]);
            begin
               if S.Valid and then S.Item.Condition = NL.Up
                 and then S.Item.Settings.Address /= NM.No_IPv4 and then Count < Maximum_Listeners
               then
                  Count := Count + 1;
                  Words (Count) := S.Item.Settings.Address;
               end if;
            end;
         end if;
      end loop;
   end Configured;

   procedure Close_Listener (I : Listener_Index) is
   begin
      if Connected and then Connection_Owner = I then
         Channels.Close (Listeners (I).Connection, CCL_Native_Bindings.Network);
         Connected := False;
      end if;
      if Listeners (I).Open then
         --  An outstanding offer goes with the listener.
         Channels.Close (Listeners (I).Stream, CCL_Native_Bindings.Network);
      end if;
      Listeners (I) := (others => <>);
   end Close_Listener;

   procedure Open_Listener (I : Listener_Index; Word : NM.IPv4_Word; Port : Unsigned_16; Opened : out Boolean) is
      Target : constant String := "@net:tcp-listen:" & Dotted (Word) & ":" & Decimal (Unsigned_32 (Port));
      Token : constant Unsigned_64 := Next_Wait;
      Reply : Message;
      Submitted, Answered : Boolean;
   begin
      Opened := False;
      Channels.Prepare (Listeners (I).Stream, Arena, Listener_Buffer (I), Listener_Buffer (I),
                        CCL_Native_Bindings.Network);
      Channels.Submit_Open (Listeners (I).Stream, CCL_Native_Bindings.Network, Target, Token, Submitted);
      if not Submitted then
         return;
      end if;
      Channels.Wait_For (Token, Reply, Answered);
      Opened := Answered and then Channels.Opened (Listeners (I).Stream, Reply);
      if Opened then
         Listeners (I).Open := True;
         Listeners (I).Word := Word;
         Listeners (I).Port := Port;
         debugPrint ("ccl-remote: listening on " & Dotted (Word) & ":" & Decimal (Unsigned_32 (Port)) & ASCII.LF);
      else
         debugPrint ("ccl-remote: netstack refused the listener on " & Dotted (Word) & ASCII.LF);
      end if;
   end Open_Listener;

   procedure Refresh (Port : Unsigned_16; Changed : out Boolean) is
      Words : Word_List;
      Count : Listener_Count;
      Present, Opened : Boolean;
   begin
      Changed := False;
      if not Granted then
         return;
      end if;
      Configured (Words, Count);
      --  Close the listeners whose address went away.
      for I in Listener_Index loop
         if Listeners (I).Open then
            Present := False;
            for K in 1 .. Count loop
               Present := Present or else (Words (K) = Listeners (I).Word and then Listeners (I).Port = Port);
            end loop;
            if not Present then
               Close_Listener (I);
               Changed := True;
            end if;
         end if;
      end loop;
      --  Open one for each new address.
      for K in 1 .. Count loop
         Present := False;
         for I in Listener_Index loop
            Present := Present or else (Listeners (I).Open and then Listeners (I).Word = Words (K));
         end loop;
         if not Present then
            for I in Listener_Index loop
               if not Listeners (I).Open then
                  Open_Listener (I, Words (K), Port, Opened);
                  Changed := Changed or else Opened;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end Refresh;

   function Listening return Listener_Count is
      Count : Listener_Count := 0;
   begin
      for L of Listeners loop
         if L.Open then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Listening;

   function Address (Index : Listener_Index) return String is
      Seen : Natural := 0;
   begin
      for L of Listeners loop
         if L.Open then
            Seen := Seen + 1;
            if Seen = Index then
               return Dotted (L.Word) & ":" & Decimal (Unsigned_32 (L.Port));
            end if;
         end if;
      end loop;
      return "";
   end Address;

   function Peer return String is
      IPv4_First : constant := 12;   --  ::ffff:a.b.c.d: a.b.c.d is bytes 12 .. 15
   begin
      if not CuBit.Net_Address.Is_Mapped (Peer_Address) then
         return "";
      end if;
      return Decimal (Unsigned_32 (Peer_Address (IPv4_First))) & "." &
        Decimal (Unsigned_32 (Peer_Address (IPv4_First + 1))) & "." &
        Decimal (Unsigned_32 (Peer_Address (IPv4_First + 2))) & "." &
        Decimal (Unsigned_32 (Peer_Address (IPv4_First + 3)));
   end Peer;

   procedure Accept_Connection (Deadline : Unsigned_64; Success : out Boolean) is
      Item : Channels.Arrival;
      Found, Submitted, Answered : Boolean;
      Interest : Unsigned_64;
      Reply : Message;
      Token : Unsigned_64;
   begin
      Success := False;
      if Connected then
         return;
      end if;
      loop
         Interest := 0;
         for I in Listener_Index loop
            if Listeners (I).Open then
               if not Listeners (I).Offered then
                  Channels.Prepare (Listeners (I).Connection, Arena, Connection_Buffer (I), Connection_Buffer (I),
                                    CCL_Native_Bindings.Network);
                  Channels.Offer (Listeners (I).Stream, Arena, Connection_Buffer (I), Listeners (I).Offered);
               end if;
               if Listeners (I).Offered then
                  Channels.Take_Arrival (Listeners (I).Stream, Item, Found);
                  if Found then
                     Listeners (I).Connection.Handle := Item.Channel;
                     Peer_Address := Item.Address;
                     Listeners (I).Offered := False;
                     Connection_Owner := I;
                     Connected := True;
                     Success := True;
                     return;
                  end if;
                  Channels.Want (Listeners (I).Stream, Layout.Want_Readable);
                  Interest := Interest or Channels.Mask (Listeners (I).Stream);
               end if;
            end if;
         end loop;
         exit when CuBit.Published_Clock.Milliseconds >= Deadline;
         if Interest = 0 then
            --  Nothing to wait on: sleep until Deadline (or a wake).
            Token := Next_Wait;
            Channels.Submit_Wait (CCL_Native_Bindings.Network, 0, 0, Deadline, Token, Submitted);
         else
            Token := Next_Wait;
            Channels.Submit_Wait (CCL_Native_Bindings.Network, 0, Interest, Deadline, Token, Submitted);
         end if;
         exit when not Submitted;
         Channels.Wait_For (Token, Reply, Answered);
         exit when not Answered;
      end loop;
   end Accept_Connection;

   procedure Read_Exact (Data : out String; Deadline : Unsigned_64; Success : out Boolean) is
      Got, Count : Natural := 0;
      Ready : Boolean;
   begin
      Data := [others => Character'Val (0)];
      Success := False;
      if not Connected then
         return;
      end if;
      while Got < Data'Length loop
         Channels.Read (Listeners (Connection_Owner).Connection, Data (Data'First + Got)'Address,
                        Data'Length - Got, Count);
         Got := Got + Count;
         if Count = 0 then
            if Channels.Final (Listeners (Connection_Owner).Connection) then
               return;   --  the peer finished early, or the channel failed
            end if;
            Channels.Await
              (Listeners (Connection_Owner).Connection, CCL_Native_Bindings.Network, Layout.Want_Readable, Deadline, Next_Wait, Ready);
            if not Ready then
               return;
            end if;
         end if;
      end loop;
      Success := True;
   end Read_Exact;

   procedure Write_All (Data : String; Deadline : Unsigned_64; Success : out Boolean) is
      Offset : Natural := 0;
      Put : Natural;
      Ready : Boolean;
   begin
      Success := False;
      if not Connected then
         return;
      end if;
      while Offset < Data'Length loop
         Channels.Write (Listeners (Connection_Owner).Connection, Data'Address + Storage_Offset (Offset),
                         Data'Length - Offset, Put);
         Offset := Offset + Put;
         if Put = 0 then
            if Channels.Failed (Listeners (Connection_Owner).Connection) then
               return;
            end if;
            Channels.Await
              (Listeners (Connection_Owner).Connection, CCL_Native_Bindings.Network, Layout.Want_Writable, Deadline, Next_Wait, Ready);
            if not Ready then
               return;
            end if;
         end if;
      end loop;
      Success := True;
   end Write_All;

   procedure Close_Connection is
   begin
      if Connected then
         Channels.Close (Listeners (Connection_Owner).Connection, CCL_Native_Bindings.Network);
         Connected := False;
      end if;
   end Close_Connection;

   procedure Close is
      Ignore : Boolean;
   begin
      Close_Connection;
      for I in Listener_Index loop
         Close_Listener (I);
      end loop;
      if Granted then
         Channels.Release_Arena (Arena, CCL_Native_Bindings.Network, Ignore);
         Granted := False;
      end if;
   end Close;
end Remote_Transport;
