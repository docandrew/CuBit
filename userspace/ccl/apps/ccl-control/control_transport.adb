with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Channels;

package body Control_Transport is
   package Channels renames CuBit.Net_Channels;
   Ring_Size : constant := 8192;
   Buffer_Bytes : constant := Channels.Layout.Header_Bytes + 2 * Ring_Size;
   Stream : Channels.Stream;       --  the connection: arena buffer 1, wait bit 1
   Listener : Channels.Stream;     --  the listener: arena buffer 0, wait bit 0
   Arena : Channels.Arena;
   Granted, Connected, Listening, Offered : Boolean := False;
   Wait_Token : Unsigned_64 := 16#CC10_0000_0000_0000#;
   function Next_Wait return Unsigned_64 is
   begin
      Wait_Token := Wait_Token + 1;
      return Wait_Token;
   end Next_Wait;
   procedure Listen (Network_Process : out Unsigned_64; Success : out Boolean) is
      Raw : Unsigned_64;
      Reply : Message;
      Submitted, Answered : Boolean;
      Token : constant Unsigned_64 := Next_Wait;
   begin
      Success := False;
      Network_Process := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
      if Network_Process = 0 or Network_Process = Unsigned_64'Last then return; end if;
      Raw := syscall (SYSCALL_SBRK, 2 * Buffer_Bytes);
      if Raw = Unsigned_64'Last then return; end if;
      Channels.Create_Arena
        (Arena, CAP_SLOT_NET, To_Address (Integer_Address (Raw)), Ring_Size, Ring_Size, 2,
         Granted);
      if not Granted then return; end if;
      Channels.Prepare (Listener, Arena, 0, 0, CAP_SLOT_NET);
      Channels.Submit_Open
        (Listener, CAP_SLOT_NET, "@net:tcp-listen:10.0.2.15:8080", Token, Submitted);
      if not Submitted then return; end if;
      Channels.Wait_For (Token, Reply, Answered);
      Listening := Answered and then Channels.Opened (Listener, Reply);
      Success := Listening;
   end Listen;
   --  One connection at a time: buffer 1 is offered for it, and stays
   --  offered across calls until a connection takes it.
   procedure Accept_Connection (Deadline : Unsigned_64; Success : out Boolean) is
      Item : Channels.Arrival;
      Found, Ready : Boolean;
   begin
      Success := False;
      if not Listening or Connected then return; end if;
      if not Offered then
         Channels.Prepare (Stream, Arena, 1, 1, CAP_SLOT_NET);
         Channels.Offer (Listener, Arena, 1, Offered);
         if not Offered then return; end if;
      end if;
      loop
         Channels.Take_Arrival (Listener, Item, Found);
         if Found then
            Stream.Handle := Item.Channel;
            Offered := False;
            Connected := True;
            Success := True;
            return;
         end if;
         Channels.Await
           (Listener, CAP_SLOT_NET, Channels.Layout.Want_Readable, Deadline, Next_Wait, Ready);
         if not Ready or else Channels.Failed (Listener) then return; end if;
      end loop;
   end Accept_Connection;
   procedure Read_Some
     (Data : out String; Count : out Natural; Deadline : Unsigned_64; Success : out Boolean)
   is
      Ready : Boolean;
   begin
      Data := [others => Character'Val (0)]; Count := 0; Success := False;
      if not Connected or Data'Length = 0 then return; end if;
      Channels.Await
        (Stream, CAP_SLOT_NET, Channels.Layout.Want_Readable, Deadline, Next_Wait, Ready);
      Channels.Read (Stream, Data'Address, Data'Length, Count);
      Success := Ready and then Count > 0;
   end Read_Some;
   procedure Write_All (Data : String; Success : out Boolean) is
      Offset : Natural := 0;
      Put : Natural;
      Ready : Boolean;
   begin
      Success := False;
      if not Connected then return; end if;
      while Offset < Data'Length loop
         Channels.Write
           (Stream, Data'Address + Storage_Offset (Offset), Data'Length - Offset, Put);
         Offset := Offset + Put;
         if Put = 0 then
            if Channels.Failed (Stream) then return; end if;
            Channels.Await
              (Stream, CAP_SLOT_NET, Channels.Layout.Want_Writable,
               syscall (SYSCALL_GETTIME) + 30_000, Next_Wait, Ready);
            if not Ready then return; end if;
         end if;
      end loop;
      Success := True;
   end Write_All;
   procedure Close_Connection is
   begin
      if Connected then
         Channels.Close (Stream, CAP_SLOT_NET); Connected := False;
      end if;
   end Close_Connection;
   procedure Close is
      Ignore : Boolean;
   begin
      Close_Connection;
      if Listening then
         --  An outstanding offer goes with the listener.
         Channels.Close (Listener, CAP_SLOT_NET); Listening := False; Offered := False;
      end if;
      if Granted then Channels.Release_Arena (Arena, CAP_SLOT_NET, Ignore); Granted := False; end if;
   end Close;
end Control_Transport;
