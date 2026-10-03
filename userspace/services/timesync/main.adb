pragma Ada_2022;
with CuBit.Log;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Channels;
with CuBit.Config;
with CuBit.Clocks;
with CuBit.Clock_Control; use CuBit.Clock_Control;
with CuBit.Clock_Control.Client;
with SNTP;
with Server_List;
with Nonces;

--  SNTP time synchronization. Queries the servers named by the time.servers
--  setting over peer-bound UDP channels, requires agreement among a
--  majority of at least two, and submits the combined estimate to
--  clock.svc through the separately authorized clock-control endpoint.
--  clock.svc applies its own floor, step bound and authentication policy.
procedure Main is
   use ASCII;

   Network_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Ntp;
   OK_Label : constant Unsigned_32 := 16#F000#;
   Open_Label : constant Unsigned_32 := 16#0420#;
   package Channels renames CuBit.Net_Channels;

   Default_Poll_Seconds : constant := 1_024;
   Minimum_Poll_Seconds : constant := 64;
   Maximum_Poll_Seconds : constant := 86_400;
   First_Retry_Seconds : constant := 16;
   Reply_Timeout_MS : constant := 2_000;
   Buffer_Size : constant := 4_096;

   Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, Buffer_Size);
   Buffer_Address : constant System.Address :=
     To_Address (Integer_Address (Allocation));
   Buffer : SNTP.Byte_Array (1 .. Buffer_Size)
     with Import, Address => Buffer_Address;
   --  The channel lent to netstack: a header page and two small rings of
   --  datagram records (one request out, one reply in).
   Ring_Size : constant := 4_096;
   Stream_Bytes : constant := Channels.Layout.Header_Bytes + 2 * Ring_Size;
   Stream_Allocation : constant Unsigned_64 := syscall (SYSCALL_SBRK, Stream_Bytes);
   Arena : Channels.Arena;
   Stream : Channels.Stream;
   Wait_Token : Unsigned_64 := 16#7153_0000_0000_0000#;
   Granted, Hardware_Nonces : Boolean;

   --  Servers that answered Kiss-o'-Death are not asked again until the
   --  setting changes or the service restarts (RFC 4330 section 8).
   Kissed : array (1 .. SNTP.Maximum_Servers) of Boolean := [others => False];
   Last_Setting : String (1 .. 256) := [others => ' '];
   Last_Length : Natural := 0;
   Failures : Natural := 0;

   procedure Setting
     (Key : String; Data : out String; Length : out Natural; Found : out Boolean)
   is
      Address : System.Address;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      Data := [others => ' '];
      Length := 0;
      CuBit.Config.get (Key, Address, Length, Status);
      Found := Status = CuBit.Config.OK and then Length in 1 .. Data'Length;
      if Found then
         declare
            Source : String (1 .. Length) with Import, Address => Address;
         begin
            Data (Data'First .. Data'First + Length - 1) := Source;
         end;
      else
         Length := 0;
      end if;
   end Setting;

   function Poll_Seconds return Unsigned_64 is
      Data : String (1 .. 16);
      Length : Natural;
      Found : Boolean;
      Value : Unsigned_64 := 0;
   begin
      Setting ("time.poll-seconds", Data, Length, Found);
      if not Found then
         return Default_Poll_Seconds;
      end if;
      for C of Data (1 .. Length) loop
         if C not in '0' .. '9' or else Value > Maximum_Poll_Seconds then
            return Default_Poll_Seconds;
         end if;
         Value := Value * 10 + Character'Pos (C) - Character'Pos ('0');
      end loop;
      return (if Value in Minimum_Poll_Seconds .. Maximum_Poll_Seconds
              then Value else Default_Poll_Seconds);
   end Poll_Seconds;

   procedure Channel_Call (Label : Unsigned_32; Length : Unsigned_8;
                           W0, W1, W2, W3 : Unsigned_64; Reply : out Message) is
   begin
      Reply := NULL_MESSAGE;
      Reply.tag.label := Label;
      Reply.tag.length := Length;
      Reply.words := [W0, W1, W2, W3];
      Reply.tag := capCall (Network_Slot, Reply);
   end Channel_Call;

   procedure Query
     (Item : Server_List.Server; Result : out SNTP.Estimate;
      Status : out SNTP.Rejection; Reached : out Boolean)
   is
      Scheme : String (1 .. Server_List.Maximum_Scheme_Length);
      Scheme_Length : Natural;
      Reply : Message;
      Nonce : constant Unsigned_64 := Nonces.Next;
      Sent, Received : Unsigned_64;
      Delivered, Ready, Truncated, Found : Boolean;
      Got : Natural;
   begin
      Result := (others => <>);
      Status := SNTP.Wrong_Length;
      Reached := False;
      Server_List.Scheme (Item, Scheme, Scheme_Length);
      Channels.Reset (Stream);
      declare
         Name : String (1 .. Scheme_Length) with Import,
           Address => Stream.Base + Storage_Offset (Channels.Layout.Target_At);
      begin
         Name := Scheme (1 .. Scheme_Length);
      end;
      Channel_Call (Open_Label, Unsigned_8 (Scheme_Length), Stream.Arena,
                    Unsigned_64 (Stream.Buffer), 0, 0, Reply);
      if Reply.tag.label /= OK_Label then
         CuBit.Log.Warning ("timesync: cannot open " & Scheme (1 .. Scheme_Length));
         return;
      end if;
      Stream.Handle := Reply.words (0);
      Buffer (1 .. SNTP.Packet_Length) := SNTP.Request (Nonce);
      Sent := syscall (SYSCALL_GETTIME);
      Channels.Send_Datagram (Stream, Buffer_Address, SNTP.Packet_Length, Delivered);
      if Delivered then
         --  Read more than a bare packet so extension fields are detected
         --  (and rejected) rather than silently truncated.
         Wait_Token := Wait_Token + 1;
         Channels.Await (Stream, Network_Slot, Channels.Layout.Want_Readable,
                         Sent + Reply_Timeout_MS, Wait_Token, Ready);
         Channels.Receive_Datagram (Stream, Buffer_Address, 512, Got, Truncated, Found);
         Received := syscall (SYSCALL_GETTIME);
         if Ready and then Found and then not Truncated then
            Reached := True;
            SNTP.Evaluate (Buffer (1 .. Got), Nonce, Sent, Received, Result, Status);
         end if;
      end if;
      Channels.Close (Stream, Network_Slot);
      if not Reached then
         CuBit.Log.Warning ("timesync: no reply from " & Scheme (1 .. Scheme_Length));
      end if;
   end Query;

   --  One poll of every configured server. True when the clock accepted
   --  the combined estimate.
   function Poll return Boolean is
      Text : String (1 .. 256);
      Length : Natural;
      Found, Parsed : Boolean;
      Servers : Server_List.Server_Array;
      Count : SNTP.Server_Count;
      Estimates : SNTP.Estimate_Array := [others => <>];
      Usable : SNTP.Server_Count := 0;
      Combined : SNTP.Estimate;
      Agreeing : SNTP.Server_Count;
      Agreed, Submitted : Boolean;
      Result : Outcome;
      Quality : CuBit.Clocks.Time_Quality;
      Before, After : CuBit.Clocks.Snapshot;
      Before_OK, After_OK : Boolean;
      use type SNTP.Rejection;
   begin
      Setting ("time.servers", Text, Length, Found);
      if not Found then
         CuBit.Log.Warning ("timesync: time.servers not set; not synchronizing");
         return False;
      end if;
      if Length /= Last_Length or else Text (1 .. Length) /= Last_Setting (1 .. Length) then
         Kissed := [others => False];
         Last_Setting (1 .. Length) := Text (1 .. Length);
         Last_Length := Length;
      end if;
      Server_List.Parse (Text (1 .. Length), Servers, Count, Parsed);
      if not Parsed then
         CuBit.Log.Warning ("timesync: time.servers is malformed; not synchronizing");
         return False;
      end if;
      for I in 1 .. Count loop
         if not Kissed (I) then
            declare
               One : SNTP.Estimate;
               Status : SNTP.Rejection;
               Reached : Boolean;
            begin
               Query (Servers (I), One, Status, Reached);
               if Reached and then Status = SNTP.Accepted then
                  Usable := Usable + 1;
                  Estimates (Usable) := One;
               elsif Reached then
                  CuBit.Log.Info ("timesync: server " & Servers (I).Host (1 .. Servers (I).Length) &
                              " reply rejected: " & SNTP.Name (Status));
                  if Status = SNTP.Kiss_Of_Death then
                     Kissed (I) := True;
                  end if;
               end if;
            end;
         end if;
      end loop;
      SNTP.Combine (Estimates, Usable, Combined, Agreeing, Agreed);
      if not Agreed then
         CuBit.Log.Warning ("timesync: no majority agreement among" & Usable'Image &
                     " usable replies");
         return False;
      end if;
      CuBit.Clocks.Read (Before, Before_OK);
      CuBit.Clock_Control.Client.Submit
        ((UTC_MS => Combined.UTC_MS,
          Observed_Monotonic_MS => Combined.Observed_MS,
          Uncertainty_MS => Combined.Uncertainty_MS,
          Sources => Agreeing,
          Authenticated => False),
         Result, Quality, Submitted);
      if not Submitted then
         CuBit.Log.Warning ("timesync: clock refused the adjustment request");
         return False;
      end if;
      CuBit.Log.Info ("timesync: " & Agreeing'Image & " of" & Count'Image &
                  " servers agree, uncertainty" & Combined.Uncertainty_MS'Image &
                  " ms; clock: " & Name (Result));
      if Result = Stepped then
         CuBit.Clocks.Read (After, After_OK);
         if Before_OK and then After_OK then
            CuBit.Log.Info ("timesync: clock stepped from UTC" & Before.UTC_Seconds'Image &
                        " to" & After.UTC_Seconds'Image & " quality " &
                        CuBit.Clocks.Name (After.Quality));
         end if;
      end if;
      return Result = Stepped;
   end Poll;

   Ignore : Unsigned_64;
   Wait_Seconds : Unsigned_64;
begin
   --  timesync sleeps between polls: no event loop, so records go out as written.
   CuBit.Log.Set_Delivery (CuBit.Log.Immediate);
   CuBit.Log.Info ("timesync: started");
   if Allocation = Unsigned_64'Last then
      CuBit.Log.Warning ("timesync: buffer allocation failed");
      return;
   end if;
   if Stream_Allocation = Unsigned_64'Last then
      CuBit.Log.Warning ("timesync: channel allocation failed");
      return;
   end if;
   Channels.Create_Arena
     (Arena, Network_Slot, To_Address (Integer_Address (Stream_Allocation)),
      Ring_Size, Ring_Size, 1, Granted);
   if not Granted then
      CuBit.Log.Warning ("timesync: no network authority; exiting");
      return;
   end if;
   Channels.Prepare (Stream, Arena, 0, 0, Network_Slot);
   Nonces.Initialize (Hardware_Nonces);
   if not Hardware_Nonces then
      CuBit.Log.Warning ("timesync: RDRAND unavailable; using weaker TSC-derived nonces");
   end if;
   CuBit.Log.Info ("timesync: network ready; first poll in 3 s");
   --  Give netmgr a moment to configure the interface before the first poll.
   Ignore := syscall (SYSCALL_SLEEP, 3_000);
   loop
      if Poll then
         Failures := 0;
         --  Spread polls from many machines: up to 63 s of jitter.
         Wait_Seconds := Poll_Seconds + (Nonces.Next and 63);
      else
         Failures := Natural'Min (Failures + 1, 16);
         Wait_Seconds := Unsigned_64'Min
           (Poll_Seconds, First_Retry_Seconds * 2 ** Natural'Min (Failures - 1, 10));
      end if;
      Ignore := syscall (SYSCALL_SLEEP, Wait_Seconds * 1_000);
   end loop;
end Main;
