with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;

package body Control_Transport is
   Buffer_Size : constant := 8192;
   Address : System.Address;
   Grant : CuBit.Memory_Grants.Grant_Reference;
   Channel, Listener : Unsigned_64 := 0;
   Granted, Connected, Listening : Boolean := False;
   type Network_Operation is
     (Write_Channel, Read_Channel, Shut_Channel, Bind_Listener, Accept_Channel, Close_Listener);
   for Network_Operation use
     (Write_Channel => 16#0421#, Read_Channel => 16#0422#,
      Shut_Channel => 16#0423#, Bind_Listener => 16#0424#,
      Accept_Channel => 16#0425#, Close_Listener => 16#0427#);
   Reply_OK : constant Unsigned_32 := 16#F000#;
   function Call (Op : Network_Operation; Request : in out Message) return Boolean is
      Tag : MessageTag;
   begin
      Request.tag.label := Unsigned_32 (Network_Operation'Enum_Rep (Op));
      Tag := capCall (CAP_SLOT_NET, Request);
      return Tag.label = Reply_OK;
   end Call;
   procedure Listen (Network_Process : out Unsigned_64; Success : out Boolean) is
      Raw : Unsigned_64;
      Request : Message := NULL_MESSAGE;
   begin
      Success := False;
      Network_Process := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
      if Network_Process = 0 or Network_Process = Unsigned_64'Last then return; end if;
      Raw := syscall (SYSCALL_SBRK, Buffer_Size);
      if Raw = Unsigned_64'Last then return; end if;
      Address := To_Address (Integer_Address (Raw));
      CuBit.Memory_Grants.Create_Via_Capability (CAP_SLOT_NET, Address, 2, True, Grant, Granted);
      if not Granted then return; end if;
      Request.tag.length := 2;
      Request.words (0) := 16#0A00_020F#; Request.words (1) := 8080;
      Listening := Call (Bind_Listener, Request);
      if Listening then Listener := Request.words (0); end if;
      Success := Listening;
   end Listen;
   procedure Accept_Connection (Deadline : Unsigned_64; Success : out Boolean) is
      Request : Message := NULL_MESSAGE;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Wait_Ms : constant Unsigned_64 :=
        (if Deadline <= Now then 0 else Unsigned_64'Min (Deadline - Now, 30_000));
   begin
      Success := False;
      if not Listening or Connected then return; end if;
      -- Four inline IPC words: upper half of the buffer-size word carries
      -- the bounded wait when the deadline flag is present.
      Request.tag.length := 4;
      Request.tag.flags := 1;
      Request.words (0) := Listener; Request.words (1) := Grant.slot;
      Request.words (2) := Shift_Left (Wait_Ms, 32) or Buffer_Size;
      Request.words (3) := Grant.generation;
      Connected := Call (Accept_Channel, Request);
      if Connected then Channel := Request.words (0); end if;
      Success := Connected;
   end Accept_Connection;
   procedure Read_Some
     (Data : out String; Count : out Natural; Deadline : Unsigned_64; Success : out Boolean)
   is
      Request : Message := NULL_MESSAGE;
      Limit : constant Natural := Natural'Min (Buffer_Size, Data'Length);
      Buffer : String (1 .. Buffer_Size) with Import, Address => Address;
   begin
      Data := [others => Character'Val (0)]; Count := 0; Success := False;
      if not Connected or Limit = 0 then return; end if;
      Request.tag.length := 4;
      Request.words (0) := Channel; Request.words (2) := Unsigned_64 (Limit);
      Request.words (3) := Deadline;
      if not Call (Read_Channel, Request) or else Request.words (0) = 0 or else
        Request.words (0) > Unsigned_64 (Limit) then return; end if;
      Count := Natural (Request.words (0));
      Data (Data'First .. Data'First + Count - 1) := Buffer (1 .. Count);
      Success := True;
   end Read_Some;
   procedure Write_All (Data : String; Success : out Boolean) is
      Offset : Natural := 0;
      Count : Natural;
      Request : Message;
      Buffer : String (1 .. Buffer_Size) with Import, Address => Address;
   begin
      Success := False;
      if not Connected then return; end if;
      while Offset < Data'Length loop
         -- Current netstack does not segment oversized application writes.
         Count := Natural'Min (1024, Data'Length - Offset);
         Buffer (1 .. Count) := Data (Data'First + Offset .. Data'First + Offset + Count - 1);
         Request := NULL_MESSAGE; Request.tag.length := 3;
         Request.words (0) := Channel; Request.words (2) := Unsigned_64 (Count);
         if not Call (Write_Channel, Request) or else Request.words (0) /= Unsigned_64 (Count)
         then return; end if;
         Offset := Offset + Count;
      end loop;
      Success := True;
   end Write_All;
   procedure Close_Connection is
      Request : Message := NULL_MESSAGE;
      Ignore : Boolean;
   begin
      if Connected then
         Request.tag.length := 1; Request.words (0) := Channel;
         Ignore := Call (Shut_Channel, Request); Connected := False;
      end if;
   end Close_Connection;
   procedure Close is
      Request : Message := NULL_MESSAGE;
      Ignore : Boolean;
   begin
      Close_Connection;
      if Listening then
         Request.tag.length := 1; Request.words (0) := Listener;
         Ignore := Call (Close_Listener, Request); Listening := False;
      end if;
      if Granted then CuBit.Memory_Grants.Revoke (Grant, Ignore); Granted := False; end if;
   end Close;
end Control_Transport;
