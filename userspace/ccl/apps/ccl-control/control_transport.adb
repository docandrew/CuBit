with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;

package body Control_Transport is
   Buffer_Size : constant := 8192;
   Address : System.Address;
   Grant, Channel : Unsigned_64 := 0;
   Granted, Connected : Boolean := False;
   type Network_Operation is (Open_Channel, Write_Channel, Read_Channel, Shut_Channel);
   for Network_Operation use
     (Open_Channel => 16#0420#, Write_Channel => 16#0421#,
      Read_Channel => 16#0422#, Shut_Channel => 16#0423#);
   Reply_OK : constant Unsigned_32 := 16#F000#;
   function Call (Op : Network_Operation; Request : in out Message) return Boolean is
      Tag : MessageTag;
   begin
      Request.tag.label := Unsigned_32 (Network_Operation'Enum_Rep (Op));
      Tag := capCall (CAP_SLOT_NET, Request);
      return Tag.label = Reply_OK;
   end Call;
   procedure Open (Network_Process : out Unsigned_64; Success : out Boolean) is
      Scheme : constant String := "@net:tcp:10.0.2.2:9440";
      Raw : Unsigned_64;
      Request : Message := NULL_MESSAGE;
   begin
      Success := False;
      Network_Process := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
      if Network_Process = 0 or Network_Process = Unsigned_64'Last then return; end if;
      Raw := syscall (SYSCALL_SBRK, Buffer_Size);
      if Raw = Unsigned_64'Last then return; end if;
      Address := To_Address (Integer_Address (Raw));
      createGrant (ProcessID (Network_Process), Address, 2, True, Grant, Granted);
      if not Granted then return; end if;
      declare
         Buffer : String (1 .. Buffer_Size) with Import, Address => Address;
      begin
         Buffer := [others => Character'Val (0)];
         Buffer (1 .. Scheme'Length) := Scheme;
      end;
      Request.tag.length := Unsigned_8 (Scheme'Length);
      Request.words (0) := Grant; Request.words (1) := Buffer_Size;
      Request.words (3) := syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Grant);
      Connected := Call (Open_Channel, Request);
      if Connected then Channel := Request.words (0); end if;
      Success := Connected;
   end Open;
   procedure Read_Exact (Data : out String; Success : out Boolean) is
      Offset : Natural := 0;
      Request : Message;
      Count : Natural;
      Buffer : String (1 .. Buffer_Size) with Import, Address => Address;
   begin
      Data := [others => Character'Val (0)]; Success := False;
      if not Connected then return; end if;
      while Offset < Data'Length loop
         Request := NULL_MESSAGE; Request.tag.length := 3;
         Request.words (0) := Channel;
         Request.words (2) := Unsigned_64 (Natural'Min (Buffer_Size, Data'Length - Offset));
         if not Call (Read_Channel, Request) or else Request.words (0) = 0 or else
           Request.words (0) > Unsigned_64 (Natural'Min (Buffer_Size, Data'Length - Offset))
         then return; end if;
         Count := Natural (Request.words (0));
         Data (Data'First + Offset .. Data'First + Offset + Count - 1) := Buffer (1 .. Count);
         Offset := Offset + Count;
      end loop;
      Success := True;
   end Read_Exact;
   procedure Write_All (Data : String; Success : out Boolean) is
      Offset : Natural := 0;
      Count : Natural;
      Request : Message;
      Buffer : String (1 .. Buffer_Size) with Import, Address => Address;
   begin
      Success := False;
      if not Connected then return; end if;
      while Offset < Data'Length loop
         --  Current netstack does not segment oversized application writes.
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
   procedure Close is
      Request : Message := NULL_MESSAGE;
      Ignore : Boolean;
   begin
      if Connected then
         Request.tag.length := 1; Request.words (0) := Channel;
         Ignore := Call (Shut_Channel, Request); Connected := False;
      end if;
      if Granted then revokeGrant (Grant); Granted := False; end if;
   end Close;
end Control_Transport;
