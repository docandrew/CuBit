------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Imports;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
with CuBit.Net_Channel_Layout;
with CuBit.Net_Control_Queues;
with CuBit.Libc_Net_Addresses; use CuBit.Libc_Net_Addresses;
with CuBit.Libc_Net_Names;
with CuBit.Libc_Net_Targets;

package body CuBit.Libc_Net is

   package K renames CuBit.Kernel_ABI;
   package Rings renames CuBit.Channel_Rings;
   package Layout renames CuBit.Net_Channel_Layout;
   package NCQ renames CuBit.Net_Control_Queues;
   package Names renames CuBit.Libc_Net_Names;
   package Targets renames CuBit.Libc_Net_Targets;

   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.C.size_t;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.unsigned_long;
   use type System.Address;
   use type CuBit.Datagram_Rings.Put_Result;

   --  netstack's SHUT by message (kernel/src/ipc_labels.ads; tests/libc-ada).
   OP_NET_SHUT : constant := 16#0423#;

   Ring_Bytes : constant := 64 * 1_024;
   Buffer_Bytes : constant := Layout.Header_Bytes + 2 * Ring_Bytes;
   Arena_Buffers : constant := 16;                --  sockets per arena
   Maximum_Arenas : constant := (Layout.Maximum_Wait_Bit + 1) / Arena_Buffers;
   Offers : constant := 4;                        --  a listener's offered sockets
   Maximum_Scopes : constant := 16;
   subtype Wait_Bit is Natural range 0 .. Layout.Maximum_Wait_Bit;
   Wait_Token : constant Unsigned_64 := 16#FF#;    --  a WAIT's; an OPEN's is serial << 8 | bit
   Shut_Token : constant Unsigned_64 := 2 ** 63;   --  SHUT completions carry it
   Bit_Mask : constant Unsigned_64 := 16#FF#;
   Grant_Read_Write : constant := 1;
   Forever : constant unsigned_long := unsigned_long'Last;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));
   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));
   function Error (Value : int) return long is (-long (Value));

   function Kernel (Number : K.System_Call; A0, A1, A2, A3, A4, A5 : Unsigned_64 := 0)
     return Unsigned_64 renames CuBit.Kernel_Calls.Call;

   procedure Compiler_Barrier;
   procedure Compiler_Barrier is
   begin
      Asm ("", Clobber => "memory", Volatile => True);
   end Compiler_Barrier;

   procedure Full_Fence;
   procedure Full_Fence is
   begin
      Asm ("mfence", Clobber => "memory", Volatile => True);
   end Full_Fence;

   subtype Lock_Word is CuBit.Libc_Imports.Lock_Word;
   procedure Lock (Word : access Lock_Word) renames CuBit.Libc_Imports.Lock;
   procedure Unlock (Word : access Lock_Word) renames CuBit.Libc_Imports.Unlock;

   function Calloc (Count, Size : size_t) return System.Address
   with Import, Convention => C, External_Name => "calloc";
   procedure Free (Memory : System.Address)
   with Import, Convention => C, External_Name => "free";
   function Mmap
     (Address : System.Address; Length : size_t; Protection, Flags : int;
      Descriptor : int; Offset : long) return System.Address
   with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : size_t) return int
   with Import, Convention => C, External_Name => "munmap";

   --  The readiness futex (CuBit.Libc_Descriptors).
   procedure Readiness_Changed
   with Import, Convention => C, External_Name => "__cubit_readiness_changed";
   function Readiness_Sequence return int
   with Import, Convention => C, External_Name => "__cubit_readiness_seq";
   procedure Readiness_Wait (Sequence : int; Deadline : unsigned_long)
   with Import, Convention => C, External_Name => "__cubit_readiness_wait";

   --  musl's own: a numeric address (src/network/lookup_ipliteral.c).
   function Lookup_IP_Literal (Result, Name : System.Address; Family : int) return int
   with Import, Convention => C, External_Name => "__lookup_ipliteral";

   function Map_Pages (Bytes : size_t) return Unsigned_64;
   function Map_Pages (Bytes : size_t) return Unsigned_64 is
      Area : constant System.Address :=
        Mmap (System.Null_Address, Bytes, int (PROT_READ + PROT_WRITE),
              int (MAP_PRIVATE + MAP_ANONYMOUS), -1, 0);
   begin
      return (if Area = CuBit.Libc_Imports.MAP_FAILED then 0 else Value_Of (Area));
   end Map_Pages;

   procedure Unmap (Area : Unsigned_64; Bytes : size_t);
   procedure Unmap (Area : Unsigned_64; Bytes : size_t) is
      Ignore : int;
   begin
      if Area /= 0 then
         Ignore := Munmap (To_Address (Area), Bytes);
      end if;
   end Unmap;

   --  A message to netstack through the endpoint in Slot: its reply label
   --  (0 if the call failed) and words.
   type Words is array (0 .. 3) of Unsigned_64;
   function Call (Slot : Natural; Label : Unsigned_32; Length : Unsigned_8;
                  W0, W1, W2, W3 : Unsigned_64; Reply : out Words) return Unsigned_32;
   function Call (Slot : Natural; Label : Unsigned_32; Length : Unsigned_8;
                  W0, W1, W2, W3 : Unsigned_64; Reply : out Words) return Unsigned_32
   is
      M : aliased K.Message :=
        (Label => Label, Length => Length, Words => [W0, W1, W2, W3], others => <>);
      Tag : constant Unsigned_64 :=
        Kernel (K.Call_Via_Endpoint_Capability, Unsigned_64 (Slot), Value_Of (M'Address));
   begin
      Reply := Words (M.Words);
      return (if Tag = K.Failed then 0 else Unsigned_32 (Tag and 16#FFFF_FFFF#));
   end Call;

   function Submit (Slot : Natural; Label : Unsigned_32; Length : Unsigned_8;
                    W0, W1, W2, W3 : Unsigned_64; Token : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Submit (Unsigned_64 (Slot), Label, Length, W0, W1, W2, W3, Token) = 1);

   function Lend (Endpoint : Natural; Area : Unsigned_64; Pages : Unsigned_64;
                  Slot, Generation : out Unsigned_64) return Boolean;
   function Lend (Endpoint : Natural; Area : Unsigned_64; Pages : Unsigned_64;
                  Slot, Generation : out Unsigned_64) return Boolean
   is
      Ignore : Unsigned_64;
   begin
      Generation := 0;
      Slot := Kernel (K.Create_Shared_Memory_Grant_Via_Capability, Unsigned_64 (Endpoint),
                      Area, Pages, Grant_Read_Write);
      if Slot = K.Failed then
         return False;
      end if;
      Generation := Kernel (K.Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = K.Failed or else Generation = 0 then
         Ignore := Kernel (K.Revoke_Shared_Memory_Grant, Slot);
         return False;
      end if;
      return True;
   end Lend;

   ---------------------------------------------------------------------------
   --  Scopes: each netstack endpoint among the program's capabilities, and
   --  what netstack says its scope allows.
   ---------------------------------------------------------------------------
   Scopes : array (1 .. Maximum_Scopes) of Scope;
   pragma Suppress_Initialization (Scopes);
   Scope_Count : Natural range 0 .. Maximum_Scopes := 0;
   Scopes_Known : Boolean := False;
   Scopes_Lock : aliased Lock_Word := 0;

   procedure Discover_Scopes;
   procedure Discover_Scopes is
      Self, Netstack : Unsigned_64;
      Info : array (1 .. K.Capability_Words) of Unsigned_64;
      Reply : Words;
      Found : Scope;
      Valid : Boolean;
   begin
      Lock (Scopes_Lock'Access);
      if not Scopes_Known then
         Self := Kernel (K.Get_Process_Id);
         Netstack := Kernel (K.Info, K.Registered_Driver, K.Driver_Netstack);
         for Slot in 0 .. K.Capability_Slots - 1 loop
            exit when Scope_Count = Maximum_Scopes;
            Info := [others => 0];
            if Kernel (K.Inspect_Capability, Self, Unsigned_64 (Slot), Value_Of (Info'Address)) = 1
              and then Info (1) = K.Capability_Endpoint and then Info (4) = Netstack
              and then Call (Slot, Layout.OP_NET_SCOPE, 0, 0, 0, 0, 0, Reply) = K.Reply_OK
            then
               Decode (Slot, Reply (0), Reply (1), Reply (2), Found, Valid);
               if Valid then
                  Scope_Count := Scope_Count + 1;
                  Scopes (Scope_Count) := Found;
               end if;
            end if;
         end loop;
         Scopes_Known := True;
      end if;
      Unlock (Scopes_Lock'Access);
   end Discover_Scopes;

   --  The endpoint of a scope allowing Action on Target (or a name) and
   --  Port, or -1.
   function Scope_Slot (Action : Unsigned_8; Named : Boolean; Target : Address;
                        Port : Unsigned_16) return Integer;
   function Scope_Slot (Action : Unsigned_8; Named : Boolean; Target : Address;
                        Port : Unsigned_16) return Integer is
   begin
      Discover_Scopes;
      for I in 1 .. Scope_Count loop
         if Allows (Scopes (I), Action, Named, Target, Port) then
            return Scopes (I).Slot;
         end if;
      end loop;
      return -1;
   end Scope_Slot;

   --  An endpoint for the process's own requests (arenas, WAIT, KICK).
   function Any_Slot return Integer;
   function Any_Slot return Integer is
   begin
      Discover_Scopes;
      return (if Scope_Count > 0 then Scopes (1).Slot else -1);
   end Any_Slot;

   ---------------------------------------------------------------------------
   --  Host names behind placeholder addresses.
   ---------------------------------------------------------------------------
   Name_Table : Names.Table;
   pragma Suppress_Initialization (Name_Table);
   Names_Lock : aliased Lock_Word := 0;

   --  The placeholder (network order, as in sin_addr) for the C string
   --  Name of Length bytes; 0 if the table is full.
   function Name_Address (Name : String) return Unsigned_32;
   function Name_Address (Name : String) return Unsigned_32 is
      Index : Names.Name_Count;
   begin
      if Name'Length not in Names.Name_Length then
         return 0;
      end if;
      Lock (Names_Lock'Access);
      Names.Index_Of (Name_Table, Name, Index);
      Unlock (Names_Lock'Access);
      if Index = 0 then
         return 0;
      end if;
      declare
         Host : constant Unsigned_32 := Names.Placeholder (Index);
      begin
         --  To network order: the bytes of the host-order value, reversed.
         return Shift_Left (Host and 16#FF#, 24) or Shift_Left (Shift_Right (Host, 8) and 16#FF#, 16)
           or Shift_Left (Shift_Right (Host, 16) and 16#FF#, 8) or Shift_Right (Host, 24);
      end;
   end Name_Address;

   --  The IPv4 bytes as a host-order value.
   function Host_Order (IPv4 : Octets) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (IPv4 (1)), 24) or Shift_Left (Unsigned_32 (IPv4 (2)), 16)
      or Shift_Left (Unsigned_32 (IPv4 (3)), 8) or Unsigned_32 (IPv4 (4)));

   --  The name a placeholder address stands for: its index, or 0.
   function Name_Index (IPv4 : Octets) return Natural;
   function Name_Index (IPv4 : Octets) return Natural is
      Index : constant Natural := Names.Index_From (Host_Order (IPv4));
   begin
      if Index = 0 then
         return 0;
      end if;
      Lock (Names_Lock'Access);
      declare
         Known : constant Boolean := Index <= Name_Table.Count;
      begin
         Unlock (Names_Lock'Access);
         return (if Known then Index else 0);
      end;
   end Name_Index;

   ---------------------------------------------------------------------------
   --  Sockets.
   ---------------------------------------------------------------------------
   type Tcp_State is (Idle, Connecting, Open, Failed);

   --  struct sockaddr_in.
   type Sockaddr_In is record
      Family : Unsigned_16 := 0;
      Port   : Unsigned_16 := 0;     --  network order
      IPv4   : Octets := [others => 0];
      Zero   : Storage_Array (1 .. 8) := [others => 0];
   end record with Convention => C;
   Sockaddr_In_Bytes : constant := 16;
   pragma Compile_Time_Error (Sockaddr_In'Size /= Sockaddr_In_Bytes * 8, "sockaddr_in");

   function Port_Of (S : Sockaddr_In) return Unsigned_16 is
     (Shift_Left (S.Port and 16#FF#, 8) or Shift_Right (S.Port, 8));
   function Network_Port (Port : Unsigned_16) return Unsigned_16 is
     (Shift_Left (Port and 16#FF#, 8) or Shift_Right (Port, 8));

   type Offered_Sockets is array (1 .. Offers) of Unsigned_64;   --  addresses

   type Socket_Object is record
      Lock        : aliased Lock_Word := 0;
      Write_Lock  : aliased Lock_Word := 0;   --  one producer of the send ring
      Read_Lock   : aliased Lock_Word := 0;   --  one consumer of the receive ring
      State       : Tcp_State := Idle;
      Error       : int := 0;                 --  pending SO_ERROR
      Closing     : Boolean := False;
      Write_Shut  : Boolean := False;
      Refs        : Natural := 0;             --  the descriptor, and an OPEN or SHUT in flight
      Bit         : Integer range -1 .. Wait_Bit'Last := -1;
      Slot        : Integer range -1 .. 63 := -1;
      Channel, Open_Token, Shut_Token : Unsigned_64 := 0;
      Buffer      : Unsigned_64 := 0;         --  its arena buffer
      Arena       : Integer range -1 .. Maximum_Arenas - 1 := -1;
      Index       : Natural range 0 .. Arena_Buffers - 1 := 0;
      Tx          : Rings.Producer;           --  we produce the send ring
      Rx          : Rings.Consumer;           --  and consume the receive ring
      Peer, Local : Sockaddr_In;
      Bound, Listening : Boolean := False;
      Offered     : Offered_Sockets := [others => 0];
      Target      : Targets.Target;
      Target_Length : Targets.Target_Length := 0;
   end record;
   Socket_Bytes : constant size_t := Socket_Object'Size / 8;

   function Word_At (S : Socket_Object; Offset : Natural) return Unsigned_32;
   function Word_At (S : Socket_Object; Offset : Natural) return Unsigned_32 is
      Word : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (S.Buffer + Unsigned_64 (Offset));
   begin
      return Word;
   end Word_At;

   procedure Set_Word (S : Socket_Object; Offset : Natural; Value : Unsigned_32);
   procedure Set_Word (S : Socket_Object; Offset : Natural; Value : Unsigned_32) is
      Word : Unsigned_32 with Import, Volatile,
        Address => To_Address (S.Buffer + Unsigned_64 (Offset));
   begin
      Word := Value;
   end Set_Word;

   function Send_Ring (S : Socket_Object) return Unsigned_64 is (S.Buffer + Layout.Header_Bytes);
   function Receive_Ring (S : Socket_Object) return Unsigned_64 is
     (S.Buffer + Layout.Header_Bytes + Ring_Bytes);

   --  Global socket state: which socket holds each wait bit, OPENs, the
   --  WAIT at netstack and who collects completions.
   Net_Lock : aliased Lock_Word := 0;
   By_Bit : array (Wait_Bit) of Unsigned_64 := [others => 0];
   pragma Suppress_Initialization (By_Bit);
   Open_Serial : Unsigned_64 := 0;
   Opens_Outstanding : Integer := 0;
   Wait_Outstanding : Boolean := False;          --  a WAIT is at netstack
   Wait_Deadline : unsigned_long := 0;            --  ... when it gives up
   Wait_Mask : Unsigned_64 := 0;                  --  ... and its sockets
   Waiter : Boolean := False with Volatile;       --  one thread collects completions
   Followers : Natural := 0;                      --  threads waiting for that collector
   --  Sockets waiting threads want to hear about, counted per wait bit.
   Interest_Count : array (Wait_Bit) of Natural;
   pragma Suppress_Initialization (Interest_Count);
   Interest : Unsigned_64 := 0;

   function Bit_Of (B : Wait_Bit) return Unsigned_64 is (Shift_Left (1, B));

   procedure Add_Interest (Mask : Unsigned_64);
   procedure Add_Interest (Mask : Unsigned_64) is
   begin
      for B in Wait_Bit loop
         if (Mask and Bit_Of (B)) /= 0 then
            if Interest_Count (B) = 0 then
               Interest := Interest or Bit_Of (B);
            end if;
            Interest_Count (B) := Interest_Count (B) + 1;
         end if;
      end loop;
   end Add_Interest;

   procedure Drop_Interest (Mask : Unsigned_64);
   procedure Drop_Interest (Mask : Unsigned_64) is
   begin
      for B in Wait_Bit loop
         if (Mask and Bit_Of (B)) /= 0 and then Interest_Count (B) > 0 then
            Interest_Count (B) := Interest_Count (B) - 1;
            if Interest_Count (B) = 0 then
               Interest := Interest and not Bit_Of (B);
            end if;
         end if;
      end loop;
   end Drop_Interest;

   procedure End_Wait;
   procedure End_Wait is
      Slot : constant Integer := Any_Slot;
      Ignore : Boolean;
   begin
      if Slot >= 0 then
         Ignore := Submit (Slot, Layout.OP_NET_KICK, 2, 0, Layout.End_Wait, 0, 0,
                           CuBit.Kernel_Calls.No_Completion_Token);
      end if;
   end End_Wait;

   procedure Kick (S : Socket_Object; Flags : Unsigned_64);
   procedure Kick (S : Socket_Object; Flags : Unsigned_64) is
      Ignore : Boolean;
   begin
      if S.Slot >= 0 and then S.Bit >= 0 then
         Ignore := Submit (S.Slot, Layout.OP_NET_KICK, 2, Bit_Of (S.Bit), Flags, 0, 0,
                           CuBit.Kernel_Calls.No_Completion_Token);
      end if;
   end Kick;

   function Status_Error (Status : Unsigned_32) return int is
     (case Status is
        when Layout.Status_Reset          => ECONNRESET,
        when Layout.Status_Timed_Out      => ETIMEDOUT,
        when Layout.Status_Unreachable    => EHOSTUNREACH,
        when Layout.Status_Protocol_Error => EPROTO,
        when others                       => 0);

   ---------------------------------------------------------------------------
   --  Channel arenas lent to netstack, and which buffers sockets hold.
   --  Memory and grants are kept for the process's lifetime; a buffer comes
   --  back only after netstack released its channel.
   ---------------------------------------------------------------------------
   type Arena is record
      Base   : Unsigned_64 := 0;
      Handle : Unsigned_64 := 0;
      Used   : Unsigned_32 := 0;          --  a bit per buffer
   end record;
   Arenas : array (0 .. Maximum_Arenas - 1) of Arena;
   pragma Suppress_Initialization (Arenas);
   Arena_Count : Natural range 0 .. Maximum_Arenas := 0;
   Arena_Lock : aliased Lock_Word := 0;
   All_Buffers : constant Unsigned_32 := 2 ** Arena_Buffers - 1;

   --  Lend netstack a new arena (a call: once per Arena_Buffers sockets).
   function Lend_Arena return long;
   function Lend_Arena return long is
      Endpoint : constant Integer := Any_Slot;
      Bytes : constant size_t := Arena_Buffers * Buffer_Bytes;
      Area : Unsigned_64;
      Slot, Generation : Unsigned_64;
      Reply : Words;
      Ignore : Unsigned_64;
   begin
      if Endpoint < 0 then
         return Error (EACCES);
      end if;
      Area := Map_Pages (Bytes);
      if Area = 0 then
         return Error (ENOMEM);
      end if;
      if not Lend (Endpoint, Area, Unsigned_64 (Bytes) / K.Page_Bytes, Slot, Generation)
        or else Call (Endpoint, Layout.OP_NET_ARENA, 4, Slot, Generation,
                      Ring_Bytes or Shift_Left (Unsigned_64 (Ring_Bytes), 32),
                      Arena_Buffers, Reply) /= K.Reply_OK
      then
         if Generation /= 0 then
            Ignore := Kernel (K.Revoke_Shared_Memory_Grant, Slot);
         end if;
         Unmap (Area, Bytes);
         return Error (EACCES);
      end if;
      Arenas (Arena_Count) := (Base => Area, Handle => Reply (0), Used => 0);
      Arena_Count := Arena_Count + 1;
      return 0;
   end Lend_Arena;

   --  Take a free buffer for S, lending a new arena if all are held.
   function Take_Buffer (S : in out Socket_Object) return long;
   function Take_Buffer (S : in out Socket_Object) return long is
      R : long := 0;
   begin
      Lock (Arena_Lock'Access);
      loop
         for A in 0 .. Arena_Count - 1 loop
            if Arenas (A).Used /= All_Buffers then
               for I in 0 .. Arena_Buffers - 1 loop
                  if (Arenas (A).Used and Shift_Left (1, I)) = 0 then
                     Arenas (A).Used := Arenas (A).Used or Shift_Left (1, I);
                     S.Arena := A;
                     S.Index := I;
                     S.Buffer := Arenas (A).Base + Unsigned_64 (I * Buffer_Bytes);
                     Unlock (Arena_Lock'Access);
                     declare
                        Header : Storage_Array (1 .. Layout.Header_Bytes)
                        with Import, Address => To_Address (S.Buffer);
                     begin
                        Header := [others => 0];      --  indices at 0
                     end;
                     return 0;
                  end if;
               end loop;
            end if;
         end loop;
         if Arena_Count = Maximum_Arenas then
            R := Error (ENOBUFS);
            exit;
         end if;
         R := Lend_Arena;
         exit when R /= 0;
      end loop;
      Unlock (Arena_Lock'Access);
      return R;
   end Take_Buffer;

   procedure Give_Buffer (S : in out Socket_Object);
   procedure Give_Buffer (S : in out Socket_Object) is
   begin
      if S.Arena < 0 then
         return;
      end if;
      Lock (Arena_Lock'Access);
      Arenas (S.Arena).Used := Arenas (S.Arena).Used and not Shift_Left (1, S.Index);
      Unlock (Arena_Lock'Access);
      S.Arena := -1;
   end Give_Buffer;

   procedure Release (Address : Unsigned_64);
   procedure Release (Address : Unsigned_64) is
      S : Socket_Object with Import, Address => To_Address (Address);
      Last : Boolean;
   begin
      Lock (S.Lock'Access);
      S.Refs := S.Refs - 1;
      Last := S.Refs = 0;
      Unlock (S.Lock'Access);
      if not Last then
         return;
      end if;
      Lock (Net_Lock'Access);
      if S.Bit >= 0 then
         By_Bit (S.Bit) := 0;
      end if;
      Unlock (Net_Lock'Access);
      Give_Buffer (S);
      Free (To_Address (Address));
   end Release;

   ---------------------------------------------------------------------------
   --  Control queues: OPEN and SHUT into a queue pair lent to netstack (one
   --  per scope endpoint), with a KICK only when netstack's wake word shows
   --  it asleep; its answers complete our WAIT and are reaped in Drain. With
   --  no queue, OPEN is refused as backpressure and SHUT goes by message.
   ---------------------------------------------------------------------------
   type Control_Queue is record
      Slot    : Integer := -1;           --  the endpoint
      Refused : Boolean := True;         --  netstack gave none
      Base    : Unsigned_64 := 0;
      Client  : NCQ.Queues.Client;
      Kicked  : Unsigned_32 := 0;        --  the wake epoch last kicked
   end record;
   Queues : array (1 .. Maximum_Scopes) of Control_Queue;
   pragma Suppress_Initialization (Queues);
   Queue_Count : Natural range 0 .. Maximum_Scopes := 0;
   Queue_Lock : aliased Lock_Word := 0;

   function Queue_Header (Q : Control_Queue; Offset : Natural) return Unsigned_32;
   function Queue_Header (Q : Control_Queue; Offset : Natural) return Unsigned_32 is
      Word : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (Q.Base + Unsigned_64 (Offset));
   begin
      return Word;
   end Queue_Header;

   procedure Set_Queue_Header (Q : Control_Queue; Offset : Natural; Value : Unsigned_32);
   procedure Set_Queue_Header (Q : Control_Queue; Offset : Natural; Value : Unsigned_32) is
      Word : Unsigned_32 with Import, Volatile,
        Address => To_Address (Q.Base + Unsigned_64 (Offset));
   begin
      Word := Value;
   end Set_Queue_Header;

   --  The queue for endpoint Slot, lent now if it has none; 0: use
   --  messages. Queue_Lock is held.
   function Queue_For (Slot : Natural) return Natural;
   function Queue_For (Slot : Natural) return Natural is
      Area, Grant, Generation : Unsigned_64;
      Reply : Words;
      Ignore : Unsigned_64;
   begin
      for I in 1 .. Queue_Count loop
         if Queues (I).Slot = Slot then
            return (if Queues (I).Refused then 0 else I);
         end if;
      end loop;
      if Queue_Count = Maximum_Scopes then
         return 0;
      end if;
      Queue_Count := Queue_Count + 1;
      Queues (Queue_Count) :=
        (Slot => Slot, Refused => True, Base => 0, Kicked => 0,
         Client => (Requests => (Produced => 0, Fill => 0),
                    Answers => (Consumed => 0, Available => 0), Pending => 0));
      Area := Map_Pages (Layout.Queue_Bytes);
      if Area = 0 then
         return 0;
      end if;
      if not Lend (Slot, Area, Layout.Queue_Bytes / K.Page_Bytes, Grant, Generation)
        or else Call (Slot, Layout.OP_NET_QUEUE, 2, Grant, Generation, 0, 0, Reply) /= K.Reply_OK
      then
         if Generation /= 0 then
            Ignore := Kernel (K.Revoke_Shared_Memory_Grant, Grant);
         end if;
         Unmap (Area, Layout.Queue_Bytes);
         return 0;
      end if;
      Queues (Queue_Count).Base := Area;
      Queues (Queue_Count).Refused := False;
      return Queue_Count;
   end Queue_For;

   --  Queue a request on endpoint Slot; False if it must go another way.
   function Queue_Submit (Slot : Natural; Token : Unsigned_64; Operation : Unsigned_32;
                          Length : Unsigned_32; Object : Unsigned_64; Buffer : Unsigned_32)
     return Boolean;
   function Queue_Submit (Slot : Natural; Token : Unsigned_64; Operation : Unsigned_32;
                          Length : Unsigned_32; Object : Unsigned_64; Buffer : Unsigned_32)
     return Boolean
   is
      I : Natural;
      OK, Do_Kick : Boolean := False;
      Wake : Unsigned_32;
      Ignore : Boolean;
   begin
      Lock (Queue_Lock'Access);
      I := Queue_For (Slot);
      if I = 0 then
         Unlock (Queue_Lock'Access);
         return False;
      end if;
      declare
         Q : Control_Queue renames Queues (I);
         Ring : NCQ.Queues.Submissions.Ring with Import,
           Address => To_Address (Q.Base + Layout.Queue_Requests_At);
      begin
         NCQ.Queues.Accept_Taken
           (Q.Client, NCQ.Queues.Submissions.Index
              (Queue_Header (Q, Layout.Queue_Submissions_At + Layout.Queue_Consumed_At)), OK);
         --  Room in the ring, and an answer slot for every request out.
         if not NCQ.Queues.Can_Submit (Q.Client) then
            Unlock (Queue_Lock'Access);
            return False;
         end if;
         NCQ.Queues.Submit
           (Q.Client, Ring, NCQ.Queues.Token (Token),
            (Operation => Operation, Length => Length, Object => Object, Buffer => Buffer,
             Reserved => 0));
         Compiler_Barrier;              --  the entry before the count
         Set_Queue_Header (Q, Layout.Queue_Submissions_At + Layout.Queue_Produced_At,
                           Unsigned_32 (Q.Client.Requests.Produced));
         Full_Fence;                    --  the count before the wake word
         Wake := Queue_Header (Q, Layout.Queue_Submissions_At + Layout.Queue_Wake_At);
         Do_Kick := Wake /= 0 and then Wake /= Q.Kicked;
         if Do_Kick then
            Q.Kicked := Wake;
         end if;
      end;
      Unlock (Queue_Lock'Access);
      if Do_Kick then
         Ignore := Submit (Slot, Layout.OP_NET_KICK, 2, 0, Layout.Kick_Queue, 0, 0,
                           CuBit.Kernel_Calls.No_Completion_Token);
      end if;
      return True;
   end Queue_Submit;

   --  The kernel's completion entry (process.ads CompletionEntry).
   type Completion is record
      Request_Id, Token : Unsigned_64 := 0;
      Message : K.Message;
      From, Padding : Unsigned_32 := 0;
      Status : Unsigned_64 := 0;
      Valid : Unsigned_8 := 0;
      Tail : Storage_Array (1 .. 7) := [others => 0];
   end record with Convention => C;
   Completion_Bytes : constant := 88;
   pragma Compile_Time_Error (Completion'Size /= Completion_Bytes * 8, "completion entry");

   procedure Send_Shut_Now (S : Socket_Object);
   procedure Send_Shut_Now (S : Socket_Object) is
      Reply : Words;
      Ignore : Unsigned_32;
   begin
      if S.Slot >= 0 then
         Ignore := Call (S.Slot, OP_NET_SHUT, 1, S.Channel, 0, 0, 0, Reply);
      end if;
   end Send_Shut_Now;

   --  SHUT without waiting: the socket keeps its buffer and wait bit (a
   --  reference) until the completion says netstack released the channel.
   procedure Send_Shut (Address : Unsigned_64);
   procedure Send_Shut (Address : Unsigned_64) is
      S : Socket_Object with Import, Address => To_Address (Address);
      Token : Unsigned_64;
   begin
      Lock (Net_Lock'Access);
      Open_Serial := Open_Serial + 1;
      Token := Shut_Token or Shift_Left (Open_Serial, 8) or Unsigned_64 (Natural'Max (S.Bit, 0));
      Unlock (Net_Lock'Access);
      Lock (S.Lock'Access);
      S.Shut_Token := Token;
      S.Refs := S.Refs + 1;
      Unlock (S.Lock'Access);
      if S.Slot >= 0 and then Queue_Submit (S.Slot, Token, Layout.Queue_Shut, 0, S.Channel, 0) then
         return;
      end if;
      Lock (S.Lock'Access);
      S.Shut_Token := 0;
      S.Refs := S.Refs - 1;
      Unlock (S.Lock'Access);
      Send_Shut_Now (S);
   end Send_Shut;

   --  An OPEN finished: the socket is open or failed; one closed while
   --  connecting gives its channel back.
   procedure Opened (Address : Unsigned_64; C : Completion);
   procedure Opened (Address : Unsigned_64; C : Completion) is
      S : Socket_Object with Import, Address => To_Address (Address);
      OK : constant Boolean := C.Status = 0 and then C.Message.Label = K.Reply_OK;
      Closing : Boolean;
   begin
      Lock (S.Lock'Access);
      S.Open_Token := 0;
      if OK then
         S.Channel := C.Message.Words (0);
         S.State := Open;
      else
         S.State := Failed;
         S.Error := ECONNREFUSED;
      end if;
      Closing := S.Closing;
      Unlock (S.Lock'Access);
      if OK and then Closing then
         Send_Shut (Address);
      end if;
      Release (Address);                --  the OPEN's reference
   end Opened;

   procedure Dispatch (C : Completion);
   procedure Dispatch (C : Completion) is
      Bit : constant Unsigned_64 := C.Token and Bit_Mask;
      Address : Unsigned_64;
      Match : Boolean;
   begin
      if C.Token = Wait_Token then
         Lock (Net_Lock'Access);
         Wait_Outstanding := False;
         Unlock (Net_Lock'Access);
         return;
      elsif Bit > Layout.Maximum_Wait_Bit then
         return;
      end if;
      Lock (Net_Lock'Access);
      Address := By_Bit (Natural (Bit));
      if (C.Token and Shut_Token) /= 0 then
         declare
            S : Socket_Object with Import, Address => To_Address (Address);
         begin
            Match := Address /= 0 and then S.Shut_Token = C.Token;
            Unlock (Net_Lock'Access);
            if Match then
               S.Shut_Token := 0;
               Release (Address);       --  the SHUT's reference
            end if;
         end;
         return;
      end if;
      declare
         S : Socket_Object with Import, Address => To_Address (Address);
      begin
         Match := Address /= 0 and then S.Open_Token = C.Token;
         if Match then
            Opens_Outstanding := Opens_Outstanding - 1;
         end if;
      end;
      Unlock (Net_Lock'Access);
      if Match then
         Opened (Address, C);
      end if;
   end Dispatch;

   --  Take netstack's answers from every queue and dispatch them as the
   --  completions of the requests they answer; how many there were.
   function Reap_Answers return Natural;
   function Reap_Answers return Natural is
      Total : Natural := 0;
      Got : array (1 .. Layout.Queue_Slots) of Completion;
      N : Natural;
      OK : Boolean;
   begin
      for I in 1 .. Queue_Count loop
         N := 0;
         Lock (Queue_Lock'Access);
         if not Queues (I).Refused then
            declare
               Q : Control_Queue renames Queues (I);
               Ring : constant NCQ.Queues.Completions.Ring with Import,
                 Address => To_Address (Q.Base + Layout.Queue_Answers_At);
               Answer : NCQ.Queues.Completion;
            begin
               NCQ.Queues.Completions.Accept_Produced
                 (Q.Client.Answers, NCQ.Queues.Completions.Index
                    (Queue_Header (Q, Layout.Queue_Completions_At + Layout.Queue_Produced_At)), OK);
               Compiler_Barrier;        --  the answers after their count
               while Q.Client.Answers.Available > 0 and then N < Got'Length loop
                  NCQ.Queues.Reap (Q.Client, Ring, Answer, OK);
                  if OK then
                     N := N + 1;
                     Got (N) := (Token => Unsigned_64 (Answer.Tag), Status => 0, others => <>);
                     Got (N).Message.Label :=
                       (if Answer.Answer.Status = Layout.Answer_OK then K.Reply_OK else K.Reply_Error);
                     Got (N).Message.Words (0) := Answer.Answer.Value;
                  end if;
               end loop;
               Compiler_Barrier;        --  copied out before the slots go back
               Set_Queue_Header (Q, Layout.Queue_Completions_At + Layout.Queue_Consumed_At,
                                 Unsigned_32 (Q.Client.Answers.Consumed));
            end;
         end if;
         Unlock (Queue_Lock'Access);
         for J in 1 .. N loop
            Dispatch (Got (J));
         end loop;
         Total := Total + N;
      end loop;
      return Total;
   end Reap_Answers;

   --  Only the submitting collector consumes its kernel WAIT completion;
   --  opportunistic callers reap the shared control queues only.
   procedure Drain (Block : Boolean);
   procedure Drain (Block : Boolean) is
      Batch : constant := 8;
      C : array (1 .. Batch) of Completion;
      N : Unsigned_64 := 0;
      Answers : Natural;
   begin
      if Block then
         N := Kernel (K.Wait_Completion, Value_Of (C'Address), Batch, 1);
         if N > Batch then
            N := 0;
         end if;
      end if;
      for I in 1 .. Natural (N) loop
         Dispatch (C (I));
      end loop;
      Answers := Reap_Answers;
      if N > 0 or else Answers > 0 then
         Readiness_Changed;
      end if;
   end Drain;

   --  Release collection before waking followers: one woken during the
   --  drain must get a second wake once collection is free.
   procedure Release_Collector;
   procedure Release_Collector is
      Wake : Boolean;
   begin
      Lock (Net_Lock'Access);
      Waiter := False;
      Wake := Followers /= 0;
      Unlock (Net_Lock'Access);
      if Wake then
         Readiness_Changed;
      end if;
   end Release_Collector;

   --  Opportunistic collection, with the same ownership as blocking.
   procedure Collect;
   procedure Collect is
      Take : Boolean;
   begin
      Lock (Net_Lock'Access);
      Take := not Waiter;
      if Take then
         Waiter := True;
      end if;
      Unlock (Net_Lock'Access);
      if Take then
         Drain (False);
         Release_Collector;
      end if;
   end Collect;

   procedure Net_Interrupt is
      Interrupt : Boolean;
   begin
      Lock (Net_Lock'Access);
      Interrupt := Waiter and then Wait_Outstanding;
      Unlock (Net_Lock'Access);
      if Interrupt then
         End_Wait;
      end if;
   end Net_Interrupt;

   function Tcp_Mask (Socket : System.Address) return Unsigned_64 is
      S : Socket_Object with Import, Address => Socket;
   begin
      return (if S.Bit >= 0 then Bit_Of (S.Bit) else 0);
   end Tcp_Mask;

   procedure Net_Wait (Sequence : int; Deadline : unsigned_long; Mask : Unsigned_64) is
      Fresh, Stale, Pending : Boolean;
      Wanted : Unsigned_64;
      Endpoint : Integer := -1;
   begin
      Lock (Net_Lock'Access);
      Add_Interest (Mask);
      if Waiter then
         --  The waiter's WAIT must cover our sockets: if not, end it.
         Stale := Wait_Outstanding and then (Interest and not Wait_Mask) /= 0;
         Followers := Followers + 1;
         Unlock (Net_Lock'Access);
         if Stale then
            End_Wait;
         end if;
         Readiness_Wait (Sequence, Deadline);
         Lock (Net_Lock'Access);
         Followers := Followers - 1;
         Drop_Interest (Mask);
         Unlock (Net_Lock'Access);
         return;
      end if;
      Waiter := True;
      Fresh := not Wait_Outstanding;
      Stale := not Fresh and then ((Interest and not Wait_Mask) /= 0 or else Deadline < Wait_Deadline);
      Wanted := Interest;
      if Fresh then
         Wait_Outstanding := True;
         Wait_Mask := Wanted;
         Wait_Deadline := Deadline;
      end if;
      Unlock (Net_Lock'Access);
      if Fresh then
         Endpoint := Any_Slot;
      end if;
      if Fresh and then (Endpoint < 0 or else
                         not Submit (Endpoint, Layout.OP_NET_WAIT, 3, 0, Unsigned_64 (Deadline),
                                     Wanted, 0, Wait_Token))
      then
         Lock (Net_Lock'Access);
         Wait_Outstanding := False;
         Drop_Interest (Mask);
         Unlock (Net_Lock'Access);
         Release_Collector;
         Readiness_Wait (Sequence, Deadline);
         return;
      end if;
      --  The outstanding WAIT misses a socket or outlasts our deadline.
      if Stale or else Readiness_Sequence /= Sequence then
         End_Wait;
      end if;
      --  Kernel completions belong to the submitting thread: harvest the
      --  WAIT here, ending it if need be.
      loop
         Drain (True);
         Lock (Net_Lock'Access);
         Pending := Wait_Outstanding;
         Unlock (Net_Lock'Access);
         exit when not Pending;
         End_Wait;
      end loop;
      Lock (Net_Lock'Access);
      Drop_Interest (Mask);
      Unlock (Net_Lock'Access);
      Release_Collector;
   end Net_Wait;

   function Tcp_New return System.Address is
      Memory : constant System.Address := Calloc (1, Socket_Bytes);
   begin
      if Memory /= System.Null_Address then
         declare
            S : Socket_Object with Import, Address => Memory;
         begin
            S.Refs := 1;
            S.Bit := -1;
            S.Slot := -1;
            S.Arena := -1;
            S.Tx := Rings.New_Producer (Ring_Bytes);
            S.Rx := Rings.New_Consumer (Ring_Bytes);
         end;
      end if;
      return Memory;
   end Tcp_New;

   function Free_Bit (Address : Unsigned_64) return Integer;
   function Free_Bit (Address : Unsigned_64) return Integer is
   begin
      for B in Wait_Bit loop
         if By_Bit (B) = 0 then
            By_Bit (B) := Address;
            return B;
         end if;
      end loop;
      return -1;
   end Free_Bit;

   --  Give S a buffer and a wait bit, and lay out the channel header
   --  netstack reads when a channel opens there.
   function Prepare_Channel (Address : Unsigned_64) return long;
   function Prepare_Channel (Address : Unsigned_64) return long is
      S : Socket_Object with Import, Address => To_Address (Address);
      R : long := Take_Buffer (S);
      Bit : Integer;
   begin
      if R = Error (ENOBUFS) then
         Collect;          --  closed sockets whose SHUT completed give theirs back
         R := Take_Buffer (S);
      end if;
      if R /= 0 then
         return R;
      end if;
      S.Tx := Rings.New_Producer (Ring_Bytes);
      S.Rx := Rings.New_Consumer (Ring_Bytes);
      Lock (Net_Lock'Access);
      Bit := Free_Bit (Address);
      Unlock (Net_Lock'Access);
      if Bit < 0 then
         Collect;
         Lock (Net_Lock'Access);
         Bit := Free_Bit (Address);
         Unlock (Net_Lock'Access);
      end if;
      if Bit < 0 then
         return Error (ENOBUFS);
      end if;
      S.Bit := Bit;
      Set_Word (S, Layout.Tx_Size_At, Ring_Bytes);
      Set_Word (S, Layout.Rx_Size_At, Ring_Bytes);
      Set_Word (S, Layout.Wait_Bit_At, Unsigned_32 (Bit));
      return 0;
   end Prepare_Channel;

   --  Submit OPEN for S's target (prepared) and, unless Nonblocking, wait
   --  for its completion.
   function Open_Channel (Address : Unsigned_64; Nonblocking : int) return long;
   function Open_Channel (Address : Unsigned_64; Nonblocking : int) return long is
      S : Socket_Object with Import, Address => To_Address (Address);
      Sequence : int;
      State : Tcp_State;
   begin
      Lock (Net_Lock'Access);
      Open_Serial := Open_Serial + 1;
      S.Open_Token := Shift_Left (Open_Serial, 8) or Unsigned_64 (S.Bit);
      Opens_Outstanding := Opens_Outstanding + 1;
      Unlock (Net_Lock'Access);
      declare
         Target : String (1 .. S.Target_Length) with Import,
           Address => To_Address (S.Buffer + Layout.Target_At);
      begin
         Target := S.Target (1 .. S.Target_Length);
      end;
      Lock (S.Lock'Access);
      S.State := Connecting;
      S.Refs := S.Refs + 1;
      Unlock (S.Lock'Access);
      --  A refused OPEN is backpressure: a direct fallback would tie the
      --  reply to this thread, which may not be the one polling.
      if not Queue_Submit (S.Slot, S.Open_Token, Layout.Queue_Open, Unsigned_32 (S.Target_Length),
                           Arenas (S.Arena).Handle, Unsigned_32 (S.Index))
      then
         Lock (Net_Lock'Access);
         Opens_Outstanding := Opens_Outstanding - 1;
         Unlock (Net_Lock'Access);
         Lock (S.Lock'Access);
         S.State := Failed;
         S.Error := EAGAIN;
         S.Open_Token := 0;
         S.Refs := S.Refs - 1;
         Unlock (S.Lock'Access);
         return Error (EAGAIN);
      end if;
      if Nonblocking /= 0 then
         return Error (EINPROGRESS);
      end if;
      loop
         Sequence := Readiness_Sequence;
         Collect;
         Lock (S.Lock'Access);
         State := S.State;
         Unlock (S.Lock'Access);
         if State = Open then
            return 0;
         elsif State = Failed then
            return Error (S.Error);
         end if;
         Net_Wait (Sequence, Forever, 0);
      end loop;
   end Open_Channel;

   --  The text of the C string of a host name, as the names table keeps it.
   procedure Name_Text (Index : Positive; Result : out String; Length : out Natural);
   procedure Name_Text (Index : Positive; Result : out String; Length : out Natural) is
   begin
      Lock (Names_Lock'Access);
      Length := Name_Table.Entries (Index).Length;
      Result (Result'First .. Result'First + Length - 1) := Name_Table.Entries (Index).Text (1 .. Length);
      Unlock (Names_Lock'Access);
   end Name_Text;

   function Tcp_Connect (Socket, Address : System.Address; Length : unsigned;
                         Nonblocking : int) return long
   is
      S : Socket_Object with Import, Address => Socket;
      State : Tcp_State;
      Fits : Boolean;
      R : long;
   begin
      if Address = System.Null_Address or else Length < Sockaddr_In_Bytes then
         return Error (EINVAL);
      end if;
      declare
         Peer : constant Sockaddr_In with Import, Address => Address;
         Index : constant Natural := Name_Index (Peer.IPv4);
         Host : String (1 .. Names.Name_Bytes);
         Host_Length : Natural;
      begin
         if Unsigned_32 (Peer.Family) /= Unsigned_32 (AF_INET) then
            return Error (EAFNOSUPPORT);
         end if;
         Lock (S.Lock'Access);
         State := S.State;
         Unlock (S.Lock'Access);
         if State = Connecting then
            return Error (EALREADY);
         elsif State = Open then
            return Error (EISCONN);
         elsif State = Failed then
            return Error (S.Error);
         end if;
         if Index /= 0 then
            Name_Text (Index, Host, Host_Length);
         else
            Targets.Dotted (Peer.IPv4, Host, Host_Length);
         end if;
         Targets.Format (Targets.Connect_Prefix, Host (1 .. Host_Length), Port_Of (Peer),
                         S.Target, S.Target_Length, Fits);
         if not Fits then
            return Error (ENAMETOOLONG);
         end if;
         S.Peer := Peer;
         S.Slot := Scope_Slot (Connect_TCP, Index /= 0, Mapped (Peer.IPv4), Port_Of (Peer));
         if S.Slot < 0 then
            return Error (EACCES);      --  no scope allows it
         end if;
      end;
      --  A buffer in an arena lent to netstack.
      R := Prepare_Channel (Value_Of (Socket));
      if R /= 0 then
         return R;
      end if;
      return Open_Channel (Value_Of (Socket), Nonblocking);
   end Tcp_Connect;

   --  Receive-ring bytes available now, as the ring rules allow (0 if
   --  netstack broke them). Read_Lock is held.
   function Receivable (S : in out Socket_Object) return Natural;
   function Receivable (S : in out Socket_Object) return Natural is
      OK : Boolean;
   begin
      Rings.Accept_Produced (S.Rx, Rings.Index (Word_At (S, Layout.Rx_Produced_At)), OK);
      Compiler_Barrier;
      return S.Rx.Available;
   end Receivable;

   --  Copy up to Count received bytes out and release them.
   function Ring_Read (S : in out Socket_Object; Buffer : Unsigned_64; Count : Unsigned_64)
     return Natural;
   function Ring_Read (S : in out Socket_Object; Buffer : Unsigned_64; Count : Unsigned_64)
     return Natural
   is
      First, Length_1, Length_2 : Natural;
      Ignore : Natural;
      Take, Part_1 : Natural;
   begin
      if Unsigned_64 (S.Rx.Available) < Count then
         Ignore := Receivable (S);
      end if;
      if S.Rx.Available = 0 then
         return 0;
      end if;
      Rings.Data_Slices (S.Rx, First, Length_1, Length_2);
      Take := Natural (Unsigned_64'Min (Count, Unsigned_64 (Length_1 + Length_2)));
      Part_1 := Natural'Min (Take, Length_1);
      declare
         Target_1 : Storage_Array (1 .. Storage_Offset (Part_1))
         with Import, Address => To_Address (Buffer);
         Source_1 : constant Storage_Array (1 .. Storage_Offset (Part_1))
         with Import, Address => To_Address (Receive_Ring (S) + Unsigned_64 (First));
         Target_2 : Storage_Array (1 .. Storage_Offset (Take - Part_1))
         with Import, Address => To_Address (Buffer + Unsigned_64 (Part_1));
         Source_2 : constant Storage_Array (1 .. Storage_Offset (Take - Part_1))
         with Import, Address => To_Address (Receive_Ring (S));
      begin
         Target_1 := Source_1;
         Target_2 := Source_2;
      end;
      Rings.Consume (S.Rx, Take);
      Compiler_Barrier;
      Set_Word (S, Layout.Rx_Consumed_At, Unsigned_32 (S.Rx.Consumed));
      if Take > 0 then
         Full_Fence;
         if (Word_At (S, Layout.Kick_Wanted_At) and Layout.Kick_On_Receive) /= 0 then
            Kick (S, 0);
         end if;
      end if;
      return Take;
   end Ring_Read;

   --  Arm a notification, then look again: netstack may have moved before
   --  it could see the flag.
   procedure Want (S : Socket_Object; Flags : Unsigned_32);
   procedure Want (S : Socket_Object; Flags : Unsigned_32) is
      Old : constant Unsigned_32 := Word_At (S, Layout.Want_At);
   begin
      if (Old and Flags) /= Flags then
         Set_Word (S, Layout.Want_At, Old or Flags);
      end if;
      Full_Fence;
   end Want;

   function Tcp_Read (Socket, Buffer : System.Address; Count : size_t;
                      Nonblocking : int) return long
   is
      S : Socket_Object with Import, Address => Socket;
      Sequence : int;
      State : Tcp_State;
      Err : int;
      Got : Natural;
      Status : Unsigned_32;
      Ready : Natural;
   begin
      loop
         Sequence := Readiness_Sequence;
         Collect;
         Lock (S.Lock'Access);
         State := S.State;
         Err := S.Error;
         Unlock (S.Lock'Access);
         if State = Idle then
            return Error (ENOTCONN);
         elsif State = Failed then
            return (if Err /= 0 and then Err /= ECONNREFUSED then Error (Err) else 0);
         elsif State = Open then
            Lock (S.Read_Lock'Access);
            Got := Ring_Read (S, Value_Of (Buffer), Unsigned_64 (Count));
            Status := Word_At (S, Layout.Status_At);
            if Got = 0 and then Status >= Layout.Status_Peer_Finished then
               Got := Ring_Read (S, Value_Of (Buffer), Unsigned_64 (Count));  --  the last bytes precede the status
            end if;
            Unlock (S.Read_Lock'Access);
            if Got > 0 or else Count = 0 then
               return long (Got);
            elsif Status = Layout.Status_Peer_Finished then
               return 0;
            elsif Status_Error (Status) /= 0 then
               return Error (Status_Error (Status));
            elsif Nonblocking /= 0 then
               return Error (EAGAIN);
            end if;
            Want (S, Layout.Want_Readable);
            Lock (S.Read_Lock'Access);
            Ready := Receivable (S);
            Unlock (S.Read_Lock'Access);
            if Ready = 0 and then Word_At (S, Layout.Status_At) < Layout.Status_Peer_Finished then
               Net_Wait (Sequence, Forever, Tcp_Mask (Socket));
            end if;
         elsif Nonblocking /= 0 then
            return Error (EAGAIN);
         else
            Net_Wait (Sequence, Forever, Tcp_Mask (Socket));
         end if;
      end loop;
   end Tcp_Read;

   --  Send-ring free space, as the ring rules allow. Write_Lock is held.
   function Sendable (S : in out Socket_Object) return Natural;
   function Sendable (S : in out Socket_Object) return Natural is
      OK : Boolean;
   begin
      Rings.Accept_Consumed (S.Tx, Rings.Index (Word_At (S, Layout.Tx_Consumed_At)), OK);
      return Rings.Space (S.Tx);
   end Sendable;

   --  Copy up to Count bytes into the send ring and publish them.
   function Ring_Write (S : in out Socket_Object; Source : Unsigned_64; Count : Unsigned_64)
     return Natural;
   function Ring_Write (S : in out Socket_Object; Source : Unsigned_64; Count : Unsigned_64)
     return Natural
   is
      First, Length_1, Length_2 : Natural;
      Put, Part_1 : Natural;
   begin
      if Unsigned_64 (Rings.Space (S.Tx)) < Count and then Sendable (S) = 0 then
         return 0;
      end if;
      if Rings.Space (S.Tx) = 0 then
         return 0;
      end if;
      Rings.Free_Slices (S.Tx, First, Length_1, Length_2);
      Put := Natural (Unsigned_64'Min (Count, Unsigned_64 (Length_1 + Length_2)));
      Part_1 := Natural'Min (Put, Length_1);
      declare
         Target_1 : Storage_Array (1 .. Storage_Offset (Part_1))
         with Import, Address => To_Address (Send_Ring (S) + Unsigned_64 (First));
         Source_1 : constant Storage_Array (1 .. Storage_Offset (Part_1))
         with Import, Address => To_Address (Source);
         Target_2 : Storage_Array (1 .. Storage_Offset (Put - Part_1))
         with Import, Address => To_Address (Send_Ring (S));
         Source_2 : constant Storage_Array (1 .. Storage_Offset (Put - Part_1))
         with Import, Address => To_Address (Source + Unsigned_64 (Part_1));
      begin
         Target_1 := Source_1;
         Target_2 := Source_2;
      end;
      Rings.Commit (S.Tx, Put);
      Compiler_Barrier;
      Set_Word (S, Layout.Tx_Produced_At, Unsigned_32 (S.Tx.Produced));
      if Put > 0 then
         Full_Fence;
         if (Word_At (S, Layout.Kick_Wanted_At) and Layout.Kick_On_Send) /= 0 then
            Kick (S, 0);
         end if;
      end if;
      return Put;
   end Ring_Write;

   function Tcp_Write (Socket, Vectors : System.Address; Count : int;
                       Nonblocking : int) return long
   is
      S : Socket_Object with Import, Address => Socket;
      Sequence : int;
      State : Tcp_State;
      Shut : Boolean;
      Total : long := 0;
      type Vector_Array is array (1 .. IOV_MAX) of Io_Vector;
      V : constant Vector_Array with Import, Address => Vectors;
   begin
      if Count not in 0 .. IOV_MAX then
         return Error (EINVAL);
      end if;
      loop
         Sequence := Readiness_Sequence;
         Collect;
         Lock (S.Lock'Access);
         State := S.State;
         Shut := S.Write_Shut or else S.Closing;
         Unlock (S.Lock'Access);
         if State = Idle then
            return Error (ENOTCONN);
         elsif State = Failed or else Shut then
            return Error (EPIPE);
         end if;
         exit when State = Open;
         if Nonblocking /= 0 then
            return Error (EAGAIN);
         end if;
         Net_Wait (Sequence, Forever, 0);
      end loop;
      Lock (S.Write_Lock'Access);
      for I in 1 .. Integer (Count) loop
         declare
            Position : Unsigned_64 := V (I).Base;
            Left : Unsigned_64 := V (I).Length;
            Put : Natural;
         begin
            while Left > 0 loop
               Sequence := Readiness_Sequence;
               if Status_Error (Word_At (S, Layout.Status_At)) /= 0 then
                  Unlock (S.Write_Lock'Access);
                  return (if Total > 0 then Total else Error (EPIPE));
               end if;
               Put := Ring_Write (S, Position, Left);
               Position := Position + Unsigned_64 (Put);
               Left := Left - Unsigned_64 (Put);
               Total := Total + long (Put);
               exit when Left = 0;
               if Put = 0 then
                  if Nonblocking /= 0 then
                     Unlock (S.Write_Lock'Access);
                     return (if Total > 0 then Total else Error (EAGAIN));
                  end if;
                  Want (S, Layout.Want_Writable);
                  if Sendable (S) = 0 then
                     Net_Wait (Sequence, Forever, Tcp_Mask (Socket));
                     Collect;
                  end if;
               end if;
            end loop;
         end;
      end loop;
      Unlock (S.Write_Lock'Access);
      return Total;
   end Tcp_Write;

   function Has (Events : Integer_16; Mask : Natural) return Integer_16 is
     (Integer_16 (Unsigned_16'Mod (Events) and Unsigned_16 (Mask)));
   function Union (A, B : Integer_16) return Integer_16 is
     (Integer_16 (Unsigned_16'Mod (A) or Unsigned_16'Mod (B)));

   function Tcp_Poll (Socket : System.Address; Events : Integer_16) return Integer_16 is
      S : Socket_Object with Import, Address => Socket;
      Result : Integer_16 := 0;
      State : Tcp_State;
      Write_Shut : Boolean;
   begin
      Collect;
      Lock (S.Lock'Access);
      State := S.State;
      Write_Shut := S.Write_Shut;
      Unlock (S.Lock'Access);
      case State is
         when Idle => return POLLHUP;
         when Connecting => return 0;
         when Failed => return Union (POLLERR + POLLHUP, Has (Events, POLLOUT + POLLIN));
         when Open => null;
      end case;
      if S.Listening then
         --  Readable when a connection has arrived.
         for Look in 1 .. 2 loop
            Lock (S.Read_Lock'Access);
            Result := (if Receivable (S) > 0 then Has (Events, POLLIN + POLLRDNORM) else 0);
            Unlock (S.Read_Lock'Access);
            exit when Result /= 0 or else Look = 2 or else Has (Events, POLLIN + POLLRDNORM) = 0;
            Want (S, Layout.Want_Readable);
         end loop;
         return Result;
      end if;
      for Look in 1 .. 2 loop
         declare
            Status : constant Unsigned_32 := Word_At (S, Layout.Status_At);
            Readable, Writable : Natural;
            Final : constant Boolean := Status >= Layout.Status_Peer_Finished;
            Flags : Unsigned_32 := 0;
         begin
            Lock (S.Read_Lock'Access);
            Readable := Receivable (S);
            Unlock (S.Read_Lock'Access);
            Lock (S.Write_Lock'Access);
            Writable := Sendable (S);
            Unlock (S.Write_Lock'Access);
            Result := 0;
            if not Write_Shut and then (Writable > 0 or else Status_Error (Status) /= 0) then
               Result := Union (Result, Has (Events, POLLOUT + POLLWRNORM));
            end if;
            if Readable > 0 or else Final then
               Result := Union (Result, Has (Events, POLLIN + POLLRDNORM));
            end if;
            if Final then
               Result := Union (Result, Has (Events, POLLRDHUP));
            end if;
            if Status_Error (Status) /= 0 then
               Result := Union (Result, POLLERR + POLLHUP);
            end if;
            exit when Result /= 0 or else Look = 2;
            --  Not ready: ask netstack to tell this process's waiter.
            if Has (Events, POLLIN + POLLRDNORM + POLLRDHUP) /= 0 then
               Flags := Flags or Layout.Want_Readable;
            end if;
            if Has (Events, POLLOUT + POLLWRNORM) /= 0 then
               Flags := Flags or Layout.Want_Writable;
            end if;
            exit when Flags = 0;
            Want (S, Flags);
         end;
      end loop;
      return Result;
   end Tcp_Poll;

   function Tcp_Bind (Socket, Address : System.Address; Length : unsigned) return long is
      S : Socket_Object with Import, Address => Socket;
      Busy : Boolean;
   begin
      if Address = System.Null_Address or else Length < Sockaddr_In_Bytes then
         return Error (EINVAL);
      end if;
      declare
         Local : constant Sockaddr_In with Import, Address => Address;
      begin
         if Unsigned_32 (Local.Family) /= Unsigned_32 (AF_INET) then
            return Error (EAFNOSUPPORT);
         end if;
         Lock (S.Lock'Access);
         Busy := S.State /= Idle or else S.Bound;
         if not Busy then
            S.Local := Local;
            S.Bound := True;
         end if;
         Unlock (S.Lock'Access);
      end;
      return (if Busy then Error (EINVAL) else 0);
   end Tcp_Bind;

   --  Offer a new socket to listener L for its next arriving connection:
   --  one offer record in the listener's send ring.
   function Offer (Listener : Unsigned_64; Slot : Positive) return long;
   function Offer (Listener : Unsigned_64; Slot : Positive) return long is
      L : Socket_Object with Import, Address => To_Address (Listener);
      T_Address : constant Unsigned_64 := Value_Of (Tcp_New);
      R : long;
      Result : CuBit.Datagram_Rings.Put_Result := CuBit.Datagram_Rings.No_Room;
      Ignore : Natural;
   begin
      if T_Address = 0 then
         return Error (ENOMEM);
      end if;
      declare
         T : Socket_Object with Import, Address => To_Address (T_Address);
         Item : Rings.Bytes (0 .. Layout.Offer_Bytes - 1) := [others => 0];
      begin
         T.Slot := L.Slot;                    --  accepted channels are the listen scope's
         R := Prepare_Channel (T_Address);
         if R /= 0 then
            Release (T_Address);
            return R;
         end if;
         for B in 0 .. 7 loop
            Item (Layout.Offer_Arena_At + B) :=
              Unsigned_8 (Shift_Right (Arenas (T.Arena).Handle, 8 * B) and 16#FF#);
         end loop;
         for B in 0 .. 3 loop
            Item (Layout.Offer_Buffer_At + B) := Unsigned_8 (Shift_Right (Unsigned_32 (T.Index), 8 * B) and 16#FF#);
         end loop;
         Lock (L.Write_Lock'Access);
         if Sendable (L) > 0 or else L.Tx.Fill = 0 then
            declare
               Ring : Rings.Bytes (0 .. Ring_Bytes - 1) with Import, Address => To_Address (Send_Ring (L));
            begin
               CuBit.Datagram_Rings.Put (L.Tx, Ring, Item, Result);
            end;
         end if;
         if Result = CuBit.Datagram_Rings.Put then
            Compiler_Barrier;
            Set_Word (L, Layout.Tx_Produced_At, Unsigned_32 (L.Tx.Produced));
            Full_Fence;
            if (Word_At (L, Layout.Kick_Wanted_At) and Layout.Kick_On_Send) /= 0 then
               Kick (L, 0);
            end if;
         end if;
         Unlock (L.Write_Lock'Access);
      end;
      if Result /= CuBit.Datagram_Rings.Put then
         Release (T_Address);
         return Error (ENOBUFS);
      end if;
      Lock (L.Lock'Access);
      L.Offered (Slot) := T_Address;
      Unlock (L.Lock'Access);
      return 0;
   end Offer;

   procedure Refill_Offers (Listener : Unsigned_64);
   procedure Refill_Offers (Listener : Unsigned_64) is
      L : Socket_Object with Import, Address => To_Address (Listener);
      Empty : Boolean;
   begin
      for I in 1 .. Offers loop
         Lock (L.Lock'Access);
         Empty := L.Offered (I) = 0;
         Unlock (L.Lock'Access);
         if Empty and then Offer (Listener, I) /= 0 then
            return;
         end if;
      end loop;
   end Refill_Offers;

   function Tcp_Listen (Socket : System.Address; Backlog : int) return long is
      pragma Unreferenced (Backlog);   --  netstack keeps the backlog
      S : Socket_Object with Import, Address => Socket;
      State : Tcp_State;
      Bound, Listening : Boolean;
      Port : Unsigned_16;
      IPv4 : Octets;
      Host : String (1 .. Targets.Dotted_Bytes);
      Host_Length : Natural;
      Fits : Boolean;
      R : long;
   begin
      Lock (S.Lock'Access);
      State := S.State;
      Bound := S.Bound;
      Listening := S.Listening;
      Unlock (S.Lock'Access);
      if Listening then
         return 0;
      elsif State /= Idle then
         return Error (EINVAL);
      elsif not Bound or else S.Local.Port = 0 then
         return Error (EOPNOTSUPP);      --  a listener needs the port its scope names
      end if;
      Port := Port_Of (S.Local);
      IPv4 := S.Local.IPv4;
      --  A listener names one address, its tcp-listen scope's; INADDR_ANY
      --  takes that.
      if IPv4 = [0, 0, 0, 0] then
         Discover_Scopes;
         for I in 1 .. Scope_Count loop
            if Scopes (I).Action = Listen_TCP and then Port in Scopes (I).First .. Scopes (I).Last then
               IPv4 := IPv4_Of (Scopes (I).Network);
            end if;
         end loop;
      end if;
      S.Slot := Scope_Slot (Listen_TCP, False, Mapped (IPv4), Port);
      if S.Slot < 0 then
         return Error (EACCES);         --  no scope allows it
      end if;
      Targets.Dotted (IPv4, Host, Host_Length);
      Targets.Format (Targets.Listen_Prefix, Host (1 .. Host_Length), Port,
                      S.Target, S.Target_Length, Fits);
      if not Fits then
         return Error (ENAMETOOLONG);
      end if;
      R := Prepare_Channel (Value_Of (Socket));
      if R /= 0 then
         return R;
      end if;
      S.Listening := True;
      R := Open_Channel (Value_Of (Socket), 0);
      if R /= 0 then
         return (if R = Error (ECONNREFUSED) then Error (EADDRINUSE) else R);
      end if;
      Refill_Offers (Value_Of (Socket));
      return 0;
   end Tcp_Listen;

   --  The next arrival on listener L: the offered socket it opened in. 1 if
   --  one was taken, 0 if none, negative on a broken ring.
   function Take_Arrival (Listener : Unsigned_64; Result : out Unsigned_64) return long;
   function Take_Arrival (Listener : Unsigned_64; Result : out Unsigned_64) return long is
      L : Socket_Object with Import, Address => To_Address (Listener);
      A : Rings.Bytes (0 .. Layout.Arrival_Bytes - 1) := [others => 0];
      Got : Natural := 0;
      Cut : Boolean := False;
      Taken : CuBit.Datagram_Rings.Take_Result := CuBit.Datagram_Rings.Empty;
      Found : Unsigned_64 := 0;
      function Long_At (At_Byte : Natural; Bytes : Natural) return Unsigned_64;
      function Long_At (At_Byte : Natural; Bytes : Natural) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         for B in reverse 0 .. Bytes - 1 loop
            V := Shift_Left (V, 8) or Unsigned_64 (A (At_Byte + B));
         end loop;
         return V;
      end Long_At;
   begin
      Result := 0;
      Lock (L.Read_Lock'Access);
      if Receivable (L) > 0 then
         declare
            Ring : constant Rings.Bytes (0 .. Ring_Bytes - 1)
            with Import, Address => To_Address (Receive_Ring (L));
         begin
            CuBit.Datagram_Rings.Take (L.Rx, Ring, A, Got, Cut, Taken);
         end;
         Compiler_Barrier;
         Set_Word (L, Layout.Rx_Consumed_At, Unsigned_32 (L.Rx.Consumed));
      end if;
      Unlock (L.Read_Lock'Access);
      case Taken is
         when CuBit.Datagram_Rings.Empty => return 0;
         when CuBit.Datagram_Rings.Malformed => return Error (EPROTO);
         when CuBit.Datagram_Rings.Taken => null;
      end case;
      if Got /= Layout.Arrival_Bytes or else Cut then
         return Error (EPROTO);
      end if;
      declare
         Channel : constant Unsigned_64 := Long_At (Layout.Arrival_Channel_At, 8);
         Arena_Handle : constant Unsigned_64 := Long_At (Layout.Arrival_Arena_At, 8);
         Index : constant Unsigned_64 := Long_At (Layout.Arrival_Buffer_At, 4);
         Port : constant Unsigned_16 := Unsigned_16 (Long_At (Layout.Arrival_Port_At, 2));
         Peer_Address : Address;
      begin
         for B in Address_Index loop
            Peer_Address (B) := A (Layout.Arrival_Address_At + B);
         end loop;
         if not Matches (Peer_Address, Mapped ([0, 0, 0, 0]), IPv4_Mapped_Prefix) then
            return Error (EPROTO);       --  IPv6: not yet here
         end if;
         Lock (L.Lock'Access);
         for I in 1 .. Offers loop
            if L.Offered (I) /= 0 then
               declare
                  O : Socket_Object with Import, Address => To_Address (L.Offered (I));
               begin
                  if O.Arena >= 0 and then Arenas (O.Arena).Handle = Arena_Handle
                    and then Unsigned_64 (O.Index) = Index
                  then
                     Found := L.Offered (I);
                     L.Offered (I) := 0;
                     exit;
                  end if;
               end;
            end if;
         end loop;
         Unlock (L.Lock'Access);
         if Found = 0 then
            return Error (EPROTO);       --  netstack named a buffer never offered
         end if;
         declare
            T : Socket_Object with Import, Address => To_Address (Found);
         begin
            Lock (T.Lock'Access);
            T.Channel := Channel;
            T.State := Open;
            T.Peer := (Family => Unsigned_16 (AF_INET), Port => Network_Port (Port),
                       IPv4 => IPv4_Of (Peer_Address), Zero => [others => 0]);
            Unlock (T.Lock'Access);
         end;
      end;
      Result := Found;
      return 1;
   end Take_Arrival;

   procedure Copy_Address (From : Sockaddr_In; Address, Length : System.Address);
   procedure Copy_Address (From : Sockaddr_In; Address, Length : System.Address) is
      Size : unsigned with Import, Address => Length;
      Count : constant Storage_Offset := Storage_Offset (unsigned'Min (Size, Sockaddr_In_Bytes));
      Source : Storage_Array (1 .. Sockaddr_In_Bytes) with Import, Address => From'Address;
      Target : Storage_Array (1 .. Count) with Import, Address => Address;
   begin
      Target := Source (1 .. Count);
      Size := Sockaddr_In_Bytes;
   end Copy_Address;

   function Tcp_Accept (Listener, Result, Address, Length : System.Address;
                        Nonblocking : int) return long
   is
      L : Socket_Object with Import, Address => Listener;
      OK : Boolean;
      Sequence : int;
      R : long;
      Taken : Unsigned_64;
      Ready : Natural;
      Out_Socket : System.Address with Import, Address => Result;
   begin
      Lock (L.Lock'Access);
      OK := L.Listening and then L.State = Open;
      Unlock (L.Lock'Access);
      if not OK then
         return Error (EINVAL);
      end if;
      loop
         Sequence := Readiness_Sequence;
         Collect;
         R := Take_Arrival (Value_Of (Listener), Taken);
         if R < 0 then
            return R;
         end if;
         if R > 0 then
            Refill_Offers (Value_Of (Listener));
            Out_Socket := To_Address (Taken);
            if Address /= System.Null_Address and then Length /= System.Null_Address then
               declare
                  T : Socket_Object with Import, Address => To_Address (Taken);
               begin
                  Copy_Address (T.Peer, Address, Length);
               end;
            end if;
            return 0;
         end if;
         Refill_Offers (Value_Of (Listener));        --  after a failed refill
         if Nonblocking /= 0 then
            return Error (EAGAIN);
         end if;
         Want (L, Layout.Want_Readable);
         Lock (L.Read_Lock'Access);
         Ready := Receivable (L);
         Unlock (L.Read_Lock'Access);
         if Ready = 0 then
            Net_Wait (Sequence, Forever, Tcp_Mask (Listener));
         end if;
      end loop;
   end Tcp_Accept;

   function Tcp_Local (Socket, Address, Length : System.Address) return long is
      S : Socket_Object with Import, Address => Socket;
      Local : Sockaddr_In := (Family => Unsigned_16 (AF_INET), others => <>);
   begin
      Lock (S.Lock'Access);
      if S.Bound then
         Local := S.Local;
      end if;
      Unlock (S.Lock'Access);
      Copy_Address (Local, Address, Length);
      return 0;
   end Tcp_Local;

   function Tcp_So_Error (Socket : System.Address) return long is
      S : Socket_Object with Import, Address => Socket;
      Err : int;
      Is_Open : Boolean;
   begin
      Lock (S.Lock'Access);
      Err := (if S.State = Failed then S.Error else 0);
      Is_Open := S.State = Open;
      Unlock (S.Lock'Access);
      if Is_Open then
         Err := Status_Error (Word_At (S, Layout.Status_At));
      end if;
      return long (Err);
   end Tcp_So_Error;

   function Tcp_Peer (Socket, Address, Length : System.Address) return long is
      S : Socket_Object with Import, Address => Socket;
      State : Tcp_State;
   begin
      Lock (S.Lock'Access);
      State := S.State;
      Unlock (S.Lock'Access);
      if State /= Open then
         return Error (ENOTCONN);
      end if;
      Copy_Address (S.Peer, Address, Length);
      return 0;
   end Tcp_Peer;

   function Tcp_Shutdown (Socket : System.Address; How : int) return long is
      S : Socket_Object with Import, Address => Socket;
      First, Is_Open : Boolean;
   begin
      if How = SHUT_WR or else How = SHUT_RDWR then
         Lock (S.Lock'Access);
         First := not S.Write_Shut;
         S.Write_Shut := True;
         Is_Open := S.State = Open;
         Unlock (S.Lock'Access);
         --  FIN once netstack has sent what the ring holds.
         if First and then S.Buffer /= 0 then
            Set_Word (S, Layout.Shut_Write_At, 1);
            if Is_Open then
               Kick (S, 0);
            end if;
         end if;
      end if;
      return 0;
   end Tcp_Shutdown;

   procedure Tcp_Close (Socket : System.Address) is
      S : Socket_Object with Import, Address => Socket;
      Is_Open : Boolean;
      Arrived : Unsigned_64;
   begin
      Lock (S.Lock'Access);
      S.Closing := True;
      Is_Open := S.State = Open;
      Unlock (S.Lock'Access);
      if S.Listening and then Is_Open then
         --  Once SHUT returns no more arrive: close those that did, and
         --  take back the offers (netstack dropped them with it).
         Send_Shut_Now (S);
         while Take_Arrival (Value_Of (Socket), Arrived) = 1 loop
            Tcp_Close (To_Address (Arrived));
         end loop;
         for I in 1 .. Offers loop
            if S.Offered (I) /= 0 then
               Release (S.Offered (I));
            end if;
            S.Offered (I) := 0;
         end loop;
         Release (Value_Of (Socket));
         return;
      end if;
      --  SHUT takes what is left in the send ring, then sends FIN; an OPEN
      --  still in flight is shut when it completes.
      if Is_Open then
         Send_Shut (Value_Of (Socket));
      end if;
      Release (Value_Of (Socket));
   end Tcp_Close;

   ---------------------------------------------------------------------------
   --  getaddrinfo's lookup.
   ---------------------------------------------------------------------------
   --  musl's struct address (src/network/lookup.h).
   type Lookup_Address is record
      Family   : int := 0;
      Scope_Id : unsigned := 0;
      Bytes    : Address := [others => 0];
      Sort_Key : int := 0;
   end record with Convention => C;
   Canonical_Bytes : constant := 256;
   Localhost : constant Octets := [127, 0, 0, 1];

   function Same_Name (A, B : String) return Boolean renames Names.Same;

   function Lookup_Name (Results, Canonical, Name : System.Address;
                         Family, Flags : int) return int
   is
      First : Lookup_Address with Import, Address => Results;
      Canon : String (1 .. Canonical_Bytes) with Import, Address => Canonical;
      Length : Natural := 0;
      Count : int;
      procedure Set_IPv4 (IPv4 : Octets);
      procedure Set_IPv4 (IPv4 : Octets) is
      begin
         First := (Family => int (AF_INET), others => <>);
         First.Bytes (0 .. 3) := [IPv4 (1), IPv4 (2), IPv4 (3), IPv4 (4)];
      end Set_IPv4;
   begin
      Canon (1) := Character'Val (0);
      if Name /= System.Null_Address then
         declare
            Text : constant String (1 .. Canonical_Bytes - 1) with Import, Address => Name;
         begin
            while Length < Text'Length and then Text (Length + 1) /= Character'Val (0) loop
               Length := Length + 1;
            end loop;
            if Length not in 1 .. Canonical_Bytes - 2 then
               return EAI_NONAME;
            end if;
            Canon (1 .. Length) := Text (1 .. Length);
            Canon (Length + 1) := Character'Val (0);
         end;
      end if;
      if long (Family) not in AF_UNSPEC | AF_INET | AF_INET6 then
         return EAI_FAMILY;
      end if;
      --  No name: the wildcard (passive) or loopback address.
      if Name = System.Null_Address then
         Set_IPv4 (if (Unsigned_32'Mod (Flags) and Unsigned_32 (AI_PASSIVE)) /= 0
                   then [0, 0, 0, 0] else Localhost);
         return 1;
      end if;
      Count := Lookup_IP_Literal (Results, Name, Family);
      if Count /= 0 then
         return Count;
      elsif (Unsigned_32'Mod (Flags) and Unsigned_32 (AI_NUMERICHOST)) /= 0
        or else long (Family) = AF_INET6         --  names are IPv4 placeholders
      then
         return EAI_NONAME;
      end if;
      declare
         Text : constant String (1 .. Length) with Import, Address => Name;
         Placeholder : Unsigned_32;
      begin
         if Same_Name (Text, "localhost") or else Same_Name (Text, "localhost.") then
            Set_IPv4 (Localhost);
            return 1;
         end if;
         Placeholder := Name_Address (Text);        --  network order
         if Placeholder = 0 then
            return EAI_MEMORY;
         end if;
         Set_IPv4 ([Unsigned_8 (Placeholder and 16#FF#), Unsigned_8 (Shift_Right (Placeholder, 8) and 16#FF#),
                    Unsigned_8 (Shift_Right (Placeholder, 16) and 16#FF#), Unsigned_8 (Shift_Right (Placeholder, 24))]);
         return 1;
      end;
   end Lookup_Name;

end CuBit.Libc_Net;
