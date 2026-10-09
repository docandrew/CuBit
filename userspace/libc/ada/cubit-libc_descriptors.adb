------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Child_Exits;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Imports;
with CuBit.Libc_Rings; use CuBit.Libc_Rings;
with CuBit.Libc_Directory_Entries;
with CuBit.Libc_Descriptor_Rules; use CuBit.Libc_Descriptor_Rules;
with CuBit.Libc_Time;
with CuBit.Path_Names_C;
with CuBit.Program_Descriptions;
with CuBit.Outlet_Rings;
with CuBit.Grant_References;
with CuBit.Stream_Regions;
with CuBit.Stream_Rings;

package body CuBit.Libc_Descriptors is

   package K renames CuBit.Kernel_ABI;
   package Entries renames CuBit.Libc_Directory_Entries;

   use type Interfaces.C.long;
   use type Interfaces.C.size_t;
   use type Interfaces.C.unsigned_long;

   ---------------------------------------------------------------------------
   --  Other parts of the libc (file.c, net.c, the stream producer) and musl.
   ---------------------------------------------------------------------------
   function File_Open (Path : System.Address; Directory : int; Options : Unsigned_64;
                       Handle, Size : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_file_open";
   procedure File_Close (Handle : Unsigned_64; Directory : int)
   with Import, Convention => C, External_Name => "__cubit_file_close";
   function File_Read_At (Handle : Unsigned_64; Buffer : System.Address;
                          Count : size_t; Offset : Unsigned_64) return long
   with Import, Convention => C, External_Name => "__cubit_file_read_at";
   function File_Write_At (Handle : Unsigned_64; Buffer : System.Address;
                           Count : size_t; Offset : Unsigned_64) return long
   with Import, Convention => C, External_Name => "__cubit_file_write_at";
   function File_Flush (Handle : Unsigned_64) return long
   with Import, Convention => C, External_Name => "__cubit_file_flush";
   function Directory_Read_Page (Handle : Unsigned_64; Page : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_dir_read_page";
   function File_Describe (Handle : Unsigned_64; Inspection : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_file_describe";
   function File_Resize (Handle : Unsigned_64; Size : Unsigned_64) return long
   with Import, Convention => C, External_Name => "__cubit_file_resize";
   function Resolve (Path, Result : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_resolve";
   function Change_Directory (Path : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_chdir";

   function Tcp_New return System.Address
   with Import, Convention => C, External_Name => "__cubit_tcp_new";
   procedure Tcp_Close (Socket : System.Address)
   with Import, Convention => C, External_Name => "__cubit_tcp_close";
   function Tcp_Accept (Listener, Result, Address, Length : System.Address;
                        Nonblocking : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_accept";
   function Tcp_Read (Socket, Buffer : System.Address; Count : size_t;
                      Nonblocking : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_read";
   function Tcp_Write (Socket, Vectors : System.Address; Count : int;
                       Nonblocking : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_write";
   function Tcp_Poll (Socket : System.Address; Events : Integer_16) return Integer_16
   with Import, Convention => C, External_Name => "__cubit_tcp_poll";
   function Tcp_Mask (Socket : System.Address) return Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_tcp_mask";
   procedure Net_Wait (Sequence : int; Deadline : unsigned_long; Sockets : Unsigned_64)
   with Import, Convention => C, External_Name => "__cubit_net_wait";
   procedure Net_Interrupt
   with Import, Convention => C, External_Name => "__cubit_net_interrupt";

   procedure Stream_Create (Stream : Unsigned_16; Pages : Interfaces.C.unsigned;
                            Type_Tag : Unsigned_16)
   with Import, Convention => C, External_Name => "cubit_stream_create";
   procedure Stream_Adopt (Stream : Unsigned_16; Pages : Interfaces.C.unsigned;
                           Base : System.Address)
   with Import, Convention => C, External_Name => "cubit_stream_adopt";
   --  SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT's write access.
   Grant_Write : constant := 1;
   function Stream_Write (Stream : Unsigned_16; Data : System.Address;
                          Length : Unsigned_32; Type_Tag : Unsigned_16) return Unsigned_32
   with Import, Convention => C, External_Name => "cubit_stream_write";
   function Stream_Handle_Message (From : long; Message : System.Address) return int
   with Import, Convention => C, External_Name => "cubit_stream_handle_message";
   Stream_Poll_On_Write : int
   with Import, Convention => C, External_Name => "cubit_stream_poll_on_write";

   function Calloc (Count, Size : size_t) return System.Address
   with Import, Convention => C, External_Name => "calloc";
   procedure Free (Memory : System.Address)
   with Import, Convention => C, External_Name => "free";

   type Thread_Start is access function (Argument : System.Address)
     return System.Address with Convention => C;
   function Pthread_Attr_Init (Attributes : System.Address) return int
   with Import, Convention => C, External_Name => "pthread_attr_init";
   function Pthread_Attr_Set_Detach_State (Attributes : System.Address; State : int)
     return int
   with Import, Convention => C, External_Name => "pthread_attr_setdetachstate";
   function Pthread_Attr_Set_Stack_Size (Attributes : System.Address; Size : size_t)
     return int
   with Import, Convention => C, External_Name => "pthread_attr_setstacksize";
   function Pthread_Create (Thread, Attributes : System.Address; Start : Thread_Start;
                            Argument : System.Address) return int
   with Import, Convention => C, External_Name => "pthread_create";
   function Pthread_Attr_Destroy (Attributes : System.Address) return int
   with Import, Convention => C, External_Name => "pthread_attr_destroy";

   Sequentially_Consistent : constant int := 5;     --  __ATOMIC_SEQ_CST
   function Atomic_Add (Where : System.Address; Value : Unsigned_32; Order : int)
     return Unsigned_32
   with Import, Convention => Intrinsic, External_Name => "__atomic_add_fetch_4";
   function Atomic_Exchange (Where : System.Address; Value : Unsigned_32; Order : int)
     return Unsigned_32
   with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_4";
   Minus_One : constant Unsigned_32 := Unsigned_32'Last;   --  adds as -1

   ---------------------------------------------------------------------------
   --  The table.
   ---------------------------------------------------------------------------
   Maximum_Descriptors : constant := 1_024;
   subtype Descriptor is Natural range 0 .. Maximum_Descriptors - 1;
   First_Free_Descriptor : constant := 3;     --  after the standard ones

   --  Unset (zero, as the table starts): a descriptor the manifest maps
   --  onto an outlet is then that connector's stream, any other is free
   --  (Settle). None: closed or free.
   type Kinds is (Unset, None, Stream_Out, File, Directory, Pipe_Read, Pipe_Write,
                  Socket_Pair, Tcp);

   --  CuBit.Streams' entry types.
   Type_Raw_Bytes  : constant := CuBit.Stream_Rings.ELEMENT_RAW_BYTES;
   Type_Text_Line  : constant := CuBit.Stream_Rings.ELEMENT_TEXT_LINE;

   --  The descriptor map (Adopt_Ports): for each descriptor number, the
   --  ring of the outlet it writes (0: none), its pages and entry
   --  type. Zero-filled (.bss).
   type Port_Entry is record
      Ring : Unsigned_16 := 0;
      Pages : Interfaces.C.unsigned := 0;
      Type_Tag : Unsigned_16 := 0;
   end record;
   Ports : array (CuBit.Program_Descriptions.Descriptor_Number) of Port_Entry;
   pragma Suppress_Initialization (Ports);
   Maximum_Record  : constant := 4_096;
   Dispatcher_Stack_Bytes : constant := 64 * 1_024;

   --  Pointers (to a listing, a name, rings, a socket) as integers, so the
   --  table is a static aggregate with no elaboration code.
   type Descriptor_Entry is record
      Kind          : Kinds := None;      --  None when set by an aggregate
      Stream        : Unsigned_16 := 0;
      Created       : Boolean := False;
      Handle        : Unsigned_64 := 0;     --  filesystem.svc handle
      Offset, Size  : Unsigned_64 := 0;
      Close_On_Exec : Boolean := False;
      Flags         : long := 0;
      Listing       : Unsigned_64 := 0;     --  Directory: its Listing
      Name          : Unsigned_64 := 0;     --  Directory: its CuBit name
      Ring          : Unsigned_64 := 0;     --  the ring this end reads
      Peer          : Unsigned_64 := 0;     --  Pair: the ring it writes
      Socket        : Unsigned_64 := 0;     --  Tcp
   end record;

   --  Zero-filled (.bss): every entry starts Unset.
   Table : array (Descriptor) of Descriptor_Entry;
   pragma Suppress_Initialization (Table);

   --  An Unset entry as what it stands for: the standard stream of fd 1
   --  or 2, else free.
   procedure Settle (Fd : Descriptor);
   procedure Settle (Fd : Descriptor) is
   begin
      if Table (Fd).Kind = Unset then
         Table (Fd) :=
           (Kind => (if Fd <= Ports'Last and then Ports (Fd).Ring /= 0 then Stream_Out else None),
            Stream => (if Fd <= Ports'Last then Ports (Fd).Ring else 0),
            Created => False, Handle | Offset | Size => 0, Close_On_Exec => False,
            Flags => 0, Listing | Name | Ring | Peer | Socket => 0);
      end if;
   end Settle;

   --  A outlet ring's size and entry type, from the descriptor map.
   function Pages_Of (Ring : Unsigned_16) return Interfaces.C.unsigned;
   function Pages_Of (Ring : Unsigned_16) return Interfaces.C.unsigned is
   begin
      for P of Ports loop
         if P.Ring = Ring then
            return P.Pages;
         end if;
      end loop;
      return 1;
   end Pages_Of;

   function Type_Of (Ring : Unsigned_16) return Unsigned_16;
   function Type_Of (Ring : Unsigned_16) return Unsigned_16 is
   begin
      for P of Ports loop
         if P.Ring = Ring then
            return P.Type_Tag;
         end if;
      end loop;
      return Type_Raw_Bytes;
   end Type_Of;

   procedure Adopt_Ports (Trailer : System.Address; Length : Natural) is
      package PD renames CuBit.Program_Descriptions;
      package PR renames CuBit.Outlet_Rings;
      use type PD.Connector_Direction;
      use type PD.Element_Kind;
      S : PD.Signature;
      Rings : PR.Table;
      Accepted, Present, Rings_Valid : Boolean := False;
      Ring_Length : PR.Table_Length := 0;
   begin
      for P of Ports loop
         P := (others => <>);
      end loop;
      if Length = 0 or else Length > PR.Maximum_Bytes + PD.Maximum_Descriptor_Bytes then
         return;
      end if;
      declare
         Item : constant PR.Bytes (1 .. Length) with Import, Address => Trailer;
      begin
         PR.Measure (Item, Present, Ring_Length);
         if Present then
            PR.Decode (Item (1 .. Ring_Length), Rings, Rings_Valid);
         end if;
         if Length - Ring_Length not in 1 .. PD.Maximum_Descriptor_Bytes then
            return;
         end if;
         declare
            Description : constant PD.Bytes (1 .. Length - Ring_Length)
            with Import, Address => Item (Ring_Length + 1)'Address;
         begin
            PD.Decode (Description, S, Accepted);
         end;
      end;
      if not Accepted then
         return;
      end if;
      for D of S.Descriptors (1 .. S.Descriptor_Total) loop
         if D.Number > 0 and then S.Connectors (D.Target).Direction = PD.Outlet then
            Ports (D.Number) :=
              (Ring => PD.Ring_Id (D.Target),
               Pages => Interfaces.C.unsigned (S.Connectors (D.Target).Pages),
               Type_Tag => (if S.Connectors (D.Target).Element = PD.Text_Lines
                            then Type_Text_Line else Type_Raw_Bytes));
         end if;
      end loop;
      --  Rings the launcher lent: produce into them (Stream_Create then
      --  finds them and makes none).
      if Rings_Valid then
         for E of Rings.Entries (1 .. Rings.Count) loop
            if E.Outlet < S.Connector_Total and then S.Connectors (E.Outlet).Direction = PD.Outlet then
               declare
                  Pages : constant Unsigned_64 := Unsigned_64 (S.Connectors (E.Outlet).Pages);
                  Reference : constant CuBit.Grant_References.Reference :=
                    (if CuBit.Grant_References.Valid_Wire (E.Grant)
                     then CuBit.Grant_References.Decode (E.Grant) else (others => <>));
                  Mapped : constant Unsigned_64 :=
                    (if CuBit.Grant_References.Valid_Wire (E.Grant)
                     then CuBit.Kernel_Calls.Call
                       (K.Acquire_Shared_Memory_Grant, Reference.slot, Reference.generation,
                        Rings.Owner, 0, CuBit.Stream_Regions.Region_Bytes (Natural (Pages)),
                        Grant_Write)
                     else K.Failed);
               begin
                  if Mapped /= K.Failed then
                     Stream_Adopt (PD.Ring_Id (E.Outlet), Interfaces.C.unsigned (Pages),
                                   To_Address (Integer_Address (Mapped)));
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Adopt_Ports;

   function Is_Free (Fd : Descriptor) return Boolean;
   function Is_Free (Fd : Descriptor) return Boolean is
   begin
      Settle (Fd);
      return Table (Fd).Kind = None;
   end Is_Free;

   --  Descriptor allocation and each file's offset; the stream table.
   Table_Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;
   Stream_Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;

   procedure Lock (Word : access CuBit.Libc_Imports.Lock_Word)
     renames CuBit.Libc_Imports.Lock;
   procedure Unlock (Word : access CuBit.Libc_Imports.Lock_Word)
     renames CuBit.Libc_Imports.Unlock;

   --  A pipe or socket-pair ring, shared by its ends (calloc'd).
   type Ring_Object is record
      Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;
      R    : Ring;
   end record;
   Ring_Object_Bytes : constant size_t := Ring_Object'Size / 8;

   --  A directory being listed: one page and the next entry.
   type Listing is record
      Page   : Entries.Page;
      Next   : Natural := 0;
      Count  : Natural := 0;
      Loaded : Boolean := False;
      Ended  : Boolean := False;
   end record;
   Listing_Bytes : constant size_t := Listing'Size / 8;

   --  Directory.Inspection.V1, as file.c fills it.
   type Inspection is record
      Valid, Mode : Unsigned_32 := 0;
      Size, Modified_Ms, Changed_Ms, Accessed_Ms : Unsigned_64 := 0;
      Links, Owner, Group, Reserved : Unsigned_32 := 0;
      Object : Unsigned_64 := 0;
   end record with Convention => C;

   --  struct stat (musl, x86-64; tests/libc-ada checks the field order).
   type Unused_Words is array (1 .. 3) of Integer_64;
   type Stat_Record is record
      Device, Inode, Links : Unsigned_64;
      Mode, Owner, Group, Padding : Unsigned_32;
      Special_Device : Unsigned_64;
      Size, Block_Size, Blocks : Integer_64;
      Accessed, Modified, Changed : Timespec;
      Unused : Unused_Words;
   end record with Convention => C;
   Stat_Bytes : constant := 144;
   pragma Compile_Time_Error (Stat_Record'Size /= Stat_Bytes * 8, "struct stat");
   Default_Block_Size : constant := 4_096;
   Volume_Shift : constant := 32;    --  an inspection's object: volume << 32 | inode

   function To_Long is new Ada.Unchecked_Conversion (Unsigned_64, long);

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));
   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));
   function Error (Value : int) return long is (-long (Value));

   function Valid_Descriptor (Fd : int) return Boolean;
   function Valid_Descriptor (Fd : int) return Boolean is
   begin
      if Fd not in 0 .. Maximum_Descriptors - 1 then
         return False;
      end if;
      Settle (Integer (Fd));
      return Table (Integer (Fd)).Kind /= None;
   end Valid_Descriptor;

   function Nonblocking (E : Descriptor_Entry) return int is
     (if Has (E.Flags, O_NONBLOCK) then 1 else 0);

   ---------------------------------------------------------------------------
   --  Readiness.
   ---------------------------------------------------------------------------
   Events : aliased int := 0 with Volatile;
   --  Threads waiting on Events: the wake is skipped when there are none.
   --  Both counters change with locked (fully ordered) instructions, so
   --  either the waker sees the waiter or the waiter's futex check sees the
   --  new count and does not sleep.
   Waiters : aliased int := 0 with Volatile;

   procedure Readiness_Changed is
      Ignore : Unsigned_32;
      Ignore_Woken : Unsigned_64;
   begin
      Ignore := Atomic_Add (Events'Address, 1, Sequentially_Consistent);
      if Waiters /= 0 then
         Ignore_Woken := CuBit.Kernel_Calls.Call
           (K.Futex_Wake, Value_Of (Events'Address), K.Forever);
      end if;
      Net_Interrupt;
   end Readiness_Changed;

   function Readiness_Sequence return int is (Events);

   --  Wait until Events moves on from Sequence, or the deadline (kernel
   --  milliseconds; Forever: none) passes.
   procedure Readiness_Wait (Sequence : int; Deadline : unsigned_long) is
      Ignore : Unsigned_32;
      Ignore_Result : Unsigned_64;
   begin
      Ignore := Atomic_Add (Waiters'Address, 1, Sequentially_Consistent);
      Ignore_Result := CuBit.Kernel_Calls.Call
        (K.Futex_Wait, Value_Of (Events'Address),
         Unsigned_64 (Unsigned_32'Mod (Sequence)), Unsigned_64 (Deadline));
      Ignore := Atomic_Add (Waiters'Address, Minus_One, Sequentially_Consistent);
   end Readiness_Wait;

   ---------------------------------------------------------------------------
   --  The mailbox dispatcher: every message the process receives; stream
   --  subscriptions are served, anything else is not this libc's yet.
   ---------------------------------------------------------------------------
   procedure Note_Event (Item : System.Address)
   with Import, Convention => C, External_Name => "__cubit_note_event";

   function Dispatcher (Unused : System.Address) return System.Address
   with Convention => C;
   function Dispatcher (Unused : System.Address) return System.Address is
      M : aliased K.Message;
      From : Unsigned_64;
      Ignore : int;
      Ignore_Refused : Unsigned_64;
   begin
      loop
         M := (others => <>);
         From := CuBit.Kernel_Calls.Call (K.Receive, Value_Of (M'Address));
         --  Receive takes events too: a child's exit goes to waitpid's
         --  bookkeeping (it would otherwise be lost to the stream handler).
         --  Only the kernel's (no sender): a client's call with that label is
         --  not one.
         if M.Label = CuBit.Child_Exits.Event_Label and then From = 0 then
            Note_Event (M'Address);
         else
            Lock (Stream_Lock'Access);
            Ignore := Stream_Handle_Message (To_Long (From), M'Address);
            Unlock (Stream_Lock'Access);
            --  A request this libc does not serve is refused, never left
            --  waiting for a reply that would not come.
            if Ignore = 0 and then From /= 0 then
               Ignore_Refused := CuBit.Kernel_Calls.Call (K.Reply, From, Unsigned_64 (K.Reply_Error));
            end if;
         end if;
         exit when False;                --  the thread serves until exit
      end loop;
      return System.Null_Address;
   end Dispatcher;

   Dispatcher_Started : aliased int := 0 with Volatile;
   function Dispatcher_Running return int is (Dispatcher_Started)
   with Export, Convention => C, External_Name => "__cubit_dispatcher_running";
   procedure Start_Dispatcher;
   procedure Start_Dispatcher is
      Attributes : Storage_Array (1 .. Pthread_Attribute_Bytes);
      Thread : aliased Interfaces.C.unsigned_long := 0;
      Ignore : int;
   begin
      if Atomic_Exchange (Dispatcher_Started'Address, 1, Sequentially_Consistent) /= 0 then
         return;
      end if;
      Ignore := Pthread_Attr_Init (Attributes'Address);
      Ignore := Pthread_Attr_Set_Detach_State (Attributes'Address, PTHREAD_CREATE_DETACHED);
      Ignore := Pthread_Attr_Set_Stack_Size (Attributes'Address, Dispatcher_Stack_Bytes);
      Ignore := Pthread_Create (Thread'Address, Attributes'Address,
                                Dispatcher'Access, System.Null_Address);
      Ignore := Pthread_Attr_Destroy (Attributes'Address);
   end Start_Dispatcher;

   ---------------------------------------------------------------------------
   --  Allocation.
   ---------------------------------------------------------------------------
   --  The lowest free descriptor from From, claimed as Kind; -EMFILE if none.
   function Allocate (Kind : Kinds) return long;
   function Allocate (Kind : Kinds) return long is
   begin
      Lock (Table_Lock'Access);
      for Fd in First_Free_Descriptor .. Descriptor'Last loop
         if Is_Free (Fd) then
            Table (Fd) := (Kind => Kind, others => <>);
            Unlock (Table_Lock'Access);
            return long (Fd);
         end if;
      end loop;
      Unlock (Table_Lock'Access);
      return Error (EMFILE);
   end Allocate;

   function New_Ring return Unsigned_64;
   function New_Ring return Unsigned_64 is
      Memory : constant System.Address := Calloc (1, Ring_Object_Bytes);
   begin
      return Value_Of (Memory);
   end New_Ring;

   procedure Release_Ring (Address : Unsigned_64; Reader : Boolean);
   procedure Release_Ring (Address : Unsigned_64; Reader : Boolean) is
      Object : Ring_Object with Import, Address => To_Address (Address);
      Last : Boolean;
   begin
      Lock (Object.Lock'Access);
      if Reader then
         Object.R.Readers := Object.R.Readers - 1;
      else
         Object.R.Writers := Object.R.Writers - 1;
      end if;
      Last := Object.R.Readers = 0 and then Object.R.Writers = 0;
      Unlock (Object.Lock'Access);
      if Last then
         Free (To_Address (Address));
      end if;
   end Release_Ring;

   procedure Set_Pair (Pair : System.Address; First, Second : long);
   procedure Set_Pair (Pair : System.Address; First, Second : long) is
      type Two is array (1 .. 2) of int;
      Result : Two with Import, Address => Pair;
   begin
      Result := [int (First), int (Second)];
   end Set_Pair;

   function Pipe (Pair : System.Address; Flags : int) return long is
      R : constant Unsigned_64 := New_Ring;
      Reader, Writer : long;
   begin
      if R = 0 then
         return Error (ENOMEM);
      end if;
      Reader := Allocate (Pipe_Read);
      if Reader < 0 then
         Free (To_Address (R));
         return Reader;
      end if;
      Writer := Allocate (Pipe_Write);
      if Writer < 0 then
         Table (Integer (Reader)).Kind := None;
         Free (To_Address (R));
         return Writer;
      end if;
      declare
         Object : Ring_Object with Import, Address => To_Address (R);
      begin
         Object.R.Readers := 1;
         Object.R.Writers := 1;
      end;
      for End_Fd in 1 .. 2 loop
         declare
            E : Descriptor_Entry renames
              Table (Integer (if End_Fd = 1 then Reader else Writer));
         begin
            E.Ring := R;
            E.Flags := To_Long ((Bits (long (Flags)) and Bits (O_NONBLOCK))
                                 or Bits (if End_Fd = 1 then O_RDONLY else O_WRONLY));
            E.Close_On_Exec := Has (long (Flags), O_CLOEXEC);
         end;
      end loop;
      Set_Pair (Pair, Reader, Writer);
      return 0;
   end Pipe;

   --  socketpair(AF_UNIX, SOCK_STREAM): end 0 reads ring A and writes ring
   --  B, end 1 the reverse.
   function Socketpair (Pair : System.Address; Flags : int) return long is
      A : constant Unsigned_64 := New_Ring;
      B : constant Unsigned_64 := New_Ring;
      X, Y : long;
   begin
      if A = 0 or else B = 0 then
         Free (To_Address (A));
         Free (To_Address (B));
         return Error (ENOMEM);
      end if;
      X := Allocate (Socket_Pair);
      if X < 0 then
         Free (To_Address (A));
         Free (To_Address (B));
         return X;
      end if;
      Y := Allocate (Socket_Pair);
      if Y < 0 then
         Table (Integer (X)).Kind := None;
         Free (To_Address (A));
         Free (To_Address (B));
         return Y;
      end if;
      for Each in 1 .. 2 loop
         declare
            Object : Ring_Object with Import,
              Address => To_Address (if Each = 1 then A else B);
         begin
            Object.R.Readers := 1;
            Object.R.Writers := 1;
         end;
      end loop;
      Table (Integer (X)).Ring := A;
      Table (Integer (X)).Peer := B;
      Table (Integer (Y)).Ring := B;
      Table (Integer (Y)).Peer := A;
      for Each in 1 .. 2 loop
         declare
            Fd : constant Integer := Integer (if Each = 1 then X else Y);
         begin
            Table (Fd).Flags :=
              To_Long ((Bits (long (Flags)) and Bits (O_NONBLOCK)) or Bits (O_RDWR));
            Table (Fd).Close_On_Exec := Has (long (Flags), O_CLOEXEC);
         end;
      end loop;
      Set_Pair (Pair, X, Y);
      return 0;
   end Socketpair;

   ---------------------------------------------------------------------------
   --  Pipes and socket pairs.
   ---------------------------------------------------------------------------
   function Ring_Write (E : Descriptor_Entry; Vectors : System.Address; Count : int)
     return long;
   function Ring_Write (E : Descriptor_Entry; Vectors : System.Address; Count : int)
     return long
   is
      Target : constant Unsigned_64 := (if E.Kind = Socket_Pair then E.Peer else E.Ring);
      Object : Ring_Object with Import, Address => To_Address (Target);
      type Vector_Array is array (1 .. IOV_MAX) of Io_Vector;
      V : constant Vector_Array with Import, Address => Vectors;
      Wanted : Unsigned_64 := 0;
      Done : Natural;
      Sequence : int;
   begin
      if Count not in 0 .. IOV_MAX then
         return Error (EINVAL);
      end if;
      for I in 1 .. Integer (Count) loop
         Wanted := Wanted + V (I).Length;
      end loop;
      if Wanted = 0 then
         return 0;
      end if;
      loop
         Sequence := Events;
         Done := 0;
         Lock (Object.Lock'Access);
         if Object.R.Readers = 0 then
            Unlock (Object.Lock'Access);
            return Error (EPIPE);
         end if;
         --  Copy what fits, in order: a short write is allowed.
         for I in 1 .. Integer (Count) loop
            declare
               Chunk : constant Natural :=
                 Natural (Unsigned_64'Min (V (I).Length, Ring_Bytes));
               Source : constant Bytes (1 .. Chunk)
               with Import, Address => To_Address (V (I).Base);
               Put_Count : Ring_Count;
            begin
               Put (Object.R, Source, Put_Count);
               Done := Done + Put_Count;
               exit when Unsigned_64 (Put_Count) < V (I).Length;
            end;
         end loop;
         Unlock (Object.Lock'Access);
         if Done > 0 then
            Readiness_Changed;
            return long (Done);
         elsif Has (E.Flags, O_NONBLOCK) then
            return Error (EAGAIN);
         end if;
         Readiness_Wait (Sequence, unsigned_long (K.Forever));
      end loop;
   end Ring_Write;

   function Ring_Read (E : Descriptor_Entry; Buffer : System.Address; Count : size_t)
     return long;
   function Ring_Read (E : Descriptor_Entry; Buffer : System.Address; Count : size_t)
     return long
   is
      Object : Ring_Object with Import, Address => To_Address (E.Ring);
      Wanted : constant Natural := Natural (size_t'Min (Count, Ring_Bytes));
      Target : Bytes (1 .. Wanted) with Import, Address => Buffer;
      Got : Ring_Count;
      Ended : Boolean;
      Sequence : int;
   begin
      if Wanted = 0 then
         return 0;
      end if;
      loop
         Sequence := Events;
         Lock (Object.Lock'Access);
         if Object.R.Length > 0 then
            Take (Object.R, Target, Got);
            Unlock (Object.Lock'Access);
            Readiness_Changed;
            return long (Got);
         end if;
         Ended := Object.R.Writers = 0;
         Unlock (Object.Lock'Access);
         if Ended then
            return 0;
         elsif Has (E.Flags, O_NONBLOCK) then
            return Error (EAGAIN);
         end if;
         Readiness_Wait (Sequence, unsigned_long (K.Forever));
      end loop;
   end Ring_Read;

   ---------------------------------------------------------------------------
   --  Sockets.
   ---------------------------------------------------------------------------
   function Socket_Tcp (Flags : int) return long is
      Socket : constant System.Address := Tcp_New;
      Fd : long;
   begin
      if Socket = System.Null_Address then
         return Error (ENOMEM);
      end if;
      Fd := Allocate (Tcp);
      if Fd < 0 then
         Tcp_Close (Socket);
         return Fd;
      end if;
      Table (Integer (Fd)).Socket := Value_Of (Socket);
      Table (Integer (Fd)).Flags :=
        To_Long ((Bits (long (Flags)) and Bits (O_NONBLOCK)) or Bits (O_RDWR));
      Table (Integer (Fd)).Close_On_Exec := Has (long (Flags), O_CLOEXEC);
      return Fd;
   end Socket_Tcp;

   function Accept_Connection (Fd : int; Address, Length : System.Address; Flags : int)
     return long
   is
      Blocking : aliased int := 0;
      Listener : constant System.Address := Tcp_Of (Fd, Blocking'Address);
      Accepted : aliased System.Address := System.Null_Address;
      R : long;
      New_Fd : long;
   begin
      if Listener = System.Null_Address then
         return (if Is_Socket (Fd) /= 0 then Error (EOPNOTSUPP) else Error (ENOTSOCK));
      end if;
      R := Tcp_Accept (Listener, Accepted'Address, Address, Length, Blocking);
      if R /= 0 then
         return R;
      end if;
      New_Fd := Allocate (Tcp);
      if New_Fd < 0 then
         Tcp_Close (Accepted);
         return New_Fd;
      end if;
      Table (Integer (New_Fd)).Socket := Value_Of (Accepted);
      Table (Integer (New_Fd)).Flags :=
        To_Long ((Bits (long (Flags)) and Bits (O_NONBLOCK)) or Bits (O_RDWR));
      Table (Integer (New_Fd)).Close_On_Exec := Has (long (Flags), O_CLOEXEC);
      return New_Fd;
   end Accept_Connection;

   function Tcp_Of (Fd : int; Nonblocking : System.Address) return System.Address is
   begin
      if not Valid_Descriptor (Fd) or else Table (Integer (Fd)).Kind /= Tcp then
         return System.Null_Address;
      end if;
      if Nonblocking /= System.Null_Address then
         declare
            Result : int with Import, Address => Nonblocking;
         begin
            Result := CuBit.Libc_Descriptors.Nonblocking (Table (Integer (Fd)));
         end;
      end if;
      return To_Address (Table (Integer (Fd)).Socket);
   end Tcp_Of;

   function Is_Socket (Fd : int) return int is
     (if Valid_Descriptor (Fd) and then Table (Integer (Fd)).Kind in Tcp | Socket_Pair
      then 1 else 0);

   ---------------------------------------------------------------------------
   --  Files and directories.
   ---------------------------------------------------------------------------
   function Open (Path : System.Address; Flags : int) return long is
      Options : Unsigned_64;
      Valid : Boolean;
      Directory_Wanted : Boolean := Has (long (Flags), O_DIRECTORY);
      Handle, Size : aliased Unsigned_64 := 0;
      R : long;
      Fd : long;
      List, Name : Unsigned_64 := 0;
   begin
      Open_Options (long (Flags), Options, Valid);
      if not Valid then
         return Error (EINVAL);
      elsif Directory_Wanted and then Options /= OPEN_READ_ONLY then
         return Error (EISDIR);                 --  directories are read-only
      end if;
      R := File_Open (Path, (if Directory_Wanted then 1 else 0), Options,
                      Handle'Address, Size'Address);
      if R = Error (ENOTDIR) and then not Directory_Wanted
        and then not Has (long (Flags), O_NOFOLLOW) and then Options = OPEN_READ_ONLY
      then
         --  open(2) of a directory without O_DIRECTORY also succeeds.
         Directory_Wanted := True;
         R := File_Open (Path, 1, Options, Handle'Address, Size'Address);
      end if;
      if R /= 0 then
         return R;
      end if;
      if Directory_Wanted then
         declare
            Resolved : String (1 .. PATH_MAX);
            Length : constant long := Resolve (Path, Resolved'Address);
         begin
            List := Value_Of (Calloc (1, Listing_Bytes));
            if Length > 0 and then Length < PATH_MAX then
               Name := Value_Of (Calloc (1, size_t (Length) + 1));
               if Name /= 0 then
                  declare
                     Copy : String (1 .. Natural (Length))
                     with Import, Address => To_Address (Name);
                  begin
                     Copy := Resolved (1 .. Natural (Length));
                  end;
               end if;
            end if;
            if List = 0 or else Name = 0 then
               File_Close (Handle, 1);
               Free (To_Address (List));
               Free (To_Address (Name));
               return Error (ENOMEM);
            end if;
         end;
      end if;
      Fd := Allocate (if Directory_Wanted then Directory else File);
      if Fd < 0 then
         File_Close (Handle, (if Directory_Wanted then 1 else 0));
         Free (To_Address (List));
         Free (To_Address (Name));
         return Fd;
      end if;
      Table (Integer (Fd)).Name := Name;
      Table (Integer (Fd)).Handle := Handle;
      Table (Integer (Fd)).Size := Size;
      Table (Integer (Fd)).Flags := long (Flags);
      Table (Integer (Fd)).Close_On_Exec := Has (long (Flags), O_CLOEXEC);
      Table (Integer (Fd)).Listing := List;
      return Fd;
   end Open;

   --  The path a directory-relative call means: Path itself when it is
   --  absolute or Directory is AT_FDCWD (relative names then start from
   --  the working directory), else Path under the directory, resolved
   --  into Buffer (CuBit.Path_Names, the directory's name as the base).
   function At_Path (Directory : int; Path, Buffer : System.Address; Size : size_t;
                     Result : System.Address) return long
   is
      Out_Path : System.Address with Import, Address => Result;
      First : constant Character with Import, Address => Path;
   begin
      if Path = System.Null_Address then
         return Error (EFAULT);
      elsif long (Directory) = AT_FDCWD or else First = '/' or else First = '@' then
         Out_Path := Path;
         return 0;
      elsif First = Character'Val (0) then
         return Error (ENOENT);
      elsif not Valid_Descriptor (Directory) then
         return Error (EBADF);
      elsif Table (Integer (Directory)).Kind /= CuBit.Libc_Descriptors.Directory
        or else Table (Integer (Directory)).Name = 0
      then
         return Error (ENOTDIR);
      elsif Size < 2 then
         return Error (ENAMETOOLONG);
      end if;
      declare
         Base_Address : constant System.Address := To_Address (Table (Integer (Directory)).Name);
         Base : constant String (1 .. PATH_MAX) with Import, Address => Base_Address;
         Base_Length : Natural := 0;
         Length : long;
      begin
         while Base_Length < PATH_MAX and then Base (Base_Length + 1) /= Character'Val (0) loop
            Base_Length := Base_Length + 1;
         end loop;
         Length := CuBit.Path_Names_C.Resolve
           (Base_Address, size_t (Base_Length), Path, Buffer, Size - 1);
         if Length < 0 then
            return Length;
         end if;
         declare
            Text : String (1 .. Natural (Length) + 1) with Import, Address => Buffer;
         begin
            Text (Text'Last) := Character'Val (0);
         end;
         Out_Path := Buffer;
         return 0;
      end;
   end At_Path;

   function Fchdir (Fd : int) return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      elsif Table (Integer (Fd)).Kind /= Directory or else Table (Integer (Fd)).Name = 0 then
         return Error (ENOTDIR);
      end if;
      return Change_Directory (To_Address (Table (Integer (Fd)).Name));
   end Fchdir;

   procedure Fill_From (S : in out Stat_Record; I : Inspection);
   procedure Fill_From (S : in out Stat_Record; I : Inspection) is
   begin
      if (I.Valid and INSPECTED_SIZE) /= 0 then
         S.Size := Integer_64 (Unsigned_64'Min (I.Size, Unsigned_64 (Integer_64'Last)));
         S.Blocks := Integer_64 (Blocks (I.Size));
      end if;
      if (I.Valid and INSPECTED_TIMES) /= 0 then
         S.Modified := CuBit.Libc_Time.From_Milliseconds (I.Modified_Ms);
         S.Changed := CuBit.Libc_Time.From_Milliseconds (I.Changed_Ms);
         S.Accessed := CuBit.Libc_Time.From_Milliseconds (I.Accessed_Ms);
      end if;
      --  The stored type and permission bits (S_IFMT and 07777, as ext2's).
      if (I.Valid and INSPECTED_MODE) /= 0 then
         S.Mode := I.Mode;
      end if;
      if (I.Valid and INSPECTED_LINKS) /= 0 then
         S.Links := Unsigned_64 (I.Links);
      end if;
      if (I.Valid and INSPECTED_OWNER) /= 0 then
         S.Owner := I.Owner;
         S.Group := I.Group;
      end if;
      if (I.Valid and INSPECTED_OBJECT) /= 0 then
         S.Device := Shift_Right (I.Object, Volume_Shift);
         S.Inode := I.Object and 16#FFFF_FFFF#;
      end if;
   end Fill_From;

   function Empty_Stat return Stat_Record is
     ((Device | Inode | Special_Device => 0, Links => 1,
       Mode | Owner | Group | Padding => 0,
       Size | Blocks => 0, Block_Size => Default_Block_Size,
       Accessed | Modified | Changed => (0, 0), Unused => [others => 0]));

   function Path_Stat (Path, Status : System.Address) return long is
      Handle, Size : aliased Unsigned_64 := 0;
      Directory_Found : Boolean := False;
      R : long := File_Open (Path, 0, OPEN_READ_ONLY, Handle'Address, Size'Address);
      Found : aliased Inspection;
      Described : Boolean;
      S : Stat_Record with Import, Address => Status;
   begin
      if R = Error (ENOTDIR) then
         Directory_Found := True;
         R := File_Open (Path, 1, OPEN_READ_ONLY, Handle'Address, Size'Address);
      end if;
      if R /= 0 then
         return R;
      end if;
      Described := File_Describe (Handle, Found'Address) = 0;
      File_Close (Handle, (if Directory_Found then 1 else 0));
      S := Empty_Stat;
      S.Mode := (if Directory_Found then S_IFDIR + 8#555# else S_IFREG + 8#444#);
      S.Size := Integer_64 (Unsigned_64'Min (Size, Unsigned_64 (Integer_64'Last)));
      S.Blocks := Integer_64 (Blocks (Size));
      if Described then
         Fill_From (S, Found);
      end if;
      return 0;
   end Path_Stat;

   function Ftruncate (Fd : int; Length : long) return long is
      R : long;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      elsif Length < 0 then
         return Error (EINVAL);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if E.Kind /= File or else not Writable (E.Flags) then
            return Error (EINVAL);
         end if;
         R := File_Resize (E.Handle, Unsigned_64 (Length));
         if R = 0 then
            E.Size := Unsigned_64 (Length);
         end if;
         return R;
      end;
   end Ftruncate;

   function Path_Truncate (Path : System.Address; Length : long) return long is
      Handle, Size : aliased Unsigned_64 := 0;
      R : long;
   begin
      if Length < 0 then
         return Error (EINVAL);
      end if;
      R := File_Open (Path, 0, OPEN_WRITE_ONLY, Handle'Address, Size'Address);
      if R /= 0 then
         return R;
      end if;
      R := File_Resize (Handle, Unsigned_64 (Length));
      File_Close (Handle, 0);
      return R;
   end Path_Truncate;

   --  A positioned write; the file's size follows it.
   function File_Write (E : in out Descriptor_Entry; Buffer : System.Address;
                        Count : size_t; At_Offset : Unsigned_64) return long;
   function File_Write (E : in out Descriptor_Entry; Buffer : System.Address;
                        Count : size_t; At_Offset : Unsigned_64) return long
   is
      Put : constant long := File_Write_At (E.Handle, Buffer, Count, At_Offset);
   begin
      if Put > 0 then
         Lock (Table_Lock'Access);
         if At_Offset + Unsigned_64 (Put) > E.Size then
            E.Size := At_Offset + Unsigned_64 (Put);
         end if;
         Unlock (Table_Lock'Access);
      end if;
      return Put;
   end File_Write;

   function Pwrite (Fd : int; Buffer : System.Address; Count : size_t; Offset : long)
     return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if E.Kind = Directory then
            return Error (EISDIR);
         elsif E.Kind /= File then
            return Error (ESPIPE);
         elsif not Writable (E.Flags) then
            return Error (EBADF);
         elsif Offset < 0 then
            return Error (EINVAL);
         elsif Count = 0 then
            return 0;
         end if;
         return File_Write (E, Buffer, Count, Unsigned_64 (Offset));
      end;
   end Pwrite;

   --  write(2) on a file: at the descriptor's offset (the end with O_APPEND).
   function File_Writev (E : in out Descriptor_Entry; Vectors : System.Address;
                         Count : int) return long;
   function File_Writev (E : in out Descriptor_Entry; Vectors : System.Address;
                         Count : int) return long
   is
      type Vector_Array is array (1 .. IOV_MAX) of Io_Vector;
      V : constant Vector_Array with Import, Address => Vectors;
      Total : long := 0;
      At_Offset : Unsigned_64;
      Put : long;
   begin
      if not Writable (E.Flags) then
         return Error (EBADF);
      elsif Count not in 0 .. IOV_MAX then
         return Error (EINVAL);
      end if;
      for I in 1 .. Integer (Count) loop
         if V (I).Length > 0 then
            Lock (Table_Lock'Access);
            At_Offset := (if Has (E.Flags, O_APPEND) then E.Size else E.Offset);
            Unlock (Table_Lock'Access);
            Put := File_Write (E, To_Address (V (I).Base), size_t (V (I).Length), At_Offset);
            if Put < 0 then
               return (if Total > 0 then Total else Put);
            end if;
            Lock (Table_Lock'Access);
            E.Offset := At_Offset + Unsigned_64 (Put);
            Unlock (Table_Lock'Access);
            Total := Total + Put;
            exit when Unsigned_64 (Put) < V (I).Length;
         end if;
      end loop;
      return Total;
   end File_Writev;

   function Fsync (Fd : int) return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if E.Kind = Directory then
            return 0;                  --  directory updates are written through
         elsif E.Kind /= File then
            return Error (EINVAL);
         end if;
         --  The service flushes only through a write-authorized handle; a
         --  read-only one has written nothing.
         return (if Writable (E.Flags) then File_Flush (E.Handle) else 0);
      end;
   end Fsync;

   function Stream_Writev (E : in out Descriptor_Entry; Vectors : System.Address;
                           Count : int) return long;
   function Stream_Writev (E : in out Descriptor_Entry; Vectors : System.Address;
                           Count : int) return long
   is
      type Vector_Array is array (1 .. IOV_MAX) of Io_Vector;
      V : constant Vector_Array with Import, Address => Vectors;
      Total : long := 0;
      First_Write : Boolean;
      Ignore : Unsigned_32;
   begin
      if Count not in 0 .. IOV_MAX then
         return Error (EINVAL);
      end if;
      Lock (Stream_Lock'Access);
      First_Write := not E.Created;
      if not E.Created then
         Stream_Poll_On_Write := 0;
         Stream_Create (E.Stream, Pages_Of (E.Stream), Type_Of (E.Stream));
         E.Created := True;
      end if;
      for I in 1 .. Integer (Count) loop
         declare
            Left : Unsigned_64 := V (I).Length;
            Position : Unsigned_64 := V (I).Base;
            Chunk : Unsigned_64;
         begin
            while Left > 0 loop
               Chunk := Unsigned_64'Min (Left, Maximum_Record);
               Ignore := Stream_Write (E.Stream, To_Address (Position),
                                       Unsigned_32 (Chunk), Type_Of (E.Stream));
               Position := Position + Chunk;
               Left := Left - Chunk;
            end loop;
         end;
         Total := Total + To_Long (V (I).Length);
      end loop;
      Unlock (Stream_Lock'Access);
      if First_Write then
         Start_Dispatcher;
      end if;
      return Total;
   end Stream_Writev;

   function Writev (Fd : int; Vectors : System.Address; Count : int) return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         case E.Kind is
            when File =>
               return File_Writev (E, Vectors, Count);
            when Pipe_Write | Socket_Pair =>
               return Ring_Write (E, Vectors, Count);
            when Tcp =>
               return Tcp_Write (To_Address (E.Socket), Vectors, Count, Nonblocking (E));
            when Stream_Out =>
               return Stream_Writev (E, Vectors, Count);
            when Directory =>
               return Error (EISDIR);
            when Unset | None | Pipe_Read =>
               return Error (EBADF);
         end case;
      end;
   end Writev;

   function Pread (Fd : int; Buffer : System.Address; Count : size_t; Offset : long)
     return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if E.Kind = Directory then
            return Error (EISDIR);
         elsif E.Kind /= File then
            return Error (EINVAL);                  --  output streams
         elsif Offset < 0 then
            return Error (EINVAL);
         elsif Count = 0 or else Unsigned_64 (Offset) >= E.Size then
            return 0;
         end if;
         return File_Read_At (E.Handle, Buffer, Count, Unsigned_64 (Offset));
      end;
   end Pread;

   function Read (Fd : int; Buffer : System.Address; Count : size_t) return long is
      At_Offset : Unsigned_64;
      Got : long;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         case E.Kind is
            when Pipe_Read | Socket_Pair =>
               return Ring_Read (E, Buffer, Count);
            when Tcp =>
               return Tcp_Read (To_Address (E.Socket), Buffer, Count, Nonblocking (E));
            when File =>
               Lock (Table_Lock'Access);
               At_Offset := E.Offset;
               Unlock (Table_Lock'Access);
               if At_Offset > Unsigned_64 (long'Last) then
                  return 0;
               end if;
               Got := Pread (Fd, Buffer, Count, long (At_Offset));
               if Got > 0 then
                  Lock (Table_Lock'Access);
                  E.Offset := At_Offset + Unsigned_64 (Got);
                  Unlock (Table_Lock'Access);
               end if;
               return Got;
            when others =>
               return Pread (Fd, Buffer, Count, 0);
         end case;
      end;
   end Read;

   function Lseek (Fd : int; Offset : long; Whence : int) return long is
      Result : Unsigned_64;
      Valid : Boolean;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if E.Kind = Directory then
            if Offset /= 0 or else Whence /= SEEK_SET then
               return Error (EINVAL);
            end if;
            return Error (ENOSYS);              --  rewinddir: reopen instead
         elsif E.Kind /= File then
            return Error (ESPIPE);              --  streams, pipes, sockets
         end if;
         Lock (Table_Lock'Access);
         Seek (Whence, Integer_64 (Offset), E.Offset, E.Size, Result, Valid);
         if Valid then
            E.Offset := Result;
         end if;
         Unlock (Table_Lock'Access);
         return (if Valid then long (Result) else Error (EINVAL));
      end;
   end Lseek;

   function Close (Fd : int) return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         case E.Kind is
            when File | Directory =>
               File_Close (E.Handle, (if E.Kind = Directory then 1 else 0));
            when Pipe_Read | Pipe_Write =>
               Release_Ring (E.Ring, Reader => E.Kind = Pipe_Read);
               Readiness_Changed;
            when Tcp =>
               Tcp_Close (To_Address (E.Socket));
            when Socket_Pair =>
               Release_Ring (E.Ring, Reader => True);
               Release_Ring (E.Peer, Reader => False);
               Readiness_Changed;
            when Unset | None | Stream_Out =>
               null;
         end case;
         Free (To_Address (E.Listing));
         Free (To_Address (E.Name));
         Lock (Table_Lock'Access);
         E := (others => <>);
         Unlock (Table_Lock'Access);
      end;
      return 0;
   end Close;

   function Fstat (Fd : int; Status : System.Address) return long is
      S : Stat_Record with Import, Address => Status;
      Found : aliased Inspection;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         S := Empty_Stat;
         case E.Kind is
            when File | Directory =>
               if E.Kind = File then
                  S.Mode := S_IFREG + (if Writable (E.Flags) then 8#644# else 8#444#);
                  S.Size := Integer_64 (Unsigned_64'Min (E.Size, Unsigned_64 (Integer_64'Last)));
                  S.Blocks := Integer_64 (Blocks (E.Size));
               else
                  S.Mode := S_IFDIR + 8#555#;
               end if;
               S.Inode := E.Handle;
               if File_Describe (E.Handle, Found'Address) = 0 then
                  Fill_From (S, Found);
               end if;
            when Pipe_Read | Pipe_Write =>
               S.Mode := S_IFIFO + 8#600#;
            when Socket_Pair | Tcp =>
               S.Mode := S_IFSOCK + 8#600#;
            when Stream_Out | Unset | None =>
               S.Mode := S_IFIFO + 8#200#;      --  a write-only stream
         end case;
      end;
      return 0;
   end Fstat;

   --  dup: a second descriptor for the same object, at the lowest free
   --  number at or above Minimum (exactly Target when it is not negative).
   --  Streams, pipes and socket pairs share their object; files,
   --  directories and sockets cannot be duplicated yet (their service
   --  handles have one owner).
   function Dup (Fd, Minimum, Target, Close_On_Exec : int) return long is
      To : Integer;
      Rings : array (1 .. 2) of Unsigned_64 := [0, 0];
      Kind : Kinds;
      Ignore : long;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      Kind := Table (Integer (Fd)).Kind;
      if Kind in File | Directory | Tcp then
         return Error (EOPNOTSUPP);
      elsif Target >= Maximum_Descriptors or else Minimum >= Maximum_Descriptors then
         return Error (EINVAL);
      elsif Target >= 0 and then Target = Fd then
         return long (Fd);
      end if;
      if Target >= 0 and then not Is_Free (Integer (Target)) then
         Ignore := Close (Target);
      end if;
      Lock (Table_Lock'Access);
      if Target >= 0 then
         To := Integer (Target);
      else
         To := Integer (int'Max (Minimum, 0));
         while To < Maximum_Descriptors and then not Is_Free (To) loop
            To := To + 1;
         end loop;
      end if;
      if To >= Maximum_Descriptors or else not Is_Free (To) then
         Unlock (Table_Lock'Access);
         return Error (EMFILE);
      end if;
      Table (To) := Table (Integer (Fd));
      Table (To).Close_On_Exec := Close_On_Exec /= 0;
      Rings := [Table (Integer (Fd)).Ring, (if Kind = Socket_Pair then Table (Integer (Fd)).Peer else 0)];
      Unlock (Table_Lock'Access);
      for I in Rings'Range loop
         if Rings (I) /= 0 then
            declare
               Object : Ring_Object with Import, Address => To_Address (Rings (I));
            begin
               Lock (Object.Lock'Access);
               if Kind = Pipe_Read or else (Kind = Socket_Pair and then I = 1) then
                  Object.R.Readers := Natural'Min (Object.R.Readers + 1, Maximum_Ends);
               else
                  Object.R.Writers := Natural'Min (Object.R.Writers + 1, Maximum_Ends);
               end if;
               Unlock (Object.Lock'Access);
            end;
         end if;
      end loop;
      return long (To);
   end Dup;

   procedure Report_Unsupported (What : System.Address; Value : long)
   with Import, Convention => C, External_Name => "report_unsupported";
   Fcntl_Text : aliased constant String := "fcntl" & Character'Val (0);

   function Fcntl (Fd : int; Command : int; Argument : long) return long is
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
      begin
         if Command = F_GETFD then
            return (if E.Close_On_Exec then FD_CLOEXEC else 0);
         elsif Command = F_SETFD then
            E.Close_On_Exec := Has (Argument, FD_CLOEXEC);
            return 0;
         elsif Command = F_GETFL then
            return (if E.Kind = Stream_Out then O_WRONLY
                    else To_Long (Bits (E.Flags) and not Bits (O_CLOEXEC)));
         elsif Command = F_SETFL then
            if E.Flags in 0 .. long'Last - O_NONBLOCK then
               E.Flags := Set_Status_Flags (E.Flags, Argument);
            end if;
            return 0;
         elsif Command = F_DUPFD or else Command = F_DUPFD_CLOEXEC then
            return Dup (Fd, int (long'Max (long'Min (Argument, long (int'Last)), 0)), -1,
                        (if Command = F_DUPFD_CLOEXEC then 1 else 0));
         end if;
         Report_Unsupported (Fcntl_Text'Address, long (Command));
         return Error (EINVAL);
      end;
   end Fcntl;

   --  getdents64 over Directory.Page.V1 (CuBit.Libc_Directory_Entries).
   function Getdents (Fd : int; Buffer : System.Address; Count : size_t) return long is
      Used : Natural := 0;
      Fits : Boolean;
      Valid : Boolean;
      Capacity : constant Natural :=
        Natural (size_t'Min (Count, Entries.Maximum_Buffer_Bytes));
      Target : Entries.Bytes (0 .. Capacity - 1) with Import, Address => Buffer;
      R : long;
   begin
      if not Valid_Descriptor (Fd) then
         return Error (EBADF);
      elsif Table (Integer (Fd)).Kind /= Directory then
         return Error (ENOTDIR);
      end if;
      declare
         E : Descriptor_Entry renames Table (Integer (Fd));
         D : Listing with Import, Address => To_Address (E.Listing);
      begin
         loop
            if not D.Loaded or else D.Next >= D.Count then
               exit when D.Ended;
               R := Directory_Read_Page (E.Handle, D.Page'Address);
               if R /= 0 then
                  return (if Used > 0 then long (Used) else R);
               end if;
               Entries.Header (D.Page, Valid, D.Count, D.Ended);
               if not Valid then
                  return Error (EIO);
               end if;
               D.Next := 0;
               D.Loaded := True;
            end if;
            if D.Next < D.Count then
               Entries.Encode (D.Page, D.Next, Target, Used, Fits);
               if not Fits then
                  return (if Used = 0 then Error (EINVAL) else long (Used));
               end if;
               D.Next := D.Next + 1;
            end if;
         end loop;
      end;
      return long (Used);
   end Getdents;

   ---------------------------------------------------------------------------
   --  poll.
   ---------------------------------------------------------------------------
   function Flag (Events : Integer_16; Mask : Natural) return Integer_16 is
     (Integer_16 (Unsigned_16'Mod (Events) and Unsigned_16 (Mask)));
   function Union (A, B : Integer_16) return Integer_16 is
     (Integer_16 (Unsigned_16'Mod (A) or Unsigned_16'Mod (B)));

   --  Readiness of the descriptors' objects. An output stream never blocks
   --  (it drops the oldest records instead), so it is always writable; a
   --  file is always readable; a pipe end is ready when it has data (or
   --  room) or its other end is closed; no object: invalid.
   function Scan (Polls : System.Address; Count : unsigned_long;
                  Sockets : out Unsigned_64) return long;
   function Scan (Polls : System.Address; Count : unsigned_long;
                  Sockets : out Unsigned_64) return long
   is
      Limit : constant Natural := Natural (unsigned_long'Min (Count, Maximum_Descriptors * 4));
      type Poll_Array is array (1 .. Limit) of Poll_Descriptor;
      P : Poll_Array with Import, Address => Polls;
      Ready : long := 0;
      Got : Integer_16;
   begin
      Sockets := 0;
      for I in P'Range loop
         Got := 0;
         if P (I).Descriptor < 0 then
            null;
         elsif not Valid_Descriptor (P (I).Descriptor) then
            Got := POLLNVAL;
         else
            declare
               E : Descriptor_Entry renames Table (Integer (P (I).Descriptor));
               Want : constant Integer_16 := P (I).Events;
            begin
               case E.Kind is
                  when Stream_Out =>
                     Got := Flag (Want, POLLOUT + POLLWRNORM);
                  when Pipe_Read =>
                     declare
                        Object : Ring_Object with Import, Address => To_Address (E.Ring);
                     begin
                        Lock (Object.Lock'Access);
                        if Object.R.Length > 0 then
                           Got := Flag (Want, POLLIN + POLLRDNORM);
                        end if;
                        if Object.R.Writers = 0 then
                           Got := Union (Got, POLLHUP);
                        end if;
                        Unlock (Object.Lock'Access);
                     end;
                  when Pipe_Write =>
                     declare
                        Object : Ring_Object with Import, Address => To_Address (E.Ring);
                     begin
                        Lock (Object.Lock'Access);
                        if Object.R.Length < Ring_Bytes then
                           Got := Flag (Want, POLLOUT + POLLWRNORM);
                        end if;
                        if Object.R.Readers = 0 then
                           Got := Union (Got, POLLERR);
                        end if;
                        Unlock (Object.Lock'Access);
                     end;
                  when Tcp =>
                     Got := Tcp_Poll (To_Address (E.Socket), Want);
                     Sockets := Sockets or Tcp_Mask (To_Address (E.Socket));
                  when Socket_Pair =>
                     declare
                        Incoming : Ring_Object with Import, Address => To_Address (E.Ring);
                        Outgoing : Ring_Object with Import, Address => To_Address (E.Peer);
                     begin
                        Lock (Incoming.Lock'Access);
                        if Incoming.R.Length > 0 then
                           Got := Flag (Want, POLLIN + POLLRDNORM);
                        end if;
                        if Incoming.R.Writers = 0 then
                           Got := Union (Got, Union (POLLHUP, Flag (Want, POLLIN + POLLRDHUP)));
                        end if;
                        Unlock (Incoming.Lock'Access);
                        Lock (Outgoing.Lock'Access);
                        if Outgoing.R.Length < Ring_Bytes and then Outgoing.R.Readers > 0 then
                           Got := Union (Got, Flag (Want, POLLOUT + POLLWRNORM));
                        end if;
                        Unlock (Outgoing.Lock'Access);
                     end;
                  when File | Directory | Unset | None =>
                     Got := Flag (Want, POLLIN + POLLRDNORM);     --  files never block
               end case;
            end;
         end if;
         P (I).Returned := Got;
         if Got /= 0 then
            Ready := Ready + 1;
         end if;
      end loop;
      return Ready;
   end Scan;

   --  poll: wait until a descriptor is ready or the deadline passes
   --  (kernel milliseconds; Forever: none, 0: do not wait).
   function Poll (Polls : System.Address; Count, Deadline : unsigned_long) return long is
      Sequence : int;
      Sockets : Unsigned_64;
      Ready : long;
   begin
      loop
         Sequence := Events;
         Ready := Scan (Polls, Count, Sockets);
         if Ready /= 0 or else Deadline = 0
           or else (Deadline /= unsigned_long (K.Forever)
                    and then unsigned_long (CuBit.Kernel_Calls.Call (K.Get_Time)) >= Deadline)
         then
            return Ready;
         end if;
         if Sockets /= 0 then
            Net_Wait (Sequence, Deadline, Sockets);
         else
            Readiness_Wait (Sequence, Deadline);
         end if;
      end loop;
   end Poll;

end CuBit.Libc_Descriptors;
