------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Process_IDs;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Time; use CuBit.Libc_Time;
with CuBit.Libc_Select;
with CuBit.Libc_Reports;
with CuBit.Libc_Imports;
with CuBit.Linux_System_Calls; use CuBit.Linux_System_Calls;

package body CuBit.Libc_System_Calls is

   package K renames CuBit.Kernel_ABI;

   use type Interfaces.C.long;
   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   use type Interfaces.C.unsigned_long;
   use type Interfaces.C.unsigned;
   use type System.Address;

   subtype int is Interfaces.C.int;
   subtype size_t is Interfaces.C.size_t;
   subtype unsigned_long is Interfaces.C.unsigned_long;

   NUL : constant Character := Character'Val (0);

   --  The main thread's tid (threads THREAD_CREATE makes are 1 .. 1023).
   Main_Thread_Id : constant := 16#3FFF_FFFF#;
   --  Until the kernel reports its CPU count: four.
   Default_CPU_Mask : constant Unsigned_64 := 16#F#;
   CPU_Mask_Bytes : constant := 8;
   --  The largest cpu_set_t a caller passes (CPU_SETSIZE bits).
   Maximum_CPU_Set_Bytes : constant := 128;
   --  getrandom: RDRAND attempts per word before giving up.
   Random_Attempts : constant := 16;
   Random_Word_Bytes : constant := 8;
   --  Page alignment a file mapping's offset needs.
   Page_Mask : constant Unsigned_64 := K.Page_Bytes - 1;
   --  system calls reported unimplemented, once each.
   Reported_Numbers : constant := 512;

   ---------------------------------------------------------------------------
   --  The libc's C (fd.c, file.c, net.c) until it is Ada.
   ---------------------------------------------------------------------------
   function Fd_Writev (Fd : int; Vectors : System.Address; Count : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_writev";
   function Fd_Read (Fd : int; Buffer : System.Address; Count : size_t) return long
   with Import, Convention => C, External_Name => "__cubit_fd_read";
   function Fd_Pread (Fd : int; Buffer : System.Address; Count : size_t;
                      Offset : long) return long
   with Import, Convention => C, External_Name => "__cubit_fd_pread";
   function Fd_Pwrite (Fd : int; Buffer : System.Address; Count : size_t;
                       Offset : long) return long
   with Import, Convention => C, External_Name => "__cubit_fd_pwrite";
   function Fd_Fsync (Fd : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_fsync";
   function Fd_Lseek (Fd : int; Offset : long; Whence : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_lseek";
   function Fd_Open (Path : System.Address; Flags : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_open";
   Remove_File      : constant int := 0;    --  CUBIT_REMOVE_FILE
   Remove_Directory : constant int := 1;    --  CUBIT_REMOVE_DIRECTORY
   function Path_Remove (Path : System.Address; Kind : int) return long
   with Import, Convention => C, External_Name => "__cubit_path_remove";
   function Path_Mkdir (Path : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_path_mkdir";
   function Path_Rename (From, To : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_path_rename";
   function Fd_Close (Fd : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_close";
   function Fd_Fstat (Fd : int; Status : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_fd_fstat";
   function Fd_Fcntl (Fd : int; Command : int; Argument : long) return long
   with Import, Convention => C, External_Name => "__cubit_fd_fcntl";
   function Fd_Getdents (Fd : int; Buffer : System.Address; Count : size_t) return long
   with Import, Convention => C, External_Name => "__cubit_fd_getdents";
   function Fd_Poll (Polls : System.Address; Count : unsigned_long;
                     Deadline : unsigned_long) return long
   with Import, Convention => C, External_Name => "__cubit_fd_poll";
   function Fd_Pipe (Pair : System.Address; Flags : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_pipe";
   function Fd_Socketpair (Pair : System.Address; Flags : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_socketpair";
   function Fd_Dup (Fd, Minimum, Target, Close_On_Exec : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_dup";
   function Path_Stat (Path, Status : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_path_stat";
   function Get_Cwd (Buffer : System.Address; Size : size_t) return long
   with Import, Convention => C, External_Name => "__cubit_getcwd";
   function Change_Directory (Path : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_chdir";
   function Fd_Fchdir (Fd : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_fchdir";
   function At_Path (Directory : int; Path, Buffer : System.Address; Size : size_t;
                     Result : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_at_path";
   function Path_Access (Path, Directory, May_Write, Inspection, Described :
                           System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_path_access";
   function Fd_Ftruncate (Fd : int; Length : long) return long
   with Import, Convention => C, External_Name => "__cubit_fd_ftruncate";
   function Path_Truncate (Path : System.Address; Length : long) return long
   with Import, Convention => C, External_Name => "__cubit_path_truncate";
   function Fd_Accept (Fd : int; Address, Length : System.Address; Flags : int)
     return long
   with Import, Convention => C, External_Name => "__cubit_fd_accept";
   function Fd_Socket_Tcp (Flags : int) return long
   with Import, Convention => C, External_Name => "__cubit_fd_socket_tcp";
   function Fd_Tcp (Fd : int; Nonblocking : System.Address) return System.Address
   with Import, Convention => C, External_Name => "__cubit_fd_tcp";
   function Fd_Is_Socket (Fd : int) return int
   with Import, Convention => C, External_Name => "__cubit_fd_is_socket";
   function Tcp_Connect (Socket, Address : System.Address;
                         Length : Interfaces.C.unsigned; Nonblocking : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_connect";
   function Tcp_So_Error (Socket : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_so_error";
   function Tcp_Peer (Socket, Address, Length : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_peer";
   function Tcp_Local (Socket, Address, Length : System.Address) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_local";
   function Tcp_Bind (Socket, Address : System.Address;
                      Length : Interfaces.C.unsigned) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_bind";
   function Tcp_Listen (Socket : System.Address; Backlog : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_listen";
   function Tcp_Shutdown (Socket : System.Address; How : int) return long
   with Import, Convention => C, External_Name => "__cubit_tcp_shutdown";

   --  file.c's struct cubit_inspection: Valid and Mode, then 56 bytes.
   type Inspection is record
      Valid, Mode : Unsigned_32 := 0;
      Rest : Storage_Array (1 .. 56) := [others => 0];
   end record with Convention => C;
   Inspected_Mode : constant Unsigned_32 := 4;   --  CUBIT_INSPECTED_MODE

   ---------------------------------------------------------------------------
   --  Helpers.
   ---------------------------------------------------------------------------
   function Address_Of (Value : long) return System.Address is
     (To_Address (Integer_Address (Unsigned_64'Mod (Value))));
   function Bits (Value : long) return Unsigned_64 is (Unsigned_64'Mod (Value));
   --  A C int argument: the low 32 bits of its register, as the ABI passes
   --  it (a plain conversion of a wider value would be out of range).
   function To_Int is new Ada.Unchecked_Conversion (Unsigned_32, int);
   --  A result register: the bits, as C's long.
   function To_Long is new Ada.Unchecked_Conversion (Unsigned_64, long);
   function Low (Value : long) return int is (To_Int (Unsigned_32'Mod (Value)));
   function Has (Value, Flag : long) return Boolean is
     ((Bits (Value) and Bits (Flag)) /= 0);
   function Error (Value : int) return long is (-long (Value));

   function Kernel (Number : K.System_Call; A0, A1, A2, A3 : Unsigned_64 := 0)
     return Unsigned_64 is (CuBit.Kernel_Calls.Call (Number, A0, A1, A2, A3));

   procedure Zero (Where : System.Address; Count : Natural);
   procedure Zero (Where : System.Address; Count : Natural) is
      Bytes : Storage_Array (1 .. Storage_Offset (Count))
      with Import, Address => Where;
   begin
      Bytes := [others => 0];
   end Zero;

   procedure Console (Text : String);
   procedure Console (Text : String) is
      Ignore : constant Unsigned_64 := Kernel
        (K.Write, K.Console, Unsigned_64 (To_Integer (Text'Address)), Text'Length);
   begin
      null;
   end Console;

   function Now_Milliseconds return Unsigned_64 is (Kernel (K.Get_Time));

   --  The kernel's high-resolution clock (HPET or invariant TSC), or the
   --  millisecond clock where it has none.
   function Now_Microseconds return Unsigned_64;
   function Now_Microseconds return Unsigned_64 is
      Count : constant Unsigned_64 := Kernel (K.Read_Monotonic_Microseconds);
   begin
      if Count = K.Failed then
         return Now_Milliseconds * Microseconds_Per_Millisecond;
      end if;
      return Count;
   end Now_Microseconds;

   --  Milliseconds since the Unix epoch: the kernel's wall-clock offset,
   --  published by clock.svc while its time is current, plus the
   --  millisecond clock. Without one, time since boot, reported once.
   Wall_Clock_Missing_Reported : Boolean := False with Volatile;
   function Realtime_Milliseconds return Unsigned_64;
   function Realtime_Milliseconds return Unsigned_64 is
      Offset : constant Unsigned_64 := Kernel (K.Info, K.Wall_Clock_Offset);
      Now : constant Unsigned_64 := Now_Milliseconds;
   begin
      if Offset /= 0 and then Offset /= K.Failed then
         return Saturating_Add (Offset, Now);
      end if;
      if not Wall_Clock_Missing_Reported then
         Wall_Clock_Missing_Reported := True;
         Console ("cubit-libc: no wall-clock time yet (clock service not " &
                  "started or not current); realtime is time since boot" &
                  Character'Val (10));
      end if;
      return Now;
   end Realtime_Milliseconds;

   procedure Exit_Process (Code : int) with No_Return;
   procedure Exit_Process (Code : int) is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := Kernel (K.Exit_Process, Unsigned_64'Mod (Code));
      end loop;
   end Exit_Process;

   ---------------------------------------------------------------------------
   --  Reports.
   ---------------------------------------------------------------------------
   Seen_Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;
   Seen : CuBit.Libc_Reports.Seen_Table :=
     (Entries => [others => (What => [others => ' '], Length => 0, Value => 0)],
      Count => 0);
   Reported : array (0 .. Reported_Numbers - 1) of Boolean := [others => False];

   procedure Report (Prefix, What : String; Value : Integer_64);
   procedure Report (Prefix, What : String; Value : Integer_64) is
      Text : CuBit.Libc_Reports.Line;
      Length : CuBit.Libc_Reports.Line_Length;
   begin
      CuBit.Libc_Reports.Format (Prefix, What, Value, Text, Length);
      Console (Text (1 .. Length));
   end Report;

   procedure Unsupported (What : String; Value : long);
   procedure Unsupported (What : String; Value : long) is
      Is_New : Boolean;
   begin
      CuBit.Libc_Imports.Lock (Seen_Lock'Access);
      CuBit.Libc_Reports.First_Time (Seen, What, Integer_64 (Value), Is_New);
      CuBit.Libc_Imports.Unlock (Seen_Lock'Access);
      if Is_New then
         Report ("unsupported", What, Integer_64 (Value));
      end if;
   end Unsupported;

   procedure Report_Unsupported (What : System.Address; Value : long) is
      Bound : constant := CuBit.Libc_Reports.What_Bytes;
      Text : constant String (1 .. Bound) with Import, Address => What;
      Length : Natural := 0;
   begin
      if What /= System.Null_Address then
         while Length < Bound and then Text (Length + 1) /= NUL loop
            Length := Length + 1;
         end loop;
      end if;
      Unsupported (Text (1 .. Length), Value);
   end Report_Unsupported;

   procedure Unimplemented (Number : long);
   procedure Unimplemented (Number : long) is
   begin
      if Number in 0 .. Reported_Numbers - 1 then
         if Reported (Integer (Number)) then
            return;
         end if;
         Reported (Integer (Number)) := True;
      end if;
      Report ("unimplemented", "system call", Integer_64 (Number));
   end Unimplemented;

   ---------------------------------------------------------------------------
   --  Memory.
   ---------------------------------------------------------------------------
   function Grow_Break (Wanted : Unsigned_64) return long;
   function Grow_Break (Wanted : Unsigned_64) return long is
      Current : constant Unsigned_64 := Kernel (K.Grow_Heap, 0);
   begin
      if Wanted <= Current then
         return To_Long (Current);              --  CuBit's heap only grows
      elsif Kernel (K.Grow_Heap, Wanted - Current) = K.Failed then
         return To_Long (Current);
      end if;
      return To_Long (Wanted);
   end Grow_Break;

   function Protect_Or_Release (Base : Unsigned_64; Length : Unsigned_64;
                                Protection : long) return long;
   function Protect_Or_Release (Base : Unsigned_64; Length : Unsigned_64;
                                Protection : long) return long is
   begin
      if Protection /= PROT_READ + PROT_WRITE
        and then Kernel (K.Protect_Owned_Memory, Base, Length, Bits (Protection)) /= 0
      then
         declare
            Ignore : constant Unsigned_64 :=
              Kernel (K.Release_Owned_Memory, Base, Length);
         begin
            return Error (ENOMEM);
         end;
      end if;
      return To_Long (Base);
   end Protect_Or_Release;

   function Map (Length : Unsigned_64; Protection, Flags : long; Fd : int;
                 Offset : long) return long;
   function Map (Length : Unsigned_64; Protection, Flags : long; Fd : int;
                 Offset : long) return long
   is
      Allowed_Flags : constant Unsigned_64 :=
        Bits (MAP_PRIVATE) or Bits (MAP_ANONYMOUS) or Bits (MAP_STACK)
        or Bits (MAP_NORESERVE);
      Base : Unsigned_64;
   begin
      if Length = 0 then
         return Error (EINVAL);
      elsif Protection not in PROT_NONE | PROT_READ | PROT_READ + PROT_WRITE
        or else Has (Flags, MAP_FIXED)
        or else (Bits (Flags) and Bits (MAP_TYPE)) /= Bits (MAP_PRIVATE)
        or else (Bits (Flags) and not Allowed_Flags) /= 0
      then
         return Error (ENOTSUP);
      elsif Length > K.Maximum_Owned_Bytes then
         return Error (ENOMEM);
      end if;
      Base := Kernel (K.Allocate_Owned_Memory, Length);
      if Base = 0 or else Base = K.Failed then
         return Error (ENOMEM);
      end if;
      if not Has (Flags, MAP_ANONYMOUS) then
         --  A file: a private copy of its bytes (writes are not carried
         --  back), from a page-aligned offset.
         declare
            Got : long;
            Ignore : Unsigned_64;
         begin
            if (Bits (Offset) and Page_Mask) /= 0 then
               Ignore := Kernel (K.Release_Owned_Memory, Base, Length);
               return Error (EINVAL);
            end if;
            Got := Fd_Pread (Fd, To_Address (Integer_Address (Base)),
                             size_t (Length), Offset);
            if Got < 0 then
               Ignore := Kernel (K.Release_Owned_Memory, Base, Length);
               return Got;
            end if;
         end;
      end if;
      return Protect_Or_Release (Base, Length, Protection);
   end Map;

   ---------------------------------------------------------------------------
   --  Futexes.
   ---------------------------------------------------------------------------
   --  A relative timeout as a millisecond deadline (null: none).
   function Relative_Deadline (Timeout : System.Address; Valid_Time : out Boolean)
     return Unsigned_64;
   function Relative_Deadline (Timeout : System.Address; Valid_Time : out Boolean)
     return Unsigned_64
   is
      T : constant Timespec with Import, Address => Timeout;
   begin
      Valid_Time := True;
      if Timeout = System.Null_Address then
         return Forever;
      elsif not Valid (T) then
         Valid_Time := False;
         return Forever;
      end if;
      return After (Now_Milliseconds, Milliseconds (T));
   end Relative_Deadline;

   --  An absolute time on the monotonic or realtime clock as a deadline on
   --  the millisecond clock.
   function Absolute_Deadline (Timeout : System.Address; Realtime : Boolean;
                               Valid_Time : out Boolean) return Unsigned_64;
   function Absolute_Deadline (Timeout : System.Address; Realtime : Boolean;
                               Valid_Time : out Boolean) return Unsigned_64
   is
      T : constant Timespec with Import, Address => Timeout;
   begin
      Valid_Time := True;
      if Timeout = System.Null_Address then
         return Forever;
      elsif not Valid (T) then
         Valid_Time := False;
         return Forever;
      elsif Realtime then
         return Wall_Deadline
           (Now_Milliseconds, Realtime_Milliseconds, Milliseconds (T));
      end if;
      return Monotonic_Deadline (Now_Milliseconds, Now_Microseconds, Microseconds (T));
   end Absolute_Deadline;

   function Futex (Word : System.Address; Operation, Value : int;
                   Timeout : System.Address; Compare : int) return long;
   function Futex (Word : System.Address; Operation, Value : int;
                   Timeout : System.Address; Compare : int) return long
   is
      Command : constant int := To_Int (Unsigned_32'Mod (Operation)
        and not Unsigned_32 (FUTEX_PRIVATE + FUTEX_CLOCK_REALTIME));
      Realtime : constant Boolean :=
        (Unsigned_32'Mod (Operation) and Unsigned_32 (FUTEX_CLOCK_REALTIME)) /= 0;
      Deadline : Unsigned_64;
      Valid_Time : Boolean;
      Result : Unsigned_64;
   begin
      if Command = FUTEX_WAIT or else Command = FUTEX_WAIT_BITSET then
         Deadline := (if Command = FUTEX_WAIT
                      then Relative_Deadline (Timeout, Valid_Time)
                      else Absolute_Deadline (Timeout, Realtime, Valid_Time));
         if not Valid_Time then
            return Error (EINVAL);
         end if;
         Result := Kernel (K.Futex_Wait, Unsigned_64 (To_Integer (Word)),
                           Unsigned_64 (Unsigned_32'Mod (Value)), Deadline);
         return (case Result is
                   when K.Futex_Woken     => 0,
                   when K.Futex_Retry     => Error (EAGAIN),
                   when K.Futex_Timed_Out => Error (ETIMEDOUT),
                   when others            => Error (EFAULT));
      elsif Command = FUTEX_WAKE or else Command = Futex_Wake_Bitset then
         return To_Long (Kernel (K.Futex_Wake, Unsigned_64 (To_Integer (Word)),
                                  (if Value < 0 then 0 else Unsigned_64 (Value))));
      elsif Command = FUTEX_CMP_REQUEUE or else Command = FUTEX_REQUEUE then
         if Command = FUTEX_CMP_REQUEUE then
            declare
               Current : constant int with Import, Volatile, Address => Word;
            begin
               if Current /= Compare then
                  return Error (EAGAIN);
               end if;
            end;
         end if;
         --  No requeue: wake every waiter; they recheck and contend.
         --  Correct for condition variables (spurious wakeups are
         --  allowed), only less efficient.
         return To_Long (Kernel (K.Futex_Wake, Unsigned_64 (To_Integer (Word)),
                                  K.Forever));
      end if;
      return Error (ENOSYS);
   end Futex;

   ---------------------------------------------------------------------------
   --  Time.
   ---------------------------------------------------------------------------
   function Coarse (Clock : long) return Boolean is
     (Clock = CLOCK_REALTIME or else Clock = CLOCK_REALTIME_COARSE);

   function Clock_Get_Time (Clock : long; Where : System.Address) return long;
   function Clock_Get_Time (Clock : long; Where : System.Address) return long is
   begin
      if Clock not in CLOCK_REALTIME | CLOCK_REALTIME_COARSE | CLOCK_MONOTONIC
                    | CLOCK_MONOTONIC_RAW | CLOCK_MONOTONIC_COARSE | CLOCK_BOOTTIME
      then
         Unsupported ("clock", Clock);
         return Error (EINVAL);
      elsif Where = System.Null_Address then
         return Error (EFAULT);
      end if;
      declare
         T : Timespec with Import, Address => Where;
      begin
         T := (if Coarse (Clock) then From_Milliseconds (Realtime_Milliseconds)
               else From_Microseconds (Now_Microseconds));
      end;
      return 0;
   end Clock_Get_Time;

   function Sleep_Until (Deadline : Unsigned_64) return long;
   function Sleep_Until (Deadline : Unsigned_64) return long is
      Ignore : Unsigned_64;
   begin
      while Now_Microseconds < Deadline loop
         Ignore := Kernel (K.Sleep_Until_Monotonic_Microsecond, Deadline);
      end loop;
      return 0;
   end Sleep_Until;

   function Sleep (Clock : long; Flags : long; Request : System.Address) return long;
   function Sleep (Clock : long; Flags : long; Request : System.Address) return long is
      T : constant Timespec with Import, Address => Request;
   begin
      if Request = System.Null_Address then
         return Error (EFAULT);
      elsif not Valid (T) then
         return Error (EINVAL);
      elsif not Has (Flags, TIMER_ABSTIME) then
         return Sleep_Until (Saturating_Add (Now_Microseconds, Microseconds (T)));
      elsif Clock = CLOCK_REALTIME then
         return Sleep_Until (Wall_Deadline
           (Now_Microseconds,
            (if Realtime_Milliseconds > Unsigned_64'Last / Microseconds_Per_Millisecond
             then Unsigned_64'Last
             else Realtime_Milliseconds * Microseconds_Per_Millisecond),
            Microseconds (T)));
      end if;
      return Sleep_Until (Microseconds (T));
   end Sleep;

   ---------------------------------------------------------------------------
   --  poll and select.
   ---------------------------------------------------------------------------
   function Select_Descriptors
     (Count : long; Read, Write, Except : System.Address; Deadline : Unsigned_64)
      return long;
   function Select_Descriptors
     (Count : long; Read, Write, Except : System.Address; Deadline : Unsigned_64)
      return long
   is
      package S renames CuBit.Libc_Select;
      Sets : constant S.Given :=
        (Read => Read /= System.Null_Address,
         Write => Write /= System.Null_Address,
         Error => Except /= System.Null_Address);
      Empty : constant S.Descriptor_Set := [others => False];
      Polls : S.Poll_Array;
      Used : S.Limit;
      Result : long;
      Ready : Natural;
      Invalid : Boolean;
      Read_Out, Write_Out, Error_Out : S.Descriptor_Set;
   begin
      if Count not in 0 .. FD_SETSIZE then
         return Error (EINVAL);
      end if;
      declare
         Read_In : constant S.Descriptor_Set with Import, Address => Read;
         Write_In : constant S.Descriptor_Set with Import, Address => Write;
         Error_In : constant S.Descriptor_Set with Import, Address => Except;
      begin
         S.Gather (Natural (Count), Sets,
                   (if Sets.Read then Read_In else Empty),
                   (if Sets.Write then Write_In else Empty),
                   (if Sets.Error then Error_In else Empty), Polls, Used);
      end;
      Result := Fd_Poll (Polls'Address, unsigned_long (Used), unsigned_long (Deadline));
      if Result < 0 then
         return Result;
      end if;
      S.Scatter (Polls, Used, Sets, Read_Out, Write_Out, Error_Out, Ready, Invalid);
      if Invalid then
         return Error (EBADF);
      end if;
      declare
         Read_Set : S.Descriptor_Set with Import, Address => Read;
         Write_Set : S.Descriptor_Set with Import, Address => Write;
         Error_Set : S.Descriptor_Set with Import, Address => Except;
      begin
         if Sets.Read then
            Read_Set := Read_Out;
         end if;
         if Sets.Write then
            Write_Set := Write_Out;
         end if;
         if Sets.Error then
            Error_Set := Error_Out;
         end if;
      end;
      return long (Ready);
   end Select_Descriptors;

   ---------------------------------------------------------------------------
   --  Randomness: RDRAND (not yet the entropy service).
   ---------------------------------------------------------------------------
   procedure Random_Word (Value : out Unsigned_64; Ok : out Boolean);
   procedure Random_Word (Value : out Unsigned_64; Ok : out Boolean) is
      Carry : Unsigned_8;
   begin
      Value := 0;
      Ok := False;
      for Attempt in 1 .. Random_Attempts loop
         Asm ("rdrand %0" & ASCII.LF & "setc %1",
              Outputs => [Unsigned_64'Asm_Output ("=r", Value),
                          Unsigned_8'Asm_Output ("=qm", Carry)],
              Volatile => True);
         if Carry /= 0 then
            Ok := True;
            return;
         end if;
      end loop;
   end Random_Word;

   function Get_Random (Buffer : System.Address; Length : size_t) return long;
   function Get_Random (Buffer : System.Address; Length : size_t) return long is
      Done : size_t := 0;
      Value : Unsigned_64;
      Ok : Boolean;
   begin
      while Done < Length loop
         Random_Word (Value, Ok);
         if not Ok then
            return (if Done > 0 then To_Long (Unsigned_64 (Done)) else Error (EAGAIN));
         end if;
         declare
            subtype Word_Bytes is Storage_Array (1 .. Random_Word_Bytes);
            function To_Bytes is new Ada.Unchecked_Conversion (Unsigned_64, Word_Bytes);
            Count : constant size_t := size_t'Min (Length - Done, Random_Word_Bytes);
            Source : constant Word_Bytes := To_Bytes (Value);
            Target : Storage_Array (1 .. Storage_Offset (Count))
            with Import, Address => Buffer + Storage_Offset (Done);
         begin
            Target := Source (1 .. Storage_Offset (Count));
            Done := Done + Count;
         end;
      end loop;
      return To_Long (Unsigned_64 (Length));
   end Get_Random;

   ---------------------------------------------------------------------------
   --  Process and identity.
   ---------------------------------------------------------------------------
   --  The thread's id as clone/set_tid_address gave it to musl: musl's
   --  struct pthread, at the thread pointer.
   function Thread_Id return long;
   function Thread_Id return long is
      Self : Unsigned_64;
   begin
      Asm ("mov %%fs:0, %0", Outputs => Unsigned_64'Asm_Output ("=r", Self),
           Volatile => True);
      declare
         Tid : constant int with Import,
           Address => To_Address (Integer_Address (Self)) + Pthread_Tid_Offset;
      begin
         return long (Tid);
      end;
   end Thread_Id;

   --  Thread names (PR_SET_NAME/PR_GET_NAME), kept per thread in the
   --  process: the kernel has no thread names yet.
   Thread_Name : String (1 .. Thread_Name_Bytes) := [others => NUL];
   pragma Thread_Local_Storage (Thread_Name);

   procedure Put_Name (Field : System.Address; Text : String);
   procedure Put_Name (Field : System.Address; Text : String) is
      Target : String (1 .. Text'Length) with Import, Address => Field;
   begin
      Target := Text;
   end Put_Name;

   function Uname (Where : System.Address) return long;
   function Uname (Where : System.Address) return long is
   begin
      if Where = System.Null_Address then
         return Error (EFAULT);
      end if;
      Zero (Where, Utsname_Field_Bytes * Utsname_Fields);
      Put_Name (Where, "CuBit");                                   --  sysname
      Put_Name (Where + Utsname_Field_Bytes, "cubit");             --  nodename
      Put_Name (Where + 2 * Utsname_Field_Bytes, "0.1");           --  release
      Put_Name (Where + 3 * Utsname_Field_Bytes, "0.1");           --  version
      Put_Name (Where + 4 * Utsname_Field_Bytes, "x86_64");        --  machine
      return 0;
   end Uname;

   ---------------------------------------------------------------------------
   --  Files and descriptors.
   ---------------------------------------------------------------------------
   --  path under the descriptor Directory (an *at call), resolved into
   --  Buffer; Result is then the path to use.
   type Path_Buffer is new String (1 .. PATH_MAX);

   function Resolve_At (Directory : long; Path : System.Address;
                        Buffer : in out Path_Buffer; Result : out System.Address)
     return long;
   function Resolve_At (Directory : long; Path : System.Address;
                        Buffer : in out Path_Buffer; Result : out System.Address)
     return long
   is
      Out_Path : aliased System.Address := System.Null_Address;
      R : constant long := At_Path (Low (Directory), Path, Buffer'Address,
                                    Buffer'Length, Out_Path'Address);
   begin
      Result := Out_Path;
      return R;
   end Resolve_At;

   function Check_Access (Path : System.Address; Mode : int) return long;
   function Check_Access (Path : System.Address; Mode : int) return long is
      Directory, May_Write, Described : aliased int := 0;
      Found : aliased Inspection;
      R : long := Path_Access (Path, Directory'Address, May_Write'Address,
                               Found'Address, Described'Address);
      Status : Storage_Array (1 .. 256);    --  struct stat, 144 bytes
   begin
      if R = Error (ENOSYS) then
         R := Path_Stat (Path, Status'Address);
         if R /= 0 then
            return R;
         end if;
         return (if (Unsigned_32'Mod (Mode) and Unsigned_32 (W_OK)) /= 0
                 then Error (EROFS) else 0);
      elsif R /= 0 then
         return R;
      elsif (Unsigned_32'Mod (Mode) and Unsigned_32 (W_OK)) /= 0
        and then May_Write = 0
      then
         return Error (EACCES);
      elsif (Unsigned_32'Mod (Mode) and Unsigned_32 (X_OK)) /= 0
        and then Described /= 0
        and then (Found.Valid and Inspected_Mode) /= 0
        and then (Found.Mode and (S_IXUSR + S_IXGRP + S_IXOTH)) = 0
      then
         return Error (EACCES);
      end if;
      return 0;
   end Check_Access;

   function Read_Vectors (Fd : int; Vectors : System.Address; Count : long) return long;
   function Read_Vectors (Fd : int; Vectors : System.Address; Count : long) return long is
      type Vector_Array is array (1 .. IOV_MAX) of Io_Vector;
      V : constant Vector_Array with Import, Address => Vectors;
      Total : long := 0;
      Got : long;
   begin
      if Count not in 0 .. IOV_MAX then
         return Error (EINVAL);
      end if;
      for I in 1 .. Integer (Count) loop
         Got := Fd_Read (Fd, To_Address (Integer_Address (V (I).Base)),
                         size_t (V (I).Length));
         if Got < 0 then
            return (if Total > 0 then Total else Got);
         end if;
         Total := Total + Got;
         exit when Unsigned_64 (Got) < V (I).Length;
      end loop;
      return Total;
   end Read_Vectors;

   function Socket_Flags (Type_Bits : long) return int is
     (int ((if Has (Type_Bits, SOCK_NONBLOCK) then O_NONBLOCK else 0)
           + (if Has (Type_Bits, SOCK_CLOEXEC) then O_CLOEXEC else 0)));

   function Tcp_Of (Fd : long) return System.Address is
     (Fd_Tcp (Low (Fd), System.Null_Address));

   function Socket_Error (Fd : long; Socket_Code, Other_Code : int) return long is
     (if Fd_Is_Socket (Low (Fd)) /= 0 then Error (Socket_Code) else Error (Other_Code));

   function Get_Socket_Option (Fd, Level, Name : long;
                               Value, Length : System.Address) return long;
   function Get_Socket_Option (Fd, Level, Name : long;
                               Value, Length : System.Address) return long
   is
      Socket : constant System.Address := Tcp_Of (Fd);
      Size : Interfaces.C.unsigned with Import, Address => Length;
      Option : int := 0;
      Socket_Receive_Buffer : constant int := 32_768;
   begin
      if Socket = System.Null_Address and then Fd_Is_Socket (Low (Fd)) = 0 then
         return Error (ENOTSOCK);
      elsif Size < int'Size / 8 then
         return Error (EINVAL);
      end if;
      if Level = SOL_SOCKET and then Name = SO_ERROR then
         Option := (if Socket /= System.Null_Address
                    then Low (Tcp_So_Error (Socket)) else 0);
      elsif Level = SOL_SOCKET and then Name = SO_TYPE then
         Option := int (SOCK_STREAM);
      elsif Level = SOL_SOCKET and then Name in SO_RCVBUF | SO_SNDBUF then
         Option := Socket_Receive_Buffer;
      elsif not (Level = IPPROTO_TCP and then Name = TCP_NODELAY)
        and then not (Level = SOL_SOCKET and then Name = SO_KEEPALIVE)
      then
         Unsupported ("getsockopt", Name);
         return Error (ENOPROTOOPT);
      end if;
      declare
         Target : int with Import, Address => Value;
      begin
         Target := Option;
      end;
      Size := int'Size / 8;
      return 0;
   end Get_Socket_Option;

   function Get_Socket_Name (Fd : long; Address, Length : System.Address) return long;
   function Get_Socket_Name (Fd : long; Address, Length : System.Address) return long is
      Socket : constant System.Address := Tcp_Of (Fd);
      --  struct sockaddr_in: family, port, address, zero padding.
      Sockaddr_In_Bytes : constant := 16;
      Size : Interfaces.C.unsigned with Import, Address => Length;
   begin
      if Socket /= System.Null_Address then
         return Tcp_Local (Socket, Address, Length);
      elsif Fd_Is_Socket (Low (Fd)) = 0 then
         return Error (ENOTSOCK);
      end if;
      --  An unbound socket: AF_INET, everything else zero.
      declare
         Local : Storage_Array (1 .. Sockaddr_In_Bytes) := [others => 0];
         Count : constant Storage_Offset :=
           Storage_Offset (Interfaces.C.unsigned'Min (Size, Sockaddr_In_Bytes));
         Target : Storage_Array (1 .. Count) with Import, Address => Address;
      begin
         Local (1) := Storage_Element (AF_INET);
         Target := Local (1 .. Count);
         Size := Sockaddr_In_Bytes;
      end;
      return 0;
   end Get_Socket_Name;

   ---------------------------------------------------------------------------
   --  The dispatch.
   ---------------------------------------------------------------------------
   function Dispatch (N, A, B, C, D, E, F : long) return long is
      Buffer, Second_Buffer : Path_Buffer;
      Path, Second : System.Address;
      R : long;
   begin
      case N is
         when SYS_write | SYS_sendto =>
            if N = SYS_sendto and then E /= 0 then
               return Error (EOPNOTSUPP);          --  connected pairs only
            end if;
            declare
               V : aliased constant Io_Vector := (Bits (B), Bits (C));
            begin
               return Fd_Writev (Low (A), V'Address, 1);
            end;
         when SYS_writev =>
            return Fd_Writev (Low (A), Address_Of (B), Low (C));
         when SYS_read =>
            return Fd_Read (Low (A), Address_Of (B), size_t'Mod (C));
         when SYS_recvfrom =>                      --  connected pairs only
            if E /= 0 then
               return Error (EOPNOTSUPP);
            end if;
            return Fd_Read (Low (A), Address_Of (B), size_t'Mod (C));
         when SYS_readv =>
            return Read_Vectors (Low (A), Address_Of (B), C);
         when SYS_pread64 =>
            return Fd_Pread (Low (A), Address_Of (B), size_t'Mod (C), D);
         when SYS_pwritev2 =>
            return Error (EOPNOTSUPP);             --  musl then uses pwrite64
         when SYS_pwrite64 =>
            return Fd_Pwrite (Low (A), Address_Of (B), size_t'Mod (C), D);
         when SYS_fsync | SYS_fdatasync =>
            return Fd_Fsync (Low (A));
         when SYS_close =>
            return Fd_Close (Low (A));
         when SYS_ioctl =>
            if B /= FIONBIO then
               return Error (ENOTTY);              --  no device or terminal
            elsif C = 0 then
               return Error (EFAULT);
            end if;
            R := Fd_Fcntl (Low (A), F_GETFL, 0);
            if R < 0 then
               return R;
            end if;
            declare
               On : constant int with Import, Address => Address_Of (C);
            begin
               R := To_Long (if On /= 0 then Bits (R) or Bits (O_NONBLOCK)
                              else Bits (R) and not Bits (O_NONBLOCK));
            end;
            return Fd_Fcntl (Low (A), F_SETFL, R);
         when SYS_fstat =>
            return Fd_Fstat (Low (A), Address_Of (B));
         when SYS_lseek =>
            return Fd_Lseek (Low (A), B, Low (C));
         when SYS_open =>
            return Fd_Open (Address_Of (A), Low (B));
         when SYS_openat =>
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            return (if R /= 0 then R else Fd_Open (Path, Low (C)));
         when SYS_stat | SYS_lstat =>
            return Path_Stat (Address_Of (A), Address_Of (B));
         when SYS_unlink =>
            return Path_Remove (Address_Of (A), Remove_File);
         when SYS_rmdir =>
            return Path_Remove (Address_Of (A), Remove_Directory);
         when SYS_unlinkat =>
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            return (if R /= 0 then R
                    else Path_Remove (Path, (if Has (C, AT_REMOVEDIR)
                                             then Remove_Directory else Remove_File)));
         when SYS_mkdir =>
            return Path_Mkdir (Address_Of (A));
         when SYS_mkdirat =>
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            return (if R /= 0 then R else Path_Mkdir (Path));
         when SYS_rename =>
            return Path_Rename (Address_Of (A), Address_Of (B));
         when SYS_renameat | SYS_renameat2 =>
            if N = SYS_renameat2 and then E /= 0 then
               return Error (EINVAL);              --  no RENAME_* flags
            end if;
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            if R = 0 then
               R := Resolve_At (C, Address_Of (D), Second_Buffer, Second);
            end if;
            return (if R /= 0 then R else Path_Rename (Path, Second));
         when SYS_newfstatat =>
            if Has (D, AT_EMPTY_PATH) then
               declare
                  First : constant Character with Import, Address => Address_Of (B);
               begin
                  if First = NUL then
                     return Fd_Fstat (Low (A), Address_Of (C));
                  end if;
               end;
            end if;
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            return (if R /= 0 then R else Path_Stat (Path, Address_Of (C)));
         when SYS_access =>
            return Check_Access (Address_Of (A), Low (B));
         when SYS_faccessat | SYS_faccessat2 =>
            R := Resolve_At (A, Address_Of (B), Buffer, Path);
            return (if R /= 0 then R else Check_Access (Path, Low (C)));
         when SYS_readlink | SYS_readlinkat =>
            --  No symbolic links: an existing name is not one.
            if N = SYS_readlinkat then
               R := Resolve_At (A, Address_Of (B), Buffer, Path);
               if R /= 0 then
                  return R;
               end if;
            else
               Path := Address_Of (A);
            end if;
            declare
               Status : Storage_Array (1 .. 256);  --  struct stat, 144 bytes
            begin
               R := Path_Stat (Path, Status'Address);
            end;
            return (if R /= 0 then R else Error (EINVAL));
         when SYS_ftruncate =>
            return Fd_Ftruncate (Low (A), B);
         when SYS_truncate =>
            return Path_Truncate (Address_Of (A), B);
         when SYS_getdents64 =>
            return Fd_Getdents (Low (A), Address_Of (B), size_t'Mod (C));
         when SYS_fcntl =>
            return Fd_Fcntl (Low (A), Low (B), C);
         when SYS_getcwd =>
            return Get_Cwd (Address_Of (A), size_t'Mod (B));
         when SYS_chdir =>
            return Change_Directory (Address_Of (A));
         when SYS_fchdir =>
            return Fd_Fchdir (Low (A));

         when SYS_brk =>
            return Grow_Break (Bits (A));
         when SYS_mmap =>
            return Map (Bits (B), C, D, Low (E), F);
         when SYS_munmap =>
            return (if Kernel (K.Release_Owned_Memory, Bits (A), Bits (B)) = 0
                    then 0 else Error (EINVAL));
         when SYS_madvise =>
            --  Advisory no-ops only. In particular no fork-related mapping
            --  semantics: AWS-LC probes support with invalid advice.
            --  DONTNEED does not reclaim backing yet.
            return (if C in MADV_NORMAL | MADV_RANDOM | MADV_SEQUENTIAL
                          | MADV_WILLNEED | MADV_DONTNEED | MADV_FREE
                    then 0 else Error (EINVAL));
         when SYS_mprotect =>
            if C not in PROT_NONE | PROT_READ | PROT_READ + PROT_WRITE then
               return Error (ENOSYS);
            end if;
            return (if Kernel (K.Protect_Owned_Memory, Bits (A), Bits (B), Bits (C)) = 0
                    then 0 else Error (EINVAL));
         when SYS_mremap =>
            return Error (ENOMEM);                 --  musl falls back to copying

         when SYS_futex =>
            return Futex (Address_Of (A), Low (B), Low (C), Address_Of (D), Low (F));
         when SYS_set_tid_address =>
            return Main_Thread_Id;
         when SYS_set_robust_list | SYS_sigaltstack =>
            return 0;
         when SYS_exit =>
            loop
               R := To_Long (Kernel (K.Thread_Exit));
            end loop;
         when SYS_exit_group =>
            Exit_Process (Low (A));
         when SYS_getpid =>
            --  The identity (KERN-003) folded to a pid_t, as posix_spawn
            --  reports children.
            declare
               Self : constant CuBit.Process_IDs.Process_ID :=
                 CuBit.Process_IDs.From_Word (Kernel (K.Get_Process_Id));
            begin
               return (if CuBit.Process_IDs.Is_Process (Self)
                       then long (CuBit.Process_IDs.POSIX_Of (Self))
                       else -1);
            end;
         when SYS_gettid =>
            return Thread_Id;
         when SYS_sched_yield =>
            R := To_Long (Kernel (K.Yield));
            return 0;
         when SYS_sched_getaffinity =>
            if B < CPU_Mask_Bytes then
               return Error (EINVAL);
            end if;
            Zero (Address_Of (C), Natural (long'Min (B, Maximum_CPU_Set_Bytes)));
            declare
               Mask : Unsigned_64 with Import, Address => Address_Of (C);
            begin
               Mask := Default_CPU_Mask;
            end;
            return CPU_Mask_Bytes;

         when SYS_clock_gettime =>
            return Clock_Get_Time (A, Address_Of (B));
         when SYS_clock_getres =>
            if B /= 0 then
               declare
                  T : Timespec with Import, Address => Address_Of (B);
               begin
                  T := (Seconds => 0,
                        Nanoseconds => (if Coarse (A) then Nanoseconds_Per_Millisecond
                                        else Nanoseconds_Per_Microsecond));
               end;
            end if;
            return 0;
         when SYS_nanosleep =>
            return Sleep (CLOCK_MONOTONIC, 0, Address_Of (A));
         when SYS_clock_nanosleep =>
            return Sleep (A, B, Address_Of (C));

         when SYS_poll =>
            return Fd_Poll (Address_Of (A), unsigned_long'Mod (B),
                            unsigned_long ((if Low (C) < 0 then Forever
                                            elsif C = 0 then 0
                                            else After (Now_Milliseconds, Bits (C)))));
         when SYS_ppoll =>
            declare
               Valid_Time : Boolean;
               Deadline : constant Unsigned_64 :=
                 Relative_Deadline (Address_Of (C), Valid_Time);
            begin
               if not Valid_Time then
                  return Error (EINVAL);
               end if;
               return Fd_Poll (Address_Of (A), unsigned_long'Mod (B),
                               unsigned_long (Deadline));
            end;
         when SYS_select =>
            declare
               T : constant Timeval with Import, Address => Address_Of (E);
            begin
               if E /= 0 and then not Valid (T) then
                  return Error (EINVAL);
               end if;
               return Select_Descriptors
                 (A, Address_Of (B), Address_Of (C), Address_Of (D),
                  (if E = 0 then Forever
                   else After (Now_Milliseconds, Milliseconds (T))));
            end;
         when SYS_pselect6 =>
            --  No signals are delivered, so the mask (F) changes nothing.
            declare
               Valid_Time : Boolean;
               Deadline : constant Unsigned_64 :=
                 Relative_Deadline (Address_Of (E), Valid_Time);
            begin
               if not Valid_Time then
                  return Error (EINVAL);
               end if;
               return Select_Descriptors
                 (A, Address_Of (B), Address_Of (C), Address_Of (D), Deadline);
            end;

         when SYS_pipe =>
            return Fd_Pipe (Address_Of (A), 0);
         when SYS_pipe2 =>
            return Fd_Pipe (Address_Of (A), Low (B));
         when SYS_socketpair =>
            --  Connected local stream pairs only.
            if A /= AF_UNIX
              or else To_Long (Bits (B) and Bits (Socket_Type_Mask)) /= SOCK_STREAM
            then
               return Error (EAFNOSUPPORT);
            end if;
            return Fd_Socketpair (Address_Of (D), Socket_Flags (B));
         when SYS_socket =>
            if A = AF_INET
              and then To_Long (Bits (B) and Bits (Socket_Type_Mask)) = SOCK_STREAM
              and then (C = 0 or else C = IPPROTO_TCP)
            then
               return Fd_Socket_Tcp (Socket_Flags (B));
            elsif A = AF_INET then
               return Error (EPROTONOSUPPORT);
            end if;
            return Error (EAFNOSUPPORT);
         when SYS_connect =>
            declare
               Nonblocking : aliased int := 0;
               Socket : constant System.Address :=
                 Fd_Tcp (Low (A), Nonblocking'Address);
            begin
               if Socket = System.Null_Address then
                  return Socket_Error (A, EISCONN, ENOTSOCK);
               end if;
               return Tcp_Connect (Socket, Address_Of (B),
                                   Interfaces.C.unsigned'Mod (C), Nonblocking);
            end;
         when SYS_getsockopt =>
            return Get_Socket_Option (A, B, C, Address_Of (D), Address_Of (E));
         when SYS_setsockopt =>
            --  netstack chooses these; the common tuning options are accepted.
            if Fd_Is_Socket (Low (A)) = 0 then
               return Error (ENOTSOCK);
            elsif (B = IPPROTO_TCP and then C = TCP_NODELAY)
              or else (B = SOL_SOCKET and then C in SO_KEEPALIVE | SO_REUSEADDR
                                               | SO_RCVBUF | SO_SNDBUF | SO_LINGER)
            then
               return 0;
            end if;
            Unsupported ("setsockopt", C);
            return Error (ENOPROTOOPT);
         when SYS_getpeername =>
            declare
               Socket : constant System.Address := Tcp_Of (A);
            begin
               if Socket = System.Null_Address then
                  return Socket_Error (A, ENOTCONN, ENOTSOCK);
               end if;
               return Tcp_Peer (Socket, Address_Of (B), Address_Of (C));
            end;
         when SYS_getsockname =>
            return Get_Socket_Name (A, Address_Of (B), Address_Of (C));
         when SYS_bind | SYS_listen =>
            declare
               Socket : constant System.Address := Tcp_Of (A);
            begin
               if Socket = System.Null_Address then
                  return Socket_Error (A, EOPNOTSUPP, ENOTSOCK);
               end if;
               return (if N = SYS_bind
                       then Tcp_Bind (Socket, Address_Of (B), Interfaces.C.unsigned'Mod (C))
                       else Tcp_Listen (Socket, Low (B)));
            end;
         when SYS_accept =>
            return Fd_Accept (Low (A), Address_Of (B), Address_Of (C), 0);
         when SYS_accept4 =>
            return Fd_Accept (Low (A), Address_Of (B), Address_Of (C), Socket_Flags (D));
         when SYS_dup =>
            return Fd_Dup (Low (A), 0, -1, 0);
         when SYS_dup2 =>
            return Fd_Dup (Low (A), 0, Low (B), 0);
         when SYS_dup3 =>
            if A = B then
               return Error (EINVAL);
            end if;
            return Fd_Dup (Low (A), 0, Low (B), (if Has (C, O_CLOEXEC) then 1 else 0));
         when SYS_shutdown =>
            declare
               Socket : constant System.Address := Tcp_Of (A);
            begin
               if Socket /= System.Null_Address then
                  return Tcp_Shutdown (Socket, Low (B));
               end if;
               return (if Fd_Is_Socket (Low (A)) /= 0 then 0 else Error (ENOTSOCK));
            end;

         when SYS_getrandom =>
            return Get_Random (Address_Of (A), size_t'Mod (B));

         when SYS_rt_sigprocmask =>
            --  No signal is ever blocked.
            if C /= 0 and then D > 0 then
               Zero (Address_Of (C),
                     Natural (long'Min (D, Signal_Set_Maximum_Bytes)));
            end if;
            return 0;
         when SYS_rt_sigaction =>
            if C /= 0 then
               Zero (Address_Of (C), Signal_Action_Bytes);
            end if;
            return 0;
         when SYS_kill | SYS_tkill | SYS_tgkill =>
            declare
               Signal : constant long := (if N = SYS_tgkill then C else B);
            begin
               if Signal = 0 then
                  return 0;
               end if;
               --  No handlers: a signal ends the process.
               Exit_Process (Low (Signal_Exit_Base + Signal));
            end;

         when SYS_getuid | SYS_geteuid | SYS_getgid | SYS_getegid =>
            return 0;
         when SYS_uname =>
            return Uname (Address_Of (A));
         when SYS_prlimit64 =>
            if D /= 0 then
               declare
                  type Limits is array (1 .. 2) of Unsigned_64;
                  Limit : Limits with Import, Address => Address_Of (D);
               begin
                  Limit := [others => RLIM_INFINITY];
               end;
            end if;
            return 0;
         when SYS_prctl =>
            if A = PR_SET_NAME then
               declare
                  Source : constant String (1 .. Thread_Name_Bytes - 1)
                  with Import, Address => Address_Of (B);
               begin
                  Thread_Name := [others => NUL];
                  for I in Source'Range loop
                     exit when Source (I) = NUL;
                     Thread_Name (I) := Source (I);
                  end loop;
               end;
               return 0;
            elsif A = PR_GET_NAME then
               declare
                  Target : String (1 .. Thread_Name_Bytes)
                  with Import, Address => Address_Of (B);
               begin
                  Target := Thread_Name;
               end;
               return 0;
            end if;
            return Error (EINVAL);
         when SYS_membarrier =>
            return Error (ENOSYS);
         when others =>
            Unimplemented (N);
            return Error (ENOSYS);
      end case;
   end Dispatch;

end CuBit.Libc_System_Calls;
