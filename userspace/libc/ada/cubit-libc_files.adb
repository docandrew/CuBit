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
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Filesystem_Queues;
with CuBit.Libc_File_Cache;
with CuBit.Libc_Dirty_Map;
with CuBit.Libc_Park_Table;
with CuBit.Path_Names;
with CuBit.Path_Names_C;

package body CuBit.Libc_Files is

   package K renames CuBit.Kernel_ABI;
   package FQ renames CuBit.Filesystem_Queues;
   package Cache_Model renames CuBit.Libc_File_Cache;
   package Dirty_Model renames CuBit.Libc_Dirty_Map;
   package Parks renames CuBit.Libc_Park_Table;
   package Q renames FQ.Queues;

   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.C.size_t;
   use type System.Address;

   ---------------------------------------------------------------------------
   --  The protocol (CuBit.Filesystems; tests/libc-ada checks these).
   ---------------------------------------------------------------------------
   Filesystem_Slot : constant := 1;
   OP_OPEN                : constant := 16#0001#;
   OP_CLOSE               : constant := 16#0002#;
   OP_OPEN_DIRECTORY      : constant := 16#0005#;
   OP_SEEK                : constant := 16#0006#;
   OP_READ_DIRECTORY_PAGE : constant := 16#0007#;
   OP_RENAME              : constant := 16#0008#;
   OP_CLOSE_DIRECTORY     : constant := 16#0009#;
   OP_FLUSH_FILE          : constant := 16#000C#;
   OP_READ_AT             : constant := 16#000D#;
   OP_WRITE_AT            : constant := 16#000E#;
   OP_RESIZE_FILE         : constant := 16#000F#;
   OP_UNLINK              : constant := 16#0010#;
   OP_MKDIR               : constant := 16#0011#;
   OP_RMDIR               : constant := 16#0012#;
   REPLY_NO_SPACE          : constant := 16#F002#;
   REPLY_READ_ONLY         : constant := 16#F003#;
   REPLY_OUT_OF_RANGE      : constant := 16#F004#;
   REPLY_ACCESS_DENIED     : constant := 16#F007#;
   REPLY_WRONG_OBJECT_TYPE : constant := 16#F009#;
   REPLY_ALREADY_EXISTS    : constant := 16#F00A#;
   REPLY_NOT_FOUND         : constant := 16#F00B#;
   REPLY_SHARING_VIOLATION : constant := 16#F00F#;
   REPLY_NOT_EMPTY         : constant := 16#F010#;
   REPLY_IS_DIRECTORY      : constant := 16#F011#;
   REPLY_INVALID_MOVE      : constant := 16#F012#;
   REPLY_CROSS_VOLUME      : constant := 16#F013#;
   PROTOCOL_VERSION : constant := 2;   --  CuBit.Filesystems: Directory.Page.V2
   Seek_From_End : constant := 2;
   Open_Read_Only : constant Unsigned_64 := 0;
   --  The kinds __cubit_path_remove takes (cubit_fd.h).
   Remove_Directory_Kind : constant int := 1;

   EEXIST    : constant int := 17;
   ENOSPC    : constant int := 28;
   EBUSY     : constant int := 16;
   ENOTEMPTY : constant int := 39;

   Bounce_Pages : constant := 64;
   Bounce_Bytes : constant := Bounce_Pages * K.Page_Bytes;
   Arena_Bytes : constant := FQ.Transfer_Bytes;
   --  The regions this client lends (CuBit.Channel_Protocol.Region_Pages):
   --  an arena has a control page before its buffers.
   Queue_Region_Pages : constant := FQ.Client_Pages;
   Arena_Region_Pages : constant := 1 + FQ.Transfer_Pages;
   Dirty_Region_Pages : constant := 1 + FQ.Dirty_Arena_Pages;
   Control_Bytes : constant := CuBit.Channel_Protocol.CONTROL_BYTES;
   Grant_Read_Write : constant := 1;
   --  Waiting for an answer: spin (the service answers within
   --  microseconds on another CPU), then yield between looks (it may need
   --  ours), then block in WAIT.
   Answer_Spins  : constant := 256;
   Answer_Yields : constant := 64;
   --  Answers nobody waits for (close, park, write-back) carry this bit.
   Async_Token : constant Unsigned_64 := 2 ** 63;
   Page_Bytes : constant := K.Page_Bytes;
   Readahead_Minimum : constant := 1;
   Readahead_Maximum : constant := Arena_Bytes / Page_Bytes;
   Cache_Chunk_Pages : constant := 512;
   Directory_Batch_Pages : constant := 64;
   Directory_Page_Bytes : constant := 4_096;
   Inspection_Bytes : constant := 64;

   ---------------------------------------------------------------------------
   --  Kernel calls, memory and the lock.
   ---------------------------------------------------------------------------
   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));
   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));
   function Error (Value : int) return long is (-long (Value));

   function Kernel (Number : K.System_Call; A0, A1, A2, A3, A4, A5 : Unsigned_64 := 0)
     return Unsigned_64 renames CuBit.Kernel_Calls.Call;

   function Mmap
     (Address : System.Address; Length : size_t; Protection, Flags : int;
      Descriptor : int; Offset : long) return System.Address
   with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : size_t) return int
   with Import, Convention => C, External_Name => "munmap";
   procedure Debug_Write (Text : System.Address; Length : size_t)
   with Import, Convention => C, External_Name => "cubit_debug_write";

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

   procedure Zero (Area : Unsigned_64; Bytes : Natural);
   procedure Zero (Area : Unsigned_64; Bytes : Natural) is
      Memory : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Area);
   begin
      Memory := [others => 0];
   end Zero;

   procedure Copy (Target, Source : Unsigned_64; Bytes : Natural);
   procedure Copy (Target, Source : Unsigned_64; Bytes : Natural) is
      To : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Target);
      From : constant Storage_Array (1 .. Storage_Offset (Bytes))
      with Import, Address => To_Address (Source);
   begin
      To := From;
   end Copy;

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

   procedure Pause;
   procedure Pause is
   begin
      Asm ("pause", Volatile => True);
   end Pause;

   FS_Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;
   procedure Lock;
   procedure Lock is
   begin
      CuBit.Libc_Imports.Lock (FS_Lock'Access);
   end Lock;
   procedure Unlock;
   procedure Unlock is
   begin
      CuBit.Libc_Imports.Unlock (FS_Lock'Access);
   end Unlock;

   --  A grant of Pages at Area to the filesystem service, in wire form; 0
   --  on failure.
   function Lend (Area : Unsigned_64; Pages : Unsigned_64) return Unsigned_64;
   function Lend (Area : Unsigned_64; Pages : Unsigned_64) return Unsigned_64 is
      Slot : constant Unsigned_64 := Kernel
        (K.Create_Shared_Memory_Grant_Via_Capability, Filesystem_Slot, Area, Pages,
         Grant_Read_Write);
      Generation : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      if Slot = K.Failed then
         return 0;
      end if;
      Generation := Kernel (K.Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = K.Failed or else Generation = 0 then
         Ignore := Kernel (K.Revoke_Shared_Memory_Grant, Slot);
         return 0;
      end if;
      return Shift_Left (Generation, K.Generation_Shift) or Slot;
   end Lend;

   procedure Revoke (Reference : Unsigned_64);
   procedure Revoke (Reference : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      if Reference /= 0 then
         Ignore := Kernel (K.Revoke_Shared_Memory_Grant, Reference and 16#FFFF_FFFF#);
      end if;
   end Revoke;

   --  One message to the service: its reply label, and word 0.
   function Call (Label : Unsigned_32; Length : Unsigned_8; W0, W1, W2, W3 : Unsigned_64;
                  Reply : out Unsigned_64) return Unsigned_32;
   function Call (Label : Unsigned_32; Length : Unsigned_8; W0, W1, W2, W3 : Unsigned_64;
                  Reply : out Unsigned_64) return Unsigned_32
   is
      M : aliased K.Message :=
        (Label => Label, Length => Length, Words => [W0, W1, W2, W3], others => <>);
      Tag : constant Unsigned_64 := Kernel
        (K.Call_Via_Endpoint_Capability, Filesystem_Slot, Value_Of (M'Address), CuBit.Kernel_ABI.Forever);
   begin
      Reply := M.Words (0);
      return Unsigned_32 (Tag and 16#FFFF_FFFF#);
   end Call;

   function Call (Label : Unsigned_32; Length : Unsigned_8; W0, W1, W2, W3 : Unsigned_64)
     return Unsigned_32;
   function Call (Label : Unsigned_32; Length : Unsigned_8; W0, W1, W2, W3 : Unsigned_64)
     return Unsigned_32
   is
      Ignore : Unsigned_64;
   begin
      return Call (Label, Length, W0, W1, W2, W3, Ignore);
   end Call;

   function To_Errno (Label : Unsigned_32) return long is
     (case Label is
        when REPLY_NOT_FOUND         => Error (ENOENT),
        when REPLY_ACCESS_DENIED     => Error (EACCES),
        when REPLY_WRONG_OBJECT_TYPE => Error (ENOTDIR),
        when REPLY_ALREADY_EXISTS    => Error (EEXIST),
        when REPLY_READ_ONLY         => Error (EROFS),
        when REPLY_NO_SPACE          => Error (ENOSPC),
        when REPLY_OUT_OF_RANGE      => Error (EINVAL),
        when REPLY_NOT_EMPTY         => Error (ENOTEMPTY),
        when REPLY_SHARING_VIOLATION => Error (EBUSY),
        when REPLY_IS_DIRECTORY      => Error (EISDIR),
        when REPLY_INVALID_MOVE      => Error (EINVAL),
        when REPLY_CROSS_VOLUME      => Error (EXDEV),
        when others                  => Error (EIO));

   ---------------------------------------------------------------------------
   --  The bounce buffer: one grant for message requests (no queue).
   ---------------------------------------------------------------------------
   Bounce : Unsigned_64 := 0;
   Bounce_Grant : Unsigned_64 := 0;     --  wire form

   function Lend_Bounce return long;
   function Lend_Bounce return long is
      Area : Unsigned_64;
   begin
      if Bounce /= 0 then
         return 0;
      end if;
      Area := Map_Pages (Bounce_Bytes);
      if Area = 0 then
         return Error (ENOMEM);
      end if;
      Bounce_Grant := Lend (Area, Bounce_Pages);
      if Bounce_Grant = 0 then
         Unmap (Area, Bounce_Bytes);
         return Error (EACCES);
      end if;
      Bounce := Area;
      return 0;
   end Lend_Bounce;

   function Grant_Slot return Unsigned_64 is (Bounce_Grant and 16#FFFF_FFFF#);
   function Grant_Generation return Unsigned_64 is (Shift_Right (Bounce_Grant, 32));

   ---------------------------------------------------------------------------
   --  The request queue.
   ---------------------------------------------------------------------------
   --  This client's region, the service's (mapped read-only), the arenas'
   --  first buffers, and the queue pair's channel number at the service.
   Queue, Server, Arena, Dirty : Unsigned_64 := 0;
   Queue_Number : Unsigned_64 := 0;
   Queue_Refused : Boolean := False;
   Client : Q.Client :=
     (Requests => (Produced => 0, Fill => 0),
      Answers => (Consumed => 0, Available => 0), Pending => 0);
   Token_Count : Unsigned_64 := 0;
   Async_Pending : Natural := 0;
   Kicked : Unsigned_32 := 0;
   --  Requests written while set go with the next one written while clear.
   Hold : Boolean := False;
   --  An early write-back under way (its token), and one finished since the
   --  dirty map was last rebuilt.
   Writeback_Token : Unsigned_64 := 0;
   Writeback_Done : Boolean := False;
   --  The last answer reaped: its token (without Async_Token), and for an
   --  open the rights its handle carries.
   Last_Token : Unsigned_64 := 0;
   Last_Rights : Unsigned_32 := 0;

   --  The words of this client's region (it writes them) and of the
   --  service's (read-only here).
   procedure Set_Client_Word (Offset : Natural; Value : Unsigned_32);
   procedure Set_Client_Word (Offset : Natural; Value : Unsigned_32) is
      Word : Unsigned_32 with Import, Volatile,
        Address => To_Address (Queue + Unsigned_64 (Offset));
   begin
      Word := Value;
   end Set_Client_Word;

   function Server_Word (Offset : Natural) return Unsigned_32;
   function Server_Word (Offset : Natural) return Unsigned_32 is
      Word : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (Server + Unsigned_64 (Offset));
   begin
      return Word;
   end Server_Word;

   --  The service's wake word showed it asleep: the channel protocol's
   --  one-way kick on the queue's channel.
   procedure Kick;
   procedure Kick is
      Ignore : constant Unsigned_64 := CuBit.Kernel_Calls.Submit
        (Filesystem_Slot, CuBit.Channel_Protocol.OP_KICK, 1, Queue_Number, 0, 0, 0,
         CuBit.Kernel_Calls.No_Completion_Token);
   begin
      null;
   end Kick;

   --  Open one channel on the filesystem endpoint (CuBit.Channel_Protocol),
   --  lending Area (Pages pages): its number there, and the service's
   --  region when it grants one back (Peer_Bytes of it), or False.
   function Open_Channel
     (Connector : Unsigned_16; Item : CuBit.Channel_Contracts.Contract;
      Area : Unsigned_64; Pages : Unsigned_64; Peer_Bytes : Unsigned_64;
      Number, Peer : out Unsigned_64; Own_Grant : out Unsigned_64) return Boolean;
   function Open_Channel
     (Connector : Unsigned_16; Item : CuBit.Channel_Contracts.Contract;
      Area : Unsigned_64; Pages : Unsigned_64; Peer_Bytes : Unsigned_64;
      Number, Peer : out Unsigned_64; Own_Grant : out Unsigned_64) return Boolean
   is
      use type CuBit.Channel_Contracts.Channel_Kind;
      Words : constant CuBit.Channel_Contracts.Words := CuBit.Channel_Contracts.Encode (Item);
      Flags : constant Unsigned_64 :=
        (if Item.Kind = CuBit.Channel_Contracts.Arena then Grant_Read_Write else 0) + K.Grant_Notify;
      Slot, Generation, Tag, Mapped : Unsigned_64;
      M : aliased K.Message;
   begin
      Number := 0;
      Peer := 0;
      Own_Grant := 0;
      Slot := Kernel (K.Create_Shared_Memory_Grant_Via_Capability, Filesystem_Slot, Area, Pages, Flags);
      if Slot = K.Failed then
         return False;
      end if;
      Generation := Kernel (K.Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = K.Failed or else Generation = 0 then
         Revoke (Slot);
         return False;
      end if;
      Own_Grant := Shift_Left (Generation, K.Generation_Shift) or Slot;
      M := (Label => CuBit.Channel_Protocol.OP_OPEN_PRODUCING,
            Length => CuBit.Channel_Protocol.Open_Words, Reserved => Connector,
            Words => [Words (0), Words (1), Words (2), Own_Grant], others => <>);
      Tag := Kernel (K.Call_Via_Endpoint_Capability, Filesystem_Slot, Value_Of (M'Address), CuBit.Kernel_ABI.Forever);
      if Unsigned_32 (Tag and 16#FFFF_FFFF#) /= K.Reply_OK then
         Revoke (Own_Grant);
         Own_Grant := 0;
         return False;
      end if;
      Number := M.Words (0);
      if Peer_Bytes > 0 then
         Mapped := Kernel
           (K.Acquire_Shared_Memory_Grant_Via_Capability, Filesystem_Slot,
            M.Words (1) and 16#FFFF_FFFF#, Shift_Right (M.Words (1), K.Generation_Shift),
            0, Peer_Bytes, 0);
         if Mapped = K.Failed or else Mapped = 0 then
            Revoke (Own_Grant);
            Own_Grant := 0;
            return False;
         end if;
         Peer := Mapped;
      end if;
      return True;
   end Open_Channel;

   --  Open the queue's channels once: the transfer arena, the dirty arena
   --  (without it, no write delegations; fine), then the queue pair.
   --  Whether the queue is usable.
   function Queue_Ready return Boolean;
   function Queue_Ready return Boolean is
      Q_Area, A_Area, D_Area : Unsigned_64;
      Q_Ref, A_Ref, D_Ref : Unsigned_64 := 0;
      Ignore_Number, Ignore_Peer, Server_Region : Unsigned_64;
      Dirty_Opened : Boolean := False;
   begin
      if Queue /= 0 then
         return True;
      elsif Queue_Refused then
         return False;
      end if;
      Queue_Refused := True;
      Q_Area := Map_Pages (Queue_Region_Pages * Page_Bytes);
      A_Area := Map_Pages (Arena_Region_Pages * Page_Bytes);
      D_Area := Map_Pages (Dirty_Region_Pages * Page_Bytes);
      if Q_Area /= 0 and then A_Area /= 0
        and then Open_Channel (FQ.Transfer_Connector, FQ.TRANSFER_CONTRACT, A_Area,
                               Arena_Region_Pages, 0, Ignore_Number, Ignore_Peer, A_Ref)
      then
         Dirty_Opened := D_Area /= 0
           and then Open_Channel (FQ.Dirty_Connector, FQ.DIRTY_CONTRACT, D_Area,
                                  Dirty_Region_Pages, 0, Ignore_Number, Ignore_Peer, D_Ref);
         Zero (Q_Area, Queue_Region_Pages * Page_Bytes);
         if Open_Channel (FQ.Queue_Connector, FQ.QUEUE_CONTRACT, Q_Area, Queue_Region_Pages,
                          FQ.Server_Pages * Page_Bytes, Queue_Number, Server_Region, Q_Ref)
         then
            Queue := Q_Area;
            Server := Server_Region;
            Arena := A_Area + Control_Bytes;
            --  Touched once now, so no read or write later takes a
            --  first-touch fault on them.
            Zero (Arena, Arena_Bytes);
            if Dirty_Opened then
               Dirty := D_Area + Control_Bytes;
               Zero (Dirty, FQ.Dirty_Arena_Bytes);
            else
               Unmap (D_Area, Dirty_Region_Pages * Page_Bytes);
            end if;
            Queue_Refused := False;
            return True;
         end if;
      end if;
      Revoke (D_Ref);
      Revoke (A_Ref);
      Revoke (Q_Ref);
      Unmap (D_Area, Dirty_Region_Pages * Page_Bytes);
      Unmap (A_Area, Arena_Region_Pages * Page_Bytes);
      Unmap (Q_Area, Queue_Region_Pages * Page_Bytes);
      return False;
   end Queue_Ready;

   --  Take the next answer, waiting for it.
   procedure Reap (Token : out Unsigned_64; Status : out Unsigned_32; Value, Spare : out Unsigned_64);
   procedure Reap (Token : out Unsigned_64; Status : out Unsigned_32; Value, Spare : out Unsigned_64) is
      Looks : Natural := 0;
      OK : Boolean;
      Answer : Q.Completion;
      Ring : constant Q.Completions.Ring with Import, Address => To_Address (Server + FQ.Server_Answers_At);
      Ignore : Unsigned_32;
   begin
      Token := 0;
      Status := 0;
      Value := 0;
      Spare := 0;
      loop
         Q.Completions.Accept_Produced
           (Client.Answers, Q.Completions.Index (Server_Word (FQ.Server_Answered_At)), OK);
         Compiler_Barrier;              --  the answer after its count
         exit when Client.Answers.Available > 0;
         if Looks < Answer_Spins then
            Pause;
         elsif Looks < Answer_Spins + Answer_Yields then
            Value := Kernel (K.Yield);
         else
            Ignore := Call (FQ.OP_FS_WAKE, 0, 0, 0, 0, 0);   --  returns once one waits
            Looks := 0;
         end if;
         Looks := Looks + 1;
      end loop;
      Q.Reap (Client, Ring, Answer, OK);
      Compiler_Barrier;                 --  copied out before the slot goes back
      Set_Client_Word (FQ.Client_Reaped_At, Unsigned_32 (Client.Answers.Consumed));
      if not OK then
         return;                        --  an answer nobody asked for
      end if;
      Token := Unsigned_64 (Answer.Tag);
      Status := Answer.Answer.Status;
      Last_Rights := Answer.Answer.Reserved;
      Value := Answer.Answer.Value;
      Spare := Answer.Answer.Spare;
      if (Token and Async_Token) /= 0 and then Async_Pending > 0 then
         Async_Pending := Async_Pending - 1;
      end if;
      Last_Token := Token and not Async_Token;
      if Token = Writeback_Token then   --  entries freed: our map is stale
         Writeback_Token := 0;
         Writeback_Done := True;
      end if;
   end Reap;

   procedure Reap_One;
   procedure Reap_One is
      T, V, S : Unsigned_64;
      Status : Unsigned_32;
   begin
      Reap (T, Status, V, S);
   end Reap_One;

   --  Reap answers already there, without waiting (only async ones can be:
   --  callers hold the lock between a request and its answer).
   procedure Reap_Ready;
   procedure Reap_Ready is
   begin
      while Async_Pending > 0
        and then Server_Word (FQ.Server_Answered_At) /= Unsigned_32 (Client.Answers.Consumed)
      loop
         Reap_One;
      end loop;
   end Reap_Ready;

   --  Put one request on the queue; its token.
   function Submit (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64;
                    Is_Async : Boolean) return Unsigned_64;
   function Submit (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64;
                    Is_Async : Boolean) return Unsigned_64
   is
      Ring : Q.Submissions.Ring with Import, Address => To_Address (Queue + FQ.Client_Requests_At);
      OK : Boolean;
      Token : Unsigned_64;
      Wake : Unsigned_32;
   begin
      loop
         Q.Accept_Taken (Client, Q.Submissions.Index (Server_Word (FQ.Server_Taken_At)), OK);
         exit when Q.Can_Submit (Client);
         Reap_One;                      --  only async answers can be out
      end loop;
      Token_Count := Token_Count + 1;
      Token := Token_Count or (if Is_Async then Async_Token else 0);
      if Is_Async then
         Async_Pending := Async_Pending + 1;
      end if;
      Q.Submit (Client, Ring, Q.Token (Token),
                (Operation => Operation, Options => Options, Handle => Handle,
                 Position => Position, Length => Length, Arena_Offset => 0,
                 Spare_1 => 0, Spare_2 => 0));
      Compiler_Barrier;                 --  the entry before the count
      if Hold then
         return Token;
      end if;
      Set_Client_Word (FQ.Client_Submitted_At, Unsigned_32 (Client.Requests.Produced));
      Full_Fence;                       --  the count before the wake word
      Wake := Server_Word (FQ.Server_Wake_At);
      if Wake /= 0 and then Wake /= Kicked then
         Kicked := Wake;
         Kick;
      end if;
      return Token;
   end Submit;

   procedure Submit_Async (Operation : Unsigned_32; Handle : Unsigned_64);
   procedure Submit_Async (Operation : Unsigned_32; Handle : Unsigned_64) is
      Ignore : constant Unsigned_64 := Submit (Operation, 0, Handle, 0, 0, True);
   begin
      null;
   end Submit_Async;

   --  One request, waiting for its answer: its status, value and spare.
   function Request (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64;
                     Value, Spare : out Unsigned_64) return Unsigned_32;
   function Request (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64;
                     Value, Spare : out Unsigned_64) return Unsigned_32
   is
      Token : constant Unsigned_64 := Submit (Operation, Options, Handle, Position, Length, False);
      Got : Unsigned_64;
      Status : Unsigned_32;
   begin
      loop
         Reap (Got, Status, Value, Spare);
         exit when Got = Token or else (Got and Async_Token) = 0;
      end loop;
      return (if Got = Token then Status else 0);
   end Request;

   function Request (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64)
     return Unsigned_32;
   function Request (Operation, Options : Unsigned_32; Handle, Position, Length : Unsigned_64)
     return Unsigned_32
   is
      V, S : Unsigned_64;
   begin
      return Request (Operation, Options, Handle, Position, Length, V, S);
   end Request;

   procedure Arena_Put (Text : String);
   procedure Arena_Put (Text : String) is
      Target : String (1 .. Text'Length) with Import, Address => To_Address (Arena);
   begin
      Target := Text;
   end Arena_Put;

   ---------------------------------------------------------------------------
   --  Names and the working directory.
   ---------------------------------------------------------------------------
   subtype Name_Text is String (1 .. CuBit.Path_Names.Maximum_Name_Bytes);
   Working_Lock : aliased CuBit.Libc_Imports.Lock_Word := 0;
   Working : Name_Text :=
     CuBit.Path_Names.System_Volume & "/"
     & [1 .. CuBit.Path_Names.Maximum_Name_Bytes - CuBit.Path_Names.System_Volume'Length - 1
        => ' '];
   Working_Length : Natural := CuBit.Path_Names.System_Volume'Length + 1;
   --  Whether the working directory was chosen (a chdir that saw it, or a
   --  launcher's procmgr checked the process may read); until then
   --  relative names start at the system volume's root, as absolute ones
   --  do, and children are given no directory.
   Working_Chosen : Boolean := False;

   --  The CuBit name for a path into Result (Name_Text'Length bytes); its
   --  length, or -errno.
   function Name_Of (Path : System.Address; Result : System.Address) return long;
   function Name_Of (Path : System.Address; Result : System.Address) return long is
      Base : Name_Text;
      Length : Natural;
   begin
      CuBit.Libc_Imports.Lock (Working_Lock'Access);
      Base := Working;
      Length := Working_Length;
      CuBit.Libc_Imports.Unlock (Working_Lock'Access);
      return CuBit.Path_Names_C.Resolve
        (Base'Address, size_t (Length), Path, Result, Name_Text'Length);
   end Name_Of;

   function Resolve (Path, Result : System.Address) return long is
     (Name_Of (Path, Result));

   function Change_Directory (Path : System.Address) return long is
      Name : aliased Name_Text;
      Length : constant long := Name_Of (Path, Name'Address);
      Handle, Size : aliased Unsigned_64 := 0;
      Terminated : String (1 .. Name_Text'Length + 1);
      R : long;
   begin
      if Length < 0 then
         return Length;
      end if;
      Terminated (1 .. Natural (Length)) := Name (1 .. Natural (Length));
      Terminated (Natural (Length) + 1) := Character'Val (0);
      --  A directory the process may read: it must exist and be one, and
      --  its scopes must show it. Nothing is accepted unseen.
      R := Open (Terminated'Address, 1, Open_Read_Only, Handle'Address, Size'Address);
      if R /= 0 then
         return R;
      end if;
      Close (Handle, 1);
      CuBit.Libc_Imports.Lock (Working_Lock'Access);
      Working (1 .. Natural (Length)) := Name (1 .. Natural (Length));
      Working_Length := Natural (Length);
      Working_Chosen := True;
      CuBit.Libc_Imports.Unlock (Working_Lock'Access);
      return 0;
   end Change_Directory;

   --  The launcher's working directory (launch block), if it is a CuBit
   --  name; otherwise the system volume's root stays.
   procedure Working_Directory_Start (Name : System.Address) is
      Resolved : aliased Name_Text;
      Length : long;
      First : constant Character with Import, Address => Name;
   begin
      if Name = System.Null_Address or else First /= CuBit.Path_Names.Volume_Mark then
         return;
      end if;
      Length := Name_Of (Name, Resolved'Address);
      if Length > 0 then
         Working (1 .. Natural (Length)) := Resolved (1 .. Natural (Length));
         Working_Length := Natural (Length);
         Working_Chosen := True;
      end if;
   end Working_Directory_Start;

   function Working_Directory_Name (Result, Chosen : System.Address) return size_t is
      Length : Natural;
   begin
      CuBit.Libc_Imports.Lock (Working_Lock'Access);
      Length := Working_Length;
      declare
         Target : String (1 .. Length) with Import, Address => Result;
      begin
         Target := Working (1 .. Length);
      end;
      if Chosen /= System.Null_Address then
         declare
            Flag : int with Import, Address => Chosen;
         begin
            Flag := (if Working_Chosen then 1 else 0);
         end;
      end if;
      CuBit.Libc_Imports.Unlock (Working_Lock'Access);
      return size_t (Length);
   end Working_Directory_Name;

   function Get_Working_Directory (Buffer : System.Address; Size : size_t) return long is
      Name : aliased Name_Text;
      Length : constant size_t := Working_Directory_Name (Name'Address, System.Null_Address);
   begin
      return CuBit.Path_Names_C.Display (Name'Address, Length, Buffer, Size);
   end Get_Working_Directory;

   ---------------------------------------------------------------------------
   --  Delegations and the page cache.
   ---------------------------------------------------------------------------
   subtype Handle_Slot is Natural range 0 .. FQ.Maximum_Delegations - 1;

   type Handle_State is record
      Handle         : Unsigned_64 := 0;          --  0: none
      File           : Cache_Model.File_Link := 0; --  0: not cached
      Size           : Unsigned_64 := 0;
      Next_Offset    : Unsigned_64 := 0;          --  where a sequential reader reads next
      Readahead      : Natural := 0;              --  pages
      Write          : Boolean := False;          --  a write delegation
      Generation     : Unsigned_32 := 0;          --  the namespace generation before its open
      Park_Token     : Unsigned_64 := 0;          --  its PARK request
      Service_Parked : Boolean := False;          --  PARK sent
   end record;
   Empty_Handle : constant Handle_State :=
     (Handle => 0, File => 0, Size => 0, Next_Offset => 0, Readahead => 0, Write => False,
      Generation => 0, Park_Token => 0, Service_Parked => False);
   --  Zero-filled (.bss): all-zero is the empty state.
   Handles : array (Handle_Slot) of Handle_State;
   pragma Suppress_Initialization (Handles);


   Cache : Cache_Model.Cache;
   pragma Suppress_Initialization (Cache);

   --  Each cache slot's 4 KiB buffer (mapped in chunks as the cache grows).
   Page_Data : array (Cache_Model.Page_Slot) of Unsigned_64;
   pragma Suppress_Initialization (Page_Data);


   function Delegation_Word (Slot : Handle_Slot; At_Byte : Natural) return Unsigned_32 is
     (Server_Word (FQ.Delegations_At + Slot * FQ.Delegation_Bytes + At_Byte));

   function Delegation_Long (Slot : Handle_Slot; At_Byte : Natural) return Unsigned_64;
   function Delegation_Long (Slot : Handle_Slot; At_Byte : Natural) return Unsigned_64 is
      Word : constant Unsigned_64 with Import, Volatile,
        Address => To_Address (Server + Unsigned_64 (FQ.Delegations_At
                               + Slot * FQ.Delegation_Bytes + At_Byte));
   begin
      return Word;
   end Delegation_Long;

   function Delegation_Valid (Slot : Handle_Slot) return Boolean is
     (Delegation_Word (Slot, FQ.Delegation_Valid_At) /= 0);

   --  A handle's slot, or -1 for one the cache does not track.
   function Slot_Of (Handle : Unsigned_64) return Integer;
   function Slot_Of (Handle : Unsigned_64) return Integer is
      Code : constant Unsigned_64 := Handle and 16#FFFF_FFFF#;
   begin
      return (if Code in 1 .. FQ.Maximum_Delegations then Integer (Code) - 1 else -1);
   end Slot_Of;

   function File_Uses return Cache_Model.File_Uses;
   function File_Uses return Cache_Model.File_Uses is
      Uses : Cache_Model.File_Uses := [others => False];
   begin
      for S in Handle_Slot loop
         if Handles (S).Handle /= 0 and then Handles (S).File /= 0 then
            Uses (Handles (S).File - 1) := True;
         end if;
      end loop;
      return Uses;
   end File_Uses;

   --  The handle's delegation, if it has one: its file's cache, size, mode.
   procedure Cache_Delegated (Slot : Handle_Slot);
   procedure Cache_Delegated (Slot : Handle_Slot) is
      Inode, Version, Delegated_Size : Unsigned_64;
      Mode : Unsigned_32;
      File : Cache_Model.File_Slot;
      Found : Boolean;
   begin
      if not Delegation_Valid (Slot) then
         return;
      end if;
      Compiler_Barrier;
      Inode := Delegation_Long (Slot, FQ.Delegation_Inode_At);
      Version := Delegation_Long (Slot, FQ.Delegation_Version_At);
      Delegated_Size := Delegation_Long (Slot, FQ.Delegation_Size_At);
      Compiler_Barrier;
      if not Delegation_Valid (Slot) or else Inode = 0 then
         return;
      end if;
      Mode := Delegation_Word (Slot, FQ.Delegation_Mode_At);
      Cache_Model.File_For (Cache, Inode, Version, File_Uses, File, Found);
      if not Found then
         return;
      end if;
      Handles (Slot).File := File + 1;
      Handles (Slot).Size := Delegated_Size;
      Handles (Slot).Write := Mode = FQ.Write_Delegation and then Dirty /= 0;
   end Cache_Delegated;

   --  After an open: cached under the handle's delegation, if any.
   procedure Cache_Opened (Handle, Size : Unsigned_64);
   procedure Cache_Opened (Handle, Size : Unsigned_64) is
      Slot : constant Integer := Slot_Of (Handle);
   begin
      if Slot < 0 then
         return;
      end if;
      Handles (Slot) := (Empty_Handle with delta
                           Handle => Handle, Size => Size, Readahead => Readahead_Minimum);
      Cache_Delegated (Slot);
   end Cache_Opened;

   --  The cached handle's slot, or -1.
   function Cached (Handle : Unsigned_64) return Integer;
   function Cached (Handle : Unsigned_64) return Integer is
      Slot : constant Integer := Slot_Of (Handle);
   begin
      if Slot < 0 or else Queue = 0 then
         return -1;
      elsif Handles (Slot).Handle /= Handle or else Handles (Slot).File = 0 then
         return -1;
      elsif not Delegation_Valid (Slot) then
         Handles (Slot).File := 0;
         return -1;
      end if;
      return Slot;
   end Cached;

   function Find_Page (File : Cache_Model.File_Link; Page : Unsigned_64)
     return Cache_Model.Page_Link;
   function Find_Page (File : Cache_Model.File_Link; Page : Unsigned_64)
     return Cache_Model.Page_Link
   is
      Found : Cache_Model.Page_Link;
   begin
      if File = 0 then
         return 0;
      end if;
      Cache_Model.Find (Cache, File - 1, Page, Found);
      return Found;
   end Find_Page;

   --  A slot for (File, Page), its buffer mapped (in chunks) as the cache
   --  grows; 0 if none.
   function New_Page (File : Cache_Model.File_Link; Page : Unsigned_64)
     return Cache_Model.Page_Link;
   function New_Page (File : Cache_Model.File_Link; Page : Unsigned_64)
     return Cache_Model.Page_Link
   is
      Next : constant Natural := Cache.Pages_Backed;
      Slot : Cache_Model.Page_Link;
   begin
      if File = 0 then
         return 0;
      end if;
      if Next < Cache_Model.Maximum_Pages and then Page_Data (Next) = 0
        and then Next mod Cache_Chunk_Pages = 0
      then
         declare
            Chunk : constant Unsigned_64 := Map_Pages (Cache_Chunk_Pages * Page_Bytes);
         begin
            if Chunk /= 0 then
               Zero (Chunk, Cache_Chunk_Pages * Page_Bytes);       --  no faults later
               for P in 0 .. Cache_Chunk_Pages - 1 loop
                  exit when Next + P >= Cache_Model.Maximum_Pages;
                  Page_Data (Next + P) := Chunk + Unsigned_64 (P * Page_Bytes);
               end loop;
            end if;
         end;
      end if;
      Cache_Model.New_Page
        (Cache, File - 1, Page,
         Next < Cache_Model.Maximum_Pages and then Page_Data (Next) /= 0, Slot);
      return Slot;
   end New_Page;

   function Data_Of (Slot : Cache_Model.Page_Link) return Unsigned_64 is
     (Page_Data (Slot - 1));

   --  Read through the cache: bytes read, or -1 to go to the service.
   function Cache_Read (Slot : Handle_Slot; Buffer : Unsigned_64; Count : Unsigned_64;
                        Offset : Unsigned_64) return long;
   function Cache_Read (Slot : Handle_Slot; Buffer : Unsigned_64; Count : Unsigned_64;
                        Offset : Unsigned_64) return long
   is
      C : Handle_State renames Handles (Slot);
      Wanted : Unsigned_64 := Count;
      Done : Unsigned_64 := 0;
   begin
      if Offset >= C.Size then
         return 0;
      end if;
      Wanted := Unsigned_64'Min (Wanted, C.Size - Offset);
      C.Readahead := (if Offset = C.Next_Offset
                      then Natural'Min (C.Readahead * 2, Readahead_Maximum)
                      else Readahead_Minimum);
      while Done < Wanted loop
         declare
            At_Byte : constant Unsigned_64 := Offset + Done;
            Page : constant Unsigned_64 := At_Byte / Page_Bytes;
            Inside : constant Unsigned_64 := At_Byte mod Page_Bytes;
            Take : constant Unsigned_64 := Unsigned_64'Min (Page_Bytes - Inside, Wanted - Done);
            P : Cache_Model.Page_Link := Find_Page (C.File, Page);
         begin
            if P = 0 then
               --  Fill from the service: this page and the readahead after it.
               declare
                  First : constant Unsigned_64 := Page * Page_Bytes;
                  Want : Unsigned_64 := Unsigned_64'Max
                    (Unsigned_64 (C.Readahead) * Page_Bytes, Offset + Wanted - First);
                  Got, Spare, Whole : Unsigned_64;
                  Label : Unsigned_32;
               begin
                  Want := Unsigned_64'Min (Unsigned_64'Min (Want, Arena_Bytes), C.Size - First);
                  Label := Request (FQ.Queue_Read_At, 0, C.Handle, First, Want, Got, Spare);
                  if Label /= K.Reply_OK or else Got > Want then
                     return -1;
                  end if;
                  --  The service may answer short. Only whole pages are
                  --  cached, and a partial one only when it ends the file:
                  --  a page cut short elsewhere is not known past the cut,
                  --  and zero-filling it would serve zeros for file bytes.
                  Whole := (if First + Got >= C.Size then Got
                            else Got - Got mod Page_Bytes);
                  if Whole < (At_Byte - First) + Take then
                     return -1;
                  end if;
                  declare
                     Off : Unsigned_64 := 0;
                  begin
                     while Off < Whole loop
                        declare
                           N : Cache_Model.Page_Link := Find_Page (C.File, Page + Off / Page_Bytes);
                           Length : constant Unsigned_64 := Unsigned_64'Min (Whole - Off, Page_Bytes);
                        begin
                           if N = 0 then
                              N := New_Page (C.File, Page + Off / Page_Bytes);
                           end if;
                           if N = 0 then
                              return -1;
                           end if;
                           Copy (Data_Of (N), Arena + Off, Natural (Length));
                           if Length < Page_Bytes then
                              Zero (Data_Of (N) + Length, Natural (Page_Bytes - Length));
                           end if;
                        end;
                        Off := Off + Page_Bytes;
                     end loop;
                  end;
                  P := Find_Page (C.File, Page);
                  if P = 0 then
                     return -1;
                  end if;
               end;
            end if;
            Copy (Buffer + Done, Data_Of (P) + Inside, Natural (Take));
            Done := Done + Take;
         end;
      end loop;
      Compiler_Barrier;                 --  the copies before the second look
      if not Delegation_Valid (Slot) then
         C.File := 0;
         return -1;
      end if;
      C.Next_Offset := Offset + Done;
      return long (Done);
   end Cache_Read;

   --  Our own write through a delegated handle: cached pages follow it.
   procedure Cache_Wrote (Handle : Unsigned_64; Buffer : Unsigned_64; Count : Unsigned_64;
                          Offset : Unsigned_64);
   procedure Cache_Wrote (Handle : Unsigned_64; Buffer : Unsigned_64; Count : Unsigned_64;
                          Offset : Unsigned_64)
   is
      Slot : constant Integer := Cached (Handle);
      Done : Unsigned_64 := 0;
   begin
      if Slot < 0 then
         return;
      end if;
      while Done < Count loop
         declare
            At_Byte : constant Unsigned_64 := Offset + Done;
            Inside : constant Unsigned_64 := At_Byte mod Page_Bytes;
            Take : constant Unsigned_64 := Unsigned_64'Min (Page_Bytes - Inside, Count - Done);
            P : Cache_Model.Page_Link := Find_Page (Handles (Slot).File, At_Byte / Page_Bytes);
         begin
            --  A whole page written is cached as it is.
            if P = 0 and then Inside = 0 and then Take = Page_Bytes then
               P := New_Page (Handles (Slot).File, At_Byte / Page_Bytes);
            end if;
            if P /= 0 then
               Copy (Data_Of (P) + Inside, Buffer + Done, Natural (Take));
            end if;
            Done := Done + Take;
         end;
      end loop;
      Handles (Slot).Size := Unsigned_64'Max (Handles (Slot).Size, Offset + Count);
      --  The service moved the version on for this write; our pages have it.
      if Handles (Slot).File /= 0 then
         Cache_Model.Set_Version
           (Cache, Handles (Slot).File - 1, Delegation_Long (Slot, FQ.Delegation_Version_At));
      end if;
   end Cache_Wrote;

   ---------------------------------------------------------------------------
   --  Buffered writes under a write delegation (the dirty arena). A written
   --  page is kept whole in the cache and copied to a dirty entry the
   --  service harvests on close, flush and write-back. An entry is taken by
   --  compare-and-swap (sequence made odd) and released by making it even;
   --  the delegation is checked again once it is held, so a recall that
   --  raced the write is noticed and the write goes to the service instead.
   ---------------------------------------------------------------------------
   Dirty_Map : Dirty_Model.Map;
   pragma Suppress_Initialization (Dirty_Map);

   Dirty_Cursor : Dirty_Model.Entry_Index := 0;

   function Entries return Dirty_Model.Entry_Table;
   function Entries return Dirty_Model.Entry_Table is
      Table : constant Dirty_Model.Entry_Table with Import, Address => To_Address (Dirty);
   begin
      return Table;
   end Entries;

   procedure Dirty_Rebuild;
   procedure Dirty_Rebuild is
   begin
      Dirty_Model.Rebuild (Dirty_Map, Entries);
      Writeback_Done := False;
   end Dirty_Rebuild;

   function Sequence_Address (E : Dirty_Model.Entry_Index) return System.Address is
     (To_Address (Dirty + Unsigned_64 (E * FQ.Dirty_Entry_Bytes + FQ.Dirty_Sequence_At)));

   function Compare_And_Swap (Where : System.Address; Expected, Desired : Unsigned_32)
     return Boolean;
   function Compare_And_Swap (Where : System.Address; Expected, Desired : Unsigned_32)
     return Boolean
   is
      Value : aliased Unsigned_32 := Expected;
      function Builtin (Ptr, Expected_Ptr : System.Address; Desired : Unsigned_32;
                        Weak : Boolean; Success, Failure : int) return Boolean
      with Import, Convention => Intrinsic, External_Name => "__atomic_compare_exchange_4";
      Sequentially_Consistent : constant int := 5;
   begin
      return Builtin (Where, Value'Address, Desired, False,
                      Sequentially_Consistent, Sequentially_Consistent);
   end Compare_And_Swap;

   procedure Store_Release (Where : System.Address; Value : Unsigned_32);
   procedure Store_Release (Where : System.Address; Value : Unsigned_32) is
      procedure Builtin (Ptr : System.Address; Value : Unsigned_32; Order : int)
      with Import, Convention => Intrinsic, External_Name => "__atomic_store_4";
      Release : constant int := 3;
   begin
      Builtin (Where, Value, Release);
   end Store_Release;

   function Sequence_Of (E : Dirty_Model.Entry_Index) return Unsigned_32;
   function Sequence_Of (E : Dirty_Model.Entry_Index) return Unsigned_32 is
      Word : constant Unsigned_32 with Import, Volatile, Address => Sequence_Address (E);
   begin
      return Word;
   end Sequence_Of;

   --  Take a free entry (sequence 0 -> 1); -1 if none is free.
   function Take_Free return Integer;
   function Take_Free return Integer is
      E : Dirty_Model.Entry_Index;
   begin
      for N in 1 .. FQ.Dirty_Entries loop
         E := Dirty_Cursor;
         Dirty_Cursor := (Dirty_Cursor + 1) mod FQ.Dirty_Entries;
         if Sequence_Of (E) = 0 and then Compare_And_Swap (Sequence_Address (E), 0, 1) then
            return E;
         end if;
      end loop;
      return -1;
   end Take_Free;

   --  Write Count bytes through the dirty arena; bytes written, or -1 to
   --  write through the service instead (recalled, or nothing written).
   function Cache_Write_Back (Slot : Handle_Slot; Buffer : Unsigned_64; Count : Unsigned_64;
                              Offset : Unsigned_64) return long;
   function Cache_Write_Back (Slot : Handle_Slot; Buffer : Unsigned_64; Count : Unsigned_64;
                              Offset : Unsigned_64) return long
   is
      C : Handle_State renames Handles (Slot);
      Done : Unsigned_64 := 0;
      Written_Back : Boolean := False;   --  asked for write-back since an entry
   begin
      while Done < Count loop
         declare
            At_Byte : constant Unsigned_64 := Offset + Done;
            Page : constant Unsigned_32 := Unsigned_32 (At_Byte / Page_Bytes and 16#FFFF_FFFF#);
            Inside : constant Unsigned_64 := At_Byte mod Page_Bytes;
            Take : constant Unsigned_64 := Unsigned_64'Min (Page_Bytes - Inside, Count - Done);
            First : constant Unsigned_64 := Unsigned_64 (Page) * Page_Bytes;
            P : Cache_Model.Page_Link := Find_Page (C.File, Unsigned_64 (Page));
            Tag : constant Unsigned_32 := Dirty_Model.Tag_Of (C.Handle, Slot);
            E : Integer;
            Fresh : Boolean := False;
            Held : Unsigned_32 := 0;
         begin
            --  The page as it is now, whole, in the cache.
            if P = 0 then
               if Inside = 0 and then Take = Page_Bytes then
                  P := New_Page (C.File, Unsigned_64 (Page));
               elsif First >= C.Size then
                  P := New_Page (C.File, Unsigned_64 (Page));
                  if P /= 0 then
                     Zero (Data_Of (P), Page_Bytes);
                  end if;
               else
                  declare
                     Got, Spare : Unsigned_64;
                  begin
                     --  A short answer is only the whole page when it ends
                     --  the file (as in Cache_Read): zeros past the cut
                     --  would be written back over file bytes.
                     if Request (FQ.Queue_Read_At, 0, C.Handle, First, Page_Bytes, Got, Spare)
                          /= K.Reply_OK or else Got > Page_Bytes
                       or else (Got < Page_Bytes and then First + Got < C.Size)
                     then
                        exit;
                     end if;
                     P := New_Page (C.File, Unsigned_64 (Page));
                     if P /= 0 then
                        Copy (Data_Of (P), Arena, Natural (Got));
                        Zero (Data_Of (P) + Got, Natural (Page_Bytes - Got));
                     end if;
                  end;
               end if;
               exit when P = 0;
            end if;
            --  An entry for the page, held (odd). Half the arena in use: the
            --  service starts writing it back while we go on; a full arena
            --  still waits for it.
            Reap_Ready;
            if Writeback_Done or else Dirty_Map.Tombstones > Dirty_Model.Map_Slots / 4 then
               Dirty_Rebuild;
            end if;
            if Writeback_Token = 0 and then Dirty_Map.Used >= FQ.Dirty_Entries / 2 then
               Writeback_Token := Submit (FQ.Queue_Writeback, 0, C.Handle, 0, 0, True);
            end if;
            Dirty_Model.Find (Dirty_Map, Entries, Tag, Page, E);
            if E >= 0 then
               declare
                  Even : constant Unsigned_32 := Sequence_Of (E);
               begin
                  if Even mod 2 /= 0
                    or else not Compare_And_Swap (Sequence_Address (E), Even, Even + 1)
                  then
                     E := -1;                --  taken meanwhile: a new one
                  else
                     Held := Even + 1;
                  end if;
               end;
            end if;
            if E < 0 then
               E := Take_Free;
               if E < 0 and then not Written_Back then
                  --  Full: the service writes back all our buffered pages,
                  --  then this page gets an entry.
                  Written_Back := True;
                  exit when Request (FQ.Queue_Writeback, 0, C.Handle, 0, 0) /= K.Reply_OK;
                  Dirty_Rebuild;
                  goto Again;
               end if;
               exit when E < 0;              --  still full: the rest goes through
               Written_Back := False;
               Fresh := True;
               Held := 1;
               if Dirty_Map.Used < FQ.Dirty_Entries then
                  Dirty_Map.Used := Dirty_Map.Used + 1;
               end if;
            end if;
            if not Delegation_Valid (Slot) then     --  recalled: undo
               Store_Release (Sequence_Address (E), (if Fresh then 0 else Held - 1));
               C.File := 0;
               exit;
            end if;
            Copy (Data_Of (P) + Inside, Buffer + Done, Natural (Take));
            declare
               End_Byte : constant Unsigned_64 := Unsigned_64'Max (At_Byte + Take, C.Size);
               Stop : constant Unsigned_64 := Unsigned_64'Min (End_Byte - First, Page_Bytes);
               Base : constant Unsigned_64 := Dirty + Unsigned_64 (E * FQ.Dirty_Entry_Bytes);
               Tag_Word : Unsigned_32 with Import, Volatile,
                 Address => To_Address (Base + FQ.Dirty_Slot_At);
               Page_Word : Unsigned_32 with Import, Volatile,
                 Address => To_Address (Base + FQ.Dirty_Page_At);
               Start_Word : Unsigned_16 with Import, Volatile,
                 Address => To_Address (Base + FQ.Dirty_Start_At);
               Stop_Word : Unsigned_16 with Import, Volatile,
                 Address => To_Address (Base + FQ.Dirty_Stop_At);
            begin
               Copy (Dirty + FQ.Dirty_Pages_At + Unsigned_64 (E * FQ.Dirty_Page_Bytes),
                     Data_Of (P), Natural (Stop));
               Tag_Word := Tag;
               Page_Word := Page;
               Start_Word := 0;
               Stop_Word := Unsigned_16 (Stop);
               if Fresh then
                  Dirty_Model.Remember (Dirty_Map, Tag, Page, E);
               end if;
               Store_Release (Sequence_Address (E), Held + 1);
               C.Size := End_Byte;
            end;
            Done := Done + Take;
         end;
         <<Again>>
      end loop;
      return (if Done > 0 then long (Done) else -1);
   end Cache_Write_Back;

   ---------------------------------------------------------------------------
   --  Parked handles (docs/filesystem-data-plane.md, "Metadata operations"):
   --  a closed file handle that may read, under a valid delegation, is kept
   --  open by its name instead, for a later read-only open of the name while
   --  the queue's namespace generation and the delegation both still hold.
   ---------------------------------------------------------------------------
   Park : Parks.Table;
   pragma Suppress_Initialization (Park);


   --  Forget slot's name and close its handle (async, as every close).
   procedure Handle_Close (Slot : Handle_Slot);
   procedure Handle_Close (Slot : Handle_Slot) is
      Handle : constant Unsigned_64 := Handles (Slot).Handle;
   begin
      if Park.Entries (Slot).Parked then
         Parks.Remove (Park, Slot);
      end if;
      Parks.Forget_Name (Park, Slot);
      Handles (Slot).Handle := 0;
      Submit_Async (FQ.Queue_Close, Handle);
   end Handle_Close;

   --  Close the parked handle for Name, if any (before the name changes);
   --  with Held, the close goes with the caller's next request.
   procedure Park_Drop (Name : String; Held : Boolean);
   procedure Park_Drop (Name : String; Held : Boolean) is
      Found : Parks.Link;
   begin
      if Name'Length > Parks.Name_Bytes then
         return;                          --  never parked
      end if;
      Parks.Find (Park, Name, Found);
      if Found /= 0 then
         Hold := Held;
         Handle_Close (Found - 1);
         Hold := False;
      end if;
   end Park_Drop;

   --  Park slot's handle at close; False if it cannot be.
   function Park_Handle (Slot : Handle_Slot) return Boolean;
   function Park_Handle (Slot : Handle_Slot) return Boolean is
      Other : Parks.Link;
      E : Parks.Slot_Entry renames Park.Entries (Slot);
   begin
      if E.Length = 0 or else Handles (Slot).File = 0
        or else not Delegation_Valid (Slot)
      then
         return False;
      end if;
      Parks.Find (Park, E.Name (1 .. E.Length), Other);
      if Other /= 0 then
         Handle_Close (Other - 1);          --  the newer handle is kept
      end if;
      if Park.Count >= Parks.Maximum_Parked and then Park.Oldest /= 0 then
         Handle_Close (Park.Oldest - 1);
      end if;
      --  Parked before and reused since (read-only, still delegated): the
      --  service's side is unchanged, so no request.
      if not Handles (Slot).Service_Parked then
         Handles (Slot).Park_Token :=
           Submit (FQ.Queue_Park, 0, Handles (Slot).Handle, 0, 0, True) and not Async_Token;
         Handles (Slot).Service_Parked := True;
      end if;
      if not E.Parked and then E.Length > 0 then
         Parks.Insert (Park, Slot);
      end if;
      return True;
   end Park_Handle;

   --  A read-only open of Name through a parked handle: True with its
   --  handle and size, or False to open it through the service. A parked
   --  handle that fails the checks is closed.
   function Unpark (Name : String; Handle, Size : out Unsigned_64) return Boolean;
   function Unpark (Name : String; Handle, Size : out Unsigned_64) return Boolean is
      Found : Parks.Link;
      Slot : Handle_Slot;
      Generation : Unsigned_32;
      Inode, Written_Size : Unsigned_64;
   begin
      Handle := 0;
      Size := 0;
      if Park.Count = 0 or else Name'Length > Parks.Name_Bytes then
         return False;
      end if;
      Parks.Find (Park, Name, Found);
      if Found = 0 then
         return False;
      end if;
      Slot := Found - 1;
      --  Its PARK answered first: the delegation is then a reader's.
      while Last_Token < Handles (Slot).Park_Token loop
         Reap_One;
      end loop;
      Generation := Server_Word (FQ.Server_Namespace_At);
      Compiler_Barrier;
      Inode := Delegation_Long (Slot, FQ.Delegation_Inode_At);
      Compiler_Barrier;
      if Generation /= Handles (Slot).Generation or else not Delegation_Valid (Slot)
        or else Handles (Slot).File = 0
        or else Inode /= Cache.Files (Handles (Slot).File - 1).Inode
      then
         Handle_Close (Slot);
         return False;
      end if;
      Parks.Remove (Park, Slot);
      Written_Size := Handles (Slot).Size;
      Handles (Slot).Next_Offset := 0;
      Handles (Slot).Readahead := Readahead_Minimum;
      Handles (Slot).File := 0;
      Cache_Delegated (Slot);
      if Handles (Slot).File = 0 then         --  revoked meanwhile
         Handle_Close (Slot);
         return False;
      end if;
      --  Still a write delegation: pages we wrote may not have reached the
      --  service yet, so our size is the file's.
      if Handles (Slot).Write and then Written_Size > Handles (Slot).Size then
         Handles (Slot).Size := Written_Size;
      end if;
      Handle := Handles (Slot).Handle;
      Size := Handles (Slot).Size;
      return True;
   end Unpark;

   --  At exit: parked handles are closed, and every async answer is waited
   --  for, so the service has handled them before the process goes.
   procedure Finish;
   pragma Linker_Destructor (Finish);
   procedure Finish is
   begin
      Lock;
      if Queue /= 0 then
         while Park.Oldest /= 0 loop
            Handle_Close (Park.Oldest - 1);
         end loop;
         while Async_Pending > 0 loop
            Reap_One;
         end loop;
      end if;
      Unlock;
   end Finish;

   ---------------------------------------------------------------------------
   --  The operations.
   ---------------------------------------------------------------------------
   Denied_Text : constant String := "cubit-libc: denied: ";
   Line_Feed : constant String := [1 => ASCII.LF];

   function Open (Path : System.Address; Directory : int; Options : Unsigned_64;
                  Handle, Size : System.Address) return long
   is
      Name : aliased Name_Text;
      Length : constant long := Name_Of (Path, Name'Address);
      H, S : Unsigned_64 := 0;
      Label : Unsigned_32;
      Result_Handle : Unsigned_64 with Import, Address => Handle;
   begin
      if Length < 0 then
         return Length;
      end if;
      Lock;
      if Queue_Ready then
         declare
            N : String renames Name (1 .. Natural (Length));
            Generation : Unsigned_32;
         begin
            if Directory = 0 and then Options = Open_Read_Only and then Unpark (N, H, S) then
               Unlock;
               Result_Handle := H;
               if Size /= System.Null_Address then
                  declare
                     Result_Size : Unsigned_64 with Import, Address => Size;
                  begin
                     Result_Size := S;
                  end;
               end if;
               return 0;
            end if;
            --  A writable open: our parked handle for the name goes first,
            --  or the file would have another handle and this one no write
            --  delegation.
            if Directory = 0 and then Options /= Open_Read_Only then
               Park_Drop (N, Held => True);
            end if;
            --  Read before the open: a namespace change after it resolves
            --  the name leaves this generation behind.
            Generation := Server_Word (FQ.Server_Namespace_At);
            Compiler_Barrier;
            Arena_Put (N);
            Label := (if Directory /= 0
                      then Request (FQ.Queue_Open_Directory, 0, 0, 0, Unsigned_64 (Length), H, S)
                      else Request (FQ.Queue_Open, Unsigned_32 (Options), 0, 0,
                                    Unsigned_64 (Length), H, S));
            if Directory /= 0 then
               S := 0;
            end if;
            if Label = K.Reply_OK and then Directory = 0 then
               Cache_Opened (H, S);
               declare
                  Slot : constant Integer := Slot_Of (H);
               begin
                  --  A handle that may read can be parked at close.
                  if Slot >= 0 and then (Last_Rights and FQ.Rights_Read) /= 0
                    and then not Park.Entries (Slot).Parked
                    and then N'Length <= Parks.Name_Bytes
                  then
                     Parks.Set_Name (Park, Slot, N);
                     Handles (Slot).Generation := Generation;
                  end if;
               end;
            end if;
            Unlock;
            if Label /= K.Reply_OK then
               return To_Errno (Label);
            end if;
         end;
      else
         declare
            R : constant long := Lend_Bounce;
            Ignore : Unsigned_32;
         begin
            if R /= 0 then
               Unlock;
               return R;
            end if;
            Copy (Bounce, Value_Of (Name'Address), Natural (Length));
            Label := (if Directory /= 0
                      then Call (OP_OPEN_DIRECTORY, 3, Grant_Slot, Unsigned_64 (Length),
                                 Grant_Generation, 0, H)
                      else Call (OP_OPEN, 4, Grant_Slot, Unsigned_64 (Length), Options,
                                 Grant_Generation, H));
            if Label /= K.Reply_OK then
               Unlock;
               if Label = REPLY_ACCESS_DENIED then
                  --  A request for authority the program lacks: say which.
                  Debug_Write (Denied_Text'Address, Denied_Text'Length);
                  Debug_Write (Name'Address, size_t (Length));
                  Debug_Write (Line_Feed'Address, 1);
               end if;
               return To_Errno (Label);
            end if;
            if Directory = 0 then
               Label := Call (OP_SEEK, 3, H, 0, Seek_From_End, 0, S);
               if Label /= K.Reply_OK then
                  Ignore := Call (OP_CLOSE, 1, H, 0, 0, 0);
                  Unlock;
                  return To_Errno (Label);
               end if;
            end if;
            Unlock;
         end;
      end if;
      Result_Handle := H;
      if Size /= System.Null_Address then
         declare
            Result_Size : Unsigned_64 with Import, Address => Size;
         begin
            Result_Size := S;
         end;
      end if;
      return 0;
   end Open;

   --  Directory pages fetched ahead (Directory_Read_Page).
   type Directory_Page is array (1 .. Directory_Page_Bytes) of Unsigned_8;
   type Directory_Pages is array (1 .. Directory_Batch_Pages) of Directory_Page;
   Batch_Handle : Unsigned_64 := 0;          --  0: empty
   Batch_Count, Batch_Next : Natural := 0;
   Batch : Directory_Pages;
   pragma Suppress_Initialization (Batch);


   procedure Close (Handle : Unsigned_64; Directory : int) is
      Ignore : Unsigned_32;
   begin
      Lock;
      if Directory /= 0 and then Batch_Handle = Handle then
         Batch_Handle := 0;
      end if;
      if Queue /= 0 then
         if Directory /= 0 then
            Submit_Async (FQ.Queue_Close_Directory, Handle);     --  no wait
         else
            --  Nobody waits for a close (write-back errors show at fsync).
            declare
               Slot : constant Integer := Slot_Of (Handle);
            begin
               if Slot >= 0 and then Handles (Slot).Handle = Handle then
                  if Park.Entries (Slot).Parked or else not Park_Handle (Slot) then
                     Handle_Close (Slot);
                  end if;
               else
                  Submit_Async (FQ.Queue_Close, Handle);
               end if;
            end;
         end if;
      else
         Ignore := Call ((if Directory /= 0 then OP_CLOSE_DIRECTORY else OP_CLOSE), 1,
                         Handle, 0, 0, 0);
      end if;
      Unlock;
   end Close;

   function Read_At (Handle : Unsigned_64; Buffer : System.Address; Count : size_t;
                     Offset : Unsigned_64) return long
   is
      Done : Unsigned_64 := 0;
      Wanted : constant Unsigned_64 := Unsigned_64 (Count);
      Target : constant Unsigned_64 := Value_Of (Buffer);
      Slot : Integer;
      N : long;
   begin
      Lock;
      Slot := Cached (Handle);
      if Slot >= 0 then
         N := Cache_Read (Slot, Target, Wanted, Offset);
         if N >= 0 then
            Unlock;
            return N;
         end if;
      end if;
      if Queue_Ready then
         while Done < Wanted loop
            declare
               Want : constant Unsigned_64 := Unsigned_64'Min (Wanted - Done, Arena_Bytes);
               Got, Spare : Unsigned_64;
               Label : constant Unsigned_32 :=
                 Request (FQ.Queue_Read_At, 0, Handle, Offset + Done, Want, Got, Spare);
            begin
               Got := Unsigned_64'Min (Got, Want);
               Copy (Target + Done, Arena, Natural (Got));
               Done := Done + Got;
               if Label /= K.Reply_OK then
                  Unlock;
                  return (if Done > 0 then long (Done) else To_Errno (Label));
               end if;
               exit when Got < Want;          --  end of file
            end;
         end loop;
         Unlock;
         return long (Done);
      end if;
      N := Lend_Bounce;
      if N /= 0 then
         Unlock;
         return N;
      end if;
      while Done < Wanted loop
         declare
            Want : constant Unsigned_64 := Unsigned_64'Min (Wanted - Done, Bounce_Bytes);
            Got : Unsigned_64;
            Label : constant Unsigned_32 :=
              Call (OP_READ_AT, 4, Handle, Bounce_Grant, Want, Offset + Done, Got);
         begin
            if Label /= K.Reply_OK then
               if Got > 0 and then Got <= Want then
                  Copy (Target + Done, Bounce, Natural (Got));
                  Done := Done + Got;
               end if;
               Unlock;
               return (if Done > 0 then long (Done) else To_Errno (Label));
            end if;
            Got := Unsigned_64'Min (Got, Want);
            Copy (Target + Done, Bounce, Natural (Got));
            Done := Done + Got;
            exit when Got < Want;              --  end of file
         end;
      end loop;
      Unlock;
      return long (Done);
   end Read_At;

   function Write_At (Handle : Unsigned_64; Buffer : System.Address; Count : size_t;
                      Offset : Unsigned_64) return long
   is
      Done : Unsigned_64 := 0;
      Wanted : constant Unsigned_64 := Unsigned_64 (Count);
      Source : constant Unsigned_64 := Value_Of (Buffer);
      Slot : Integer;
      N : long;
   begin
      Lock;
      Slot := Cached (Handle);
      if Slot >= 0 and then Handles (Slot).Write then
         N := Cache_Write_Back (Slot, Source, Wanted, Offset);
         if N >= 0 and then Unsigned_64 (N) = Wanted then
            Unlock;
            return N;
         end if;
         if N > 0 then
            Done := Unsigned_64 (N);
         end if;
      end if;
      if Queue_Ready then
         while Done < Wanted loop
            declare
               Want : constant Unsigned_64 := Unsigned_64'Min (Wanted - Done, Arena_Bytes);
               Put, Spare : Unsigned_64;
               Label : Unsigned_32;
            begin
               Copy (Arena, Source + Done, Natural (Want));
               Label := Request (FQ.Queue_Write_At, 0, Handle, Offset + Done, Want, Put, Spare);
               Put := Unsigned_64'Min (Put, Want);
               Cache_Wrote (Handle, Source + Done, Put, Offset + Done);
               Done := Done + Put;
               if Label /= K.Reply_OK then
                  Unlock;
                  return (if Done > 0 then long (Done) else To_Errno (Label));
               end if;
               exit when Put < Want;
            end;
         end loop;
         Unlock;
         return long (Done);
      end if;
      N := Lend_Bounce;
      if N /= 0 then
         Unlock;
         return N;
      end if;
      while Done < Wanted loop
         declare
            Want : constant Unsigned_64 := Unsigned_64'Min (Wanted - Done, Bounce_Bytes);
            Put : Unsigned_64;
            Label : Unsigned_32;
         begin
            Copy (Bounce, Source + Done, Natural (Want));
            Label := Call (OP_WRITE_AT, 4, Handle, Bounce_Grant, Want, Offset + Done, Put);
            Put := Unsigned_64'Min (Put, Want);
            Done := Done + Put;
            if Label /= K.Reply_OK then
               Unlock;
               return (if Done > 0 then long (Done) else To_Errno (Label));
            end if;
            exit when Put < Want;
         end;
      end loop;
      Unlock;
      return long (Done);
   end Write_At;

   function Flush (Handle : Unsigned_64) return long is
      Label : Unsigned_32;
   begin
      Lock;
      Label := (if Queue /= 0 then Request (FQ.Queue_Flush, 0, Handle, 0, 0)
                else Call (OP_FLUSH_FILE, 1, Handle, 0, 0, 0));
      if Dirty /= 0 then
         Dirty_Rebuild;                  --  the flush harvested this handle's
      end if;
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Flush;

   --  The handle's object as the volume records it (through the queue
   --  only: without one, -ENOSYS). A write-delegated handle's buffered
   --  pages are taken in first, which frees their entries.
   function Describe (Handle : Unsigned_64; Inspection : System.Address) return long is
      Label : Unsigned_32;
   begin
      Lock;
      if not Queue_Ready then
         Unlock;
         return Error (ENOSYS);
      end if;
      Label := Request (FQ.Queue_Describe, 0, Handle, 0, Inspection_Bytes);
      if Label = K.Reply_OK then
         Copy (Value_Of (Inspection), Arena, Inspection_Bytes);
      end if;
      if Dirty /= 0 then
         Dirty_Rebuild;
      end if;
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Describe;

   --  A file's new size (ftruncate), after this client's earlier writes.
   function Resize (Handle : Unsigned_64; Size : Unsigned_64) return long is
      Label : Unsigned_32;
      Slot : Integer;
   begin
      Lock;
      Label := (if Queue /= 0 then Request (FQ.Queue_Resize, 0, Handle, 0, Size)
                else Call (OP_RESIZE_FILE, 2, Handle, Size, 0, 0));
      if Dirty /= 0 then
         Dirty_Rebuild;
      end if;
      Slot := Slot_Of (Handle);
      if Label = K.Reply_OK and then Slot >= 0 and then Handles (Slot).Handle = Handle then
         Handles (Slot).Size := Size;
         Handles (Slot).File := 0;        --  its version moved on
         Cache_Delegated (Slot);
      end if;
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Resize;

   --  What the policy lets this process do with a name (access): whether
   --  it is a directory, may be written (or created in), and its
   --  inspection. Opens and closes a handle of its own, never a parked one.
   function Path_Access (Path, Directory, May_Write, Inspection, Described :
                           System.Address) return long
   is
      Name : aliased Name_Text;
      Length : constant long := Name_Of (Path, Name'Address);
      Is_Directory : int with Import, Address => Directory;
      Writable : int with Import, Address => May_Write;
      Was_Described : int with Import, Address => Described;
      H, S : Unsigned_64 := 0;
      Label : Unsigned_32;
   begin
      if Length < 0 then
         return Length;
      end if;
      Was_Described := 0;
      Is_Directory := 0;
      Lock;
      if not Queue_Ready then
         Unlock;
         return Error (ENOSYS);
      end if;
      Arena_Put (Name (1 .. Natural (Length)));
      Label := Request (FQ.Queue_Open, Unsigned_32 (Open_Read_Only), 0, 0,
                        Unsigned_64 (Length), H, S);
      if Label = REPLY_WRONG_OBJECT_TYPE then
         Is_Directory := 1;
         Arena_Put (Name (1 .. Natural (Length)));
         Label := Request (FQ.Queue_Open_Directory, 0, 0, 0, Unsigned_64 (Length), H, S);
      end if;
      if Label /= K.Reply_OK then
         Unlock;
         return To_Errno (Label);
      end if;
      Writable := (if (Last_Rights and FQ.Rights_Policy_Write) /= 0 then 1 else 0);
      if Request (FQ.Queue_Describe, 0, H, 0, Inspection_Bytes) = K.Reply_OK then
         Copy (Value_Of (Inspection), Arena, Inspection_Bytes);
         Was_Described := 1;
      end if;
      Submit_Async ((if Is_Directory /= 0 then FQ.Queue_Close_Directory else FQ.Queue_Close), H);
      Unlock;
      return 0;
   end Path_Access;

   --  Remove a file or an empty directory, or make a directory, by name.
   function Path_Operation (Path : System.Address; Queue_Op, Message_Op : Unsigned_32)
     return long;
   function Path_Operation (Path : System.Address; Queue_Op, Message_Op : Unsigned_32)
     return long
   is
      Name : aliased Name_Text;
      Length : constant long := Name_Of (Path, Name'Address);
      Label : Unsigned_32;
      R : long;
   begin
      if Length < 0 then
         return Length;
      end if;
      Lock;
      if Queue_Ready then
         declare
            N : String renames Name (1 .. Natural (Length));
            Parked_Handle : Unsigned_64 := 0;
            Found : Parks.Link;
         begin
            --  A parked handle for the name goes with the request: UNLINK
            --  closes it (dropping its buffered pages if the file dies with
            --  it); before RMDIR it is closed.
            if Queue_Op = FQ.Queue_Unlink and then N'Length <= Parks.Name_Bytes then
               Parks.Find (Park, N, Found);
               if Found /= 0 then
                  Parked_Handle := Handles (Found - 1).Handle;
                  Parks.Remove (Park, Found - 1);
                  Parks.Forget_Name (Park, Found - 1);
                  Handles (Found - 1).Handle := 0;
               end if;
            elsif Queue_Op = FQ.Queue_Rmdir then
               Park_Drop (N, Held => True);
            end if;
            Arena_Put (N);
            Label := Request (Queue_Op, 0, Parked_Handle, 0, Unsigned_64 (Length));
         end;
      else
         R := Lend_Bounce;
         if R /= 0 then
            Unlock;
            return R;
         end if;
         Copy (Bounce, Value_Of (Name'Address), Natural (Length));
         Label := Call (Message_Op, 4, Grant_Slot, Unsigned_64 (Length), 0, Grant_Generation);
      end if;
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Path_Operation;

   function Path_Remove (Path : System.Address; Kind : int) return long is
     (if Kind = Remove_Directory_Kind then Path_Operation (Path, FQ.Queue_Rmdir, OP_RMDIR)
      else Path_Operation (Path, FQ.Queue_Unlink, OP_UNLINK));

   function Path_Mkdir (Path : System.Address) return long is
     (Path_Operation (Path, FQ.Queue_Mkdir, OP_MKDIR));

   --  Rename or move a file or directory within a volume (POSIX
   --  replacement of an existing target; -EXDEV across volumes). Through
   --  the queue (Queue_Rename): both names in the arena, split at Position.
   function Path_Rename (From, To : System.Address) return long is
      Old_Name, New_Name : aliased Name_Text;
      Old_Length : constant long := Name_Of (From, Old_Name'Address);
      New_Length : long;
      R : long;
      Label : Unsigned_32;
   begin
      if Old_Length < 0 then
         return Old_Length;
      end if;
      New_Length := Name_Of (To, New_Name'Address);
      if New_Length < 0 then
         return New_Length;
      end if;
      Lock;
      if Queue /= 0 then                   --  the names are about to change
         Park_Drop (Old_Name (1 .. Natural (Old_Length)), Held => False);
         Park_Drop (New_Name (1 .. Natural (New_Length)), Held => False);
      end if;
      if Old_Length + New_Length <= long (Arena_Bytes) and then Queue_Ready then
         Arena_Put (Old_Name (1 .. Natural (Old_Length)) & New_Name (1 .. Natural (New_Length)));
         Label := Request (FQ.Queue_Rename, 0, 0, Unsigned_64 (Old_Length),
                           Unsigned_64 (Old_Length + New_Length));
         Unlock;
         return (if Label = K.Reply_OK then 0 else To_Errno (Label));
      end if;
      R := Lend_Bounce;
      if R /= 0 then
         Unlock;
         return R;
      end if;
      Copy (Bounce, Value_Of (Old_Name'Address), Natural (Old_Length));
      Copy (Bounce + Unsigned_64 (Old_Length), Value_Of (New_Name'Address), Natural (New_Length));
      Label := Call (OP_RENAME, 4, Grant_Slot, Unsigned_64 (Old_Length),
                     Unsigned_64 (New_Length), Grant_Generation);
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Path_Rename;

   --  One Directory.Page.V2 (no metadata: readdir needs names and kinds)
   --  into Page; the caller checks it. Through the queue, pages come in
   --  batches and are handed out one per call until the batch that ends
   --  the directory is used up.
   function Directory_Read_Page (Handle : Unsigned_64; Page : System.Address) return long is
      Target : Directory_Page with Import, Address => Page;
      R : long;
      Label : Unsigned_32;
   begin
      Lock;
      if Batch_Handle = Handle and then Batch_Next < Batch_Count then
         Batch_Next := Batch_Next + 1;
         Target := Batch (Batch_Next);
         Unlock;
         return 0;
      end if;
      if Queue_Ready then
         declare
            Filled, Spare : Unsigned_64;
         begin
            Label := Request (FQ.Queue_Read_Directory, 0, Handle, 0,
                              Directory_Batch_Pages * Directory_Page_Bytes, Filled, Spare);
            if Label = K.Reply_OK and then Filled in 1 .. Directory_Batch_Pages then
               declare
                  Pages : constant Directory_Pages with Import, Address => To_Address (Arena);
               begin
                  Batch (1 .. Natural (Filled)) := Pages (1 .. Natural (Filled));
               end;
               Batch_Handle := Handle;
               Batch_Count := Natural (Filled);
               Batch_Next := 1;
               Target := Batch (1);
               Unlock;
               return 0;
            end if;
            Unlock;
            return (if Label = K.Reply_OK then Error (EIO) else To_Errno (Label));
         end;
      end if;
      R := Lend_Bounce;
      if R /= 0 then
         Unlock;
         return R;
      end if;
      Label := Call (OP_READ_DIRECTORY_PAGE, 4, Handle, Grant_Slot, PROTOCOL_VERSION,
                     Grant_Generation);
      if Label = K.Reply_OK then
         declare
            Source : constant Directory_Page with Import, Address => To_Address (Bounce);
         begin
            Target := Source;
         end;
      end if;
      Unlock;
      return (if Label = K.Reply_OK then 0 else To_Errno (Label));
   end Directory_Read_Page;

end CuBit.Libc_Files;
