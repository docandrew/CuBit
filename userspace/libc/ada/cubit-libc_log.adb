------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Channel_Rings;
with CuBit.Console_Lines;
with CuBit.Datagram_Rings;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Imports;
with CuBit.Libc_Reports;
with CuBit.Libc_Startup;
with CuBit.Log_Publish_Rings;
with CuBit.Published_Clock;
with CuBit.Startup_Tables;
with Wait_ABI;

package body CuBit.Libc_Log is
   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   package Rings renames CuBit.Channel_Rings;
   package DR renames CuBit.Datagram_Rings;
   package K renames CuBit.Kernel_ABI;
   package Lines renames CuBit.Console_Lines;
   package Publish renames CuBit.Log_Publish_Rings;
   use type Logs.Severity;
   use type Interfaces.C.size_t;
   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type System.Address;

   Contract : CC.Contract renames Publish.CONTRACT;
   Page_Bytes : constant := 4_096;
   --  The control page and the ring (CP.Region_Pages of a queue).
   Region_Pages : constant := 1 + Publish.RING_PAGES;
   Ring_Bytes : constant := Publish.RING_PAGES * Page_Bytes;
   Region_Bytes : constant := Region_Pages * Page_Bytes;
   --  The consumer's index page: its consumed index, its waiting word and
   --  the words it publishes to producers (CuBit.Channels' layout).
   Consumer_Words_Offset : constant := 128;
   Word_Bytes : constant := 8;
   Not_Waiting : constant Unsigned_64 := 0;
   Stderr_Field : constant String := "outlet.stderr";
   Syscall_Field : constant String := "syscall.number";
   Logstore_Entry : constant String := "logstore";

   type Channel_State is (Unopened, Open, Refused);

   Guard : aliased CuBit.Libc_Imports.Lock_Word := 0;
   --  The thread holding Guard is inside (its own failure would wait on it).
   Inside : Boolean := False with Thread_Local_Storage;
   State : Channel_State := Unopened;
   Endpoint : Unsigned_64 := 0;
   Region : Unsigned_64 := 0;        --  this side's pages: control page, ring
   Peer : Unsigned_64 := 0;          --  logstore's index page, mapped read-only
   Peer_Number : Unsigned_64 := 0;   --  the channel's number at logstore
   Writer : Rings.Producer := (Size => Ring_Bytes, Produced => 0, Fill => 0);
   --  Zero-filled (.bss): no line yet (a framer reads only what it holds).
   Console_Framer, Stderr_Framer : Lines.Framer;
   pragma Suppress_Initialization (Console_Framer);
   pragma Suppress_Initialization (Stderr_Framer);
   Losses : Unsigned_64 := 0;

   function Word (Base : Unsigned_64; Offset : Natural) return Unsigned_64;
   function Word (Base : Unsigned_64; Offset : Natural) return Unsigned_64 is
      Value : constant Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
   begin
      return Value;
   end Word;

   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Unsigned_64);
   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Unsigned_64) is
      Target : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
   begin
      Target := Value;
   end Set_Word;

   --  x86-64 keeps stores, and loads, in program order; the compiler must
   --  too. A store, then a load of the peer's word, needs mfence.
   procedure Fence;
   procedure Fence is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Fence;
   procedure Full_Fence;
   procedure Full_Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Full_Fence;

   procedure Lose;
   procedure Lose is
   begin
      if Losses < Unsigned_64'Last then
         Losses := Losses + 1;
      end if;
   end Lose;

   function Logstore_Handle return Unsigned_64;
   function Logstore_Handle return Unsigned_64 is
     (CuBit.Libc_Startup.Granted (Logstore_Entry, CuBit.Startup_Tables.Endpoint));

   procedure Revoke (Wire : Unsigned_64);
   procedure Revoke (Wire : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      if Wire /= 0 then
         Ignore := CuBit.Kernel_Calls.Call (K.Revoke_Shared_Memory_Grant, Wire and 16#FFFF_FFFF#);
      end if;
   end Revoke;

   --  Under Guard: open the channel once (CuBit.Channel_Protocol, the
   --  opener producing), as CuBit.Channels.Open does for an Ada program.
   procedure Open_Channel;
   procedure Open_Channel is
      Words : constant CC.Words := CC.Encode (Contract);
      Area : System.Address;
      Slot, Generation, Own_Grant, Tag, Mapped : Unsigned_64;
      M : aliased K.Message;
      Ignore : Interfaces.C.int;

      procedure Give_Up;
      procedure Give_Up is
      begin
         Revoke (Own_Grant);
         Ignore := CuBit.Libc_Imports.munmap (Area, Interfaces.C.size_t (Region_Bytes));
         Region := 0;
      end Give_Up;
   begin
      State := Refused;
      Endpoint := Logstore_Handle;
      if Endpoint = 0 then
         return;
      end if;
      Area := CuBit.Libc_Imports.mmap
        (System.Null_Address, Interfaces.C.size_t (Region_Bytes),
         Interfaces.C.int (PROT_READ + PROT_WRITE),
         Interfaces.C.int (MAP_PRIVATE + MAP_ANONYMOUS), -1, 0);
      if Area = CuBit.Libc_Imports.MAP_FAILED then
         return;
      end if;
      Region := Unsigned_64 (To_Integer (Area));
      --  Anonymous pages are zero: only the element's identity is set.
      Set_Word (Region, CP.ELEMENT_OFFSET, Unsigned_64 (Contract.Element.Identity) and 16#FFFF#);
      Own_Grant := 0;
      Slot := CuBit.Kernel_Calls.Call
        (K.Create_Shared_Memory_Grant_Via_Capability, Endpoint, Region,
         Unsigned_64 (Region_Pages), K.Grant_Notify);
      if Slot = K.Failed then
         Give_Up;
         return;
      end if;
      Generation := CuBit.Kernel_Calls.Call (K.Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = K.Failed or else Generation = 0 then
         Revoke (Slot);
         Give_Up;
         return;
      end if;
      Own_Grant := Shift_Left (Generation, K.Generation_Shift) or Slot;
      M := (Label => CP.OP_OPEN_PRODUCING, Length => CP.Open_Words, Reserved => 0,
            Words => [Words (0), Words (1), Words (2), Own_Grant], others => <>);
      Tag := CuBit.Kernel_Calls.Call
        (K.Call_Via_Endpoint_Capability, Endpoint,
         Unsigned_64 (To_Integer (M'Address)), K.Forever);
      --  logstore accepts the ring as offered (a producer's pages are its
      --  own) and grants back its index page.
      if Unsigned_32 (Tag and 16#FFFF_FFFF#) /= K.Reply_OK
        or else (M.Words (2) /= 0 and then M.Words (2) /= Unsigned_64 (Contract.Pages))
      then
         Give_Up;
         return;
      end if;
      Mapped := CuBit.Kernel_Calls.Call
        (K.Acquire_Shared_Memory_Grant_Via_Capability, Endpoint,
         M.Words (1) and 16#FFFF_FFFF#, Shift_Right (M.Words (1), K.Generation_Shift),
         0, CP.INDEX_BYTES, 0);
      if Mapped = K.Failed or else Mapped = 0 then
         Give_Up;
         return;
      end if;
      Peer := Mapped;
      Peer_Number := M.Words (0);
      State := Open;
   end Open_Channel;

   function Minimum return Logs.Severity;
   function Minimum return Logs.Severity is
      Value : constant Unsigned_64 :=
        (if State = Open then Word (Peer, Consumer_Words_Offset + Publish.Minimum_Word * Word_Bytes)
         else 0);
   begin
      return (if Value <= Logs.Severity'Pos (Logs.Severity'Last) then Logs.Severity'Val (Value)
              else Logs.Trace);
   end Minimum;

   --  Under Guard: whether a record at Level is wanted (opening first).
   function Wanted (Level : Logs.Severity) return Boolean;
   function Wanted (Level : Logs.Severity) return Boolean is
   begin
      if State = Unopened then
         Open_Channel;
      end if;
      return State = Open and then Level >= Minimum;
   end Wanted;

   procedure Kick;
   procedure Kick is
      Ignore : constant Unsigned_64 := CuBit.Kernel_Calls.Submit
        (Endpoint, CP.OP_KICK, 1, Peer_Number, 0, 0, 0, CuBit.Kernel_Calls.No_Completion_Token);
   begin
      null;
   end Kick;

   --  Under Guard, the channel open: Value into the ring (shed, counted,
   --  when full: logstore reports the channel's sheds as a gap).
   procedure Put (Value : Logs.Log_Record);
   procedure Put (Value : Logs.Log_Record) is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Ring : Rings.Bytes (0 .. Ring_Bytes - 1)
        with Import, Address => To_Address (Integer_Address (Region + CP.CONTROL_BYTES));
      Accepted : Boolean;
      Result : DR.Put_Result;
   begin
      Logs.Encode (Value, Bytes, Used);
      declare
         Source : Rings.Bytes (0 .. Used - 1) with Import, Address => Bytes'Address;
      begin
         Rings.Accept_Consumed
           (Writer, Rings.Index (Word (Peer, CP.CONSUMED_OFFSET) and 16#FFFF_FFFF#), Accepted);
         DR.Put (Writer, Ring, Source, Result);
      end;
      case Result is
         when DR.Put =>
            Fence;
            Set_Word (Region, CP.PRODUCED_OFFSET, Unsigned_64 (Writer.Produced));
            Full_Fence;
            if Word (Peer, CP.CONSUMER_WAITING_OFFSET) /= Not_Waiting then
               Kick;
            end if;
         when DR.No_Room =>
            Set_Word (Region, CP.SHED_OFFSET, Word (Region, CP.SHED_OFFSET) + 1);
            Lose;
         when DR.Too_Large =>
            Lose;
      end case;
   end Put;

   --  Under Guard: the framed line as one record at Level, marked as
   --  console text (and Extra = 1 when Extra is a field name).
   procedure Publish_Line (Framer : Lines.Framer; Level : Logs.Severity; Extra : String);
   procedure Publish_Line (Framer : Lines.Framer; Level : Logs.Severity; Extra : String) is
      Text : Lines.Line_Buffer;
      Used : Lines.Line_Length;
      Made : Logs.Decoded;
   begin
      if Lines.Length (Framer) = 0 then
         return;
      elsif not Wanted (Level) then
         if State /= Open then
            Lose;
         end if;
         return;
      end if;
      Lines.Line (Framer, Text, Used);
      Lines.Plain (Text (1 .. Used));
      Made := Logs.Make (Text (1 .. Used), Level);
      if Made.Success then
         Made := Logs.With_Field (Made.Value, Lines.Console_Field, Logs.Truth, 1);
      end if;
      if Made.Success and then Extra'Length > 0 then
         Made := Logs.With_Field (Made.Value, Extra, Logs.Truth, 1);
      end if;
      if Made.Success then
         Put (Made.Value);
      else
         Lose;
      end if;
   end Publish_Line;

   procedure Feed
     (Framer : in out Lines.Framer; Text : System.Address; Length : Interfaces.C.size_t;
      Level : Logs.Severity; Extra : String);
   procedure Feed
     (Framer : in out Lines.Framer; Text : System.Address; Length : Interfaces.C.size_t;
      Level : Logs.Severity; Extra : String)
   is
      Complete : Boolean;
   begin
      if Length = 0 or else Text = System.Null_Address then
         return;
      elsif Inside then
         Lose;
         return;
      end if;
      Inside := True;
      CuBit.Libc_Imports.Lock (Guard'Access);
      declare
         Data : constant String (1 .. Natural (Length)) with Import, Address => Text;
      begin
         for C of Data loop
            Lines.Feed (Framer, C, Complete);
            if Complete then
               Publish_Line (Framer, Level, Extra);
               Lines.Clear (Framer);
            end if;
         end loop;
      end;
      CuBit.Libc_Imports.Unlock (Guard'Access);
      Inside := False;
   end Feed;

   procedure Write_Console (Text : System.Address; Length : Interfaces.C.size_t) is
   begin
      Feed (Console_Framer, Text, Length, Logs.Debug, "");
   end Write_Console;

   procedure Write_Stderr (Text : System.Address; Length : Interfaces.C.size_t) is
   begin
      Feed (Stderr_Framer, Text, Length, Logs.Information, Stderr_Field);
   end Write_Stderr;

   function Takes_Stderr return Boolean is (Logstore_Handle /= 0);

   procedure Report
     (Text : String; Level : Logs.Severity; Field : String; Value : Unsigned_64;
      Kind : Logs.Field_Kind := Logs.Unsigned_Integer)
   is
      Made : Logs.Decoded;
   begin
      if Inside then
         Lose;
         return;
      end if;
      Inside := True;
      CuBit.Libc_Imports.Lock (Guard'Access);
      if Wanted (Level) then
         Made := Logs.Make (Text, Level);
         --  Console text too: what the libc once wrote to the console (test
         --  builds' logstore mirrors it onto serial).
         if Made.Success then
            Made := Logs.With_Field (Made.Value, Lines.Console_Field, Logs.Truth, 1);
         end if;
         if Made.Success and then Field'Length > 0 then
            Made := Logs.With_Field (Made.Value, Field, Kind, Value);
         end if;
         if Made.Success then
            Put (Made.Value);
         else
            Lose;
         end if;
      elsif State /= Open then
         Lose;
      end if;
      CuBit.Libc_Imports.Unlock (Guard'Access);
      Inside := False;
   end Report;

   procedure Unimplemented_System_Call (Number : Unsigned_64) is
      Text : CuBit.Libc_Reports.Line;
      Length : CuBit.Libc_Reports.Line_Length;
   begin
      --  "cubit-libc: unimplemented system call N" (its line feed dropped).
      CuBit.Libc_Reports.Format ("unimplemented", "system call", Integer_64 (Number and 16#7FFF_FFFF_FFFF_FFFF#),
                                 Text, Length);
      Report (Text (1 .. Length - 1), Logs.Warning, Syscall_Field, Number);
   end Unimplemented_System_Call;

   procedure Flush is
      Waited : Natural := 0;
      Retry_Microseconds : constant := 1_000;
      Ignore : Unsigned_64;
   begin
      if State /= Open or else Inside then
         return;
      end if;
      while Waited < Flush_Wait_Ms loop
         exit when Unsigned_64 (Rings.Index (Word (Peer, CP.CONSUMED_OFFSET) and 16#FFFF_FFFF#))
                     = Unsigned_64 (Writer.Produced);
         if Waited = 0 then
            Kick;
         end if;
         Ignore := CuBit.Kernel_Calls.Call
           (K.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
            CuBit.Published_Clock.Microseconds_Now + Retry_Microseconds);
         Waited := Waited + 1;
      end loop;
   end Flush;

   function Lost return Unsigned_64 is (Losses);
end CuBit.Libc_Log;
