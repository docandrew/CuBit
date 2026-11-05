pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channels;
with CuBit.Channel_Protocol;
with CuBit.Handles.Control;
with CuBit.Messages;

package body Source_Notices is
   package LS renames CuBit.Log_Sources;
   package H renames CuBit.Handles;
   use type CuBit.Channels.Open_Result;
   use type CuBit.Channels.Put_Result;

   --  CuBit's process heap (userspace/allocator, as System.Memory uses it):
   --  zero when it refuses.
   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Block : Unsigned_64)
   with Import, Convention => C, External_Name => "cualloc_free";
   No_Block : constant Unsigned_64 := 0;

   --  How long one attempt to open the channel may wait for logstore.
   Open_Wait_Ms : constant := 1_000;
   --  The first block's notices: an allocation hint, never a limit.
   Initial_Capacity : constant := 16;

   type Notice_Array is array (Positive range <>) of LS.Source_Entry;
   Notice_Bytes : constant Unsigned_64 := Notice_Array'Component_Size / System.Storage_Unit;
   Notice_Alignment : constant Unsigned_64 := LS.Source_Entry'Alignment;

   --  Block holds Capacity notices; Block (First .. Last) are the ones not
   --  yet put, oldest first.
   Block    : Unsigned_64 := No_Block;
   Capacity : Natural := 0;
   First    : Positive := 1;
   Last     : Natural := 0;
   Given_Up : Unsigned_64 := 0;

   type Phase is (Unbound, Bound, Failed);
   State  : Phase := Unbound;
   Target : H.Endpoint := H.No_Endpoint;
   Link   : CuBit.Channels.Channel;
   Next_Try_Ms : Unsigned_64 := 0;

   function Address_Of (Item : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Item)));

   function Lost return Unsigned_64 is (Given_Up);
   function Pending return Boolean is (State /= Failed and then First <= Last);

   --  Room after Last: the notices already put are dropped first; the block
   --  doubles when the pending ones fill it. False when the heap refuses.
   function Make_Room return Boolean is
      Waiting : constant Natural := Last - First + 1;
      Wanted : Natural;
      Larger : Unsigned_64;
   begin
      if Last < Capacity then
         return True;
      end if;
      if Waiting <= Capacity / 2 and then Capacity > 0 then
         declare
            Data : Notice_Array (1 .. Capacity) with Import, Address => Address_Of (Block);
         begin
            Data (1 .. Waiting) := Data (First .. Last);
         end;
      else
         if Capacity > Natural'Last / 2 then
            return False;
         end if;
         Wanted := Natural'Max (Initial_Capacity, Capacity * 2);
         Larger := CuAlloc_Allocate (Unsigned_64 (Wanted) * Notice_Bytes, Notice_Alignment);
         if Larger = No_Block then
            return False;
         end if;
         declare
            Old_Data : constant Notice_Array (1 .. Capacity)
              with Import, Address => Address_Of (Block);
            New_Data : Notice_Array (1 .. Wanted) with Import, Address => Address_Of (Larger);
         begin
            New_Data (1 .. Waiting) := Old_Data (First .. Last);
         end;
         if Block /= No_Block then
            CuAlloc_Free (Block);
         end if;
         Block := Larger;
         Capacity := Wanted;
      end if;
      First := 1;
      Last := Waiting;
      return True;
   end Make_Room;

   procedure Keep (Item : LS.Source_Entry) is
   begin
      if not Make_Room then
         if Given_Up < Unsigned_64'Last then
            Given_Up := Given_Up + 1;
         end if;
         return;
      end if;
      declare
         Data : Notice_Array (1 .. Capacity) with Import, Address => Address_Of (Block);
      begin
         Last := Last + 1;
         Data (Last) := Item;
      end;
   end Keep;

   procedure Bind is
      Service : H.Endpoint;
      Opened : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Now : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
   begin
      if Now < Next_Try_Ms then
         return;
      end if;
      if not H.Valid (Target) then
         Service := Collector;
         if not H.Valid (Service) then
            return;
         end if;
         Target := CuBit.Handles.Control.Derive
           (H.Badged (Service, H.Read_Write, LS.Registrar_Tag (Issuance)));
         if not H.Valid (Target) then
            State := Failed;
            CuBit.Messages.debugPrint ("source notices: no registrar binding to logstore" & ASCII.LF);
            return;
         end if;
      end if;
      CuBit.Channels.Open
        (Target, LS.CONTRACT, CuBit.Channels.Producing, Link, Opened, Ignore_Refusal,
         Deadline => CuBit.Messages.Deadline_After (Open_Wait_Ms));
      case Opened is
         when CuBit.Channels.Opened =>
            State := Bound;
         when CuBit.Channels.Refused =>
            State := Failed;
            CuBit.Messages.debugPrint ("source notices: logstore refused the registrar channel" & ASCII.LF);
         when CuBit.Channels.Failed =>
            --  logstore did not answer in time: try again later.
            Next_Try_Ms := Now + Retry_Interval_Ms;
      end case;
   end Bind;

   procedure Pump is
      Bytes : LS.Entry_Buffer;
      Result : CuBit.Channels.Put_Result;
   begin
      if First > Last then
         return;
      end if;
      if State = Unbound then
         Bind;
      end if;
      if State /= Bound then
         return;
      end if;
      declare
         Data : constant Notice_Array (1 .. Capacity) with Import, Address => Address_Of (Block);
      begin
         while First <= Last loop
            LS.Encode (Data (First), Bytes);
            CuBit.Channels.Put (Link, Bytes'Address, LS.Entry_Bytes, Result);
            exit when Result = CuBit.Channels.Full;
            if Result /= CuBit.Channels.Put then
               --  The channel ended (logstore exited): nobody to tell.
               State := Failed;
               CuBit.Messages.debugPrint ("source notices: registrar channel to logstore ended" & ASCII.LF);
               exit;
            end if;
            First := First + 1;
         end loop;
      end;
      if First > Last then
         First := 1;
         Last := 0;
      end if;
   end Pump;

   procedure Started (Process : CuBit.Process_IDs.Process_ID; Identity, Name : String) is
      Word : constant Unsigned_64 := CuBit.Process_IDs.To_Word (Process);
   begin
      if State = Failed or else Word = LS.No_Source or else Name'Last = Positive'Last then
         return;
      end if;
      declare
         Who : constant LS.Source_Identity := LS.Make (Word, Identity, Name);
      begin
         if LS.Named (Who) then
            Keep ((Kind => LS.Started, Who => Who, Seen_Ms => 0));
            Pump;
         end if;
      end;
   end Started;

   procedure Exited (Process : CuBit.Process_IDs.Process_ID) is
      Word : constant Unsigned_64 := CuBit.Process_IDs.To_Word (Process);
   begin
      if State = Failed or else Word = LS.No_Source then
         return;
      end if;
      Keep ((Kind => LS.Exited, Who => LS.Make (Word, "", ""), Seen_Ms => 0));
      Pump;
   end Exited;
end Source_Notices;
