with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Diagnostics;
with Intel_GPU_DMA_Cache;
package body Intel_GPU_Buffer_Memory is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Replies renames Intel_GPU_Buffer_Reply;
   use type Replies.Outcome;
   function Last_Stage (Object : Pool) return Allocation_Stage is (Object.Stage);
   function Pending (Object : Pool) return Boolean is (Object.Active);
   function Retirement_Confirmed
     (Object : Pool; Index : Layout.Slot; Generation : Unsigned_32) return Boolean is
     (not Object.Active and then not Object.Broken and then Owner_Ready and then
      Object.Stage = Retired and then Object.Index = Index and then
      Object.Generation = Generation);
   function Result (Object : Pool) return Replies.Backing is
     (if Object.Active or Object.Broken then (Ready => False) else Object.Current);
   procedure Cancel (Object : in out Pool) is
   begin
      Object.Active := False; Object.Broken := True;
      Object.Current := (Ready => False);
      Intel_GPU_Extent_Replies.Cancel (Object.Assembly);
   end Cancel;
   function Submit_Message (Object : in out Pool; Request : Message) return Boolean is
   begin
      if Object.Serial = Unsigned_32'Last then return False; end if;
      Object.Serial := Object.Serial + 1;
      Object.Completion_Token := 16#4947_5000_0000_0000# + Unsigned_64 (Object.Serial);
      return capSubmit (15, Request, Object.Completion_Token);
   end Submit_Message;
   function Submit (Object : in out Pool) return Boolean is
      Request : Message := NULL_MESSAGE;
   begin
      Request.tag := (Layout.Request_Label, 3, 0, 0);
      Request.words := [Unsigned_64 (Object.Index), Unsigned_64 (Object.Pages),
                        Unsigned_64 (Object.Generation), 0];
      return Submit_Message (Object, Request);
   end Submit;
   function Submit_Extent (Object : in out Pool) return Boolean is
      Request : Message := NULL_MESSAGE;
   begin
      Request.tag := (Layout.Extent_Request_Label, 2, 0, 0);
      Request.words := [Unsigned_64 (Object.Extent_Index), Object.Arena_ID, 0, 0];
      return Submit_Message (Object, Request);
   end Submit_Extent;
   procedure Start
     (Object : in out Pool; Index : Layout.Slot; Pages : Layout.Page_Count;
      Started : out Boolean) is
   begin
      Started := False;
      if Object.Active then return; end if;
      Object.Current := (Ready => False);
      if Object.Broken or Object.Attempted (Index) then return; end if;
      Object.Stage := Owner_Check;
      if not Owner_Ready then
         Object.Broken := (for some Tried of Object.Attempted => Tried);
         return;
      end if;
      Object.Attempted (Index) := True;
      Object.Broken := True;
      Object.Index := Index; Object.Pages := Pages;
      Object.Generation := Object.Slot_Generations (Index);
      Object.Retiring := False;
      Object.Started_At := syscall (SYSCALL_GETTIME);
      Object.Previous := Object.Started_At;
      if Object.Started_At > Unsigned_64'Last - 30_000 then return; end if;
      Object.Stage := Submit_Request;
      if not Submit (Object) then return; end if;
      Object.Stage := Awaiting_Reply;
      Object.Active := True; Started := True;
   end Start;
   procedure Retire
     (Object : in out Pool; Index : Layout.Slot; Generation : Unsigned_32;
      All_References_Retired : Boolean; Started : out Boolean) is
      Request : Message := NULL_MESSAGE;
   begin
      Started := False;
      if Object.Active or else Object.Broken or else not All_References_Retired or else
        not Object.Attempted (Index) or else not Object.Items (Index).Ready or else
        Generation = 0 or else Generation = Unsigned_32'Last or else
        Generation /= Object.Slot_Generations (Index)
      then return; end if;
      if not Owner_Ready then Cancel (Object); return; end if;
      Object.Current := (Ready => False);
      Object.Broken := True;
      Object.Index := Index; Object.Generation := Generation;
      Object.Retiring := True;
      Object.Started_At := syscall (SYSCALL_GETTIME);
      Object.Previous := Object.Started_At;
      if Object.Started_At > Unsigned_64'Last - 30_000 then return; end if;
      Request.tag := (Layout.Retire_Request_Label, 4, 0, 0);
      Request.words := [Unsigned_64 (Index), Unsigned_64 (Generation), Object.Arena_ID, 1];
      if not Submit_Message (Object, Request) then return; end if;
      Object.Stage := Awaiting_Retirement;
      Object.Active := True; Started := True;
   end Retire;
   procedure Tick (Object : in out Pool) is
      Now : Unsigned_64;
   begin
      if not Object.Active then return; end if;
      Now := syscall (SYSCALL_GETTIME);
      if Now = Unsigned_64'Last or else Now < Object.Previous or else
        Now - Object.Started_At >= 30_000 or else not Owner_Ready
      then Cancel (Object); return; end if;
      Object.Previous := Now;
   end Tick;
   procedure Complete
     (Object : in out Pool; Receipt : CompletionEntry; Consumed : out Boolean) is
      Candidate : Replies.Backing;
      Accepted : Boolean;
   begin
      Consumed := Object.Active and then
        Receipt.token = Object.Completion_Token;
      if not Consumed then return; end if;
      Tick (Object);
      if not Object.Active then return; end if;
      Object.Stage := Validate_Reply;
      if Receipt.status /= COMPLETION_OK then Cancel (Object); return; end if;
      if Object.Retiring then
         if Receipt.msg.tag /= (Layout.Retire_Request_Label, 4, 0, 0) or else
           Receipt.msg.words /=
             [0, Layout.Allocation_Key (Object.Index, Object.Generation), Object.Arena_ID, 0]
         then Cancel (Object); return; end if;
         Object.Items (Object.Index) := (Ready => False);
         Object.Attempted (Object.Index) := False;
         Object.Slot_Generations (Object.Index) := Object.Generation + 1;
         Object.Active := False; Object.Broken := False; Object.Retiring := False;
         Object.Stage := Retired;
         return;
      end if;
      if Object.Fetching then
         if Receipt.msg.tag /= (16#F003#, 4, 0, 0) then Cancel (Object); return; end if;
         Intel_GPU_Extent_Replies.Accept_Reply
           (Object.Assembly, Intel_GPU_Extent_Replies.Words (Receipt.msg.words), Accepted);
         if not Accepted then Cancel (Object); return; end if;
         if Object.Extent_Index < Intel_GPU_Physical_Extents.Block_Index'Last then
            Object.Extent_Index := Object.Extent_Index + 1;
            if not Submit_Extent (Object) then Cancel (Object); end if;
            return;
         end if;
         Object.Mapping := Intel_GPU_Extent_Replies.Result (Object.Assembly);
         Object.Fetching := False;
      else
         if Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
           Receipt.msg.words = [0, 0, 0, 0]
         then
               Object.Stage := Submit_Request;
               if not Submit (Object) then Cancel (Object); end if;
               if Object.Active then Object.Stage := Awaiting_Reply; end if;
               return;
         elsif Receipt.msg.tag = (16#F001#, 0, 0, 0) and then
           Receipt.msg.words = [0, 0, 0, 0]
         then
               Object.Stage := Denied;
               Object.Active := False; Object.Broken := False;
               return;
         end if;
         if Receipt.msg.tag /= (16#F004#, 4, 0, 0) or else
           Receipt.msg.words (3) /= Layout.Allocation_Key
             (Object.Index, Object.Generation) or else
           Receipt.msg.words (2) = 0 or else
           Receipt.msg.words (1) /= Unsigned_64 (Object.Pages) * 4096 or else
           Receipt.msg.words (0) < Layout.CPU_Base or else
           Receipt.msg.words (0) mod 4096 /= 0 or else
           Receipt.msg.words (0) - Layout.CPU_Base > Layout.Capacity - Receipt.msg.words (1) or else
           (Object.Arena_ID /= 0 and then Object.Arena_ID /= Receipt.msg.words (2))
         then Cancel (Object); return; end if;
         Object.Requested_CPU := Receipt.msg.words (0);
         Object.Requested_Bytes := Receipt.msg.words (1);
         Object.Arena_ID := Receipt.msg.words (2);
         if not Intel_GPU_Physical_Extents.Ready (Object.Mapping) then
            Intel_GPU_Extent_Replies.Start
              (Object.Assembly, Layout.CPU_Base, Object.Arena_ID, Accepted);
            if not Accepted then Cancel (Object); return; end if;
            Object.Fetching := True;
            Object.Extent_Index := 0;
            if not Submit_Extent (Object) then Cancel (Object); end if;
            return;
         end if;
      end if;
      Candidate := Replies.From_View (Replies.From_Extents
        (Object.Mapping, Object.Arena_ID,
         Object.Requested_CPU - Layout.CPU_Base, Object.Requested_Bytes));
      if not Replies.Valid (Candidate) then Cancel (Object); return; end if;
      -- All accepted buffers share one immutable, nonaliasing backing map.
      -- Disjoint CPU slices then imply disjoint backing, without requiring
      -- neighboring CPU pages to be physically adjacent.
      Object.Stage := Validate_Backing;
      for Other of Object.Items loop
         if Other.Ready and then
           (not Replies.Same_Arena (Other, Candidate) or else
            (Candidate.CPU_Address < Other.CPU_Address + Other.Bytes and then
             Other.CPU_Address < Candidate.CPU_Address + Candidate.Bytes))
         then Cancel (Object); return; end if;
      end loop;
      declare
         type Memory_Words is array (Natural range <>) of Unsigned_64;
         Memory : Memory_Words (0 .. Natural (Candidate.Bytes / 8) - 1)
           with Import, Volatile,
             Address => To_Address (Integer_Address (Candidate.CPU_Address));
      begin
         Object.Stage := Zero_Backing;
         for Page in 0 .. Natural (Candidate.Bytes / 4096) - 1 loop
            if not Owner_Ready then Cancel (Object); return; end if;
            for Word in 0 .. 511 loop Memory (Page * 512 + Word) := 0; end loop;
         end loop;
         Object.Stage := Flush_Backing;
         if not Owner_Ready or else not Intel_GPU_DMA_Cache.Flush_Range
           (Candidate.CPU_Address, Candidate.Bytes) then Cancel (Object); return; end if;
         Object.Stage := Readback_Backing;
         for Page in 0 .. Natural (Candidate.Bytes / 4096) - 1 loop
            if not Owner_Ready then Cancel (Object); return; end if;
            for Word in 0 .. 511 loop
               if Memory (Page * 512 + Word) /= 0 then Cancel (Object); return; end if;
            end loop;
         end loop;
      end;
      if not Owner_Ready then Cancel (Object); return; end if;
      Object.Items (Object.Index) := Candidate;
      Object.Stage := Granted;
      Object.Broken := False;
      Object.Current := Candidate;
      Object.Active := False;
   end Complete;
   function Acquire
     (Object : in out Pool; Index : Layout.Slot; Pages : Layout.Page_Count)
      return Replies.Backing is
      Receipt : aliased CompletionEntry;
      Started, Consumed : Boolean;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      Start (Object, Index, Pages, Started);
      if not Started then return (Ready => False); end if;
      -- Bootstrap-only wrapper. Public dispatch uses the event-loop API.
      for Poll in 1 .. 30_000 loop
         Tick (Object);
         exit when not Pending (Object);
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 then
            Complete (Object, Receipt, Consumed);
            if not Consumed then Cancel (Object); end if;
         end if;
         exit when not Pending (Object);
         Activity := Wait_For_Activity_Until (Object.Previous + 1);
      end loop;
      if Pending (Object) then Cancel (Object); end if;
      return Result (Object);
   end Acquire;
end Intel_GPU_Buffer_Memory;
