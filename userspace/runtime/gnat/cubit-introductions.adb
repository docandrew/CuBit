pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Thread_ABI;
with CuBit.Messages;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Owned_Reservations;
with CuBit.Thread_Areas;
with CuBit.System_Facts;
with CuBit.Startup_Tables;
with CuBit.Startup_State;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Late_Introduction;

package body CuBit.Introductions is
   package ST renames CuBit.Startup_Tables;
   package LI renames CuBit.Late_Introduction;
   package HIPC renames CuBit.Handles.IPC;
   package ABI renames CuBit.Kernel_ABI;
   use type ST.Capability_Kind, ST.Need, ST.Entry_State;

   --  The Absent optional endpoint entry named Label: its index, or 0.
   function Absent_Index (Label : String) return ST.Entry_Count;
   function Absent_Index (Label : String) return ST.Entry_Count is
      Table : constant ST.Table := CuBit.Startup_State.Current;
      Index : ST.Entry_Count;
   begin
      if Label'Length not in 1 .. ST.Maximum_Name_Bytes then
         return 0;
      end if;
      Index := ST.Find (Table, ST.To_Name (Label));
      if Index /= 0
        and then Table.Entries (Index).Kind = ST.Endpoint
        and then Table.Entries (Index).Requirement = ST.Optional
        and then Table.Entries (Index).State = ST.Absent
      then
         return Index;
      end if;
      return 0;
   end Absent_Index;

   function Introducer return CuBit.Handles.Startup.Optional_Endpoint is
     (CuBit.Handles.Startup.Optional_Endpoint_Named (LI.Binding));

   function Awaitable (Label : String) return Boolean is
     (Absent_Index (Label) /= 0 and then Introducer.Present);

   function Await (Label : String) return Result is
      Index : constant ST.Entry_Count := Absent_Index (Label);
      Via : constant CuBit.Handles.Startup.Optional_Endpoint := Introducer;
      Request : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Received : HIPC.Carried;
      Tag : CuBit.Messages.MessageTag;
   begin
      if Index = 0 then
         return (Kind => Not_Absent);
      elsif not Via.Present then
         return (Kind => No_Introducer);
      end if;
      Request.tag := (label => LI.Await_Request, length => LI.Await_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (LI.Index_Word) := Unsigned_64 (Index);
      --  procmgr answers when the provider starts, which may be never: the
      --  wait is the point, and only this thread waits.
      Tag := HIPC.Call_Accepting (Via.Value, Request, CuBit.Messages.Wait_Forever, Received);
      if Tag.label = LI.Introduced then
         declare
            Taken : constant HIPC.Accepted_Endpoint := HIPC.Accept_Endpoint (Received);
         begin
            if Taken.OK then
               return (Kind => Introduced, Value => Taken.Value);
            end if;
         end;
         return (Kind => Failed);
      end if;
      declare
         Ignore : constant HIPC.Close_Result := HIPC.Close (Received);
      begin
         null;
      end;
      return (if Tag.label = LI.Not_Declared then (Kind => Not_Declared)
              elsif Tag.label = LI.Withdrawn then (Kind => Withdrawn)
              else (Kind => Failed));
   end Await;

   ---------------------------------------------------------------------------
   --  Background waits: one thread each, on a primary stack of its own
   --  (owned memory, guard page at the bottom) and a thread area. The
   --  stack is kept after the thread ends (a thread cannot give back the
   --  stack it runs on); a program waits for a handful of services at most.
   ---------------------------------------------------------------------------
   Maximum_Background : constant := 8;
   Stack_Bytes : constant := 128 * 1024;
   --  A SysV function expects RSP + 8 to be 16-byte aligned at entry.
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   type Waiter is record
      Label  : String (1 .. ST.Maximum_Name_Bytes) := [others => ' '];
      Length : Natural range 0 .. ST.Maximum_Name_Bytes := 0;
      Arrived : Arrival;
   end record;
   Waiters : array (1 .. Maximum_Background) of Waiter;
   Used : Natural range 0 .. Maximum_Background := 0;
   --  Cleared by the kernel when each thread ends.
   Exit_Words : array (1 .. Maximum_Background) of aliased Word := [others => 1];

   procedure Worker (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot)
   with Convention => C, No_Return;
   procedure Worker (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot) is
   begin
      CuBit.System_Facts.Enter_Thread (Slot);
      if Argument in 1 .. Maximum_Background then
         declare
            W : Waiter renames Waiters (Positive (Argument));
            Answer : constant Result := Await (W.Label (1 .. W.Length));
         begin
            if Answer.Kind = Introduced then
               W.Arrived (Answer.Value);
            end if;
         end;
      end if;
      CuBit.Thread_Areas.Exit_Thread;
   end Worker;

   procedure In_Background (Label : String; Arrived : not null Arrival; Started : out Boolean) is
      Stack : Unsigned_64;
      Area : Unsigned_64;
      Created : Unsigned_64;
      Ignore : Boolean;
   begin
      Started := False;
      if Used = Maximum_Background or else not Awaitable (Label) then
         return;
      end if;
      Stack := CuBit.Owned_Reservations.Reserve (Stack_Bytes);
      if Stack = 0 then
         return;
      end if;
      if not CuBit.Owned_Reservations.Commit_Steps
           (Stack + ABI.Page_Bytes, Stack_Bytes - ABI.Page_Bytes)
      then
         Ignore := CuBit.Owned_Reservations.Release (Stack, Stack_Bytes);
         return;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         Ignore := CuBit.Owned_Reservations.Release (Stack, Stack_Bytes);
         return;
      end if;
      Used := Used + 1;
      Waiters (Used).Label (1 .. Label'Length) := Label;
      Waiters (Used).Length := Label'Length;
      Waiters (Used).Arrived := Arrived;
      Created := CuBit.Kernel_Calls.Call
        (ABI.Thread_Create, Unsigned_64 (To_Integer (Worker'Address)),
         ((Stack + Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
         Unsigned_64 (Used), Area,
         Unsigned_64 (To_Integer (Exit_Words (Used)'Address)));
      if Created = Thread_ABI.Refused or else not Thread_ABI.Is_Created (Created) then
         CuBit.Thread_Areas.Discard (Area);
         Ignore := CuBit.Owned_Reservations.Release (Stack, Stack_Bytes);
         Used := Used - 1;
         return;
      end if;
      Started := True;
   end In_Background;

end CuBit.Introductions;
