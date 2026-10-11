-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Boot_Timeline body: see the specification.
-------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Spinlocks;
with x86;

package body Boot_Timeline with
   SPARK_Mode => Off
is
   package R renames Boot_Timeline_Records;

   Lock  : Spinlocks.Spinlock;
   State : R.State;
   Open  : Boolean := False with Atomic;

   function Recording return Boolean is (Open);

   function Now return Unsigned_64 is (x86.rdtsc);

   --  Call with Lock held: mirror the record's recording state.
   procedure Publish_State is
   begin
      if not R.Recording (State) then
         Open := False;
      end if;
   end Publish_State;

   function Slot_Of (Slot : Natural) return R.Process_Slot is
     (R.Process_Slot (Slot mod R.Slot_Capacity));

   procedure Initialize (Entry_Ticks : Unsigned_64) is
   begin
      Spinlocks.Initialize (Lock);
      R.Initialize (State);
      R.Mark_Stage (State, Layout.Kernel_Entry, Entry_Ticks);
      Open := True;
   end Initialize;

   procedure Mark_Stage (Stage : Layout.Kernel_Stage) is
   begin
      if not Open then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      R.Mark_Stage (State, Stage, Now);
      Publish_State;
      Spinlocks.exitCriticalSection (Lock);
   end Mark_Stage;

   procedure Set_Frequency (Frequency : Unsigned_64) is
   begin
      if not Open then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      R.Set_Frequency (State, Frequency, Now);
      Spinlocks.exitCriticalSection (Lock);
   end Set_Frequency;

   procedure Note_Spawn (Slot         : Natural;
                         Identity     : Unsigned_64;
                         Spawner      : Unsigned_64;
                         Spawner_Slot : Natural;
                         Name         : String;
                         Started      : Unsigned_64)
   is
      Text : Layout.Name_Text := [others => ' '];
   begin
      if not Open then return; end if;
      for I in 0 .. Natural'Min (Name'Length, Layout.Name_Bytes) - 1 loop
         exit when Name (Name'First + I) = ASCII.NUL;
         Text (Text'First + I) := Name (Name'First + I);
      end loop;
      Spinlocks.enterCriticalSection (Lock);
      R.Note_Spawn (State, Slot_Of (Slot), Identity, Spawner, Slot_Of (Spawner_Slot),
                    Text, Started, Now);
      Publish_State;
      Spinlocks.exitCriticalSection (Lock);
   end Note_Spawn;

   procedure Note_Event (Slot : Natural; Event : Process_Event) is
   begin
      if not Open then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      R.Note_Event (State, Slot_Of (Slot), Event, Now);
      Publish_State;
      Spinlocks.exitCriticalSection (Lock);
   end Note_Event;

   procedure Note_Wait (Slot    : Natural;
                        Waiter  : Unsigned_64;
                        Kind    : Layout.Wait_Kind;
                        Label   : Unsigned_32;
                        Target  : Unsigned_64;
                        Started : Unsigned_64)
   is
      use type Layout.Wait_Kind;
   begin
      if not Open or else Kind = Layout.No_Wait then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      R.Note_Wait (State, Slot_Of (Slot), Waiter, Kind, Label, Target, Started, Now);
      Publish_State;
      Spinlocks.exitCriticalSection (Lock);
   end Note_Wait;

   procedure Mark_Milestone (Item : Layout.Milestone; Marker : Unsigned_64) is
   begin
      if not Open then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      R.Mark_Milestone (State, Item, Marker, Now);
      Publish_State;
      Spinlocks.exitCriticalSection (Lock);
   end Mark_Milestone;

   procedure Read (Into   : System.Address;
                   Offset : Unsigned_64;
                   Length : Unsigned_64;
                   Copied : out Unsigned_64)
   is
      Wanted : Unsigned_64 := Unsigned_64'Min (Length, Layout.Read_Chunk_Bytes);
   begin
      Copied := 0;
      if Offset >= Layout.Image_Bytes or else Wanted = 0 then
         return;
      end if;
      Wanted := Unsigned_64'Min (Wanted, Layout.Image_Bytes - Offset);
      Spinlocks.enterCriticalSection (Lock);
      --  Expiry is checked on each read too, so a reader sees the window
      --  close even when no hook has run since.
      R.Expire (State, Now);
      Publish_State;
      declare
         Source : Storage_Array (1 .. Storage_Offset (Wanted))
           with Import, Address => State.Wire'Address + Storage_Offset (Offset);
         Target : Storage_Array (1 .. Storage_Offset (Wanted))
           with Import, Address => Into;
      begin
         Target := Source;
      end;
      Spinlocks.exitCriticalSection (Lock);
      Copied := Wanted;
   end Read;
end Boot_Timeline;
