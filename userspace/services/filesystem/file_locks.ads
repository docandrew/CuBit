------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Advisory file locks (Queue_Lock, flock(2) semantics): which open handles
--  hold a file shared or exclusively, which wait, and in what order they
--  are granted.
--
--  @description
--  A lock belongs to one open handle (a slot of the service's handle
--  table) and names the file the handle has open (Object_Key). It is
--  released when the handle unlocks, closes or is parked, and when its
--  process exits (the service releases every handle then), however the
--  process ended: a lock held is a live holder. Two handles of one
--  process conflict like handles of two processes (flock, not POSIX
--  record locks).
--
--  Requests are granted in arrival order per file: a request is granted
--  at once only when it is compatible with every holder and nobody waits
--  for that file; otherwise it waits (or, without waiting, is refused
--  Busy). When a holder lets go, the oldest waiter is granted if it is
--  compatible, then the next, and so on, stopping at the first that is
--  not: an exclusive waiter is not starved by a stream of shared ones.
--  Changing a held lock's mode releases it first and then requests the
--  new one (as Linux flock): never a deadlock between two upgraders.
--
--  A wait ends granted, at its deadline (absolute, ms; No_Deadline spelled
--  out for none), or when the handle goes or unlocks.
--
--  Proved (tests/filesystem-locks, level 2): no run-time errors, and that
--  holders never conflict: two handles hold one file at once only when
--  both hold it shared (Consistent, kept by every operation). The waiter
--  count (Waiting_Count) and the order of grants are checked by the
--  hosted tests against a reference model.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   --  Handle slots of the service's table.
   Capacity : Positive;
package File_Locks with SPARK_Mode, Pure is
   --  The contracts quantify over every pair of slots: proved, never
   --  evaluated at run time (the hosted tests check the same properties
   --  independently).
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   subtype Slot is Natural range 0 .. Capacity - 1;
   subtype Slot_Count is Natural range 0 .. Capacity;

   --  The file a handle has open: the same key for every handle on it.
   type Object_Key is new Unsigned_64;

   type Mode is (Unlocked, Shared, Exclusive);
   subtype Lock_Mode is Mode range Shared .. Exclusive;

   --  Absolute monotonic milliseconds.
   subtype Deadline is Unsigned_64;
   No_Deadline : constant Deadline := Deadline'Last;

   --  Arrival order of waits. Waits that would need a number past the
   --  last one are refused (Busy): 2**64 waits, never in practice.
   type Arrival is new Unsigned_64;

   type Slot_State is record
      Object   : Object_Key := 0;      --  meaningful while Held or Waiting
      Held     : Mode := Unlocked;
      Waiting  : Boolean := False;
      Wanted   : Lock_Mode := Shared;  --  while Waiting
      Order    : Arrival := 0;         --  while Waiting
      Until_At : Deadline := No_Deadline;
   end record;
   type Slot_Array is array (Slot) of Slot_State;

   type Table is record
      Slots : Slot_Array;
      Next_Order : Arrival := 1;
      --  Slots that wait (the service looks for deadlines only when some do).
      Waiting_Count : Slot_Count := 0;
   end record;

   --  No two handles hold one file unless both hold it shared.
   function Consistent (T : Table) return Boolean is
     (for all A in Slot =>
        (for all B in Slot =>
           (if A /= B and then T.Slots (A).Held /= Unlocked
               and then T.Slots (B).Held /= Unlocked
               and then T.Slots (A).Object = T.Slots (B).Object
            then T.Slots (A).Held = Shared and then T.Slots (B).Held = Shared)));

   --  Every other handle's hold on Object allows S to hold it as Wanted.
   function Compatible
     (T : Table; S : Slot; Object : Object_Key; Wanted : Lock_Mode) return Boolean is
     (for all O in Slot =>
        (if O /= S and then T.Slots (O).Held /= Unlocked and then T.Slots (O).Object = Object
         then Wanted = Shared and then T.Slots (O).Held = Shared));

   --  Some handle waits for Object.
   function Has_Waiter (T : Table; Object : Object_Key) return Boolean is
     (for some O in Slot => T.Slots (O).Waiting and then T.Slots (O).Object = Object);

   function Held (T : Table; S : Slot) return Mode is (T.Slots (S).Held);
   function Waiting (T : Table; S : Slot) return Boolean is (T.Slots (S).Waiting);
   function Object_Of (T : Table; S : Slot) return Object_Key is (T.Slots (S).Object);

   type Outcome is (Granted, Busy, Queued);

   --  Handle S (on Object) asks for Wanted. With Wait, a conflict queues it
   --  until Until_At; without, it is Busy. Released: S held another mode,
   --  which was let go first; the caller then settles Object (Settle).
   procedure Request
     (T : in out Table; S : Slot; Object : Object_Key; Wanted : Lock_Mode;
      Wait : Boolean; Until_At : Deadline; Result : out Outcome; Released : out Boolean)
   with Pre  => Consistent (T) and then not T.Slots (S).Waiting
                and then (T.Slots (S).Held = Unlocked or else T.Slots (S).Object = Object),
        Post => Consistent (T)
                and then (case Result is
                            when Granted => T.Slots (S).Held = Wanted and then not T.Slots (S).Waiting,
                            when Busy    => T.Slots (S).Held = Unlocked and then not T.Slots (S).Waiting,
                            when Queued  => Wait and then T.Slots (S).Waiting
                                            and then T.Slots (S).Held = Unlocked)
                and then T.Slots (S).Object = Object;

   --  Handle S lets go: its lock (if any) and its wait (if any) end.
   --  The caller then settles S's object when it held a lock or waited.
   procedure Release (T : in out Table; S : Slot; Was_Held, Was_Waiting : out Boolean)
   with Pre  => Consistent (T),
        Post => Consistent (T) and then T.Slots (S).Held = Unlocked
                and then not T.Slots (S).Waiting
                and then T.Slots (S).Object = T'Old.Slots (S).Object
                and then Was_Held = (T'Old.Slots (S).Held /= Unlocked)
                and then Was_Waiting = T'Old.Slots (S).Waiting;

   --  Handle S's wait (if any) ends; a lock it holds stays (its client's
   --  queue went, its handle did not). The caller then settles S's object.
   procedure Cancel_Wait (T : in out Table; S : Slot; Was_Waiting : out Boolean)
   with Pre  => Consistent (T),
        Post => Consistent (T) and then not T.Slots (S).Waiting
                and then T.Slots (S).Held = T'Old.Slots (S).Held
                and then T.Slots (S).Object = T'Old.Slots (S).Object
                and then Was_Waiting = T'Old.Slots (S).Waiting;

   --  The oldest waiter for Object, granted if it is now compatible
   --  (Found, Granted_Slot); otherwise nothing changes. Call until not
   --  Found after a lock on Object ends or one of its waits does.
   procedure Settle
     (T : in out Table; Object : Object_Key; Granted_Slot : out Slot; Found : out Boolean)
   with Pre  => Consistent (T),
        Post => Consistent (T)
                and then (if Found then T.Slots (Granted_Slot).Held /= Unlocked
                                        and then not T.Slots (Granted_Slot).Waiting
                                        and then T.Slots (Granted_Slot).Object = Object
                                        and then T'Old.Slots (Granted_Slot).Waiting);

   --  A wait whose deadline is at or before Now, ended (Found, Expired_Slot);
   --  the caller answers it and settles its object.
   procedure Expire
     (T : in out Table; Now : Deadline; Expired_Slot : out Slot; Found : out Boolean)
   with Pre  => Consistent (T),
        Post => Consistent (T)
                and then (if Found then not T.Slots (Expired_Slot).Waiting
                                        and then T'Old.Slots (Expired_Slot).Waiting);

   --  The earliest deadline of any wait (No_Deadline: none).
   function Earliest_Deadline (T : Table) return Deadline;

end File_Locks;
