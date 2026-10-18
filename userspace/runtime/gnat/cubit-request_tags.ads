------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A client's outstanding requests on one channel (CAP-002 step 3,
--  Channel_Clients): the tag each request carries and what became of it.
--
--  @description
--  Issue gives a fresh tag and an entry, only while an entry (that is, a
--  completion slot) is free. Tags only grow and are never reused, so an
--  answer to an earlier request can never match a later one. Complete
--  matches an answer to its waiting entry; an unknown tag, or one already
--  answered, changes nothing and is reported (a lying or confused server
--  costs only its own answers). Fail_All ends every waiting entry when the
--  channel ends. Release frees an entry once its waiter has read it.
--
--  Proved (level 2): used entries hold distinct tags, all below Next_Tag;
--  Issue never reuses a tag; Complete changes at most the one entry whose
--  tag matches and only from Waiting to Answered; Fail_All moves every
--  Waiting entry to Ended and nothing else; the count of used entries never
--  exceeds Capacity. The tag counter never wraps (Exhausted at its end).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   Capacity : Positive;
package CuBit.Request_Tags with Pure, SPARK_Mode is

   subtype Entry_Index is Positive range 1 .. Capacity;

   --  Tag zero is never issued.
   type Tag is new Unsigned_64;
   No_Tag    : constant Tag := 0;
   Last_Tag  : constant Tag := Tag'Last - 1;

   type Entry_State is (Free, Waiting, Answered, Ended);

   type Slot is record
      State  : Entry_State := Free;
      Number : Tag := No_Tag;
   end record;
   type Slots is array (Entry_Index) of Slot;

   type Table is record
      Entries  : Slots;
      Next_Tag : Tag := 1;
   end record;

   function Valid (T : Table) return Boolean is
     (T.Next_Tag >= 1
      and then (for all I in Entry_Index =>
                  (if T.Entries (I).State /= Free then
                     T.Entries (I).Number /= No_Tag and then T.Entries (I).Number < T.Next_Tag
                   else T.Entries (I).Number = No_Tag))
      and then (for all I in Entry_Index =>
                  (for all J in Entry_Index =>
                     (if I /= J and then T.Entries (I).State /= Free and then T.Entries (J).State /= Free
                      then T.Entries (I).Number /= T.Entries (J).Number))));

   function Has_Free (T : Table) return Boolean is
     (for some I in Entry_Index => T.Entries (I).State = Free);

   type Issue_Result is (Issued, Full, Exhausted);

   procedure Issue (T : in out Table; Number : out Tag; At_Entry : out Entry_Index;
                    Result : out Issue_Result)
   with Pre  => Valid (T),
        Post => Valid (T)
                and then (if Result = Issued then
                            Number = T'Old.Next_Tag and then T.Next_Tag = T'Old.Next_Tag + 1
                            and then T'Old.Entries (At_Entry).State = Free
                            and then T.Entries (At_Entry) = (State => Waiting, Number => Number)
                            and then (for all I in Entry_Index =>
                                        (if I /= At_Entry then T.Entries (I) = T'Old.Entries (I)))
                          else T = T'Old)
                and then (Result = Full) = (not Has_Free (T'Old) and then T'Old.Next_Tag <= Last_Tag)
                and then (Result = Exhausted) = (T'Old.Next_Tag > Last_Tag);

   type Match is (Matched, Unknown, Duplicate);

   procedure Complete (T : in out Table; Number : Tag; At_Entry : out Entry_Index;
                       Result : out Match)
   with Pre  => Valid (T),
        Post => Valid (T) and then T.Next_Tag = T'Old.Next_Tag
                and then (if Result = Matched then
                            T'Old.Entries (At_Entry) = (State => Waiting, Number => Number)
                            and then T.Entries (At_Entry) = (State => Answered, Number => Number)
                            and then (for all I in Entry_Index =>
                                        (if I /= At_Entry then T.Entries (I) = T'Old.Entries (I)))
                          else T = T'Old)
                and then (Result = Unknown) =
                           (Number = No_Tag or else
                            (for all I in Entry_Index =>
                               T'Old.Entries (I).State = Free or else T'Old.Entries (I).Number /= Number));

   procedure Fail_All (T : in out Table)
   with Pre  => Valid (T),
        Post => Valid (T) and then T.Next_Tag = T'Old.Next_Tag
                and then (for all I in Entry_Index =>
                            T.Entries (I).Number = T'Old.Entries (I).Number
                            and then T.Entries (I).State =
                              (if T'Old.Entries (I).State = Waiting then Ended
                               else T'Old.Entries (I).State));

   procedure Release (T : in out Table; At_Entry : Entry_Index)
   with Pre  => Valid (T) and then T.Entries (At_Entry).State in Answered | Ended,
        Post => Valid (T) and then T.Next_Tag = T'Old.Next_Tag
                and then T.Entries (At_Entry).State = Free
                and then (for all I in Entry_Index =>
                            (if I /= At_Entry then T.Entries (I) = T'Old.Entries (I)));

end CuBit.Request_Tags;
