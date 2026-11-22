with Interfaces;
with Compositor_Source_Content;
-- Bookkeeping for persistent per-surface GPU sources over a fixed slot set:
-- which slot holds which content key, the rows changed since the slot's
-- current content was captured, and the least-recently-used choice among
-- slots the caller can evict. Pure state only: no native resources, no
-- authority. Keys are unique, so one surface never owns two slots.
generic
   type Slot is range <>;
package Compositor_Source_Residency with SPARK_Mode, Pure is
   package C renames Compositor_Source_Content;
   use type C.Source_Key, C.Row_Band;
   type Use_Stamp is new Interfaces.Unsigned_64;
   type Entry_Record is record
      Key : C.Source_Key := C.No_Key;
      -- Rows changed since the content this slot holds or is uploading.
      Stale : C.Row_Band := C.Empty_Band;
      Last_Use : Use_Stamp := 0;
   end record;
   type Entry_Table is array (Slot) of Entry_Record;
   type State is record
      Entries : Entry_Table;
      Clock : Use_Stamp := 0;
   end record;
   type Candidates is array (Slot) of Boolean;

   function Unique_Keys (S : State) return Boolean is
     (for all I in Slot => (for all J in Slot =>
        (if I /= J and S.Entries (I).Key /= C.No_Key then S.Entries (I).Key /= S.Entries (J).Key)));
   function Valid (S : State) return Boolean is
     (Unique_Keys (S) and
      (for all I in Slot =>
         C.Normal (S.Entries (I).Stale) and S.Entries (I).Last_Use <= S.Clock and
         (if S.Entries (I).Key = C.No_Key then S.Entries (I).Stale = C.Empty_Band)));
   function Holds (S : State; Key : C.Source_Key) return Boolean is
     (Key /= C.No_Key and then (for some I in Slot => S.Entries (I).Key = Key));
   function Has_Free (S : State) return Boolean is
     (for some I in Slot => S.Entries (I).Key = C.No_Key);
   function Has_Candidate (Allowed : Candidates) return Boolean is
     (for some I in Slot => Allowed (I));

   function Find (S : State; Key : C.Source_Key) return Slot
     with Pre => Holds (S, Key),
       Post => S.Entries (Find'Result).Key = Key;
   function First_Free (S : State) return Slot
     with Pre => Has_Free (S),
       Post => S.Entries (First_Free'Result).Key = C.No_Key;
   -- Least recently used allowed slot; ties choose the lowest slot.
   function Victim (S : State; Allowed : Candidates) return Slot
     with Pre => Has_Candidate (Allowed),
       Post => Allowed (Victim'Result) and
         (for all J in Slot =>
            (if Allowed (J) then S.Entries (Victim'Result).Last_Use <= S.Entries (J).Last_Use));

   procedure Bind (S : in out State; I : Slot; Key : C.Source_Key)
     with Pre => Valid (S) and Key /= C.No_Key and not Holds (S, Key) and
                 S.Entries (I).Key = C.No_Key,
       Post => Valid (S) and S.Clock = S.Clock'Old and
         S.Entries (I).Key = Key and S.Entries (I).Stale = C.Empty_Band and
         Holds (S, Key) and Find (S, Key) = I and
         (for all J in Slot => (if J /= I then S.Entries (J) = S.Entries'Old (J)));
   procedure Unbind (S : in out State; I : Slot)
     with Pre => Valid (S),
       Post => Valid (S) and S.Clock = S.Clock'Old and
         S.Entries (I).Key = C.No_Key and
         not Holds (S, S.Entries'Old (I).Key) and
         (for all J in Slot => (if J /= I then S.Entries (J) = S.Entries'Old (J)));
   -- Mark one use; the stamp is the newest unless the clock saturated.
   procedure Touch (S : in out State; I : Slot)
     with Pre => Valid (S),
       Post => Valid (S) and S.Clock >= S.Clock'Old and
         S.Entries (I).Key = S.Entries'Old (I).Key and
         S.Entries (I).Stale = S.Entries'Old (I).Stale and
         S.Entries (I).Last_Use = S.Clock and
         (for all J in Slot => (if J /= I then S.Entries (J) = S.Entries'Old (J))) and
         (if S.Clock'Old < Use_Stamp'Last then
            (for all J in Slot => (if J /= I then S.Entries (J).Last_Use < S.Entries (I).Last_Use)));
   -- Record rows changed by a new version. Unknown keys need nothing: their
   -- first upload covers every row.
   procedure Note (S : in out State; Key : C.Source_Key; Rows : C.Row_Band)
     with Pre => Valid (S) and C.Normal (Rows),
       Post => Valid (S) and S.Clock = S.Clock'Old and
         (if Holds (S, Key) then
            Holds (S'Old, Key) and Find (S'Old, Key) = Find (S, Key) and
            C.Covers (S.Entries (Find (S, Key)).Stale, Rows) and
            C.Covers (S.Entries (Find (S, Key)).Stale, S.Entries'Old (Find (S, Key)).Stale) and
            S.Entries (Find (S, Key)).Key = Key and
            S.Entries (Find (S, Key)).Last_Use = S.Entries'Old (Find (S, Key)).Last_Use and
            (for all J in Slot => (if J /= Find (S, Key) then S.Entries (J) = S.Entries'Old (J)))
          else S = S'Old);
   -- A new content pass for slot I now covers every recorded change.
   procedure Clear_Stale (S : in out State; I : Slot)
     with Pre => Valid (S),
       Post => Valid (S) and S.Clock = S.Clock'Old and
         S.Entries (I).Stale = C.Empty_Band and
         S.Entries (I).Key = S.Entries'Old (I).Key and
         S.Entries (I).Last_Use = S.Entries'Old (I).Last_Use and
         (for all J in Slot => (if J /= I then S.Entries (J) = S.Entries'Old (J)));
end Compositor_Source_Residency;
