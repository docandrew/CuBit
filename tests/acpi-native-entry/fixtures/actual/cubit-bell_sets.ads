------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which of this process's channel grants have been rung (CAP-002 step 3).
--
--  @description
--  The kernel tells a process which grants it receives were rung (a
--  doorbell notice, or TAKE_DOORBELLS). A channel registers the grant it
--  watches; Note marks a registered grant rung; the channel's owner takes
--  the mark. A bell for a grant nobody registered (a channel closed
--  meanwhile) is dropped: it names nothing this process still reads.
--
--  Set-associative: a grant goes into the bucket its slot selects, one of
--  Ways entries. A full bucket refuses the registration and the channel
--  then looks at its rings on every wake instead (Unrouted): slower, never
--  wrong. Every entry holds the whole reference (slot and generation), so a
--  mark is never given to a different grant or another life of the slot.
--
--  Proved (level 2): Note marks exactly the registered entry equal to the
--  reference, nothing else; Take reports a mark only for that reference and
--  clears it; Register never displaces another entry; no reference is
--  registered twice.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Grant_References;

generic
   Bucket_Count : Positive;
   Ways         : Positive;
package CuBit.Bell_Sets with Pure, SPARK_Mode is

   package GR renames CuBit.Grant_References;
   use type GR.Reference;

   subtype Bucket_Index is Natural range 0 .. Bucket_Count - 1;
   subtype Way_Index is Positive range 1 .. Ways;

   type Entry_State is record
      Used : Boolean := False;
      Ref  : GR.Reference := (slot => 0, generation => 1);
      Rung : Boolean := False;
   end record;

   type Bucket is array (Way_Index) of Entry_State;
   type Set is array (Bucket_Index) of Bucket;

   Empty : constant Set := [others => [others => (Used => False, Ref => (slot => 0, generation => 1), Rung => False)]];

   function Bucket_Of (Ref : GR.Reference) return Bucket_Index is
     (Natural (Ref.slot mod GR.Global_Slot (Bucket_Count)));

   --  No reference is held twice in its bucket.
   function Distinct (S : Set) return Boolean is
     (for all B in Bucket_Index =>
        (for all I in Way_Index =>
           (for all J in Way_Index =>
              (if I /= J and then S (B) (I).Used and then S (B) (J).Used
               then S (B) (I).Ref /= S (B) (J).Ref))));

   --  Every held reference is in the bucket its slot selects.
   function Placed (S : Set) return Boolean is
     (for all B in Bucket_Index =>
        (for all I in Way_Index =>
           (if S (B) (I).Used then Bucket_Of (S (B) (I).Ref) = B)));

   function Valid (S : Set) return Boolean is (Distinct (S) and then Placed (S));

   function Holds (S : Set; Ref : GR.Reference) return Boolean is
     (for some I in Way_Index =>
        S (Bucket_Of (Ref)) (I).Used and then S (Bucket_Of (Ref)) (I).Ref = Ref);

   type Register_Result is (Registered, Already, Unrouted);

   procedure Register (S : in out Set; Ref : GR.Reference; Result : out Register_Result)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then (if Result in Registered | Already then Holds (S, Ref))
                and then (if Holds (S'Old, Ref) then Result = Already and then S = S'Old)
                and then (if Result = Unrouted then S = S'Old)
                --  Nothing else moved.
                and then (for all B in Bucket_Index =>
                            (if B /= Bucket_Of (Ref) then S (B) = S'Old (B)))
                and then (for all I in Way_Index =>
                            (if S'Old (Bucket_Of (Ref)) (I).Used
                             then S (Bucket_Of (Ref)) (I) = S'Old (Bucket_Of (Ref)) (I)));

   procedure Unregister (S : in out Set; Ref : GR.Reference)
   with Pre  => Valid (S),
        Post => Valid (S) and then not Holds (S, Ref)
                and then (for all B in Bucket_Index =>
                            (if B /= Bucket_Of (Ref) then S (B) = S'Old (B)))
                and then (for all I in Way_Index =>
                            (if not (S'Old (Bucket_Of (Ref)) (I).Used
                                     and then S'Old (Bucket_Of (Ref)) (I).Ref = Ref)
                             then S (Bucket_Of (Ref)) (I) = S'Old (Bucket_Of (Ref)) (I)));

   --  Mark Ref rung if registered (Known); otherwise nothing changes.
   procedure Note (S : in out Set; Ref : GR.Reference; Known : out Boolean)
   with Pre  => Valid (S),
        Post => Valid (S) and then Known = Holds (S'Old, Ref)
                and then (for all B in Bucket_Index =>
                            (for all I in Way_Index =>
                               S (B) (I).Used = S'Old (B) (I).Used
                               and then S (B) (I).Ref = S'Old (B) (I).Ref
                               and then S (B) (I).Rung =
                                 (S'Old (B) (I).Rung or else
                                    (S'Old (B) (I).Used and then S'Old (B) (I).Ref = Ref))));

   --  Whether Ref was rung since it was last taken; clears its mark only.
   procedure Take (S : in out Set; Ref : GR.Reference; Was_Rung : out Boolean)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then Was_Rung = (for some I in Way_Index =>
                                       S'Old (Bucket_Of (Ref)) (I).Used
                                       and then S'Old (Bucket_Of (Ref)) (I).Ref = Ref
                                       and then S'Old (Bucket_Of (Ref)) (I).Rung)
                and then (for all B in Bucket_Index =>
                            (for all I in Way_Index =>
                               S (B) (I).Used = S'Old (B) (I).Used
                               and then S (B) (I).Ref = S'Old (B) (I).Ref
                               and then S (B) (I).Rung =
                                 (S'Old (B) (I).Rung and then
                                    not (S'Old (B) (I).Used and then S'Old (B) (I).Ref = Ref))));

end CuBit.Bell_Sets;
