-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- What a debug-session Call may do (DBG-001, Debug_ABI): decided from the
-- handle, the session and the request alone.
--
-- @description
-- Proved (gnatprove level 2, tests/debug-sessions):
--  * an operation runs only on a live session whose generation and kind
--    match the handle (a revoked or detached session admits nothing);
--  * only a Debug_Observe session runs an operation on its target: the
--    other three kinds answer Not_Yet, except Describe and Detach;
--  * an operation on the target runs only while the target is the process
--    instance the session was minted for;
--  * a view range lies wholly inside the session's own slice of the view
--    aperture (so inside the aperture, and never past it), and a target
--    range lies wholly in the user half.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Debug_ABI; use Debug_ABI;

package Debug_Admission with SPARK_Mode, Pure is

   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;

   -- The facts the kernel checked for one Call (Debug_Sessions).
   type Facts is record
      Live           : Boolean := False;  -- the session is in use
      Same_Life      : Boolean := False;  -- the handle's generation is the session's
      Same_Kind      : Boolean := False;  -- the handle's kind is the session's
      Target_Current : Boolean := False;  -- the target is the instance minted for
      Kind           : Debug_Kind := Observe;
   end record;

   -- Runs on the target (needs the target live and, today, Debug_Observe).
   function On_Target (Op : Operation) return Boolean is
     (Op not in Describe | Detach);

   function Decide (F : Facts; Label, Length : Unsigned_64) return Outcome is
     (if not (F.Live and then F.Same_Life and then F.Same_Kind) then Revoked
      elsif not Is_Operation_Label (Label) then Unknown_Operation
      elsif Length /= Request_Words (To_Operation (Label)) then Malformed
      elsif To_Operation (Label) = Detach then Done
      elsif F.Kind /= Observe and then To_Operation (Label) /= Describe then Not_Yet
      elsif On_Target (To_Operation (Label)) and then not F.Target_Current then Target_Gone
      else Done)
   with Post =>
     (if Decide'Result = Done then
        F.Live and then F.Same_Life and then F.Same_Kind
        and then Is_Operation_Label (Label)
        and then Length = Request_Words (To_Operation (Label))
        and then (if On_Target (To_Operation (Label)) then
                    F.Kind = Observe and then F.Target_Current));

   -- A view range: Pages whole pages (1 .. Map_Pages_Ceiling) from a
   -- page-aligned View inside session S's slice.
   function View_Admitted (S : Session_Index; View, Pages : Unsigned_64) return Boolean is
     (Pages in 1 .. Map_Pages_Ceiling
      and then View mod Page_Bytes = 0
      and then View >= Slice_Base (S)
      and then View - Slice_Base (S) <= Slice_Bytes - Pages * Page_Bytes)
   with Post =>
     (if View_Admitted'Result then
        View >= Aperture_First
        and then View + Pages * Page_Bytes <= Slice_Base (S) + Slice_Bytes
        and then View + Pages * Page_Bytes <= Aperture_Limit);

   -- Every byte of an admitted view range lies in S's slice.
   procedure Prove_In_Slice (S : Session_Index; View, Pages, Byte : Unsigned_64) with
     Ghost,
     Pre  => View_Admitted (S, View, Pages)
             and then Byte >= View and then Byte - View < Pages * Page_Bytes,
     Post => Byte >= Slice_Base (S) and then Byte < Slice_Base (S) + Slice_Bytes
             and then Byte >= Aperture_First and then Byte < Aperture_Limit;

   -- A target range: Pages whole pages from a page-aligned user Target.
   function Target_Admitted (Target, Pages : Unsigned_64) return Boolean is
     (Pages in 1 .. Map_Pages_Ceiling
      and then Target mod Page_Bytes = 0
      and then Target < User_Limit
      and then Pages * Page_Bytes <= User_Limit - Target)
   with Post =>
     (if Target_Admitted'Result then Target + Pages * Page_Bytes <= User_Limit);

   -- Page I of a range (I < Pages): its address stays inside the range.
   function Page_Address (First, I : Unsigned_64) return Unsigned_64 is
     (First + I * Page_Bytes)
   with Pre => I < Map_Pages_Ceiling and then First <= Unsigned_64'Last - Map_Pages_Ceiling * Page_Bytes;
end Debug_Admission;
