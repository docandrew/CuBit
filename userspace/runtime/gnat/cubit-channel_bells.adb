------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Bell_Sets;

package body CuBit.Channel_Bells is

   --  256 watched grants in 64 buckets of 4: about 6 KiB.
   package Sets is new CuBit.Bell_Sets (Bucket_Count => 64, Ways => 4);
   use type Sets.Register_Result;

   Watched : Sets.Set := Sets.Empty;
   --  A watched grant's wake was noted since Clear_Noted.
   Any_Noted : Boolean := False;

   procedure Watch (Grant : GR.Reference; Result : out Watch_Result) is
      R : Sets.Register_Result;
   begin
      Sets.Register (Watched, Grant, R);
      Result := (if R = Sets.Unrouted then Unrouted else Routed);
   end Watch;

   procedure Unwatch (Grant : GR.Reference) is
   begin
      Sets.Unregister (Watched, Grant);
   end Unwatch;

   function Take (Grant : GR.Reference; Routed : Boolean) return Boolean is
      Was : Boolean;
   begin
      if not Routed then
         return True;
      end if;
      Sets.Take (Watched, Grant, Was);
      return Was;
   end Take;

   procedure Note (Grant : GR.Reference);
   procedure Note (Grant : GR.Reference) is
      Known : Boolean;
   begin
      Sets.Note (Watched, Grant, Known);
      if Known then
         Any_Noted := True;
      end if;
   end Note;

   function Noted return Boolean is (Any_Noted);

   procedure Clear_Noted is
   begin
      Any_Noted := False;
   end Clear_Noted;

   procedure Note_Notice (Label : Unsigned_32; Length : Unsigned_8; W : Words;
                          Is_Notice : out Boolean) is
      Bells : CuBit.Doorbells.Bell_Array (1 .. CuBit.Doorbells.ABI.Wakes_Per_Event);
      Count : CuBit.Doorbells.Wake_Count;
   begin
      CuBit.Doorbells.Decode (Label, Length, W, Is_Notice, Bells, Count);
      for I in 1 .. Count loop
         Note (Bells (I));
      end loop;
   end Note_Notice;

   procedure Note_Bells (Bells : CuBit.Doorbells.Bell_Array) is
   begin
      for B of Bells loop
         Note (B);
      end loop;
   end Note_Bells;

end CuBit.Channel_Bells;
