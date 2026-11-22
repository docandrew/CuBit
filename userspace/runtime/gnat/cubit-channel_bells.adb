------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Channel_Wake_ABI;

package body CuBit.Channel_Bells is

   Any_Noted : Boolean := False with Volatile;

   procedure Watch (Grant : GR.Reference; Result : out Watch_Result) is
      pragma Unreferenced (Grant);
   begin
      Result := Unrouted;
   end Watch;

   procedure Unwatch (Grant : GR.Reference) is
      pragma Unreferenced (Grant);
   begin
      null;
   end Unwatch;

   function Take (Grant : GR.Reference; Routed : Boolean) return Boolean is
      pragma Unreferenced (Grant, Routed);
   begin
      return True;
   end Take;

   procedure Note_Notice (Label : Unsigned_32; Length : Unsigned_8; W : Words;
                          Is_Notice : out Boolean) is
      pragma Unreferenced (W);
   begin
      Is_Notice := Label = Channel_Wake_ABI.Wake_Label
        and then Natural (Length) in 1 .. Channel_Wake_ABI.Wakes_Per_Event;
      if Is_Notice then
         Any_Noted := True;
      end if;
   end Note_Notice;

   procedure Note_Wake is
   begin
      Any_Noted := True;
   end Note_Wake;

   function Noted return Boolean is (Any_Noted);

   procedure Clear_Noted is
   begin
      Any_Noted := False;
   end Clear_Noted;

end CuBit.Channel_Bells;
