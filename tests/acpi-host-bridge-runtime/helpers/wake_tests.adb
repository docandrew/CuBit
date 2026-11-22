pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with CuBit.Channel_Bells;
with CuBit.Grant_References;
with Channel_Wake_ABI;
procedure Wake_Tests is
   package B renames CuBit.Channel_Bells;
   use type B.Watch_Result;
   use type Interfaces.Unsigned_32;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   First : constant CuBit.Grant_References.Reference := (slot => 7, generation => 19);
   Second : constant CuBit.Grant_References.Reference := (slot => 17, generation => 29);
   Route : B.Watch_Result;
   Noticed : Boolean;
begin
   B.Clear_Noted;
   Check (not B.Noted, "clear process-wide wake observation");
   B.Watch (First, Route); Check (Route = B.Unrouted, "first channel has no grant-specific wake route");
   B.Watch (Second, Route); Check (Route = B.Unrouted, "second channel has no grant-specific wake route");
   Check (B.Take (First, True) and B.Take (Second, False), "every channel checks even without a routed notice");
   B.Note_Notice (Channel_Wake_ABI.Wake_Label, Channel_Wake_ABI.Notice_Words,
      [0 => Channel_Wake_ABI.Wake_Word_Notice, others => 0], Noticed);
   Check (Noticed and B.Noted, "canonical single-word kernel wake notice recorded");
   Check (B.Take (First, False) and B.Take (Second, False), "one channel cannot consume another channel's wake");
   B.Clear_Noted; B.Note_Wake;
   Check (B.Noted and B.Take (First, False) and B.Take (Second, False), "ready-word wait observation wakes all ring checks");
   for Length in 0 .. Channel_Wake_ABI.Wakes_Per_Event + 1 loop
      B.Clear_Noted;
      B.Note_Notice (Channel_Wake_ABI.Wake_Label, Interfaces.Unsigned_8 (Length), [others => 0], Noticed);
      Check (Noticed = (Length in 1 .. Channel_Wake_ABI.Wakes_Per_Event), "notice length matches current runtime contract");
   end loop;
   B.Clear_Noted;
   B.Note_Notice (Channel_Wake_ABI.Wake_Label + 1, Channel_Wake_ABI.Notice_Words, [others => 0], Noticed);
   Check (not Noticed and not B.Noted, "unrelated event cannot fabricate a wake observation");
   B.Unwatch (First); B.Unwatch (Second);
   Check (B.Take (First, False) and B.Take (Second, False), "unwatch does not hide ring work");
   Ada.Text_IO.Put_Line ("Option B process wake checks" & Checks'Image);
end Wake_Tests;
