with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Mixer_Control; use Mixer_Control;
procedure Main is
   Owners : constant Owner_Table := [0 => 42, 1 => 73, 2 => 0];
   function Check (Op : Unsigned_32; Len : Unsigned_8;
                   Data : Words; Caller : Unsigned_64 := 42;
                   Flags : Unsigned_8 := 0; Reserved : Unsigned_16 := 0)
                   return Boolean is
     (Allowed (Op, Len, Flags, Reserved, Data, Caller, Owners));
begin
   declare
      Tag : constant Unsigned_64 := 16#8000_0000_0000_0001#;
      function Master (Op : Unsigned_32; Len : Unsigned_8; Data : Words;
                       Caller : Unsigned_64 := Tag;
                       Flags : Unsigned_8 := 0; Reserved : Unsigned_16 := 0)
                       return Boolean is
        (Master_Allowed (Op, Len, Flags, Reserved, Data, Caller, Tag));
   begin
      pragma Assert (Master (16#0507#, 0, [0, 0, 0, 0]));
      for Len in Unsigned_8 loop
         for Level in Unsigned_64 range 0 .. 101 loop
            for Mute in Unsigned_64 range 0 .. 2 loop
               pragma Assert
                 (Master (16#0508#, Len, [Level, Mute, 0, 0]) =
                  (Len = 2 and then Level <= 100 and then Mute <= 1));
            end loop;
         end loop;
      end loop;
      for Caller in Unsigned_64 range 0 .. 65_535 loop
         pragma Assert (not Master (16#0507#, 0, [0, 0, 0, 0], Caller));
         pragma Assert (not Master (16#0508#, 2, [100, 0, 0, 0], Caller));
      end loop;
      pragma Assert (not Master (16#0508#, 2, [Unsigned_64'Last, 0, 0, 0]));
      pragma Assert (not Master (16#0508#, 2, [100, 0, 1, 0]));
      pragma Assert (not Master (16#0508#, 2, [100, 0, 0, 1]));
      pragma Assert (not Master (16#0508#, 2, [100, 0, 0, 0], Flags => 1));
      pragma Assert (not Master (16#0508#, 2, [100, 0, 0, 0], Reserved => 1));
      pragma Assert (not Master (16#0507#, 0, [1, 0, 0, 0]));
      pragma Assert (not Master (16#0507#, 0, [0, 1, 0, 0]));
      pragma Assert (not Master (16#0509#, 0, [0, 0, 0, 0]));
      pragma Assert (not Master_Allowed (16#0507#, 0, 0, 0, [0, 0, 0, 0], 0, 0));
   end;
   pragma Assert (Check (16#0500#, 2, [48_000, 2, 0, 0]));
   pragma Assert (not Check (16#0500#, 2, [44_100, 2, 0, 0]));
   pragma Assert (not Check (16#0500#, 2, [48_000, 1, 0, 0]));
   pragma Assert (not Check (16#0500#, 2, [48_000, 16#1_0000_0002#, 0, 0]));
   pragma Assert (not Check (16#0500#, 2, [48_000, 2, 0, 0], 0));
   pragma Assert (not Check (16#0500#, 2, [48_000, 2, 0, 0], Unsigned_64'Last));
   for Len in Unsigned_8 loop
      pragma Assert
        (Check (16#0502#, Len, [0, 65_536, 0, 0]) = (Len = 2));
   end loop;
   for Gain in Unsigned_64 range 0 .. 131_073 loop
      pragma Assert
        (Check (16#0502#, 2, [0, Gain, 0, 0]) = (Gain <= 131_072));
      pragma Assert
        (Check (16#0504#, 2, [0, Gain, 0, 0]) = (Gain <= 65_536));
   end loop;
   for Op in Unsigned_32 range 16#0501# .. 16#0504# loop
      declare
         Len : constant Unsigned_8 := (if Op in 16#0501# | 16#0503# then 1 else 2);
      begin
         pragma Assert (Check (Op, Len, [0, 0, 0, 0]));
         pragma Assert (not Check (Op, Len, [1, 0, 0, 0]));
         pragma Assert (Check (Op, Len, [1, 0, 0, 0], 73));
         pragma Assert (not Check (Op, Len, [2, 0, 0, 0]));
         pragma Assert (not Check (Op, Len, [Unsigned_64'Last, 0, 0, 0]));
         pragma Assert (not Check (Op, Len, [0, Unsigned_64'Last, 0, 0]));
         pragma Assert (not Check (Op, Len, [0, 0, 1, 0]));
         pragma Assert (not Check (Op, Len, [0, 0, 0, 1]));
         pragma Assert (not Check (Op, Len, [0, 0, 0, 0], Flags => 1));
         pragma Assert (not Check (Op, Len, [0, 0, 0, 0], Reserved => 1));
      end;
   end loop;
   pragma Assert (not Check (16#0516#, 4, [0, 0, 0, 0]));
   pragma Assert (not Check (16#0505#, 2, [0, 0, 0, 0]));
   pragma Assert (not Allowed (16#0501#, 1, 0, 0, [0, 0, 0, 0], 42, [1 .. 0 => 0]));
   Put_Line ("PASS: mixer control ownership, wire bounds, formats and gains");
end Main;
