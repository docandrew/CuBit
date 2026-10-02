with Interfaces; use Interfaces;
with Intel_GPU_WOPCM;
with Ada.Text_IO;
procedure WOPCM_Tests is
   procedure Run (Mode : Natural) is
      Size : Unsigned_32 := (if Mode in 1 | 3 | 5 | 6 then 16#80001# else 0);
      Base : Unsigned_32 := (if Mode in 1 | 4 then 16#4001#
                            elsif Mode = 5 then 16#8001#
                            elsif Mode = 6 then 16#4003# else 0);
      Reads, Writes : Natural := 0;
      function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         Reads := Reads + 1;
         if Mode = 2 or (Mode = 11 and Reads = 3) or (Mode = 12 and Reads = 4)
         then return Unsigned_32'Last; end if;
         pragma Assert (Offset in 16#C050# | 16#C340#);
         return (if Offset = 16#C050# then Size else Base);
      end Read32;
      procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         pragma Assert ((Writes = 1 and Offset = 16#C050# and Value = 16#80000#)
           or (Writes = 2 and Offset = 16#C340# and Value = 16#4000#));
         Success := not ((Mode = 7 and Writes = 1) or (Mode = 8 and Writes = 2));
         if Offset = 16#C050# then
            Size := (if Mode = 9 then Value else Value or 1);
         else Base := (if Mode = 10 then Value else Value or 1); end if;
      end Write32;
      package W is new Intel_GPU_WOPCM (Read32, Write32);
      use type W.Result;
      use type W.Phase;
      Object : W.Attempt;
      Status : W.Result;
      Saved : Natural;
      Saved_Phase : W.Phase;
   begin
      W.Configure (Object, (if Mode = 13 then 0 else 1024 * 1024),
                   16#4000#, 16#80000#, 335104, Status);
      pragma Assert (Status =
        (case Mode is
           when 0 | 1 => W.Complete,
           when 2 => W.Invalid_MMIO,
           when 3 .. 6 => W.Locked_Conflict,
           when 7 | 8 => W.Write_Failed,
           when 9 .. 12 => W.Readback_Failed,
           when others => W.Rejected));
      pragma Assert (Writes = (if Mode in 0 | 8 | 10 | 12 then 2
                               elsif Mode in 7 | 9 | 11 then 1 else 0));
      if Mode = 13 then pragma Assert (Reads = 0); end if;
      Saved_Phase := W.Current (Object);
      pragma Assert (Saved_Phase = (if Mode <= 1 then W.Configured
                                   elsif Writes > 0 then W.Quarantined else W.Consumed));
      Saved := Reads + Writes;
      W.Configure (Object, 1024 * 1024, 16#4000#, 16#80000#, 335104, Status);
      pragma Assert (Status = W.Rejected and Saved = Reads + Writes);
      pragma Assert (Saved_Phase = W.Current (Object));
   end Run;
begin
   for Mode in 0 .. 13 loop Run (Mode); end loop;
   Ada.Text_IO.Put_Line ("PASS: WOPCM lock/readback/order/conflict/quarantine (14 cases)");
end WOPCM_Tests;
