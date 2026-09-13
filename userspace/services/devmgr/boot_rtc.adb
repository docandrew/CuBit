with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with Civil_Time;
package body Boot_RTC is
   procedure Read (UTC_Seconds, Monotonic_Ms : out Unsigned_64;
                   Valid : out Boolean) is
      type Registers is array (Positive range 1 .. 7) of Unsigned_8;
      Ports : constant Registers := [0, 2, 4, 7, 8, 9, 16#0B#];
      A, B : Registers;
      OK : Boolean := True;
      function Get (Index : Unsigned_8) return Unsigned_8 is
         Result : Unsigned_64;
      begin
         --  Kernel boot CMOS users have finished. Match its NMI-enabled
         --  selector convention; no competing runtime CMOS owner is admitted.
         Result := portOutp8 (16#70#, Index);
         if Result = Unsigned_64'Last then OK := False; return 0; end if;
         Result := portInp8 (16#71#);
         if Result > 255 then OK := False; return 0; end if;
         return Unsigned_8 (Result);
      end Get;
      function Number (Raw : Unsigned_8; Binary : Boolean) return Natural is
      begin
         if Binary then return Natural (Raw); end if;
         if (Raw and 15) > 9 or else Shift_Right (Raw, 4) > 9 then
            OK := False; return 0;
         end if;
         return Natural (Raw and 15) + 10 * Natural (Shift_Right (Raw, 4));
      end Number;
      Y, M, D, H, N, S : Natural;
      Binary, PM : Boolean;
      Date : Civil_Time.Date_Time;
   begin
      UTC_Seconds := 0; Monotonic_Ms := 0; Valid := False;
      --  Bounded retries with two identical snapshots outside the RTC update
      --  window. Failed/invalid hardware never blocks boot indefinitely.
      for Attempt in 1 .. 20 loop
         if (Get (16#0A#) and 16#80#) = 0 then
            for I in A'Range loop A (I) := Get (Ports (I)); end loop;
            for I in B'Range loop B (I) := Get (Ports (I)); end loop;
            Monotonic_Ms := syscall (SYSCALL_GETTIME);
            if OK and then A = B and then (Get (16#0A#) and 16#80#) = 0
              and then (Get (16#0D#) and 16#80#) /= 0
            then
               Binary := (A (7) and 4) /= 0; PM := (A (3) and 16#80#) /= 0;
               S := Number (A (1), Binary); N := Number (A (2), Binary);
               H := Number (A (3) and 16#7F#, Binary);
               D := Number (A (4), Binary); M := Number (A (5), Binary);
               Y := Number (A (6), Binary);
               if (A (7) and 2) = 0 then
                  if H not in 1 .. 12 then return; end if;
                  H := H mod 12 + (if PM then 12 else 0);
               end if;
               --  This initial RTC seed supports 2000..2099. The hardware
               --  must contain UTC; do not guess local-zone/DST interpretation.
               if not OK or else Y > 99 or else M not in 1 .. 12 or else
                 D not in 1 .. 31 or else H > 23 or else N > 59 or else S > 59
               then return; end if;
               Date := (2000 + Y, M, D, H, N, S);
               if not Civil_Time.Valid (Date) then return; end if;
               UTC_Seconds := Unsigned_64 (Civil_Time.Seconds (Date));
               Valid := Monotonic_Ms /= Unsigned_64'Last;
               return;
            end if;
         end if;
         declare
            Ignore : constant Unsigned_64 := syscall (SYSCALL_SLEEP, 2);
         begin null; end;
      end loop;
   end Read;
end Boot_RTC;
