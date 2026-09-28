with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Probe; use Intel_GPU_Probe;
with Resource_Tests;
with Boot_Tests;
with Observation_Tests;
with PCI_Power_Tests;
with Firmware_Tests;
procedure Main is
   Expected : Platform;
   Bar : BAR_Result;
begin
   Resource_Tests;
   Boot_Tests;
   Observation_Tests;
   PCI_Power_Tests;
   Firmware_Tests;
   for Device in Unsigned_16 loop
      case Device is
         when 16#5916# | 16#5921# => Expected := Kaby_Lake_ULT_GT2;
         when 16#46D0# .. 16#46D4# => Expected := Alder_Lake_N;
         when others => Expected := Unrecognized;
      end case;
      pragma Assert (Identify (16#8086#, Device, 3) = Expected);
      pragma Assert (Identify (16#1234#, Device, 3) = Unrecognized);
      pragma Assert (Identify (16#8086#, Device, 2) = Unrecognized);
   end loop;
   pragma Assert (not Contains_Register (0, 4096, 0));
   pragma Assert (not Contains_Register (4097, 4096, 0));
   pragma Assert (not Contains_Register (4096, 4096, 1));
   for Size in Unsigned_64 range 0 .. 128 loop
      for Offset in Unsigned_64 range 0 .. 132 loop
         pragma Assert
           (Contains_Register (4096, Size, Offset) =
              (Offset mod 4 = 0 and then Offset + 4 <= Size));
         if Contains_Register (4096, Size, Offset) then
            pragma Assert
              (Register_Address (4096, Size, Offset) = 4096 + Offset);
         end if;
      end loop;
   end loop;
   pragma Assert (Contains_Register (Unsigned_64'Last - 3, 4, 0));
   pragma Assert
     (Register_Address (Unsigned_64'Last - 3, 4, 0) = Unsigned_64'Last - 3);
   pragma Assert (not Contains_Register (Unsigned_64'Last - 3, 5, 0));
   pragma Assert (not Contains_Register (4096, Unsigned_64'Last, 0));
   pragma Assert (not Contains_Register (4096, 4096, Unsigned_64'Last));
   Bar := Decode_BAR (16#D000_0000#, 16#FFFF_FFFF#, False);
   pragma Assert (Bar.Status = Memory_32 and Bar.Base = 16#D000_0000#);
   pragma Assert (Bar.Words = 1 and not Bar.Prefetchable);
   Bar := Decode_BAR (16#8000_000C#, 2, True);
   pragma Assert (Bar.Status = Memory_64 and Bar.Base = 16#2_8000_0000#);
   pragma Assert (Bar.Words = 2 and Bar.Prefetchable);
   Bar := Decode_BAR (4, 1, True);
   pragma Assert (Bar.Status = Memory_64 and Bar.Base = 16#1_0000_0000#);
   pragma Assert (Decode_BAR (4, 0, True).Status = Unassigned);
   pragma Assert (Decode_BAR (0, 1, True).Status = Unassigned);
   pragma Assert (Decode_BAR (4, 1, False).Status = Missing_Upper_Word);
   for Flags in Unsigned_32 range 0 .. 15 loop
      Bar := Decode_BAR (16#D000_0000# or Flags, 0, True);
      if (Flags and 1) /= 0 then
         pragma Assert (Bar.Status = IO_Space and Bar.Base = 0);
      elsif (Flags and 6) in 2 | 6 then
         pragma Assert (Bar.Status = Unsupported_Encoding and Bar.Base = 0);
      else
         pragma Assert (Bar.Base = 16#D000_0000#);
         pragma Assert (Bar.Prefetchable = ((Flags and 8) /= 0));
      end if;
   end loop;
   Bar := Decode_BAR (16#FFFF_FFF4#, 16#FFFF_FFFF#, True);
   pragma Assert (Bar.Base = Unsigned_64'Last - 15);
   Ada.Text_IO.Put_Line ("PASS: Intel identity and register mapping admission");
end Main;
