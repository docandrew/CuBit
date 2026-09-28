package body Intel_GPU_Probe with SPARK_Mode is
   function Identify
     (Vendor, Device : Unsigned_16; Base_Class : Unsigned_8)
      return Platform is
   begin
      if Vendor /= 16#8086# or else Base_Class /= 16#03# then
         return Unrecognized;
      end if;
      --  Exact PCI identities, not CPU model or an entire device-ID prefix.
      --  Source: Linux include/drm/intel/pciids.h, KBL_ULT_GT2 / ADLN.
      --  Recognition does not assert native modesetting/rendering support.
      case Device is
         when 16#5916# | 16#5921# => return Kaby_Lake_ULT_GT2;
         when 16#46D0# .. 16#46D4# => return Alder_Lake_N;
         when others => return Unrecognized;
      end case;
   end Identify;

   function Decode_BAR
     (Low, High : Unsigned_32; Has_Upper_Word : Boolean) return BAR_Result
   is
      Result : BAR_Result;
   begin
      if (Low and 1) /= 0 then
         Result.Status := IO_Space;
         return Result;
      end if;
      case Low and 6 is
         when 0 => Result.Status := Memory_32;
         when 4 =>
            Result.Words := 2;
            if not Has_Upper_Word then
               Result.Status := Missing_Upper_Word;
               return Result;
            end if;
            Result.Status := Memory_64;
         when others =>
            -- Below-1MiB legacy memory BAR and reserved type are not
            -- admitted for this driver's register/aperture resources.
            Result.Status := Unsupported_Encoding;
            return Result;
      end case;
      Result.Base := Unsigned_64 (Low and 16#FFFF_FFF0#);
      if Result.Status = Memory_64 then
         Result.Base := Result.Base or Shift_Left (Unsigned_64 (High), 32);
      end if;
      if Result.Base = 0 then
         Result.Status := Unassigned;
      else
         Result.Prefetchable := (Low and 8) /= 0;
      end if;
      return Result;
   end Decode_BAR;

   function Contains_Register
     (Mapping_Base, Mapping_Bytes, Offset : Unsigned_64) return Boolean is
     (Mapping_Base /= 0
      and then Mapping_Base mod 4 = 0
      and then Offset mod 4 = 0
      and then Mapping_Bytes >= 4
      and then Mapping_Bytes - 1 <= Unsigned_64'Last - Mapping_Base
      and then Offset <= Mapping_Bytes - 4);

   function Register_Address
     (Mapping_Base, Mapping_Bytes, Offset : Unsigned_64) return Unsigned_64 is
      pragma Unreferenced (Mapping_Bytes);
   begin
      return Mapping_Base + Offset;
   end Register_Address;
end Intel_GPU_Probe;
