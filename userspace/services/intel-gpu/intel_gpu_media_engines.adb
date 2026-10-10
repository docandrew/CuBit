package body Intel_GPU_Media_Engines with SPARK_Mode is
   function Count (Set : VDBOX_Set) return VDBOX_Count is
      Result : VDBOX_Count := 0;
   begin
      for I in VDBOX_Instance loop
         if Set (I) then Result := Result + 1; end if;
         pragma Loop_Invariant (Result <= Natural (I) + 1);
      end loop;
      return Result;
   end Count;

   function Count (Set : VEBOX_Set) return VEBOX_Count is
      Result : VEBOX_Count := 0;
   begin
      for I in VEBOX_Instance loop
         if Set (I) then Result := Result + 1; end if;
         pragma Loop_Invariant (Result <= Natural (I) + 1);
      end loop;
      return Result;
   end Count;

   function Decode (Vendor, Device : Unsigned_16; Fuse : Fuse_Word)
     return Engines
   is
      Who : constant Identity := Identify (Vendor, Device);
      Result : Engines;
      Video_Ceiling : VDBOX_Set;
      Enhance_Ceiling : VEBOX_Set;
   begin
      if not Who.Known or else Fuse = Unreadable then return Result; end if;
      Video_Ceiling := VDBOX_Ceiling (Who.Item);
      Enhance_Ceiling := VEBOX_Ceiling (Who.Item);
      Result.Valid := True;
      Result.Render := True;
      Result.Copy := True;
      for I in VDBOX_Instance loop
         Result.Video (I) := Video_Ceiling (I) and not VDBOX_Fused_Off (Fuse, I);
         pragma Loop_Invariant
           (for all J in VDBOX_Instance'First .. I =>
              Result.Video (J) =
                (Video_Ceiling (J) and not VDBOX_Fused_Off (Fuse, J)));
      end loop;
      for I in VEBOX_Instance loop
         Result.Enhance (I) :=
           Enhance_Ceiling (I) and not VEBOX_Fused_Off (Fuse, I);
         pragma Loop_Invariant
           (for all J in VEBOX_Instance'First .. I =>
              Result.Enhance (J) =
                (Enhance_Ceiling (J) and not VEBOX_Fused_Off (Fuse, J)));
      end loop;
      for I in VDBOX_Instance loop
         Result.SFC (I) := Has_SFC (Result.Video, I);
         pragma Loop_Invariant
           (for all J in VDBOX_Instance'First .. I =>
              Result.SFC (J) = Has_SFC (Result.Video, J));
      end loop;
      Result.Video_Count := Count (Result.Video);
      Result.Enhance_Count := Count (Result.Enhance);
      return Result;
   end Decode;

   function Decode_Stable (Vendor, Device : Unsigned_16;
                           First, Second : Fuse_Word) return Engines is
   begin
      if First /= Second then return (others => <>); end if;
      return Decode (Vendor, Device, First);
   end Decode_Stable;

   function Logical (Set : Engines; I : VDBOX_Instance) return VDBOX_Logical is
      Result : VDBOX_Logical := 0;
   begin
      for J in VDBOX_Instance'First .. I loop
         pragma Loop_Invariant (Result <= Natural (J));
         exit when J = I;
         if Set.Video (J) then Result := Result + 1; end if;
      end loop;
      return Result;
   end Logical;
end Intel_GPU_Media_Engines;
