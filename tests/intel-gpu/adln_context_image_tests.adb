with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Image; use Intel_GPU_ADLN_Context_Image;
with Intel_GPU_ADLN_LRC_Initial;
with Intel_GPU_ADLN_LRC_Workaround;
with Ada.Text_IO;
procedure ADLN_Context_Image_Tests is
   Context : constant Unsigned_64 := 16#100000#;
   Result : Prepared_Image;
   Predicate : constant array (Natural range 0 .. 10) of Unsigned_32 :=
     [16#10400002#,16#10EFF8#,0,0,16#05008000#,16#00800000#,
      16#10400002#,16#10EFF8#,0,1,16#05000000#];
   Initial : constant Intel_GPU_ADLN_LRC_Initial.Initial_State :=
     Intel_GPU_ADLN_LRC_Initial.Build (16#110000#, 16#200000#, 12);
   Indirect : constant Intel_GPU_ADLN_LRC_Workaround.Indirect_Batch :=
     Intel_GPU_ADLN_LRC_Workaround.Build (Context, 65536);
begin
   Result := Build (Context, 65536, 16#110000#, 16#200000#, 12);
   pragma Assert (Result.Valid);
   for I in Result.Words'Range loop
      if I = 1043 then pragma Assert (Result.Words (I) = 16#10F005#);
      elsif I = 1045 then pragma Assert (Result.Words (I) = 16#10E002#);
      elsif I = 1047 then pragma Assert (Result.Words (I) = 16#340#);
      elsif I in 1024 .. 2047 then
         pragma Assert (Result.Words (I) = Initial.Registers (I - 1024));
      elsif I in 14336 .. 14367 then
         pragma Assert (Result.Words (I) = Indirect.Words (I - 14336));
      elsif I in 14848 .. 14858 then
         pragma Assert (Result.Words (I) = Predicate (I - 14848));
      elsif I = 15360 then pragma Assert (Result.Words (I) = 16#05000000#);
      else pragma Assert (Result.Words (I) = 0);
      end if;
   end loop;
   pragma Assert (Result.Words (14849) = 16#10EFF8#);
   pragma Assert (Result.Words (14855) = 16#10EFF8#);
   pragma Assert (Result.Words (14852) = 16#05008000#);
   for Offset in Unsigned_64 range 0 .. 15 loop
      Result := Build (Context, 65536, Context + Offset * 4096, 4096, 12);
      pragma Assert (not Result.Valid);
      pragma Assert (for all Word of Result.Words => Word = 0);
   end loop;
   pragma Assert (Build (Context, 65536, Context - 4096, 4096, 12).Valid);
   pragma Assert (not Build (Context, 65536, Context - 4096, 4096, 13).Valid);
   pragma Assert (not Build (Context, 65535, 16#110000#, 4096, 12).Valid);
   pragma Assert (not Build (Context, 65536, 16#110000#, 0, 12).Valid);
   Ada.Text_IO.Put_Line ("ADL-N64KiB context composition PASS (offline)");
end ADLN_Context_Image_Tests;
