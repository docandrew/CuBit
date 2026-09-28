with Interfaces; use Interfaces;
with Intel_GPU_Firmware; use Intel_GPU_Firmware;
procedure Firmware_Tests is
   Header : CSS_Header := [others => 0];
   Plan : Layout;
   procedure Put (Offset : Natural; Value : Unsigned_32) is
   begin
      for Byte in 0 .. 3 loop
         Header (Offset + Byte) := Unsigned_8 (Shift_Right (Value, Byte * 8) and 255);
      end loop;
   end Put;
begin
   --  Actual pinned GuC fixture: 128 CSS bytes + 334976 code bytes.
   pragma Assert (Fits_ADLN_WOPCM (2097152, 16384, 2043904, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM (0, 16384, 2043904, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM
     (Unsigned_64'Last, 16384, 2043904, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM
     (2097152, Unsigned_64'Last, 2043904, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM
     (2097152, 16384, Unsigned_64'Last, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM
     (2097152, 16384, 2043904, Unsigned_64'Last, 0));
   --  Exact capacity boundary, and every sub-page size near it.
   for Extra in Unsigned_64 range 0 .. 4096 loop
      pragma Assert (Fits_ADLN_WOPCM
        (2097152, 16384, 2043904 + Extra, 335104, 0) = (Extra = 0));
   end loop;
   pragma Assert (not Fits_ADLN_WOPCM (2097152, 16385, 360448, 335104, 0));
   pragma Assert (Fits_ADLN_WOPCM (2097152, 16384, 360448, 335104, 0));
   pragma Assert (not Fits_ADLN_WOPCM (2097152, 16384, 356352, 335104, 0));
   pragma Assert (Fits_ADLN_WOPCM (2097152, 32768, 360448, 335104, 16384));
   pragma Assert (not Fits_ADLN_WOPCM (2097152, 32768, 360448, 335104, 16388));
   pragma Assert (not Fits_ADLN_WOPCM (2097152, 32768, 360448, 335104,
                                      Unsigned_64'Last));
   pragma Assert (not Decode (Header, 4096).Valid);
   --  32 CSS DWORDs, 64 signature, 64 modulus, one exponent, 16 code.
   Put (4, 161); Put (24, 177); Put (28, 64); Put (32, 64); Put (36, 1);
   for Size in Unsigned_64 range 0 .. 800 loop
      Plan := Decode (Header, Size);
      pragma Assert (Plan.Valid = (Size >= 448));
      if Plan.Valid then
         pragma Assert (Plan.Code_Bytes = 64 and Plan.Signature_Offset = 192 and
                        Plan.Signature_Bytes = 256);
      end if;
   end loop;
   Put (24, 160);
   pragma Assert (not Decode (Header, Unsigned_64'Last).Valid);
   Put (24, 161);
   pragma Assert (not Decode (Header, Unsigned_64'Last).Valid);
   Put (24, 177); Put (28, 0);
   pragma Assert (not Decode (Header, Unsigned_64'Last).Valid);
   --  A wrapped 32-bit header sum must not admit a hostile header.
   Put (4, 29); Put (28, Unsigned_32'Last);
   Put (32, Unsigned_32'Last); Put (36, Unsigned_32'Last);
   pragma Assert (not Decode (Header, Unsigned_64'Last).Valid);
   Put (4, 33); Put (28, 1); Put (32, 0); Put (36, 0);
   Put (24, Unsigned_32'Last);
   Plan := Decode (Header, Unsigned_64'Last);
   pragma Assert (Plan.Valid and Plan.Code_Bytes = (16#FFFF_FFFF# - 33) * 4);
   pragma Assert (not Decode (Header, 4096).Valid);
   Header := [others => 0];
   Put (0, 6); Put (4, 161); Put (8, 16#10000#);
   Put (16, 16#8086#); Put (24, 16#147C1#);
   Put (28, 64); Put (32, 64); Put (36, 1);
   Put (64, 16#463104#); Put (120, 16#801000#);
   pragma Assert (Matches_Selected_ADLN_GuC (Header, 335360));
   pragma Assert (not Matches_Selected_ADLN_GuC (Header, 335359));
   pragma Assert (not Matches_Selected_ADLN_GuC (Header, 335361));
   for Index in Header'Range loop
      if Index in 0 .. 19 or else Index in 24 .. 39 or else
        Index in 64 .. 67 or else Index in 120 .. 123
      then
         declare
            Saved : constant Unsigned_8 := Header (Index);
         begin
            for Bit in 0 .. 7 loop
               Header (Index) := Saved xor Shift_Left (Unsigned_8'(1), Bit);
               pragma Assert (not Matches_Selected_ADLN_GuC (Header, 335360));
            end loop;
            Header (Index) := Saved;
         end;
      end if;
   end loop;
end Firmware_Tests;
