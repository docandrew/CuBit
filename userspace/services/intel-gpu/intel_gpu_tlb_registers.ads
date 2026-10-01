with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_TLB_Registers with SPARK_Mode is
   -- Intel IHD-OS-TGL-Vol2c-12.21 p1332 (PDF page1362):
   -- GFX_TLB_INV_CR. TGL-style RCS register, NOT a universal GPU layout.
   -- Linux v6.16 gt/intel_tlb.c cross-check: forcewake, reset serialization,
   -- per-engine request and bounded wait for request-bit clear.
   -- Writing this register is insufficient on its own: the PRM requires the
   -- engine pipeline flushed and all its memory accesses cleared first.
   -- This package describes copied words only; it grants no MMIO authority.
   GFX_Offset : constant Interfaces.Unsigned_32 := 16#CED8#;
   -- Same documented field layout: Vol2c p1334 (PDF1364).
   -- ADL-N uses Linux's ADL-P platform and Wa_2207587034 OA request.
   OA_Offset : constant Interfaces.Unsigned_32 := 16#CEEC#;
   type Bit is mod 2 with Size => 1;
   type Bits_31 is mod 2 ** 31 with Size => 31;
   type GFX_Invalidate_Register is record
      Request : Bit := 0;           -- R/W; hardware clears after completion
      Reserved : Bits_31 := 0;      -- RO, must write zero
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for GFX_Invalidate_Register use record
      Request at 0 range 0 .. 0;
      Reserved at 0 range 1 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, GFX_Invalidate_Register);
   function Encode is new Ada.Unchecked_Conversion
     (GFX_Invalidate_Register, Interfaces.Unsigned_32);
   -- Poll only the documented status field, never equality of whole words.
   -- Caller must establish a valid read after its own request; idle zero
   -- before issuance is not evidence that any translation was invalidated.
   function Pending (Raw : Interfaces.Unsigned_32) return Boolean is
     (Decode (Raw).Request /= 0);
end Intel_GPU_TLB_Registers;
