with Interfaces; use Interfaces;
-- Page-granularity authority, not per-register isolation. These contain the
-- ADL-N reset, engine and GuC control registers; they exclude display/GGTT pages.
package Intel_GPU_Reset_Pages with SPARK_Mode, Pure is
   Virtual_Base : constant Unsigned_64 := 16#6120_0000#;
   subtype Page_Index is Natural range 0 .. 15;
   -- Page 15 (0xA000): GEN6_RPNSWREQ and GEN6_PMINTRMSK, for GuC SLPC's ARAT
   -- timer unmask and the RP0 software-request fallback (Intel_GPU_SLPC).
   Frequency_Page : constant Page_Index := 15;
   function Offset (Index : Page_Index) return Unsigned_64 is
     (case Index is
         when 0 => 16#9000#, when 1 => 16#2000#, when 2 => 16#22000#,
         when 3 => 16#1C0000#, when 4 => 16#1D0000#, when 5 => 16#1C8000#,
         when 6 => 16#C000#, when 7 => 16#138000#,
         when 8 => 16#190000#, when 9 => 16#4000#, when 10 => 16#B000#,
         when 11 => 0, when 12 => 16#E000#,
         when 13 => 16#1C3000#, when 14 => 16#1D3000#,
         when 15 => 16#A000#)
     with Post => Offset'Result mod 4096 = 0 and Offset'Result < 16#200000#;
   -- devmgr answers each page's grant request with exactly that page
   -- (CAP-002: the reply carries the device memory; no slot is chosen).
end Intel_GPU_Reset_Pages;
