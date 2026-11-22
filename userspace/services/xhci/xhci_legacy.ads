with Interfaces; use Interfaces;

--  xHCI sections 7.1.1/7.1.2. Pure decoding only; no hardware access.
package XHCI_Legacy with SPARK_Mode => On, Pure is
   BIOS_OWNED : constant Unsigned_32 := 16#0001_0000#;
   OS_OWNED : constant Unsigned_32 := 16#0100_0000#;
   LEGACY_ID : constant Unsigned_32 := 1;
   function First_Offset (HCC : Unsigned_32) return Unsigned_64 is
     (Unsigned_64 (Shift_Right (HCC, 16)) * 4);
   function Next_Distance (Header : Unsigned_32) return Unsigned_64 is
     (Unsigned_64 (Shift_Right (Header, 8) and 255) * 4);
   function Fits (Offset, Size, Mapped_Bytes : Unsigned_64) return Boolean is
     (Offset >= 16#20# and then Offset mod 4 = 0 and then
      Size <= Mapped_Bytes and then Offset <= Mapped_Bytes - Size);
   function Disable_SMIs (Control : Unsigned_32) return Unsigned_32 is
     --  Preserve reserved fields, clear SMI enables, acknowledge RW1C events.
     ((Control and 16#000E_1FEE#) or 16#E000_0000#);
end XHCI_Legacy;
