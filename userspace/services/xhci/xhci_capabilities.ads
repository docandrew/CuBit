with Interfaces; use Interfaces;

--  Pure register decoding, shared by the native driver and hosted tests.
package XHCI_Capabilities with SPARK_Mode => On, Pure is
   subtype Scratchpad_Buffer_Count is Natural range 0 .. 1023;

   --  xHCI 1.2, section 5.3.4 (HCSPARAMS2). The low-significance
   --  field is at the HIGHER register position; bit 26 is unrelated.
   function Scratchpad_Count (Parameters : Unsigned_32)
      return Scratchpad_Buffer_Count;
end XHCI_Capabilities;
