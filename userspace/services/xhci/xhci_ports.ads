with Interfaces; use Interfaces;

package XHCI_Ports with SPARK_Mode => On, Pure is
   type Reset_Action is (Disconnected, Wait_For_Reset, Already_Enabled, Start_Reset);
   function Before_Enumeration (Status : Unsigned_32) return Reset_Action;
   function Reset_Write (Status : Unsigned_32) return Unsigned_32;
end XHCI_Ports;
