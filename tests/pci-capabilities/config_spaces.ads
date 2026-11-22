--  A configuration space in memory, and the walk instantiated over it
--  (the shape devmgr's instance has over its configuration reads).
pragma Ada_2022;
with Interfaces; use Interfaces;
with PCI_Capabilities; use PCI_Capabilities;

package Config_Spaces with SPARK_Mode => On is
   type Space is array (Config_Offset) of Unsigned_8;
   Current : Space := [others => 0];

   function Read (Offset : Config_Offset) return Unsigned_8 is (Current (Offset))
   with Global => Current;

   package Walk is new PCI_Capabilities.Walks (Read);
end Config_Spaces;
