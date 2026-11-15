with Fixture;
package body CuBit.Launch_Configurations is
   procedure Get (Config : out Launch_Configuration.Generic_PCI_Configuration; Valid : out Boolean) is
   begin
      Config := (others => <>); Config.Vendor := 16#8086#; Config.Device := 16#54E8#;
      Config.Window_Total := 1; Config.Windows (1).First := 16#1234_5678_9000#;
      Config.Windows (1).Bytes := 4096; Config.Delivery := Fixture.Delivery; Config.Vectors := Fixture.Vectors;
      Valid := True;
   end Get;
end CuBit.Launch_Configurations;
