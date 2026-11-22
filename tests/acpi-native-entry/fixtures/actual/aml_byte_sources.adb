pragma Ada_2022;
package body AML_Byte_Sources with SPARK_Mode is
   function Source_Length (Data : aliased Storage) return Natural is (Data.Length);
   function Source_Byte (Data : aliased Storage; Index : Natural)
      return Firmware_Tables.Byte is (Data.Bytes (Data.Bytes'First + Index));
end AML_Byte_Sources;
