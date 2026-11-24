--  gnatprove analyses a generic through an instance: Power_ACPI_Reader
--  and Power_ACPI_Sources over an Ask that may answer anything (any label,
--  any words, or none).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Power_ACPI_Facts;
with Power_ACPI_Reader;
with Power_ACPI_Sources;

package Reader_Proof with SPARK_Mode is
   procedure Ask
     (Request : Power_ACPI_Facts.Words; Label : out Unsigned_32; Reply : out Power_ACPI_Facts.Words;
      Delivered : out Boolean)
   with Global => null;
   package Reader is new Power_ACPI_Reader (Ask);
   package Sources is new Power_ACPI_Sources (Ask);
end Reader_Proof;
