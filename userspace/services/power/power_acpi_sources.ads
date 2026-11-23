------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One reading of acpi.svc's power sources (PWR-004): the AC adapter's and
--  batteries' values (Power_ACPI_Reader), mapped (Power_Source_Mapping),
--  each named by its stable ACPI identity (Power_ACPI_Identity).
--
--  @description
--  A source's path is read in seven-byte chunks against the cache
--  generation its values were read against. A chunk answered Stale (that
--  family moved on meanwhile) starts the whole reading again, at most
--  Attempts times: values and names always belong to one generation. A
--  path that cannot be read (Not_Ready, Too_Long, an answer acpi.svc never
--  writes, no answer) leaves that source its slot name.
--  Ask is power.svc's call to acpi.svc in the service and acpi.svc's own
--  codecs in tests/power-sources.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Power_Sources;
with Power_ACPI_Facts;
with Power_ACPI_Identity;
with Power_ACPI_Reader;
with Power_Source_Mapping;

generic
   with procedure Ask
     (Request : Power_ACPI_Facts.Words; Label : out Unsigned_32; Reply : out Power_ACPI_Facts.Words;
      Delivered : out Boolean);
package Power_ACPI_Sources with SPARK_Mode is
   package AF renames Power_ACPI_Facts;
   package ID renames Power_ACPI_Identity;
   package PS renames CuBit.Power_Sources;
   package PM renames Power_Source_Mapping;
   package Reader is new Power_ACPI_Reader (Ask);

   Attempts : constant := 3;

   type Path_Outcome is (Known, Moved_On, Unknown);

   --  The path of source Index read against its family's Expected
   --  generation (nonzero). Moved_On: that family's generation changed.
   procedure Read_Path
     (Index : ID.Source_Index; Expected : AF.Word; Value : out ID.Path; Outcome : out Path_Outcome;
      Requests : in out Natural)
   with Pre  => Expected /= 0,
        Post => (if Outcome /= Known then Value.Length = 0);

   type Reading is record
      Sources : PS.Source_Array := [others => (others => <>)];
      Count   : PS.Source_Count := 0;
      Paths   : ID.Path_Array := [others => ID.Unknown_Path];
      --  How many requests it took (the log's line).
      Requests : Natural := 0;
   end record;

   --  AC_Maximum_Age as Power_ACPI_Reader.Read's (0: Maximum_Age).
   procedure Read (Maximum_Age : AF.Word; Result : out Reading; AC_Maximum_Age : AF.Word := 0)
   with Pre => Maximum_Age <= AF.Any_Age and then AC_Maximum_Age <= AF.Any_Age;
end Power_ACPI_Sources;
