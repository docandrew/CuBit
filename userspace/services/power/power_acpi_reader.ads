------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One read of acpi.svc's AC adapter and batteries (PWR-004 slice 1): the
--  requests Power_ACPI_Facts names, in order, through Ask.
--
--  @description
--  Each family is asked within Maximum_Age first; a sample older than it
--  (Too_Old) is asked again at Any_Age and marked Stale, so the last
--  reading is shown as such rather than dropped. The batteries are read
--  against one inventory generation: a page answered Stale (the inventory
--  changed meanwhile) starts the batteries again, at most Battery_Attempts
--  times; after that they are left unread (no battery source). Any reply
--  acpi.svc never writes, or no reply, leaves that family unread: never a
--  value. Ask is power.svc's call to acpi.svc in the service, and
--  acpi.svc's own codec in tests/power-sources.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Power_ACPI_Facts;

generic
   --  One request to acpi.svc's observer endpoint: Label is the reply's,
   --  Delivered False when there was no reply.
   with procedure Ask
     (Request : Power_ACPI_Facts.Words; Label : out Unsigned_32; Reply : out Power_ACPI_Facts.Words;
      Delivered : out Boolean);
package Power_ACPI_Reader with SPARK_Mode is
   package AF renames Power_ACPI_Facts;

   Battery_Attempts : constant := 3;

   type Reading is record
      AC        : AF.AC_Facts;
      Batteries : AF.Battery_Array := [others => AF.Unread_Battery];
      Count     : AF.Battery_Count := 0;
      --  Each family's cache generation the values were read against (0:
      --  that family is unread): what its identities are read against.
      AC_Generation, Battery_Generation : AF.Word := 0;
      --  How many requests the read took (the log's line).
      Requests  : Natural := 0;
   end record;

   --  AC_Maximum_Age: the adapter's own (Any_Age: as fresh as acpi.svc's
   --  copy, which its Notify keeps current while the watch is live).
   procedure Read (Maximum_Age : AF.Word; Result : out Reading; AC_Maximum_Age : AF.Word := 0)
   with Pre => Maximum_Age <= AF.Any_Age and then AC_Maximum_Age <= AF.Any_Age;
end Power_ACPI_Reader;
