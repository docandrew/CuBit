pragma Ada_2022;
with System;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with ACPI_Passive_Query.Pages;
generic
   with procedure Acquire_Writable_Grant
     (Loan : CuBit.Grant_References.Reference; Expected_Owner : CuBit.Process_IDs.Process_ID;
      Byte_Length : ACPI_Passive_Query.Word; Address : out System.Address; Accepted : out Boolean);
   with procedure Return_Grant (Loan : CuBit.Grant_References.Reference; Accepted : out Boolean);
package ACPI_Grant_Byte_IO with SPARK_Mode => Off is
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   package References renames CuBit.Grant_References;
   package Processes renames CuBit.Process_IDs;
   type Acquisition is private;
   No_Acquisition : constant Acquisition;
   function Active (Item : Acquisition) return Boolean;
   -- Trusted native edge. Acquire callback must use kernel-authenticated
   -- expected owner, offset zero and write access. Address comes exclusively
   -- from that callback, never from a request/reference arithmetic. Kernel
   -- must retain the mapped extent until return while this receiver lives.
   procedure Acquire
     (Loan : References.Reference; Expected_Owner : Processes.Process_ID;
      Byte_Length : Wire.Word; Item : out Acquisition; Accepted : out Boolean);
   -- Exact acquired extent only, separate private arrays. A successful acquire
   -- with an unusable/null mapping is still Active: copies refuse, but the loan
   -- policy can return the acquisition rather than silently dropping a hold.
   procedure Copy_In (Item : Acquisition; Data : out Pages.Byte_Array; Accepted : out Boolean);
   procedure Copy_Out (Item : Acquisition; Data : Pages.Byte_Array; Accepted : out Boolean);
   -- Use only through the loan policy's single held state; this record is not
   -- an independent kernel acquisition identity or a client retirement receipt.
   procedure Release (Loan : References.Reference; Item : in out Acquisition; Accepted : out Boolean);
private
   type Acquisition is record
      Held : Boolean := False;
      Loan : References.Reference;
      Address : System.Address := System.Null_Address;
      Length : Wire.Word := 0;
   end record;
   No_Acquisition : constant Acquisition := (others => <>);
   function Active (Item : Acquisition) return Boolean is (Item.Held);
end ACPI_Grant_Byte_IO;
