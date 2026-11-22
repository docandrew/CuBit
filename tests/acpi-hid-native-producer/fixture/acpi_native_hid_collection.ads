with Interfaces; with ACPI_Requests; with ACPI_Service; with HID_Offer_Payload;
with GPIO_Bootstrap_Payload;
package ACPI_Native_HID_Collection is
   package Provenance is
      type Stamp is record
         Epoch, Catalog_Revision, Observation_Sequence : Interfaces.Unsigned_64 := 0;
         Initializer_Count : Interfaces.Unsigned_32 := 0;
         Initializer_Digest : Interfaces.Unsigned_64 := 0;
      end record;
   end Provenance;
   package Controller is
      subtype Candidate_Count is Natural range 0 .. 16;
      subtype Candidate_Index is Positive range 1 .. 16;
      type Device_Array is array (Candidate_Index) of ACPI_Service.Node_Descriptor;
      type Payload_Array is array (Candidate_Index) of HID_Offer_Payload.Request;
      type Outcome is (Not_Attempted, Prepared, Stale_Device, Device_Unavailable, Recovery_Required);
      type Report is record
         Status : Outcome := Not_Attempted;
         Working, Recovery_Needed : Boolean := False;
         Charged : Natural := 0;
         Node_Errors : Natural := 0;
         Count : Candidate_Count := 0;
         Status_Known : Boolean := False;
         Status_Flags : Interfaces.Unsigned_64 := 0;
         Values : Payload_Array;
      end record;
      type Run is limited null record;
      procedure Collect (Item : in out Run; Server : in out ACPI_Requests.State;
         Devices : Device_Array; Count : Candidate_Count;
         Admission_Device : ACPI_Service.Node_Descriptor;
         Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
         Budget : Natural; Resource_Bytes : Positive; Result : out Report);
   end Controller;
end ACPI_Native_HID_Collection;
