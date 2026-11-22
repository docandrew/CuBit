with ACPI_Requests;
package ACPI_Native_Button_Discovery is
   type Outcome is (Source_Unavailable);
   procedure Start (Server : in out ACPI_Requests.State; Fixed_Available : Boolean; Result : out Outcome);
   procedure Route (Server : ACPI_Requests.State; Accepted : out Boolean);
   procedure Begin_Stop;
   function Quiet return Boolean is (True);
   procedure Close (Released : out Boolean);
end ACPI_Native_Button_Discovery;
