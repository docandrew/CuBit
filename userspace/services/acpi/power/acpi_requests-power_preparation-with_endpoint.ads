pragma Ada_2022;
with Interfaces; with ACPI_Local_Sample_Status;
generic
   with procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64; Available : out Boolean);
   with function Register_Model return Objects.Register_Model;
   with function Owner_Ready (Server : State) return Boolean;
   Fuel : Positive := ACPI_Local_Sample_Status.Default_Fuel;
-- Trusted wire adapter: Stamp is kernel receive metadata, not a message word.
-- Recognizes Prepare only; native dispatch must retain other protocol owners.
package ACPI_Requests.Power_Preparation.With_Endpoint with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   procedure Dispatch (Server : in out State; Item : in out Run;
      Stamp : Interfaces.Unsigned_64; Request : Packet;
      Reply : out Packet; Handled : out Boolean)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
end ACPI_Requests.Power_Preparation.With_Endpoint;
