with ACPI_EC_Types;
package EC_Proof_Boundary with SPARK_Mode,
  Abstract_State => (Device with External => Async_Writers) is
   procedure Read (Register : ACPI_EC_Types.Read_Register;
                   Value : out ACPI_EC_Types.Byte;
                   Result : out ACPI_EC_Types.Access_Result)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Write (Register : ACPI_EC_Types.Write_Register;
                    Value : ACPI_EC_Types.Byte;
                    Result : out ACPI_EC_Types.Access_Result)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Clock (Now : out ACPI_EC_Types.Microseconds;
                    Available : out Boolean)
     with Import, Global => (In_Out => Device), Always_Terminates;
   function Owned return Boolean
     with Import, Global => (Input => Device), Volatile_Function;
   -- Arbitrary callback outputs. Declared effects, termination, no owner-state
   -- aliasing/reentry and truthful I/O outcomes/ownership/time remain trusted.
end EC_Proof_Boundary;
