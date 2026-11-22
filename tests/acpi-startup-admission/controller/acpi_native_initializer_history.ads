with Interfaces;
package ACPI_Native_Initializer_History is
 type Report is record Invocations:Interfaces.Unsigned_32:=3;Ordered_Digest:Interfaces.Unsigned_64:=16#0123_4567_89AB_CDEF#;Complete:Boolean:=True;end record;
 function Observe return Report;
end ACPI_Native_Initializer_History;
