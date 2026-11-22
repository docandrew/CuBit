with AML_Names;
with Firmware_Tables;
package ACPI_Resource_Names with Pure, SPARK_Mode is
   use type AML_Names.Parse_Status;
   -- Exact NUL-terminated external resource-source bytes, not AML NameString
   -- bytecode. ASCII lowercase folds to uppercase and short segments pad '_'.
   -- Root/parent prefixes and at most255 segments/prefixes are bounded.
   -- Reject empty names/segments, embedded NUL, excess segment length and
   -- non-name bytes. Prefix-only paths identify a scope; the resolver must
   -- separately require a current Device. Repeated root prefixes normalize.
   function Parse (Source : Firmware_Tables.Bytes) return AML_Names.Name_Result
     with Global => null,
       Post => (if Parse'Result.Kind = AML_Names.Accepted then
          Parse'Result.Consumed = Source'Length
          and then (not Parse'Result.Rooted or else Parse'Result.Parents = 0)
          and then (Parse'Result.Count > 0 or else Parse'Result.Rooted or else Parse'Result.Parents > 0)
          and then (for all I in 1 .. Parse'Result.Count => AML_Names.Valid (Parse'Result.Parts (I))));
end ACPI_Resource_Names;
