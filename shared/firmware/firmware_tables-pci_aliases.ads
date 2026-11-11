pragma Ada_2022;
with Firmware_Tables.MCFG;
-- Metadata lookup only: never grants access to an address. Independent broker,
-- kernel classification and read-only window checks remain mandatory.
package Firmware_Tables.PCI_Aliases with Pure, SPARK_Mode is
   package M renames Firmware_Tables.MCFG;
   type Outcome is (Resolved, Invalid_Table, Invalid_Range, Outside_Window,
                    Crosses_Bus, Ambiguous);
   type Selection (Status : Outcome := Invalid_Table) is record
      case Status is
         when Resolved =>
            Segment : M.Segment_Number;
            Bus : M.Bus_Number;
            First : Address_Value;
         when others => null;
      end case;
   end record;
   -- The complete declared region must fit one advertised bus. Every allocation
   -- is structurally/policy checked; physical overlap with another allocation
   -- refuses selection, including a partial overlap with the requested region.
   function Resolve (Data : Bytes; Base, Length : Address_Value) return Selection;
end Firmware_Tables.PCI_Aliases;
