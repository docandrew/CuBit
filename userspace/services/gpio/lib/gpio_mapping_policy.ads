pragma Ada_2022;
with Interfaces;
package GPIO_Mapping_Policy with SPARK_Mode, Pure is
   type Address is new Interfaces.Unsigned_64;
   Page_Bytes : constant Address := 4096;
   Maximum_Map_Pages : constant Address := 1024;
   User_Virtual_Limit : constant Address := 16#0000_8000_0000_0000#;
   type Description is record
      Current, Device_Memory, Readable, Writable : Boolean := False;
      First, Bytes : Address := 0;
   end record;
   type Outcome is (Allowed, No_Authority, Wrong_Kind, Missing_Rights,
      Invalid_Physical, Invalid_Virtual, Invalid_Extent, Different_Resource);
   function Admit
     (Authority : Description; Physical, Bytes, Virtual : Address;
      Page_Limit : Address) return Outcome;
   -- Exact admitted whole-page resource only; never round into neighboring
   -- bytes. Page_Limit is a required startup limit, not a fallback. Unaligned
   -- firmware windows are refused here, awaiting a mediated access path.
end GPIO_Mapping_Policy;
