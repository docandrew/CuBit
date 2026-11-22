pragma Ada_2022;
with Interfaces;
package I2C_Mapping_Policy with SPARK_Mode, Pure is
   type Address is new Interfaces.Unsigned_64;
   Page_Bytes : constant Address := 4096;
   Register_Span_Bytes : constant Address := 16#300#;
   User_Virtual_Limit : constant Address := 16#0000_8000_0000_0000#;
   type Description is record
      Current, Device_Memory, Readable, Writable : Boolean := False;
      First, Bytes : Address := 0;
   end record;
   type Outcome is (Allowed, No_Current_Authority, Wrong_Kind, Missing_Rights,
      Invalid_Physical, Invalid_Virtual, Short_Window, Different_BAR);
   function Admit
     (Authority : Description; Physical, BAR_Bytes, Virtual : Address) return Outcome
     with Post =>
       (if Admit'Result = Allowed then
          Authority.Current and then Authority.Device_Memory
          and then Authority.Readable and then Authority.Writable
          and then Physical = Authority.First
          and then Physical /= 0 and then Physical mod Page_Bytes = 0
          and then Physical <= Address'Last - Page_Bytes
          and then BAR_Bytes >= Page_Bytes and then Authority.Bytes >= Page_Bytes
          and then Virtual >= Page_Bytes and then Virtual mod Page_Bytes = 0
          and then Virtual <= User_Virtual_Limit - Page_Bytes);
   -- Deliberately maps ONE page for the named register prefix, not the entire
   -- BAR. The kernel rechecks the real handle/range and memory-map eligibility.
end I2C_Mapping_Policy;
