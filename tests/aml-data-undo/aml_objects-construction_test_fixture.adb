pragma Ada_2022;
package body AML_Objects.Construction_Test_Fixture is
   procedure Dirty_Unused (Store : in out State; Existing : Object_ID) is
   begin
      for I in Store.Bytes_Used + 1 .. Store.Byte_Capacity loop
         Store.Bytes (I) := AML_Decode.Byte (I mod 251 + 1);
      end loop;
      for I in Store.Elements_Used + 1 .. Store.Element_Capacity loop
         Store.Elements (I) := Existing;
      end loop;
   end Dirty_Unused;
   procedure Retire_Integer (Store : in out State; ID : Object_ID) is
   begin
      if not Is_Live (Store, ID) or else Kind (Store, ID) /= Integer_Object then
         raise Program_Error;
      end if;
      Store.Objects (ID) := (Stamp => Store.Objects (ID).Stamp, others => <>);
      Store.Live_Used := Store.Live_Used - 1;
   end Retire_Integer;
end AML_Objects.Construction_Test_Fixture;
