with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_VM_Table_Store is
   pragma Compile_Time_Error (Page'Object_Size /= 4096 * 8, "table metadata page size");
   function Capacity (Object : Store) return Positive is (Object.Available);
   function Word (Object : Store; Table : Table_ID;
                  Index : Intel_GPU_ADLN_PPGTT.Table_Index) return Unsigned_64 is
   begin
      if Table <= Bootstrap_Tables then return Object.Inline (Table) (Index); end if;
      if Table > Object.Available then return 0; end if;
      declare
         Data : Page with Import, Address => To_Address (Integer_Address
           (Object.Base + (Unsigned_64 (Table) - Unsigned_64 (Bootstrap_Tables) - 1) * 4096));
      begin return Data (Index); end;
   end Word;
   procedure Set_Word (Object : in out Store; Table : Table_ID;
                       Index : Intel_GPU_ADLN_PPGTT.Table_Index; Value : Unsigned_64) is
   begin
      if Table <= Bootstrap_Tables then Object.Inline (Table) (Index) := Value; return; end if;
      if Table > Object.Available then return; end if;
      declare
         Data : Page with Import, Address => To_Address (Integer_Address
           (Object.Base + (Unsigned_64 (Table) - Unsigned_64 (Bootstrap_Tables) - 1) * 4096));
      begin Data (Index) := Value; end;
   end Set_Word;
   procedure Clear (Object : in out Store; Table : Table_ID) is
   begin
      for I in Intel_GPU_ADLN_PPGTT.Table_Index loop Set_Word (Object, Table, I, 0); end loop;
   end Clear;
   procedure Copy_Page (Target : in out Store; Target_ID : Table_ID;
                        Source : Store; Source_ID : Table_ID) is
   begin
      for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
         Set_Word (Target, Target_ID, I, Word (Source, Source_ID, I));
      end loop;
   end Copy_Page;
   procedure Extend (Object : in out Store; Base, Bytes : Unsigned_64;
                     Accepted : out Boolean) is
      New_Count : Positive;
   begin
      Accepted := False;
      if Base = 0 or else Base mod 4096 /= 0 or else Bytes = 0 or else Bytes mod 4096 /= 0
        or else Base > Unsigned_64'Last - Bytes or else Bytes <= Object.Bytes
        or else Bytes - Object.Bytes > 65536
        or else Bytes / 4096 > Unsigned_64 (Table_Quota - Bootstrap_Tables)
        or else (Object.Base /= 0 and then Object.Base /= Base)
      then return; end if;
      New_Count := Bootstrap_Tables + Natural (Bytes / 4096);
      -- Caller has already committed this exact suffix. Publish availability
      -- only after its initialization; serialized owner, no callbacks here.
      for T in Object.Available + 1 .. New_Count loop
         declare
            Data : Page with Import, Address => To_Address (Integer_Address
              (Base + Unsigned_64 (T - Bootstrap_Tables - 1) * 4096));
         begin Data := [others => 0]; end;
      end loop;
      Object.Base := Base; Object.Bytes := Bytes; Object.Available := New_Count;
      Accepted := True;
   end Extend;
end Intel_GPU_VM_Table_Store;
