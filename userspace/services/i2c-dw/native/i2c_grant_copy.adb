pragma Ada_2022;
with System.Storage_Elements;
package body I2C_Grant_Copy is
   package Q renames DW_I2C_Request_Types;
   package T renames DW_I2C_Transfer;
   use type T.Count;
   use type System.Address;
   use type System.Storage_Elements.Integer_Address;
   function Valid_Range (Address : System.Address; Length : Positive) return Boolean is
     (Address /= System.Null_Address and then
      System.Storage_Elements.To_Integer (Address) <=
        System.Storage_Elements.Integer_Address'Last -
          System.Storage_Elements.Integer_Address (Length - 1));
   procedure Read_Input
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : out Bytes; Result : out Q.Copy_Result) is
      Address : System.Address;
      Acquired, Returned : Boolean;
      Position : T.Count := Data'First;
   begin
      Data := [others => 0]; Result := Q.Copied;
      if Data'Length = 0 then return; end if;
      if Data'Length > T.Count'Last then Result := Q.Refused; return; end if;
      Acquire (Grant, Caller, Interfaces.Unsigned_64 (Data'Length), False, Address, Acquired);
      if not Acquired then Result := Q.Refused; return; end if;
      if not Valid_Range (Address, Data'Length) then
         Release (Grant, Returned);
         Result := (if Returned then Q.Refused else Q.Uncertain); return;
      end if;
      declare
         Shared : Bytes (1 .. T.Count (Data'Length)) with Import, Address => Address, Volatile;
      begin
         for I in Shared'Range loop
            Data (Position) := Shared (I);
            if I /= Shared'Last then Position := Position + 1; end if;
         end loop;
      end;
      Release (Grant, Returned);
      if not Returned then Data := [others => 0]; Result := Q.Uncertain; end if;
   end Read_Input;
   procedure Write_Output
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : Bytes; Result : out Q.Copy_Result) is
      Address : System.Address;
      Acquired, Returned : Boolean;
      Position : T.Count := Data'First;
   begin
      Result := Q.Copied;
      if Data'Length = 0 then return; end if;
      if Data'Length > T.Count'Last then Result := Q.Refused; return; end if;
      Acquire (Grant, Caller, Interfaces.Unsigned_64 (Data'Length), True, Address, Acquired);
      if not Acquired then Result := Q.Refused; return; end if;
      if not Valid_Range (Address, Data'Length) then
         Release (Grant, Returned);
         Result := (if Returned then Q.Refused else Q.Uncertain); return;
      end if;
      declare
         Shared : Bytes (1 .. T.Count (Data'Length)) with Import, Address => Address, Volatile;
      begin
         for I in Shared'Range loop
            Shared (I) := Data (Position);
            if I /= Shared'Last then Position := Position + 1; end if;
         end loop;
      end;
      Release (Grant, Returned);
      if not Returned then Result := Q.Uncertain; end if;
   end Write_Output;
end I2C_Grant_Copy;
