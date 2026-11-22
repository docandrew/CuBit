pragma Ada_2022;
with System.Storage_Elements;
package body ACPI_Grant_Byte_IO with SPARK_Mode => Off is
   use type Wire.Word;
   use type System.Address;
   use type References.Reference;
   use type System.Storage_Elements.Integer_Address;
   type Mapped_Bytes is array (Positive range <>) of Pages.Byte with Volatile_Components;
   procedure Acquire
     (Loan : References.Reference; Expected_Owner : Processes.Process_ID;
      Byte_Length : Wire.Word; Item : out Acquisition; Accepted : out Boolean) is
      Address : System.Address;
   begin
      Item := No_Acquisition; Accepted := False;
      if not Processes.Is_Process (Expected_Owner) or else Byte_Length < Pages.Frame_Byte_Count
        or else Byte_Length > Wire.Word (Natural'Last) then return; end if;
      Acquire_Writable_Grant (Loan, Expected_Owner, Byte_Length, Address, Accepted);
      if Accepted then Item := (True, Loan, Address, Byte_Length); end if;
   end Acquire;
   function Accessible (Item : Acquisition; Length : Wire.Word) return Boolean is
     (Item.Held and then Item.Address /= System.Null_Address
      and then Item.Length in Pages.Frame_Byte_Count .. Wire.Word (Natural'Last)
      and then Length = Item.Length
      and then System.Storage_Elements.To_Integer (Item.Address) <=
        System.Storage_Elements.Integer_Address'Last - System.Storage_Elements.Integer_Address (Length - 1));
   procedure Copy_In (Item : Acquisition; Data : out Pages.Byte_Array; Accepted : out Boolean) is
   begin
      Data := [others => 0]; Accepted := False;
      if not Accessible (Item, Wire.Word (Data'Length)) then return; end if;
      declare
         View : Mapped_Bytes (1 .. Positive (Item.Length)) with Import, Address => Item.Address;
      begin
         for I in Data'Range loop Data (I) := View (1 + (I - Data'First)); end loop;
      end;
      Accepted := True;
   end Copy_In;
   procedure Copy_Out (Item : Acquisition; Data : Pages.Byte_Array; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Accessible (Item, Wire.Word (Data'Length)) then return; end if;
      declare
         View : Mapped_Bytes (1 .. Positive (Item.Length)) with Import, Address => Item.Address;
      begin
         for I in Data'Range loop View (1 + (I - Data'First)) := Data (I); end loop;
      end;
      Accepted := True;
   end Copy_Out;
   procedure Release (Loan : References.Reference; Item : in out Acquisition; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Item.Held and then Loan = Item.Loan then Return_Grant (Loan, Accepted); end if;
      if Accepted then Item := No_Acquisition; end if;
   end Release;
end ACPI_Grant_Byte_IO;
