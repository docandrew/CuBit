pragma Ada_2022;
with Interfaces;
with System;
with System.Storage_Elements;
package body I2C_Register_Window is
   use type P.Outcome;
   use type L.Register_ID;
   Attempted, Mapped : Boolean := False;
   Base : System.Address := System.Null_Address;
   type Register_Word is new Interfaces.Unsigned_32 with Volatile_Full_Access;
   for Register_Word'Size use 32;
   function Ready return Boolean is (Mapped);
   procedure Bind
     (Authority : Authority_Type; Physical, BAR_Bytes, Virtual : P.Address;
      Result : out Report) is
      Metadata : P.Description;
   begin
      Result := (others => <>);
      if Attempted then Result.Status := Already_Attempted; return; end if;
      Attempted := True;
      Inspect (Authority, Metadata);
      Result.Admission := P.Admit (Metadata, Physical, BAR_Bytes, Virtual);
      if Result.Admission /= P.Allowed then return; end if;
      Map_One_Page (Authority, Physical, Virtual, Mapped);
      if not Mapped then Result.Status := Map_Refused; return; end if;
      Base := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (Virtual));
      Result.Status := Bound;
   end Bind;
   -- Private helpers are called only with compile-time named word offsets.
   -- Bind established page alignment, one mapped page and non-wrapping user VA.
   function Load (Offset : Natural) return R.Word is
      use System.Storage_Elements;
      Item : Register_Word with Import, Address => Base + Storage_Offset (Offset);
   begin return R.Word (Item); end Load;
   procedure Store (Offset : Natural; Value : R.Word) is
      use System.Storage_Elements;
      Item : Register_Word with Import, Address => Base + Storage_Offset (Offset);
   begin Item := Register_Word (Value); end Store;
   procedure Reset_Read (ID : L.Register_ID; Value : out L.Word; Status : out L.Access_Status) is
   begin
      Value := 0; Status := L.Unavailable;
      if not Mapped then return; end if;
      Value := Load (Natural (L.Offset (ID))); Status := L.Access_OK;
   end Reset_Read;
   procedure Reset_Write (ID : L.Register_ID; Value : L.Word; Status : out L.Access_Status) is
   begin
      Status := L.Unavailable; if not Mapped then return; end if;
      if ID = L.Capabilities then Status := L.Denied; return; end if;
      Store (Natural (L.Offset (ID)), Value); Status := L.Access_OK;
   end Reset_Write;
   procedure Config_Read (ID : C.Register_ID; Value : out C.Word; Status : out C.Access_Status) is
   begin
      Value := 0; Status := C.Unavailable; if not Mapped then return; end if;
      if ID not in C.Enable_Status | C.Controller_Status | C.TX_Level | C.RX_Level |
        C.Data_Command | C.Abort_Source | C.Clear_Interrupts then Status := C.Denied; return; end if;
      Value := Load (Natural (C.Offset (ID))); Status := C.Access_OK;
   end Config_Read;
   procedure Config_Write (ID : C.Register_ID; Value : C.Word; Status : out C.Access_Status) is
   begin
      Status := C.Unavailable; if not Mapped then return; end if;
      if ID not in C.Interrupt_Mask | C.Enable | C.Standard_High | C.Standard_Low |
        C.Fast_High | C.Fast_Low | C.SDA_Hold | C.Control | C.Target |
        C.TX_Threshold | C.RX_Threshold then Status := C.Denied; return; end if;
      Store (Natural (C.Offset (ID)), Value); Status := C.Access_OK;
   end Config_Write;
   procedure Data_Read (ID : R.Read_Register; Value : out R.Word; Status : out R.Access_Status) is
   begin
      Value := 0; Status := R.Unavailable; if not Mapped then return; end if;
      Value := Load (Natural (R.Offset (ID))); Status := R.Access_OK;
   end Data_Read;
   procedure Data_Write (ID : R.Write_Register; Value : R.Word; Status : out R.Access_Status) is
   begin
      Status := R.Unavailable; if not Mapped then return; end if;
      Store (Natural (R.Offset (ID)), Value); Status := R.Access_OK;
   end Data_Write;
end I2C_Register_Window;
