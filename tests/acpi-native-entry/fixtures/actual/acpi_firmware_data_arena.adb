pragma Ada_2022;
with AML_Identity.Issuer;
with ACPI_Firmware_Data_Layouts;
package body ACPI_Firmware_Data_Arena is
   package L renames ACPI_Firmware_Data_Layouts;
   use type Word, AML_Identity.Identity, Store.Outcome;
   function Observe (Owner : Context) return Statistics is
     ((Owner.Ready, Owner.Working, Owner.First, Owner.Cursor, Owner.Limit,
       Store.Length (Owner.Items), Store.Capacity (Owner.Items)));
   procedure Configure (Owner : in out Context; First, Limit : Word; Result : out Outcome) is
      Issued : Boolean;
   begin
      if Owner.Working then Result := Reentered; return;
      elsif Owner.Consumed then Result := Already_Configured; return; end if;
      Owner.Consumed := True; Result := Invalid_Arena;
      if First = 0 or else First >= Limit or else Limit > L.Virtual_Limit
        or else First mod L.Page_Bytes /= 0 or else Limit mod L.Page_Bytes /= 0 then return; end if;
      AML_Identity.Issuer.Issue (Owner.Identity, Issued);
      if not Issued then Result := Identity_Exhausted; return; end if;
      Owner.First := First; Owner.Cursor := First; Owner.Limit := Limit;
      Owner.Ready := True; Result := Configured;
   end Configure;
   procedure Reserve (Owner : in out Context; Pages : Word; Item : out Reservation; Result : out Outcome) is
      Index : Natural; Status : Store.Outcome;
   begin
      Item := No_Reservation; Result := Not_Ready;
      if Owner.Working then Result := Reentered; return;
      elsif not Owner.Ready then return;
      elsif Pages = 0 then Result := Invalid_Size; return;
      elsif Pages > (Owner.Limit - Owner.Cursor) / L.Page_Bytes then Result := Arena_Exhausted; return; end if;
      Owner.Working := True;
      Store.Append (Owner.Items, (Owner.Cursor, Pages, False), Index, Status);
      Owner.Working := False;
      if Status /= Store.Stored then Result := Allocation_Refused; return; end if;
      Item := (Owner.Identity, Index); Owner.Cursor := Owner.Cursor + Pages * L.Page_Bytes; Result := Reserved;
   end Reserve;
   function Valid (Owner : Context; Item : Reservation) return Boolean is
     (Owner.Ready and then Item.Owner = Owner.Identity and then Item.Index > 0
      and then Item.Index <= Store.Length (Owner.Items));
   procedure Describe (Owner : Context; Item : Reservation; Base, Pages : out Word; Accepted : out Boolean) is
   begin
      Base := 0; Pages := 0; Accepted := not Owner.Working and then Valid (Owner, Item);
      if Accepted then
         declare Entry_Item : constant Entry_Record := Store.Read (Owner.Items, Item.Index); begin
            Base := Entry_Item.Base; Pages := Entry_Item.Pages;
         end;
      end if;
   end Describe;
   procedure Claim (Owner : in out Context; Item : Reservation; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Owner.Working or else not Valid (Owner, Item) then return; end if;
      declare Entry_Item : Entry_Record := Store.Read (Owner.Items, Item.Index); begin
         if Entry_Item.Attempted then return; end if;
         Entry_Item.Attempted := True; Store.Replace (Owner.Items, Item.Index, Entry_Item); Accepted := True;
      end;
   end Claim;
end ACPI_Firmware_Data_Arena;
