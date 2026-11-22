pragma Ada_2022;
with CuBit.Handles;
with CuBit.Launch_Configurations;
with GPIO_Kernel_Regions;
with GPIO_Native_Policy;
package body GPIO_Native_Launch is
   package R renames GPIO_Native_Resources;
   package P renames R.P;
   package LC renames Launch_Configuration;
   use type P.Address, LC.Physical_Address, R.G.Memory_Resource_Ordinal;
   Required_Windows : constant := Integer (R.G.Memory_Resource_Ordinal'Last) + 1;
   type Descriptions is array (R.G.Memory_Resource_Ordinal) of P.Description;
   procedure Validate (Caps : R.Authorities; Result : out Report) is
      Config : LC.Generic_ACPI_Configuration;
      Valid : Boolean;
      Seen : Descriptions;
   begin
      Result := (others => <>);
      CuBit.Launch_Configurations.Get (Config, Valid);
      if not Valid or else not LC.Valid_Generic_ACPI (Config) then return; end if;
      if Config.HID /= R.G.Target_HID then Result.Status := Wrong_Controller; return; end if;
      if Config.Epoch = 0 or else Config.Segments = 0 then Result.Status := Invalid_Identity; return; end if;
      for I in 1 .. Config.Segments loop
         if Config.Path (I) (1) not in 'A' .. 'Z' | '_' then Result.Status := Invalid_Identity; return; end if;
      end loop;
      if Config.Window_Total /= Required_Windows then Result.Status := Wrong_Window_Count; return; end if;
      for I in R.G.Memory_Resource_Ordinal loop
         Result.At_Resource := I;
         if not CuBit.Handles.Valid (Caps (I)) then Result.Status := Missing_Authority; return; end if;
         GPIO_Kernel_Regions.Inspect (Caps (I), Seen (I));
         declare
            D : P.Description renames Seen (I);
            W : LC.Physical_Span renames Config.Windows (Natural (I) + 1);
         begin
            if not D.Current or else not D.Device_Memory or else not D.Readable or else not D.Writable then
               Result.Status := Invalid_Authority; return;
            end if;
            if D.First /= P.Address (W.First) or else D.Bytes /= P.Address (W.Bytes) then
               Result.Status := Window_Mismatch; return;
            end if;
            if D.First = 0 or else D.First mod P.Page_Bytes /= 0 or else D.Bytes = 0
              or else D.Bytes mod P.Page_Bytes /= 0 or else D.Bytes > GPIO_Native_Policy.Maximum_Community_Bytes
              or else D.First > P.Address'Last - D.Bytes
            then Result.Status := Invalid_Window; return; end if;
            for J in R.G.Memory_Resource_Ordinal'First .. I loop
               if J < I and then D.First < Seen (J).First + Seen (J).Bytes
                 and then Seen (J).First < D.First + D.Bytes
               then Result.Status := Overlapping_Windows; return; end if;
            end loop;
         end;
      end loop;
      Result.Parent := Config.Line;
      Result.Status := Validated;
   end Validate;
end GPIO_Native_Launch;
