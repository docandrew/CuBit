pragma Ada_2022;

package body Device_Memory_Authority with SPARK_Mode is

   function Authorize
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   is
   begin
      return Physical_Ranges.Device_Bytes (M, Base, Bytes)
        and then Holds (Cap, Base, Bytes, Mode);
   end Authorize;

   function Authorize_Firmware
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   is
   begin
      return Firmware_Holds (Cap, Base, Bytes, Mode)
        and then Firmware_Mode (Kind_Of (Cap.authorityTag), Mode)
        and then Physical_Ranges.Firmware_Bytes (M, Base, Bytes, Kind_Of (Cap.authorityTag));
   end Authorize_Firmware;

   procedure Lemma_PCI_Config_Read_Only_Uncached
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) is
   begin
      pragma Assert (Kind_Of (Cap.authorityTag) = Physical_Ranges.PCI_Config_Memory);
      for P in Physical_Ranges.Page_Of (Base) .. Physical_Ranges.Page_Of (Base + (Bytes - 1)) loop
         pragma Assert (Physical_Ranges.Firmware_Page (M, P, Physical_Ranges.PCI_Config_Memory));
         pragma Assert (Physical_Ranges.In_PCI_Config (M, P));
         pragma Loop_Invariant
           (for all Q in Physical_Ranges.Page_Of (Base) .. P => Physical_Ranges.In_PCI_Config (M, Q));
      end loop;
   end Lemma_PCI_Config_Read_Only_Uncached;

   procedure Lemma_Authorities_Disjoint
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   is
   begin
      if Physical_Ranges.Device_Bytes (M, Base, Bytes)
        and then Physical_Ranges.NVS_Page (M, Physical_Ranges.Page_Of (Base))
      then
         -- An NVS span wholly contains the first page, so it touches the
         -- range, which Blocker_Free forbids for a non-mappable class.
         pragma Assert
           (for some I in 1 .. M.Count =>
              M.Spans (I).Class = Physical_Ranges.ACPI_NVS
              and then Physical_Ranges.Touches (M.Spans (I), Physical_Ranges.Page_Of (Base)));
         pragma Assert (False);
      end if;
   end Lemma_Authorities_Disjoint;

   procedure Lemma_Firmware_Mapping_Excludes_RAM
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) is
   begin
      Physical_Ranges.Lemma_Firmware_Range_Excludes_RAM
        (M, Base, Base + (Bytes - 1), Kind_Of (Cap.authorityTag));
   end Lemma_Firmware_Mapping_Excludes_RAM;

   function Explain_Mint
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64) return Explanation
   is
      Found : Boolean;
      Page  : Physical_Ranges.Page_Number;
   begin
      if not Valid_Range (Base, Bytes) then
         return (Why => Pub.Invalid_Range, Page => 0,
                 Class => Physical_Ranges.Unreported);
      elsif not (M.Sealed and then M.Complete) then
         return (Why => Pub.Classification_Unavailable, Page => 0,
                 Class => Physical_Ranges.Unreported);
      end if;
      Physical_Ranges.First_Blocked_Page
        (M, Base, Base + (Bytes - 1), Found, Page);
      -- Mint_Admissible is False on a valid range of a sealed, complete
      -- map only because a page is blocked.
      pragma Assert (Found);
      return (Why   => Pub.Blocked_Page,
              Page  => Page * Physical_Ranges.Page_Bytes,
              Class => Physical_Ranges.Class_Of (M, Page));
   end Explain_Mint;

   function Explain
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Explanation is
   begin
      if not Physical_Ranges.Device_Bytes (M, Base, Bytes) then
         return Explain_Mint (M, Base, Bytes);
      end if;
      return (Why => Pub.No_Covering_Capability, Page => 0,
              Class => Physical_Ranges.Unreported);
   end Explain;

   procedure Lemma_Authorized_Mapping_Excludes_RAM
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) is
   begin
      Physical_Ranges.Lemma_Device_Range_Excludes_RAM
        (M, Base, Base + (Bytes - 1));
   end Lemma_Authorized_Mapping_Excludes_RAM;

   procedure Lemma_Minted_Range_Excludes_RAM
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64) is
   begin
      Physical_Ranges.Lemma_Device_Range_Excludes_RAM
        (M, Base, Base + (Bytes - 1));
   end Lemma_Minted_Range_Excludes_RAM;

   procedure Lemma_Attenuation_Does_Not_Amplify
     (Child, Parent : Capability; Base, Bytes : Unsigned_64; Mode : Access_Mode)
   is
   begin
      pragma Assert (Child.rights (RIGHT_READ) <= Parent.rights (RIGHT_READ));
      pragma Assert (Child.rights (RIGHT_WRITE) <= Parent.rights (RIGHT_WRITE));
   end Lemma_Attenuation_Does_Not_Amplify;

end Device_Memory_Authority;
