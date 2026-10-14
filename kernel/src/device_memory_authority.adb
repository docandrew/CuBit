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
