pragma Ada_2022;

package body CuBit.Service_Replacement with SPARK_Mode is

   function Refusal_Of (Word : Unsigned_64) return Refusal is
     (case Word is
        when Refusal'Enum_Rep (Malformed) => Malformed,
        when Refusal'Enum_Rep (Grant_Unavailable) => Grant_Unavailable,
        when Refusal'Enum_Rep (Unknown_Service) => Unknown_Service,
        when Refusal'Enum_Rep (Not_Replaceable) => Not_Replaceable,
        when Refusal'Enum_Rep (Not_Granted) => Not_Granted,
        when Refusal'Enum_Rep (Switch_Off) => Switch_Off,
        when Refusal'Enum_Rep (Unsigned_Refused) => Unsigned_Refused,
        when Refusal'Enum_Rep (Busy) => Busy,
        when Refusal'Enum_Rep (Image_Unreadable) => Image_Unreadable,
        when Refusal'Enum_Rep (Program_Unreadable) => Program_Unreadable,
        when others => Manifest_Changed);

   function Authorize (Facts : Authority_Facts) return Decision is
   begin
      if not Facts.Entry_Known then
         return (Admitted => False, Why => Unknown_Service);
      elsif not Facts.Entry_Replaceable then
         return (Admitted => False, Why => Not_Replaceable);
      elsif not Facts.Requester_Stages then
         return (Admitted => False, Why => Not_Granted);
      elsif not Facts.Switch_On then
         return (Admitted => False, Why => Switch_Off);
      elsif not Signature_Admits (Facts.Signature, Facts.Switch_On) then
         return (Admitted => False, Why => Unsigned_Refused);
      elsif Facts.In_Progress then
         return (Admitted => False, Why => Busy);
      end if;
      return (Admitted => True);
   end Authorize;

   function Authorize_For_Boot (Facts : Authority_Facts) return Decision is
   begin
      if not Facts.Entry_Known then
         return (Admitted => False, Why => Unknown_Service);
      elsif not Facts.Requester_Stages then
         return (Admitted => False, Why => Not_Granted);
      elsif not Facts.Switch_On then
         return (Admitted => False, Why => Switch_Off);
      elsif not Signature_Admits (Facts.Signature, Facts.Switch_On) then
         return (Admitted => False, Why => Unsigned_Refused);
      end if;
      return (Admitted => True);
   end Authorize_For_Boot;

   function Content_Admits
     (Image_Readable, Program_Readable, Same_Manifest : Boolean) return Decision is
   begin
      if not Image_Readable then
         return (Admitted => False, Why => Image_Unreadable);
      elsif not Program_Readable then
         return (Admitted => False, Why => Program_Unreadable);
      elsif not Same_Manifest then
         return (Admitted => False, Why => Manifest_Changed);
      end if;
      return (Admitted => True);
   end Content_Admits;

end CuBit.Service_Replacement;
