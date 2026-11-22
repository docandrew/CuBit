pragma Ada_2022;

package body Boot_Generations with SPARK_Mode is

   function Judge (F : Pin_Facts) return Pin_Verdict is
   begin
      if not F.Entry_Known then
         return Unknown_Service;
      elsif not F.First_For_Entry then
         return Duplicate_Pin;
      elsif not F.Program_Readable then
         return Program_Unreadable;
      elsif not F.Content_Matches then
         return Content_Mismatch;
      elsif not F.Image_Readable then
         return Image_Unreadable;
      elsif not F.Same_Manifest then
         return Manifest_Changed;
      else
         return Applied;
      end if;
   end Judge;

end Boot_Generations;
