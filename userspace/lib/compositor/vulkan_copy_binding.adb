with Vulkan_Copy_FFI;
package body Vulkan_Copy_Binding with SPARK_Mode is
   procedure Draw_Client
     (Borrowed : System.Address;
      Target_Width, Target_Height, Source_Width, Source_Height : Natural;
      Destination : Desktop_Composition.Rectangle;
      Clipped : Boolean; Clip : Desktop_Composition.Rectangle;
      Result : out Outcome) is
      P : constant Desktop_Composition.Blit_Plan := Desktop_Composition.Plan
        (Target_Width, Target_Height, Source_Width, Source_Height,
         Destination, Clipped, Clip);
      Accepted : Boolean;
   begin
      if P.Width = 0 or else P.Height = 0 then
         Result := Empty;
      else
         Vulkan_Copy_FFI.Record_Plan (Borrowed, P, Accepted);
         Result := (if Accepted then Recorded else Rejected);
      end if;
   end Draw_Client;
end Vulkan_Copy_Binding;
