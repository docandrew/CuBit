with Proof_Instance;
with Buddy_Boot_Admission;
package Reservation_Properties with SPARK_Mode, Ghost is
   package Core renames Proof_Instance;
   use type Buddy_Boot_Admission.Admission_Source;

   procedure Reserve_Twice
     (This : in out Core.State; Left_Size, Right_Size : Core.Request_Size;
      Left, Right : out Core.Frame) with
     Post =>
       (if Left /= 0 and then Right /= 0 then
          (if Left <= Right then Right - Left >= Left_Size
           else Left - Right >= Right_Size))
       and then
       (if Left /= 0 then Core.Highest (This) >= Left + (Left_Size - 1));

   procedure Reserve_For_Handoff
     (This : in out Core.State; Size : Core.Request_Size; First : out Core.Frame)
     with Post =>
       (if First /= 0 then First <= Core.Frame'Last - Size + 1 and then
          (for all Item in First .. First + (Size - 1) =>
             not Core.Is_Free (This, Item) and then
             Buddy_Boot_Admission.Source_Of
               (Buddy_Boot_Admission.Frame (Item),
                Buddy_Boot_Admission.Frame (Core.Highest (This))) =
                  Buddy_Boot_Admission.Boot_Bitmap));
end Reservation_Properties;
