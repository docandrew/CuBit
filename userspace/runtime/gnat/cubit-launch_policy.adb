package body CuBit.Launch_Policy with SPARK_Mode => On is
   use type Interfaces.Unsigned_64;

   function Desktop_Approval
     (Name : String; Sender, Desktop_PID : Interfaces.Unsigned_64)
      return Network_Approval
   is
     (if Sender /= 0 and then Desktop_PID /= Interfaces.Unsigned_64'Last
        and then Sender = Desktop_PID and then Name = "netsurf.app"
      then Browser_Outbound else No_Network);

   function Allows
     (Approval : Network_Approval; Requested : Network_Authority.Scope)
      return Boolean
   is
     (case Approval is
         when No_Network => False,
         when Declared_Network => Network_Authority.Valid (Requested),
         when Browser_Outbound => Network_Authority.Includes
           (Network_Authority.Broad_Outbound_TCP, Requested));
end CuBit.Launch_Policy;
