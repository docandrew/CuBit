package body CuBit.Launch_Policy with SPARK_Mode => On is

   function Desktop_Approval
     (Name : String; Sender, Desktop_PID : CuBit.Process_IDs.Process_ID)
      return Network_Approval
   is
     (if CuBit.Process_IDs.Is_Process (Sender)
        and then CuBit.Process_IDs."=" (Sender, Desktop_PID)
        and then Name = "cubitshell.app"
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
