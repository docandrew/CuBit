--  An instance of netstack's IPv4_ICMP for proof: gnatprove analyzes a
--  generic through its instances. Send's precondition is the generic's, so
--  every frame IPv4_ICMP sends is proved Emittable. The callbacks count, so
--  that they are effects, as they are in netstack.
with Interfaces; use Interfaces;
with IPv4_Header;
with IPv4_Frame;
with IPv4_ICMP;

package IPv4_ICMP_Proof with SPARK_Mode is
   Frames, Answers, Errors : Natural := 0;
   procedure Send (Frame : IPv4_Header.Bytes) with
     Global => (In_Out => Frames), Pre => IPv4_Frame.Emittable (Frame);
   procedure Echo_Answered (From : IPv4_Header.Address; Sequence : Unsigned_16) with
     Global => (In_Out => Answers);
   procedure Error_Arrived (Message : IPv4_Header.Bytes) with Global => (In_Out => Errors);
   package ICMP is new IPv4_ICMP
     (Send => Send, Echo_Answered => Echo_Answered, Error_Arrived => Error_Arrived);
end IPv4_ICMP_Proof;
