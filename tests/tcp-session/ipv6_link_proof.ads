--  An instance of netstack's IPv6_Link for proof: gnatprove analyzes a
--  generic through its instances. Send's precondition is the generic's,
--  so every frame IPv6_Link sends is proved Emittable. Frames and log lines
--  are counted so that sending is an effect, as it is in netstack.
with IPv6_Header;
with IPv6_Frame;
with IPv6_Link;

package IPv6_Link_Proof with SPARK_Mode is
   Frames, Lines : Natural := 0;
   procedure Send (Frame : IPv6_Header.Bytes) with
     Global => (In_Out => Frames), Pre => IPv6_Frame.Emittable (Frame);
   procedure Log (Text : String) with Global => (In_Out => Lines);
   package Link is new IPv6_Link (Send => Send, Log => Log);
end IPv6_Link_Proof;
