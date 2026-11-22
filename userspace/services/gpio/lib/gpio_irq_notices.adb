pragma Ada_2022;
package body GPIO_IRQ_Notices with SPARK_Mode is
   use type CuBit.Process_IDs.Process_ID;
   procedure Configure (S : in out State; Parent_Present : Boolean; Accepted : out Boolean) is
   begin
      Accepted := not S.Configured;
      if not Accepted then return; end if;
      S.Configured := True; S.Present := Parent_Present;
   end Configure;
   procedure Note (S : in out State; Publisher : CuBit.Process_IDs.Process_ID;
      IRQ_Doorbell, Frame_Valid : Boolean; Accepted : out Boolean) is
   begin
      Accepted := S.Configured and then S.Present and then
         Publisher = CuBit.Process_IDs.No_Process and then IRQ_Doorbell and then Frame_Valid;
      if Accepted then S.Pending := True; S.Held := True; end if;
   end Note;
   procedure Take (S : in out State; Pending : out Boolean) is
   begin Pending := S.Pending; S.Pending := False; end Take;
   procedure Hold (S : in out State; Held : Boolean) is
   begin S.Held := Held or else S.Pending; end Hold;
   function Requests_Held (S : State) return Boolean is (S.Held);
   function Parent_Present (S : State) return Boolean is (S.Configured and then S.Present);
end GPIO_IRQ_Notices;
