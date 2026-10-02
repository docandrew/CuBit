package body Compositor_Close_Request with SPARK_Mode is
   procedure Request (Pending, Next_Serial : in out Word) is
   begin
      if Pending = 0 then IQ.Reserve (Next_Serial, Pending); end if;
   end Request;
   procedure Acknowledge (Pending : in out Word; After : Word) is
   begin
      if Pending <= After then Pending := 0; end if;
   end Acknowledge;
end Compositor_Close_Request;
