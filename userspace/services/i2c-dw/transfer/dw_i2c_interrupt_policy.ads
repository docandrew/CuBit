pragma Ada_2022;
with DW_I2C_Transfer;
package DW_I2C_Interrupt_Policy with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   subtype Mask is T.Word;
   use type Mask;
   RX_Underflow : constant Mask := 16#001#;
   RX_Overflow : constant Mask := 16#002#;
   RX_Full : constant Mask := 16#004#;
   TX_Overflow : constant Mask := 16#008#;
   TX_Empty : constant Mask := 16#010#;
   TX_Abort : constant Mask := 16#040#;
   Stop_Detected : constant Mask := 16#200#;
   Error_Sources : constant Mask := RX_Underflow or RX_Overflow or TX_Overflow or TX_Abort;
   All_Sources : constant Mask := Error_Sources or RX_Full or TX_Empty or Stop_Detected;
   function Wanted (Need : T.Wake_Requirement; Hardware_Wait : Boolean) return Mask
     with Post => (Wanted'Result and not All_Sources) = 0 and then
       (if not Hardware_Wait or else not Need.Active then Wanted'Result = 0);
   -- Controller owner supplies Hardware_Wait only after a successful status
   -- sample and a No_Action planner turn. Mask zero for software progress,
   -- pending accesses, idle or terminal state. Do not enable TX-empty while
   -- waiting for read credits or after all commands have been queued.
   -- These named DesignWare source bits select wakeups, not completion or
   -- permission to release hardware/views. Fault cleanup remains with owner.
end DW_I2C_Interrupt_Policy;
