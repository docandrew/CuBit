package body Intel_GPU_DMA_Lifetime with SPARK_Mode is
   function Next (Before : State; Action : Event) return State is
   begin
      case Before is
         when Private_Buffer =>
            case Action is
               when Publish => return GPU_Reachable;
               when Retire | Owner_Lost => return Reclaimable;
               when others => return Before;
            end case;
         when GPU_Reachable =>
            case Action is
               when Retire => return Draining;
               when Owner_Lost => return Quarantined;
               when others => return Before;
            end case;
         when Draining =>
            case Action is
               when Stop_Confirmed => return GPU_Stopped;
               when Owner_Lost => return Quarantined;
               when others => return Before;
            end case;
         when GPU_Stopped =>
            case Action is
               when Mapping_Revoked => return Reclaimable;
               when Owner_Lost => return Quarantined;
               when others => return Before;
            end case;
         when Reclaimable | Quarantined => return Before;
      end case;
   end Next;
end Intel_GPU_DMA_Lifetime;
