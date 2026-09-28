--  Hosted fixture: the kernel Config drags in hardware units. Keep
--  MAX_SMP_CPUS equal to kernel/src/config.ads.
package Config is
    MAX_SMP_CPUS : constant := 8;
end Config;
