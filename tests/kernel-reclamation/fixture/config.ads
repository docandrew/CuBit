-- Host fixture: only the CPU-count dependency of the production policy core.
-- The kernel's own proof/build uses its real Config package.
package Config is
    MAX_SMP_CPUS : constant := 8;
    MAX_SLAB_EXPAND_TIMES : constant := 8;
end Config;
