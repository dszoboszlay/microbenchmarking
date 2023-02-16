#!/bin/bash
ESCRIPT=_build/default/bin/microbenchmarking
CPU_FREQ=1.8GHz
CPU_CNT=$(nproc)
CPUS=$(seq 0 $(( CPU_CNT - 1 )))
read -r CPU_MIN CPU_MAX <<<$(cpufreq-info -l)

# Fix CPU frequency for the first two tests
for CPU in $CPUS; do
  sudo cpufreq-set -c $CPU -g performance --min ${CPU_FREQ} --max ${CPU_FREQ}
done

$ESCRIPT locked-cpu
sudo nice -n -20 $(which escript) $ESCRIPT high-prio+locked-cpu
sudo chown -R $USER high-prio+locked-cpu

# Unlock the CPU frequency
for CPU in $CPUS; do
  sudo cpufreq-set -c $CPU -g powersave --min ${CPU_MIN} --max ${CPU_MAX}
done

$ESCRIPT default
sudo nice -n -20 $(which escript) $ESCRIPT high-prio
sudo chown -R $USER high-prio
