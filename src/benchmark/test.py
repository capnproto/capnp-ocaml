#!/usr/bin/env python
import subprocess, os, csv, time, sys
my_dir = os.path.abspath(os.path.dirname(sys.argv[0]))
os.chdir(my_dir)

subprocess.check_call(["dune", "build", "carsales", "catrank", "eval"])
bin_dir = os.path.join(my_dir, '../../_build/default/src/benchmark')

switch = subprocess.check_output(["opam", "sw", "show"]).strip()

print "Current switch: %s" % switch

baseline = {
        ('./carsales', 'pipe', 'none'): 67433620.95888832,
        ('./catrank', 'pipe', 'packed'): 65141369.52621308,
        ('./eval', 'pipe', 'none'): 61535819.20844552,
}

results = {}

def run(cmd, base_iters, scale):
    key = '-'.join(([cmd[0][2:]] + cmd[1:]))
    base = baseline[tuple(cmd)]
    iters = int(base_iters * scale)
    cmd = cmd + [str(iters)]
    t0 = time.time()
    throughput = int(subprocess.check_output(cmd, cwd = bin_dir))
    t1 = time.time()
    t = t1 - t0
    rate = throughput / t
    frac = 100 * rate / base
    cmd = " ".join(cmd)
    print "%6.2f%% of baseline: %3.1f x %s" % (frac, scale, key)
    if scale not in results: results[scale] = {}
    results[scale][key] = frac

scale = 1.0
while scale < 10:
    run(["./carsales", "pipe", "none"], 5000, scale)
    run(["./catrank", "pipe", "packed"], 500, scale)
    run(["./eval", "pipe", "none"], 50000, scale)
    scale *= 1.5

series = sorted(results[1.0].keys())
scales = sorted(results.keys())

with open('results-%s.csv' % switch, 'wb') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["Scale"] + series)
    for scale in scales:
        res = [results[scale][k] for k in series]
        writer.writerow([scale] + res)
